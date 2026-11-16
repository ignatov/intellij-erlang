"""Talks LSP to `elp server` without the IDE: opens one module and reports elp/status and publishDiagnostics.

Usage: lsp_probe.py ELP PROJECT_DIR STYLE SECONDS [LABEL]
STYLE is the form of the file URIs the client sends: `vscode` (file:///c%3A/..., what the IntelliJ client and VS Code send on
Windows) or `plain` (file:///C:/..., what Path.as_uri gives) or `lower` (the
whole URI in lower case, as ELP itself names the files on Windows).
"""
import json
import os
import pathlib
import subprocess
import sys
import threading
import time


def uri(path, style):
    plain = pathlib.Path(path).resolve().as_uri()
    if style == "vscode" and len(plain) > 10 and plain[9] == ":":
        return "file:///" + plain[8].lower() + "%3A" + plain[10:]
    if style == "lower" and len(plain) > 10 and plain[9] == ":":
        return "file:///" + plain[8].lower() + "%3a" + plain[10:].lower()
    return plain


def main():
    elp, project, style, seconds = sys.argv[1], sys.argv[2], sys.argv[3], int(sys.argv[4])
    label = sys.argv[5] if len(sys.argv) > 5 else style
    project = str(pathlib.Path(project).resolve())
    stderr_path = os.path.join(os.path.dirname(project), "lsp-%s-stderr.txt" % label)
    stderr = open(stderr_path, "wb")
    proc = subprocess.Popen([elp, "server"], cwd=project, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=stderr)
    lock = threading.Lock()
    events = []
    next_id = [0]

    def send(message):
        body = json.dumps(message).encode()
        proc.stdin.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
        proc.stdin.flush()

    def request(method, params):
        next_id[0] += 1
        send({"jsonrpc": "2.0", "id": next_id[0], "method": method, "params": params})

    def reader():
        out = proc.stdout
        while True:
            length = None
            while True:
                line = out.readline()
                if not line:
                    return
                line = line.strip()
                if not line:
                    break
                if line.lower().startswith(b"content-length:"):
                    length = int(line.split(b":")[1])
            message = json.loads(out.read(length))
            with lock:
                events.append((time.time(), message))
            if "id" in message and "method" in message:
                result = [{}] * len(message.get("params", {}).get("items", [])) if message["method"] == "workspace/configuration" else None
                send({"jsonrpc": "2.0", "id": message["id"], "result": result})

    threading.Thread(target=reader, daemon=True).start()
    start = time.time()
    root = uri(project, style)
    request("initialize", {
        "processId": os.getpid(), "rootUri": root, "workspaceFolders": [{"uri": root, "name": "project"}],
        "capabilities": {
            "window": {"workDoneProgress": True},
            "textDocument": {"publishDiagnostics": {"relatedInformation": True}, "definition": {}},
            "experimental": {"serverStatusNotification": True},
        },
    })
    time.sleep(1)
    send({"jsonrpc": "2.0", "method": "initialized", "params": {}})
    target = os.path.join(project, "src", "shop_defects.erl")
    target_uri = uri(target, style)
    with open(target, encoding="utf-8") as f:
        text = f.read()
    send({"jsonrpc": "2.0", "method": "textDocument/didOpen",
          "params": {"textDocument": {"uri": target_uri, "languageId": "erlang", "version": 1, "text": text}}})
    found = None
    while time.time() - start < seconds and proc.poll() is None and found is None:
        time.sleep(0.5)
        with lock:
            for at, message in events:
                if message.get("method") == "textDocument/publishDiagnostics":
                    texts = [d.get("message", "") for d in message["params"]["diagnostics"]]
                    if any("unused_function/0 is unused" in t for t in texts):
                        found = (at - start, message["params"]["uri"], len(texts))
    with lock:
        statuses = [m["params"].get("status") for _, m in events if m.get("method") == "elp/status"]
        published = {}
        for _, m in events:
            if m.get("method") == "textDocument/publishDiagnostics":
                published[m["params"]["uri"]] = len(m["params"]["diagnostics"])
        logs = [m["params"].get("message", "")[:160] for _, m in events if m.get("method") in ("window/logMessage", "window/showMessage")]
    print("LSP %s (style %s) sent %s" % (label, style, target_uri))
    print("LSP %s elp/status %s" % (label, statuses))
    print("LSP %s unused_function diagnostic: %s" % (
        label, "after %.1f s in %s (%d diagnostics)" % found if found else "NOT received in %d s" % seconds))
    print("LSP %s publishDiagnostics by uri: %s" % (label, json.dumps(published)[:900]))
    print("LSP %s messages: %s" % (label, logs[:6]))
    print("LSP %s server alive %s" % (label, proc.poll() is None))
    try:
        request("shutdown", None)
        time.sleep(1)
        send({"jsonrpc": "2.0", "method": "exit"})
        proc.wait(10)
    except Exception as e:
        print("LSP %s stop: %r" % (label, e))
        proc.kill()
    stderr.close()
    with open(stderr_path, encoding="utf-8", errors="replace") as f:
        lines = [l.rstrip() for l in f if "panicked" in l or "Unknown eqwalizer" in l or "poisoned" in l]
    print("LSP %s stderr panics: %s" % (label, lines[:6]))
    return 0 if found else 1


if __name__ == "__main__":
    sys.exit(main())
