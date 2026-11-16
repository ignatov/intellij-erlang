"""Records the answers of the real ELP server on the fixture project, for the tests that feed them to the plugin's ELP code.

Usage: python3 -I record_answers.py ELP_BINARY WORK_DIR

The fixture project next to this script (project/) is copied to a new directory under WORK_DIR, because ELP must not see the
project through a symlinked path, and nothing is ever written to the fixture itself. The script starts `ELP_BINARY server` in the
copy, opens every module and header, waits until ELP has loaded the project and published its diagnostics (eqWAlizer's too),
sends the requests of CASES and writes the answers to answers/ next to this script, one JSON file per method, with the ELP version
in answers/manifest.json. Absolute paths are replaced by placeholders: ${PROJECT_URI} and ${PROJECT_PATH} for the copy of the
project, ${OTP_URI} and ${OTP_PATH} for the OTP root, so that a test substitutes its own project root.

Rename answers are only recorded, never applied. Standard library only; Python 3.9 or newer.
"""
import datetime
import json
import os
import shutil
import subprocess
import sys
import threading
import time

HERE = os.path.dirname(os.path.abspath(__file__))
FIXTURE = os.path.join(HERE, "project")
ANSWERS = os.path.join(HERE, "answers")

# A position is a needle in a file of the project with "|" at the caret: the first occurrence of the needle without the "|".
DEFINITION = [
    ("remote_function", "src/shop.erl", "shop_backend:look|up(shop_memory"),
    ("remote_module", "src/shop.erl", "shop_back|end:lookup("),
    ("local_function", "src/shop.erl", "sum([pr|ice(Name)"),
    ("local_function_in_macro_argument", "src/shop.erl", "?LOG(pr|ice(Name))"),
    ("variable", "src/shop.erl", "lookup(shop_memory, Na|me)"),
    ("variable_in_macro_argument", "src/shop.erl", "?PRICE(It|em)"),
    ("record", "src/shop.erl", "describe(#it|em{"),
    ("record_field", "src/shop.erl", "#item{na|me = Name"),
    ("record_in_other_module", "src/shop_memory.erl", "[#it|em{name = apple"),
    ("macro", "src/shop.erl", "?DEFAULT_|PRICE"),
    ("macro_with_arguments", "src/shop.erl", "?PR|ICE(Item)"),
    ("macro_in_header", "src/shop.hrl", "price = ?DEFAULT_|PRICE"),
    ("remote_type", "src/shop.erl", "price(shop_backend:k|ey())"),
    ("local_type", "src/shop_backend.erl", "lookup(module(), k|ey())"),
    ("behaviour", "src/shop_memory.erl", "-behaviour(shop_back|end)"),
    ("include", "src/shop.erl", "-include(\"sh|op.hrl\")"),
    ("otp_function", "src/shop_memory.erl", "lists:keyfi|nd("),
    ("dynamic_call", "src/shop_backend.erl", "Backend:fe|tch(Key)"),
    ("function_declaration", "src/shop.erl", "\npr|ice(Name) ->"),
    ("callback_implementation", "src/shop_memory.erl", "\nfe|tch(Key) ->"),
]

# At the start of the declaration's name: where ElpFindUsagesHandlerFactory asks for the usages of the plugin's target.
REFERENCES = [
    ("module", "src/shop_backend.erl", "-module(|shop_backend)"),
    ("exported_function", "src/shop_backend.erl", "\n|lookup(Backend, Key)"),
    ("function_used_in_macro_argument", "src/shop.erl", "\n|price(Name) ->"),
    ("local_function", "src/shop.erl", "\n|sum([], Acc)"),
    ("variable", "src/shop.erl", "price(|Name) ->"),
    ("record", "src/shop.hrl", "-record(|item"),
    ("record_field", "src/shop.hrl", "|price = ?DEFAULT_PRICE"),
    ("macro", "src/shop.hrl", "-define(|DEFAULT_PRICE"),
    ("macro_with_arguments", "src/shop.hrl", "-define(|PRICE("),
    ("type", "src/shop_backend.erl", "-type |key()"),
    ("callback", "src/shop_backend.erl", "-callback |fetch("),
    ("exported_function_without_callers", "src/shop_typed.erl", "\n|label(Name) ->"),
]

RENAME = [
    ("module", "src/shop_backend.erl", "-module(shop_back|end)", "shop_store"),
    ("exported_function", "src/shop_backend.erl", "\nlo|okup(Backend, Key)", "find"),
    ("local_function", "src/shop.erl", "\ns|um([], Acc)", "add_up"),
    ("variable", "src/shop.erl", "price(Na|me) ->", "ItemName"),
    ("record", "src/shop.hrl", "-record(it|em", "product"),
    ("record_field", "src/shop.hrl", "pr|ice = ?DEFAULT_PRICE", "cost"),
    ("macro", "src/shop.hrl", "-define(DEFAULT_|PRICE", "BASE_PRICE"),
    ("type", "src/shop_backend.erl", "-type k|ey()", "item_key"),
]

HOVER = [
    ("remote_function_with_doc", "src/shop.erl", "shop_backend:look|up(shop_memory"),
    ("local_function", "src/shop.erl", "sum([pr|ice(Name)"),
    ("otp_function", "src/shop_memory.erl", "lists:keyfi|nd("),
    ("record", "src/shop.erl", "describe(#it|em{"),
    ("macro", "src/shop.erl", "?DEFAULT_|PRICE"),
    ("remote_type", "src/shop.erl", "price(shop_backend:k|ey())"),
    ("variable", "src/shop.erl", "lookup(shop_memory, Na|me)"),
]

SIGNATURE_HELP = [
    ("remote_function", "src/shop.erl", "lookup(shop_memory, |Name)"),
    ("local_function", "src/shop.erl", "sum(Rest, |Acc + Price)"),
    ("otp_function", "src/shop_memory.erl", "lists:keyfind(Key, |#item.name"),
]

DOCUMENT_HIGHLIGHT = [
    ("variable", "src/shop.erl", "price(Na|me) ->"),
    ("function", "src/shop.erl", "sum([pr|ice(Name)"),
    ("variable_in_macro_argument", "src/shop.erl", "?PRICE(It|em)"),
]

SELECTION_RANGE = [
    ("variable", "src/shop.erl", "lookup(shop_memory, Na|me)"),
    ("expression", "src/shop.erl", "sum(Rest, Acc + Pr|ice)"),
]

CODE_ACTION = [
    ("unused_function", "src/shop_defects.erl", "unused_f|unction() -> ok."),
    ("unused_variable", "src/shop_defects.erl", "unused_variable(Unu|sed)"),
    ("undefined_function", "src/shop_defects.erl", "missing_lo|cal(1)"),
    ("local_function", "src/shop.erl", "\ns|um([], Acc)"),
]

# The caret after a typed prefix, as the IDE asks: completion is invoked on a typed character, with triggerKind 1.
COMPLETION = [
    ("remote_function", "src/shop.erl", "shop_backend:look|up(shop_memory"),
    ("module", "src/shop.erl", "shop_back|end:lookup("),
    ("local_function", "src/shop.erl", "sum([pr|ice(Name)"),
    ("variable", "src/shop.erl", "lookup(shop_memory, Na|me)"),
    ("record", "src/shop.erl", "describe(#it|em{"),
    ("record_field", "src/shop.erl", "#item{na|me = Name"),
    ("macro", "src/shop.erl", "?DEFAULT_|PRICE"),
    ("remote_type", "src/shop.erl", "price(shop_backend:k|ey())"),
    ("otp_function", "src/shop_memory.erl", "lists:keyfi|nd("),
    ("behaviour", "src/shop_memory.erl", "-behaviour(shop_back|end)"),
]

# The completion items to resolve: (the kind of a COMPLETION case, the label of one of its items).
COMPLETION_RESOLVE = [
    ("remote_function", "lookup/2"),
    ("otp_function", "keyfind/3"),
    ("local_function", "price/1"),
]

# Queries of Go to Symbol and Go to Class: a module, a function, a record, a macro, a type.
WORKSPACE_SYMBOL = ["shop_backend", "lookup", "item", "DEFAULT_PRICE", "key"]

WHOLE_FILE = ["src/shop.erl", "src/shop.hrl", "src/shop_memory.erl", "src/shop_defects.erl"]


class Server:
    def __init__(self, elp, project, log):
        self.proc = subprocess.Popen([elp, "server"], cwd=project, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                     stderr=open(log, "wb"))
        self.lock = threading.Condition()
        self.responses = {}
        self.diagnostics = {}
        self.messages = []
        self.open_progress = set()
        self.last_event = time.time()
        self.next_id = 0
        threading.Thread(target=self._read, daemon=True).start()

    def send(self, message):
        body = json.dumps(message).encode()
        self.proc.stdin.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
        self.proc.stdin.flush()

    def _read(self):
        while True:
            headers = {}
            while True:
                line = self.proc.stdout.readline()
                if not line:
                    return
                line = line.decode().strip()
                if not line:
                    break
                key, value = line.split(":", 1)
                headers[key.strip().lower()] = value.strip()
            body = json.loads(self.proc.stdout.read(int(headers["content-length"])))
            with self.lock:
                self._handle(body)
                self.lock.notify_all()

    def _handle(self, body):
        method = body.get("method")
        if "id" in body and method:  # a request of the server
            result = [None for _ in body["params"]["items"]] if method == "workspace/configuration" else None
            self.send({"jsonrpc": "2.0", "id": body["id"], "result": result})
            return
        if "id" in body:
            self.responses[body["id"]] = body
            return
        if method == "$/progress":
            self.last_event = time.time()
            token, kind = body["params"]["token"], body["params"]["value"]["kind"]
            if kind == "begin":
                self.open_progress.add(token)
            elif kind == "end":
                self.open_progress.discard(token)
        elif method == "textDocument/publishDiagnostics":
            self.last_event = time.time()
            self.diagnostics[body["params"]["uri"]] = body["params"]["diagnostics"]
        elif method == "window/showMessage":
            self.messages.append(body["params"]["message"])

    def request(self, method, params, timeout=60):
        with self.lock:
            self.next_id += 1
            request_id = self.next_id
        self.send({"jsonrpc": "2.0", "id": request_id, "method": method, "params": params})
        deadline = time.time() + timeout
        with self.lock:
            while request_id not in self.responses:
                remaining = deadline - time.time()
                if remaining <= 0:
                    raise TimeoutError(method)
                self.lock.wait(remaining)
            return self.responses.pop(request_id)

    def notify(self, method, params):
        self.send({"jsonrpc": "2.0", "method": method, "params": params})

    def wait_quiet(self, quiet=5, timeout=180):
        """Waits until no progress is open and nothing was published or reported for `quiet` seconds."""
        deadline = time.time() + timeout
        with self.lock:
            while self.open_progress or time.time() - self.last_event < quiet:
                if time.time() > deadline:
                    raise TimeoutError("ELP is still busy: " + ", ".join(sorted(self.open_progress)))
                self.lock.wait(0.5)

    def stop(self):
        try:
            self.request("shutdown", None, timeout=10)
            self.notify("exit", None)
            self.proc.wait(10)
        except (TimeoutError, subprocess.TimeoutExpired, BrokenPipeError):
            self.proc.kill()


def main():
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    elp, work = os.path.abspath(sys.argv[1]), sys.argv[2]
    os.makedirs(work, exist_ok=True)
    work = os.path.abspath(work)
    if os.path.realpath(work) != work:
        sys.exit("WORK_DIR must not be under a symlinked path: ELP breaks there (%s is %s)" % (work, os.path.realpath(work)))
    run = os.path.join(work, "run-" + datetime.datetime.now().strftime("%Y%m%d-%H%M%S"))
    project = os.path.join(run, "project")
    shutil.copytree(FIXTURE, project)
    version = subprocess.run([elp, "version"], capture_output=True, text=True, check=True).stdout.strip()
    otp_root = subprocess.run(["erl", "-noshell", "-eval", "io:put_chars(code:root_dir()), halt()."], capture_output=True,
                              text=True, check=True).stdout.strip()
    otp_root = os.path.realpath(otp_root)
    project_uri = "file://" + project
    texts = {}
    for directory, _, names in os.walk(os.path.join(project, "src")):
        for name in sorted(names):
            if name.endswith((".erl", ".hrl")):
                path = os.path.join(directory, name)
                with open(path) as f:
                    texts[os.path.relpath(path, project)] = f.read()

    server = Server(elp, project, os.path.join(run, "elp-stderr.log"))
    print("ELP %s, pid %d, project %s" % (version, server.proc.pid, project), flush=True)
    try:
        initialize = server.request("initialize", {
            "processId": os.getpid(), "rootUri": project_uri, "workspaceFolders": [{"uri": project_uri, "name": "shop"}],
            "capabilities": {
                "window": {"workDoneProgress": True},
                "workspace": {"configuration": True, "workspaceFolders": True,
                              "workspaceEdit": {"documentChanges": True, "resourceOperations": ["create", "rename", "delete"]}},
                "textDocument": {
                    "publishDiagnostics": {"relatedInformation": True},
                    "hover": {"contentFormat": ["markdown", "plaintext"]},
                    "rename": {"prepareSupport": False},
                    "completion": {"completionItem": {"snippetSupport": True, "documentationFormat": ["markdown", "plaintext"],
                                                      "resolveSupport": {"properties": ["documentation", "detail"]}}},
                    "definition": {"linkSupport": False},
                    "signatureHelp": {"signatureInformation": {"parameterInformation": {"labelOffsetSupport": True}}},
                    "inlayHint": {},
                    "foldingRange": {"lineFoldingOnly": False},
                    "selectionRange": {},
                    "codeAction": {"codeActionLiteralSupport": {"codeActionKind": {"valueSet": ["", "quickfix", "refactor"]}}},
                }}})
        server.notify("initialized", {})
        for relative, text in texts.items():
            server.notify("textDocument/didOpen", {"textDocument": {
                "uri": project_uri + "/" + relative, "languageId": "erlang", "version": 1, "text": text}})
        time.sleep(2)
        server.wait_quiet()

        def position(relative, needle):
            text = texts[relative]
            caret = needle.index("|")
            offset = text.find(needle.replace("|", ""))
            if offset < 0:
                raise ValueError("%s: no %r" % (relative, needle))
            offset += caret
            line = text.count("\n", 0, offset)
            character = offset - (text.rfind("\n", 0, offset) + 1)
            return {"line": line, "character": character}

        def document(relative):
            return {"uri": project_uri + "/" + relative}

        def whole(relative):
            text = texts[relative]
            return {"start": {"line": 0, "character": 0}, "end": {"line": text.count("\n"), "character": 0}}

        def record(name, method, cases, params):
            answers = []
            for case in cases:
                kind, relative, needle = case[0], case[1], case[2]
                at = position(relative, needle)
                request = params(relative, at, *case[3:])
                answer = server.request(method, request)
                print("%s %s -> %s" % (method, kind, json.dumps(answer.get("result", answer.get("error")))[:160]), flush=True)
                answers.append({"kind": kind, "file": relative, "needle": needle, "position": at, "params": request,
                                "result": answer.get("result"), "error": answer.get("error")})
            write(name, answers)
            return answers

        def located(relative, at):
            return {"textDocument": document(relative), "position": at}

        def write(name, value):
            text = json.dumps(value, indent=1, sort_keys=True)
            text = text.replace(project_uri, "${PROJECT_URI}").replace(project, "${PROJECT_PATH}")
            text = text.replace("file://" + otp_root, "${OTP_URI}").replace(otp_root, "${OTP_PATH}")
            with open(os.path.join(ANSWERS, name + ".json"), "w") as f:
                f.write(text + "\n")

        os.makedirs(ANSWERS, exist_ok=True)
        record("definition", "textDocument/definition", DEFINITION, located)
        record("references", "textDocument/references", REFERENCES,
               lambda relative, at: dict(located(relative, at), context={"includeDeclaration": False}))
        record("rename", "textDocument/rename", RENAME, lambda relative, at, new_name: dict(located(relative, at), newName=new_name))
        record("hover", "textDocument/hover", HOVER, located)
        record("signatureHelp", "textDocument/signatureHelp", SIGNATURE_HELP, located)
        record("documentHighlight", "textDocument/documentHighlight", DOCUMENT_HIGHLIGHT, located)
        record("selectionRange", "textDocument/selectionRange", SELECTION_RANGE,
               lambda relative, at: {"textDocument": document(relative), "positions": [at]})

        def code_action(relative, at):
            uri = project_uri + "/" + relative
            line = at["line"]
            diagnostics = [d for d in server.diagnostics.get(uri, [])
                           if d["range"]["start"]["line"] <= line <= d["range"]["end"]["line"]]
            return {"textDocument": document(relative), "range": {"start": at, "end": at},
                    "context": {"diagnostics": diagnostics, "triggerKind": 1}}

        record("codeAction", "textDocument/codeAction", CODE_ACTION, code_action)
        whole_file = [(relative, relative, "|") for relative in WHOLE_FILE]
        record("inlayHint", "textDocument/inlayHint", whole_file,
               lambda relative, at: {"textDocument": document(relative), "range": whole(relative)})
        record("foldingRange", "textDocument/foldingRange", whole_file, lambda relative, at: {"textDocument": document(relative)})
        completions = record("completion", "textDocument/completion", COMPLETION,
                             lambda relative, at: dict(located(relative, at), context={"triggerKind": 1}))
        completions = {answer["kind"]: answer["result"] for answer in completions}
        resolved = []
        for kind, label in COMPLETION_RESOLVE:
            result = completions.get(kind) or []
            items = result.get("items", []) if isinstance(result, dict) else result
            item = next((i for i in items if i.get("label") == label), None)
            answer = server.request("completionItem/resolve", item) if item is not None else {"error": "no item " + label}
            print("completionItem/resolve %s %s -> %s" % (kind, label, json.dumps(answer.get("result", answer.get("error")))[:160]),
                  flush=True)
            resolved.append({"kind": kind, "label": label, "params": item, "result": answer.get("result"), "error": answer.get("error")})
        write("completionResolve", resolved)

        symbols = []
        for query in WORKSPACE_SYMBOL:
            answer = server.request("workspace/symbol", {"query": query})
            print("workspace/symbol %s -> %s" % (query, json.dumps(answer.get("result", answer.get("error")))[:160]), flush=True)
            symbols.append({"query": query, "result": answer.get("result"), "error": answer.get("error")})
        write("workspaceSymbol", symbols)

        diagnostics = []
        for relative in texts:
            published = server.diagnostics.get(project_uri + "/" + relative, [])
            published = sorted(published, key=lambda d: (d["range"]["start"]["line"], d["range"]["start"]["character"],
                                                         str(d.get("code")), d["message"]))
            diagnostics.append({"file": relative, "diagnostics": published})
        write("publishDiagnostics", diagnostics)
        write("initialize", initialize.get("result"))
        write("manifest", {
            "elp_version": version,
            "server_messages": server.messages,
            "recorded": datetime.date.today().isoformat(),
            "command": "python3 -I testData/elp/record_answers.py ELP_BINARY WORK_DIR",
            "eqwalizer": "on: ELP's default configuration, no .elp.toml",
            "placeholders": {"${PROJECT_URI}": "the URI of the project root", "${PROJECT_PATH}": "its path",
                             "${OTP_URI}": "the URI of the OTP root", "${OTP_PATH}": "its path"},
        })
    finally:
        server.stop()
    print("answers written to", ANSWERS)


if __name__ == "__main__":
    main()
