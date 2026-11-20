# Privacy policy of the Erlang plugin

The Erlang plugin for JetBrains IDEs can send anonymous usage statistics. It sends them only if you allow it. By default it sends nothing.

## Why

The statistics tell me, Sergey Ignatov, the author of the plugin, which features people use. Then I know which features to make better and which ones nobody needs.

## When the plugin asks

The plugin asks in a notification, after you open a project with Erlang files. You can answer:

- "Yes, send statistics": the plugin starts to count.
- "Later": the plugin asks again, but not earlier than 30 days later. Closing the notification without an answer means the same.
- "No, thanks": the plugin never asks again.

The plugin asks at most 3 times in total: the first time and two reminders. After that it does not ask again, and it sends nothing. The "See what is sent" link in the notification opens the settings described below.

You can change the answer at any time on the **Erlang External Tools** page of the IDE settings (type its name into the search field of the settings), section **Usage Statistics**. The **Show what is sent** button there shows the data of today, exactly as it would be sent.

## What is sent

At most one summary a day. It is sent on a later day, for each day on which you used the plugin. A day without Erlang work sends nothing. A summary contains:

- A random installation id. The plugin makes it when you say yes. It is not connected to your JetBrains account, your computer or your name.
- The date of the day.
- The plugin version, the IDE product code and version (for example `IU`, `263`), whether the IDE is IntelliJ IDEA or another IDE, and the operating system family (mac, windows, linux).
- The country, guessed from the time zone of your computer.
- For the Erlang projects that you opened: the major release of the Erlang/OTP SDK (for example `27`), whether an SDK is set, which rebar is used (rebar3, rebar2 or none), and the number of Erlang files as a range (up to 10, up to 100, up to 1000, more).
- Whether parameter name hints are on, and the names of this plugin's inspections that you turned on or off.
- Counts of the features you used, for example: completion items chosen, go to declaration, find usages, quick documentation, reformat, rename, run and debug of each kind of Erlang run configuration, breakpoints added, builds, Dialyzer runs, and the quick fixes and intentions of this plugin, by their class name in the plugin.

## What is never sent

- Your code or any text from your files.
- The names of your files, modules, functions, records, macros, projects or run configurations.
- Paths, SDK paths, rebar commands, command-line arguments or environment variables.
- Your user name, your computer name, your IP address or your JetBrains account.
- The texts of errors.

## Where it goes

The summaries go to [Amplitude](https://amplitude.com/), an analytics service, to its data center in the United States (`api2.amplitude.com`). The plugin does not send the IP address field. Only the author of the plugin can see the data. The data is kept as long as the Amplitude plan keeps it.

## When nothing is sent

The plugin sends nothing and does not ask:

- in local and test builds of the plugin (versions with `SNAPSHOT`), in tests and in headless mode;
- if the environment variable `DO_NOT_TRACK` is `1`;
- if the IDE runs with the system property `-Derlang.usage.statistics=false`.

## How to turn it off

Clear the **Send anonymous usage statistics** check box on the **Erlang External Tools** page of the IDE settings. Then the plugin stops counting, deletes the counts that it did not send yet, and deletes the installation id. If you turn it on again later, the plugin makes a new id, so the old and the new data cannot be connected.

## Contact

For questions, open an issue at <https://github.com/ignatov/intellij-erlang/issues>.
