The setup is similar to that of org-protocol. Here `roam://` links are
defined, and need to be associated with an application. 

The gist of the setup is setting up a Bash script to trim off the
`roam://` prefix from the link, causing the desktop application to
call `emacsclient path/to/org-roam-file.org`.

## Linux

Create a desktop application. I place mine in
`~/.local/share/applications/roam.desktop`:

```
[Desktop Entry]
Name=Org-Roam Client
Exec=/home/jethro/.local/bin/launch_emacs %u
Icon=emacs-icon
Type=Application
Terminal=false
MimeType=x-scheme-handler/roam
```

Note the `Exec` key is set to a bash script poorly named
`launch_emacs`. You can set it to whatever you want.

Create the corresponding bash script, and make it executable. Here's
how it looks like:

```bash
#!/usr/bin/env bash
emacsclient "${1#*:}"
```

Finally, associate `roam://` links with the desktop application by
running in your shell:

```bash
xdg-mime default roam.desktop x-scheme-handler/roam
```

## Mac OS

One solution to this, recommended in [Issue
#115](https://github.com/jethrokuan/org-roam/issues/115), is to use
[Platypus](https://github.com/sveinbjornt/Platypus). Here are the
instructions for setting up with Platypus and Chrome:

1. Create an executable `launch-emacs.sh` script:

```sh
#!/usr/bin/env bash
/usr/local/bin/emacsclient --no-wait "${1#*:}"
```

2. Install and launch Platypus (with [Homebrew](https://brew.sh/)):

```sh
brew cask install playtpus
```

3. Playtpus settings:

- App Name: `OrgRoam`
- Script Type: `env` and `/usr/bin/env`
- Script Path: `/path/to/your/launch-emacs.sh`
- Tick Accept dropped items and click Settings
- Tick Accept dropped files
- Tick Register as URI scheme handler
- Add `roam` as a protocol
- Create the app

To disable the "confirm" prompt in Chrome, you can also make Chrome
show a checkbox to tick, so that the `OrgRoam` app will be used
without confirmation. To do this, run in a shell:

```sh
defaults write com.google.Chrome ExternalProtocolDialogShowAlwaysOpenCheckbox -bool true
```
