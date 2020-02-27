The setup is the same as org-protocol. Here `roam://` links are
defined, and need to be associated with an application. 

Across all platforms, to enable `org-roam-protocol`, you have to add
the following to your init file:

```emacs-lisp
(require 'org-roam-protocol)
```

We also need to create a desktop application for emacsclient. The
instructions for various platforms are shown below:

## Linux

Create a desktop application. I place mine in
`~/.local/share/applications/roam.desktop`:

```
[Desktop Entry]
Name=Org-Roam Client
Exec=emacsclient %u
Icon=emacs-icon
Type=Application
Terminal=false
MimeType=x-scheme-handler/roam
```

Associate `roam://` links with the desktop application by
running in your shell:

```bash
xdg-mime default roam.desktop x-scheme-handler/roam
```

To disable the "confirm" prompt in Chrome, you can also make Chrome
show a checkbox to tick, so that the `Org-Roam Client` app will be used
without confirmation. To do this, run in a shell:

```sh
sudo mkdir -p /etc/opt/chrome/policies/managed/
sudo tee /etc/opt/chrome/policies/managed/external_protocol_dialog.json >/dev/null <<'EOF'
{
  "ExternalProtocolDialogShowAlwaysOpenCheckbox": true
}
EOF
sudo chmod 644 /etc/opt/chrome/policies/managed/external_protocol_dialog.json
```

and then restart Chrome (for example, by navigating to <chrome://restart>) to
make the new policy take effect.

See [here](https://www.chromium.org/administrators/linux-quick-start)
for more info on the `/etc/opt/chrome/policies/managed` directory and
[here](https://cloud.google.com/docs/chrome-enterprise/policies/?policy=ExternalProtocolDialogShowAlwaysOpenCheckbox)
for information on the `ExternalProtocolDialogShowAlwaysOpenCheckbox`
policy.


## Mac OS

One solution to this, recommended in [Issue
#115](https://github.com/jethrokuan/org-roam/issues/115), is to use
[Platypus](https://github.com/sveinbjornt/Platypus). Here are the
instructions for setting up with Platypus and Chrome:

1. Install and launch Platypus (with [Homebrew](https://brew.sh/)):

```sh
brew cask install playtpus
```

2. Platypus settings:

- App Name: `OrgRoam`
- Script Type: `env` and `/usr/bin/env`
- Script Path: `/path/to/emacsclient $1`
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
