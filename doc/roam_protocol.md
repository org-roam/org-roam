## What is Roam protocol?

Org-roam extending `org-protocol` with 2 protocols: the `roam-file`
and `roam-ref` protocol.

## The `roam-file` protocol

This is a simple protocol that opens the path specified by the `file`
key (e.g. `org-protocol://roam-file?file=/tmp/file.org`). This is used
in the generated graph.

## The `roam-ref` Protocol

This protocol finds or creates a new note with a given `ROAM_KEY` (see
[Anatomy](anatomy.md)):

![roam-ref](images/roam-ref.gif)

To use this, create a Firefox bookmarklet as follows:

```javascript
javascript:location.href =
'org-protocol:/roam-ref?template=r&ref='
+ encodeURIComponent(location.href)
+ '&title='
+ encodeURIComponent(document.title)
```

where `template` is the template key for a template in
`org-roam-capture-ref-templates`. More documentation on the templating
system can be found [here](templating.md).

These templates should contain a `#+ROAM_KEY: ${ref}` in it.

## Setting up Org-roam protocol

To enable org-roam's protocol extensions, you have to add the
following to your init file:

```emacs-lisp
(require 'org-roam-protocol)
```

The instructions for setting up `org-protocol` can be found
[here][org-protocol-inst], but they are reproduced below.

We will also need to create a desktop application for `emacsclient`.
The instructions for various platforms are shown below:

## Linux

Create a desktop application. I place mine in
`~/.local/share/applications/org-protocol.desktop`:

```
[Desktop Entry]
Name=Org-Protocol
Exec=emacsclient %u
Icon=emacs-icon
Type=Application
Terminal=false
MimeType=x-scheme-handler/org-protocol
```

Associate `org-protocol://` links with the desktop application by
running in your shell:

```bash
xdg-mime default org-protocol.desktop x-scheme-handler/org-protocol
```

To disable the "confirm" prompt in Chrome, you can also make Chrome
show a checkbox to tick, so that the `Org-Protocol Client` app will be used
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

One solution is to use
[Platypus](https://github.com/sveinbjornt/Platypus). Here are the
instructions for setting up with Platypus and Chrome:

1. Install and launch Platypus (with [Homebrew](https://brew.sh/)):

```sh
brew cask install platypus
```
2. Create a script `launch_emacs.sh`:

```
#!/usr/bin/env bash
/usr/local/bin/emacsclient --no-wait $1
```

3. Create a Platypus app with the following settings:

```
| Setting                        | Value                     |
|--------------------------------+---------------------------|
| App Name                       | "OrgProtocol"             |
| Script Type                    | "env" · "/usr/bin/env"    |
| Script Path                    | "path/to/launch-emacs.sh" |
| Interface                      | None                      |
| Accept dropped items           | true                      |
| Remain running after execution | false                     |
```

Inside `Settings`:

```
| Setting                        | Value          |
|--------------------------------+----------------|
| Accept dropped files           | true           |
| Register as URI scheme handler | true           |
| Protocol                       | "org-protocol" |
```

To disable the "confirm" prompt in Chrome, you can also make Chrome
show a checkbox to tick, so that the `OrgProtocol` app will be used
without confirmation. To do this, run in a shell:

```sh
defaults write com.google.Chrome ExternalProtocolDialogShowAlwaysOpenCheckbox -bool true
```


##### Note for Emacs Mac Port

If you're using [Emacs Mac Port](https://github.com/railwaycat/homebrew-emacsmacport), it
registered its `Emacs.app` as the default handler for the URL scheme
`org-protocol`. We have to make our `OrgProtocol.app` the default
handler instead (replace `org.yourusername.OrgProtocol` with your app
identifier):

```
$ defaults write com.apple.LaunchServices/com.apple.launchservices.secure LSHandlers -array-add \
'{"LSHandlerPreferredVersions" = { "LSHandlerRoleAll" = "-"; }; LSHandlerRoleAll = "org.yourusername.OrgProtocol"; LSHandlerURLScheme = "org-protocol";}'
```

Then restart your computer.

[org-protocol-inst]: https://orgmode.org/worg/org-contrib/org-protocol.html
