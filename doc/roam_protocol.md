## What is Roam protocol?

Org-roam defines several protocols that help boost productivity, by
extending `org-protocol`. 

In the generated graph, to force file links to open in Emacs, we use
`org-protocol`.

In addition, we can also use a Firefox bookmarklet to associate
notes with any unique key. 

<blockquote class="imgur-embed-pub" lang="en" data-id="a/sKuEske"><a
href="//imgur.com/a/sKuEske"></a></blockquote><script async
src="//s.imgur.com/min/embed.js" charset="utf-8"></script>

Add the following snippet as a bookmarklet:

```javascript
javascript:location.href =
'org-protocol://roam-ref?template=ref&ref='
+ encodeURIComponent(location.href)
+ '&title='
+ encodeURIComponent(document.title)
```

where `template` is the template you have defined for your web
snippets. This template should contain a `#+ROAM_KEY: {ref}` in it.

## Org-protocol Setup
The setup is the exact same as org-protocol. 

Across all platforms, to enable `org-roam-protocol`, you have to add
the following to your init file:

```emacs-lisp
(require 'org-roam-protocol)
```

We also need to create a desktop application for emacsclient. The
instructions for various platforms are shown below:

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

One solution to this, recommended in [Issue
#115](https://github.com/jethrokuan/org-roam/issues/115), is to use
[Platypus](https://github.com/sveinbjornt/Platypus). Here are the
instructions for setting up with Platypus and Chrome:

1. Install and launch Platypus (with [Homebrew](https://brew.sh/)):

```sh
brew cask install playtpus
```

2. Platypus settings:

- App Name: `OrgProtocol`
- Script Type: `env` and `/usr/bin/env`
- Script Path: `/path/to/emacsclient $1`
- Tick Accept dropped items and click Settings
- Tick Accept dropped files
- Tick Register as URI scheme handler
- Add `org-protocol` as a protocol
- Create the app

To disable the "confirm" prompt in Chrome, you can also make Chrome
show a checkbox to tick, so that the `OrgProtocol` app will be used
without confirmation. To do this, run in a shell:

```sh
defaults write com.google.Chrome ExternalProtocolDialogShowAlwaysOpenCheckbox -bool true
```
