The setup is similar to that of org-protocol. Here `roam://` links are
defined, and need to be associated with an application. 

The gist of the setup is setting up a Bash script to trim off the
`roam://` prefix from the link, causing the desktop application to
call `emacsclient path/to/org-roam-file.org`.

## Setting Up for Linux

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
