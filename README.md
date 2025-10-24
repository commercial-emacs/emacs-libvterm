<pre>
-------
Install
-------
git clone https://github.com/commercial-emacs/emacs-libvterm.git
make -C emacs-libvterm install

------------------------------------------------
How this fork differs from akermu/emacs-libvterm
------------------------------------------------
C-y pastes from the shell's kill ring (subsequent M-y from same).

C-c C-y pastes from emacs's kill ring (subsequent M-y from same).

C-c C-y also exits VTermCopy mode to paste a just copied region.

Any non-prefixed (C-c) key sequence exits VTermCopy mode.

C-p immediately enters VTermCopy mode.

M-v immediately enters VTermCopy mode.

Sane handling of wrapped lines.  IFYKYK.
</pre>
