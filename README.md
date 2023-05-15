# Org-footnote-assistant 
A minor mode for editing and browsing org-footnotes from a sidebar


Org-footnote-assistant provides additional functionality to Emacs Org mode for
handling footnotes. It defines functions that allow the user to navigate between
footnote references and their definitions, and to edit the definitions in a
separate buffer.

The 'org-footnote-assistant--show-definition' function narrows the buffer to the
region of the current footnote definition, if the point is currently at a
footnote reference. The 'org-footnote-assistant--create-editor-window' function
creates a new buffer or selects an existing buffer named "\*footnote-editor\*",
narrows it to the footnote definition region, and switches to it. The
'org-footnote-assistant--goto-next-footnote' function finds the next or previous
footnote reference and opens the narrowed buffer. The 'org-footnote-new-advice'
function is an advice function that adds the ability to automatically jump to
the definition of a newly-created footnote after it has been inserted. Finally,
the 'org-footnote-assistant--goto-definition' function moves the point to the
definition of the specified footnote label.
