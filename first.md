---
title: This is my presentation
author: Jane Doe
...

# This is a slide

Slide contents.  Yay.

---
title: Presentation with incremental lists
author: John Doe
patat:
    incrementalLists: true
...

- This list
- is displayed

    * item
    * by item

- Or sometimes

    > * all at
    > * once

---
author: 'Jasper Van der Jeugt'
title: 'This is a test'
patat:
    wrap: true
    theme:
        emph: [vividBlue, onVividBlack, bold]
        imageTarget: [onDullWhite, vividRed]
        syntaxHighlighting:
            decVal: [bold, onDullRed]
...


# This is a presentation

This is _emph_ text.

```
sum :: Int -> Int -> Int
sum x y = x + y
```
