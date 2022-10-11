# kelen-sikidy

An method of divination, known as *antāλōþa*, used by the fictional Kēleni people, who have benn created by [Sylvia Sotomayor][sylvia] as a part of the lore surrounding the constructed language [Kēlen][kelen], also created by her.

The algorithm is described in the `sikidy-algorithm.md` file.

The original web implementation for the method: https://www.terjemar.net/kelen/sikidy.php

[kelen]: https://www.terjemar.net/kelen/kelen.php
[sylvia]: https://docs.google.com/forms/d/e/1FAIpQLSevX9fpoibtWJLsXsCWZ8tQpLk_z2w3Wyt0XWoolMrNHMgAXg/viewform

## Building instructions

This program uses [Stack][stack] for building the binaries.

To build and install the program, execute this command in the project's root:

```
stack build --test --copy-bins --haddock kelen-sikidy-exe
```

For more info, see the Stack documentation.

[stack]: https://www.haskellstack.org/
