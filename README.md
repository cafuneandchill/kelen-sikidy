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
stack build --test --copy-bins --haddock
```

For more info, see the Stack documentation.

[stack]: https://www.haskellstack.org/

## Demonstration

```console
$ kelen-sikidy-exe 
====== antāλōþa ======
A Method of Divination
https://www.terjemar.net/kelen/sikidy.php
----------------------
Generating the mother seed...
Mother seed:
1 0 0 0
0 0 1 0
0 1 1 0
1 1 1 0

Generating the daughter seed...
Daughter seed:
1 0 1 1 1 1 0 1
0 0 0 1 0 1 1 1
0 1 1 1 1 0 1 1
0 0 0 1 0 1 1 1

Interpreting the travel direction...
Interpreting the art to study...
Interpreting the duration of study...
A simple question for antāλōþa is which direction to travel.
The current configuration says rāhāwie or to the south-west.
It also suggests that ankeīlke (visual design) would be a good art to study for the next 6 jālūi (weeks).
```
