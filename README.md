# purescript-alterlist

A linked list (empty and non-empty variants) whose values' types alternate with each element.

Credit goes to @monoidmusician for the idea.

## Variations

| | Empty | Non-Empty |
| - | - | - |
| Last value's type is **known** at compile time  | x | AlterListCons, AlterListTree
| Last value's type is **unknown** at compile time  | AlterListEmpty | AlterListNonEmpty
