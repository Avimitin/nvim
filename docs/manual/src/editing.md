# Editing

Belows are some common use key to edit text.

| Key               | Action                                                                                                                |
|-------------------|-----------------------------------------------------------------------------------------------------------------------|
| i                 | Insert text                                                                                                           |
| a                 | Append text                                                                                                           |
| I (Capitalized i) | Go to the front of the current line and start insert                                                                  |
| A                 | Go to the end of the current line and start append                                                                    |
| o                 | Create a new line below the current line and start insert                                                             |
| O                 | Create a new line above the current line and start insert                                                             |
| r                 | Enter replace mode and replace the current character to the next key you press                                        |
| R                 | Enter replace mode and replace the current character to the next key you press. It won't stop until you press `<ESC>` |
| x                 | Delete the character under cursor                                                                                     |
| dd                | Delete the current line                                                                                               |
| D                 | Delete character fron current character to the end of the line                                                        |

## Text Object

In vim, there are a concept called "text object". It create a operable object by text property. Like `word` we mention above, a `word` is
a object that represent a sequnces of letters, digits and underscores, separated with white space.

There are another concept about text object selection called `inner` and `outer`. Like a `"double quoted"` object, if we press
`vi"`, it will select the inner text of the double quoted text. If we press `va"`, it will select the whole double quoted text,
including double quote itself. This also apply to pairs:

```text
"double quoted"
 <-- inner -->
<--  outer  -->

'single quoted'
 <-- inner -->
<--  outer  -->

(text object)
 <- inner ->
<-  outer  ->

<a href="#">text object</a>
            <- inner ->
<---       outer       --->
```

Like I mention above, inner object often select by prefix `i`, outer object often select by prefix `a`.
Editing text with text object will be a lot easier:

| Supported Object                    |
|-------------------------------------|
| Double quote `""`                   |
| Single quote `''`                   |
| Word (separated by space)           |
| Sentence (separated by `.`)         |
| Paragraph (separated by new line)   |
| `()`, `<>`, `{}`, `[]`              |
| html tag (trigger by key `it`/`at`) |

If you enable treesitter, you can gain extra text object based on syntax:

| Suffix key | Object                    |
|------------|---------------------------|
| if/af      | inner/outer function      |
| ic/ac      | inner/outer class/struct  |
| ib/ab      | inner/outer code block    |
| il/al      | inner/outer function call |
| iP/aP      | inner/outer parameter     |
| io/ao      | inner/outer condition     |
| as         | outer statement           |

Here are some common use key that accept text object as suffix:

| Key | Action  |
|-----|---------|
| v   | select  |
| d   | delete  |
| c   | change  |

For example, `ciw` can remove a whole word and enter insert mode. `daf` can delete a whole function
with function keyword itself. `vit` will select all the text inside a html tag.
