# Development specifications

## Commit convention

### Title

Use format: `<type>[.|!] <module>: <summary>`.

* _**Available type**_

- **N**: for new feature
- **F**: for bug fixes
- **M**: for miscellaneous changes
- **R**: for refactor
- **D**: for document
- Use the bang "!" to indicate that this commit contains breaking changes.

Title should be wrap in 50 Latin characters.
The first word of the title *should* be lower case and *must* be a verb.
Don't end with a period and don't attach any tense on it. You *must* write
the commit in the imperative.

*This is good:*

```text
refactor subsystem X for readability
update getting started documentation
remove deprecated methods
release version 1.0.0
```

*And this is bad:*

```text
fixed bug with Y              ==> don't use tense
changing behavior of X        ==> don't use tense
more fixes for broken stuff   ==> use verb to make order
sweet new API methods         ==> there is no verb
```

A rule of thumb is to fill this sentence:
"This patch will modify the project to...".

* This patch will modify the project to refactor subsystem X for readability
* This patch will modify the project to update getting started documentation
* This patch will modify the project to remove deprecated methods
* This patch will modify the project to release version 1.0.0

So now the none-imperative commit are not working:

* This patch will modify the project to fixed bug with Y
* This patch will modify the project to changing behavior of X
* This patch will modify the project to more fixes for broken stuff
* This patch will modify the project to sweet new API methods

### Example

```text
N. plugins: add new plugin
F. lazygit: fix the submodule issue
R! colors: replace the NormalFloat color         <- This one contains breaking change
 ^
D. readme: update the installation guide
M. ci: rename the ci filename
```

The module name is optional. If you write the module name, please use `/` to separate
the type and module.

### Body

Body is optional. You can use any markup language that is well-knowing
in the body section. And you should write down what you have done and
why you did this. Don't write about how you do this.

If you are woring on a new PR, remember to attach the issue ID and PR ID.

Also if this commit contains breaking change, remember to attach
`BREAKING CHANGE:` to told what has been change at the end of the body
section. See the section "Full Example" for details.

* Recommend format

My daily practice are listed below:

```text
Section
=======

`short code`

  fn main() {
    println!("Hello World");
  }
^^ <- indent here

* item 1
  * item 1.1
* item 2

Lorem ipsum dolor sit amet, qui minim labore [ref-1]
adipisicing minim sint cillum sint consectetur cupidatat.

Ref:
[ref-1]: https://github.com/Avimitin/commit-convention
```

* One Line Per Sentence

Besides, I recommend using one line per sentence.
Imaging you are editing a large paragraph, and you find yourself have syntax
error at the previous sentences.
You remove or add new word, it cause the editing line over 80 characters.
So you have to edit the whole paragraph to fit in 80 characters per line.

You can read
[this article](https://rhodesmill.org/brandon/2012/one-sentence-per-line/)
to know more benefit you can gain from this.

### Footer

Footer should contains all the collaborators's name and email. If someone
mention a bug, attach "Reported-by: Tom \<Tom@example.com\>". If someone
help you test the code, attach "Tested-by: Sam \<Sam@example.com\>".

If you are using GPG to sign your commit, you can attach your name at the end
of the rooter like: "Signed-off-by: Yourname \<name@example.com\>".

## Semantic version

This document add additional details for how to increase semantic version.

### API Compatibility

- Plugins
  - Major: Remove/Replace any plugin
  - Minor: Add new plugin
  - Minor: Modify plugin setting
- Keymaps
  - Major: Remove any keymap
  - Minor: Add/Replace keymap
- Options
  - Minor: Update options
- Colorscheme
  - Major: Remove any colorscheme
  - Minor: Update colorscheme setting
  - Minor: Replace default colorscheme
- utils.lua
  - Major: Remove any functionality
  - Minor: Add/Replace/Modify function

## Benchmark

I've write a Perl script to handle the benchmark.

```bash
perl ./fixtures/benchmark.pl
```

It will write result into the `./fixtures/benchmark.txt` file.

## Changelog Generate

```bash
perl ./fixtures/clog_generate.pl
```
