# 开发规范

## 提交规范

### 标题

提交格式: `<type>[.|!] <module>: <summary>`.

* _**可用类型**_

- **N**: 新功能
- **F**: bug 修复
- **M**: 杂项修改
- **R**: 重构
- **D**: 文档
- 使用"!"表示该提交包含破坏性修改。

标题应该是少于50个的拉丁字符。
标题的第一个词*应该*是小写的，而且*必须*是动词。
不要以句号结束，也不要在上面附加任何时态。
你*必须*使用承诺的语气。

*正确示例:*

```text
refactor subsystem X for readability
update getting started documentation
remove deprecated methods
release version 1.0.0
```

*错误示例:*

```text
fixed bug with Y              ==> 不应使用时态
changing behavior of X        ==> 不应使用时态
more fixes for broken stuff   ==> 第一个词应是动词
sweet new API methods         ==> 没有动词
```

填写这句话:
"This patch will modify the project to...".

* This patch will modify the project to refactor subsystem X for readability
* This patch will modify the project to update getting started documentation
* This patch will modify the project to remove deprecated methods
* This patch will modify the project to release version 1.0.0

使用错误示例不能组合为正确的句子:

* This patch will modify the project to fixed bug with Y
* This patch will modify the project to changing behavior of X
* This patch will modify the project to more fixes for broken stuff
* This patch will modify the project to sweet new API methods

### 示例

```text
N. plugins: add new plugin
F. lazygit: fix the submodule issue
R! colors: replace the NormalFloat color         <- 其中包含了破坏性的改变
 ^
D. readme: update the installation guide
M. ci: rename the ci filename
```

module name 是可选的. 如果你使用了 module name，请使用 `/` 来分开 type 和 module.

### 主体

主体是可选的，
你可以在正文部分使用任何熟知的标记语言在正文部分，
你应该写下你做了什么以及你为什么这样做。
不要写你是如何做的。

如果你提交一个新的PR，请附上你的ISSUES ID 和PR ID。

另外，如果这个提交包含了破坏性的改变，记得要附上
`BREAKING CHANGE:` 在正文的最后部分来告诉大家有什么变化。
具体可参考“[示例](#示例)”。

* 推荐格式

我的日常做法如下：

<pre>
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
</pre>

* 每行一句

此外，我建议每行一句。
在编辑一个大段落时，如果你发现自己在前几句有语法错误。
你删除或添加了新的单词，这导致编辑行超过80个字符。
因此，你必须编辑整个段落每行少于80个字符。

你可以阅读
[这篇文章](https://rhodesmill.org/brandon/2012/one-sentence-per-line/)，
了解更多你能从中获得的好处。

### 页脚

页脚应包含所有合作者的姓名和电子邮件。
如果有人提到一个错误，请附上 "Reported-by: Tom \<Tom@example.com\>"。
如果有人帮助你测试代码，请附上 "Tested-by: Sam \<Sam@example.com\>"。

如果你使用GPG来签署你的提交，你可以把你的名字附在页脚的末尾，
比如 "Signed-off-by: Yourname \<name@example.com\>"。

## 语义化版本

本文件为如何增加语义化版本描述了额外的细节。

### API 兼容性

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

## 基准测试

我写了一个perl脚本来处理基准测试。

```bash
perl ./fixtures/benchmark.pl
```

运行该脚本将会写入测试结果到文件 `./fixtures/benchmark.txt`.

## 生成更新日志

```bash
perl ./fixtures/clog_generate.pl
```
