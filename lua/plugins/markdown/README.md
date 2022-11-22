# markdown

I have configured some useful plugins to enhaced the markdown writing
experience.

## table mode

This is the most powerful plugin to improve your markdown writing experience.
Talk is cheap, click the below image to watch the video demo.

<video src="https://user-images.githubusercontent.com/30021675/151665473-d8527c7f-fc2a-415a-9878-e39927c49fc8.mp4" controls>
</video>

You can use command `:TableModeToggle` to trigger this plugin.

## Preview

You can use command `:MarkdownPreview` to open a browser to preview the
text.

![preview-gif](https://user-images.githubusercontent.com/5492542/47603494-28e90000-da1f-11e8-9079-30646e551e7a.gif)

I have configured the plugin to use [Surf](https://surf.suckless.org/) as the
preview browser. If you prefer Chrome or Firefox, you can change the setting
in lua/custom.lua file.

```lua
markdown = {
  -- must be the executable name
  preview_browser = "chrome",
},
```

## TOC

You can generate GitHub style table of content by command `:GenTocGFM`.
It also support GitLab style: `:GenTocGitLab`.
