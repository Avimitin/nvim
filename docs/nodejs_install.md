# Nodejs 和 NPM 安装

建议不要直接使用 *apt* 安装 *nodejs* 和 *npm*：一个是因为 *apt* 里的 *nodejs* 版本很老，第二个是因为 *apt* 会把 *nodejs* 安装在 `/usr` 目录下，当你全局安装时会遇到 *EACCES* 错误。

根据官方的 [文档](https://docs.npmjs.com/resolving-eacces-permissions-errors-when-installing-packages-globally) ，最好使用 [nvm](https://github.com/nvm-sh/nvm) 管理器下载：

```bash
#下载nvm
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | bash

#把nvm变得可执行
export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")" [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
```

然后执行 `nvm install node` 安装最新版本，或 `nvm install *.*` 安装指定版本。安装完之后把 `bin` 加入 `$PATH` 里。

```fish
#fish shell
set -U fish_user_path $NVM_BIN $fish_user_path
#bash
export PATH=$PATH:$NVM_BIN
```

然后执行 `node -v && npm -v` 检查能否正常启动。