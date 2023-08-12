build:
	@ruby build.rb

fnlfmt:
	@find . -type f -name '*.fnl' -exec fnlfmt --fix {} \;

test:
	@XDG_CONFIG_HOME=.. nvim
