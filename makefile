build:
	@ruby build.rb

fmt:
	@find . -type f -name '*.fnl' -exec fnlfmt --fix {} \;

test:
	@XDG_CONFIG_HOME=.. nvim
