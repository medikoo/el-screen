el-screen --- Window configurations manager for Emacs

-------------------------------------------------------------------------------
1. About
-------------------------------------------------------------------------------

Based on GNU Screen http://www.gnu.org/software/screen/
and similar Emacs implementation - elscreen
http://www.morishima.net/~naoto/software/elscreen/

Switchable window configurations. Difference from GNU Screen is that each
configuration needs to be named. Other notable feature is that
configurations are kept in files and persist between Emacs sessions.

-------------------------------------------------------------------------------
2. Installation
-------------------------------------------------------------------------------

- Put following projects (see my page on github) within your load path:
	el-kit into 'el-kit' folder
	el-index into 'el-index' folder

- Put `el-screen' project into 'el-screen' folder in your load path

- Create '.elscreen' folder within your '.emacs.d' (you may choose different
  path but then you need to reflect that with `el-screen-dir' variable)

-------------------------------------------------------------------------------
3. Usage
-------------------------------------------------------------------------------
3.1 Initialization
-------------------------------------------------------------------------------

(require 'el-screen/el-screen)
(el-screen-init)

To load existing screen or to start new screen session press C-z c and input
name.

-------------------------------------------------------------------------------
3.2 Interaction
-------------------------------------------------------------------------------

For keyboard bindings see HELP
Periodical or on exit save of configurations is done automatically.

-------------------------------------------------------------------------------
3.3 Available hooks
-------------------------------------------------------------------------------

el-screen-clear-hook
  Frame is cleared. Happens when new screen is opened or invoked manualy.
  With this hook you may setup initial frame configuration.

el-screen-switch-before-hook
	Run before new configuration is loaded. By default `save-sume-buffers' is
	added.
