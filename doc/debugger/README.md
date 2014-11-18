# How to use mruby debugger mrdb

copyright (c) 2014 Specified Non-Profit Coorporation mruby Forum

## 1.Summary
This file documents the method for using the mruby debugger 'mrdb'

## 2 debugging with mrdb

## 2.1 Building mrdb

The trunk of the mruby source tree can be checked out with following command/

```bash
$ git clone https://github.com/Kumikomi-Ruby/forum-mruby.git mruby
```

Run make command
```bash
$ cd mruby
$ make
```

By default, make command will install debugger files into mruby/bin.

You can add the path for mrdb on your host environment with following command

```bash
$ echo "export PATH=\$PATH:MRUBY_ROOT/bin" >> ~/.bashrc
$ source ~/.bashrc
```

*MRUBY_ROOT is the directory in which mruby source cords will be installed.

To confirm mrdb was instaleed properly,run mrdb with --version option

```bash
$ mrdb --version
mruby 1.1.0 (2014-11-19)
```

## 2.2 Bassic Operation

### 2.2.1 debugging mruby script file(rb file) with mrdb

To invoke mruby debugger, just type mrdb.

To specify the script file ,

```bash
$ mrdb [option] file name
```

For example : Debugging sample.rb

```bash
$ mrdb sample.rb
```

You can excute shell commands listed below

|command|description|
|:-:|:--|
|run|execute programs|
|step|execute stepping|
|continue|execute continuing program|
|break|configure the breaking point|
|delete|deleting the breaking points|
|disable|disabling the breaking points|
|enable|enabling the breaking points|
|info breakpoints|showing list of the breaking points|
|print|eavaluating and printing the values of the mruby expressions in the script|
|list|displaying the source cords|
|help|showing help|
|quit|terminating the mruby debugger|

### 2.2.2 debugging mruby binary file(mrb file) with mrdb

You can debugg the mruby binary files

#### 2.2.2.1 debuggg the binary files

* notice
To debugg mruby binary files, you need to compile mruby files with option -g.

```bash
$ mrbc -g sample.rb
```

You can debugg the mruby binary files with following command and the option -b.

```bash
$ mrdb -b sample.mrb
```

Then you can execute all debugger shell commands.

#### break command
 you can give any breakpoint to stop the program by specifiyng the line number and method name.
 And the breakpoint list will be displayed after you finished to set the breakpoint succesfully.

Usage:
```
break [file:]lineno
b [file:]lineno
break [class:]method
b [class:]method
```

The breakpoint will be numbered in serial order from 1.The number which was given to deleted breakpoint will not be given to another breakpoint again.

You can give multiple breakpoints to specified the line number and method.
Be ware that breakpoint command will not check the validity of the class name and method name.

You can see the current breakpoint information by following options.

breakpoint breakpoint number : file name. line number

breakpoint breakpoint number : [class name,] method name

#### continue command

Usage:
```
continue [N]
c [N]
```

N: the next breakpoint number

Resuming the program and will stop the program at breakpoint at N (N-1 breakpoint will be ignored)

When you run continue command without any specifying N ,Program will be stopped at next breakpoint.

Example:
```
(foo.rb:1) continue 3
```
Resuming the program and stopping the program at the third breakpoint.

#### delete command

Deleting specified breakpoint

Usage:
```
delete [breakpointno]
d [breakpointno]
```

breakpointno: breakpoint number

Example:
```
(foo.rb:1) delete
```

Deleting all brealpoint
```
(foo.rb:1) delete 1 3
```

Deleting the breakpoint 1 and 3

#### diable command

Disabling the specified breakpoint

Usage:
```
disable [breakpointno]
dis [breakpointno]
```

brealpointno: breakpoint number

Example:
```
(foo.rb:1) disable
```

Desabling all brealpoint
```
(foo.rb:1) disable 1 3
```

Disabling the breakpoint 1 and 3

#### enable command

Enababling the specified breakpoint

Usage:
```
enable [breakpointno]
e [breakpointno]
```

brealpointno: breakpoint number

Example:
```
(foo.rb:1) enable
```

Enabling all brealpoint
```
(foo.rb:1) enable 1 3
```

Enabling the breakpoint 1 and 3

#### eval command

Evaluating the string as source code and printing the value.

Same as print command, please see print command.

#### help command

Displaying the help message.

Usage:
```
help [comand]
h [command]
```
Typing help without any option will displays the command list.

#### info breakpoints command

Displaying the specified breakpoint information.

Usage:
```
info breakpoints [breakpointno]
i b [breakpointno]
```

breakpointno: breakpoint number

Typing "info breakpoints" without ant option will display all breakpoint information.
Example
```
(sample.rb:1) info breakpoints
Num     Type           Enb What  
1       breakpoint     y   at sample.rb:3                      -> file name,line number
2       breakpoint     n   in Sample_class:sample_class_method -> [class:]method name
3       breakpoint     y   in sample_global_method
```

Displaying specified the breakpoint number
```
(foo.rb:1) info breakpoints 1 3
Num     Type           Enb What  
1       breakpoint     y   at sample.rb:3  
3       breakpoint     y   in sample_global_method
```

#### list command

Displaying the cords of the source file.

Usage:
```
list [filename:]first[,last]
l [filename]:first[,last]
```

first: the opening row number
last : the closing row number

When you specify first , but not specify option "last" , you will get 10 rows.
When you don not specify both of first and last, you will next 10 rows.

Example:
```
Specifying file name and first row number
sample.rb:1) list sample2.rb:5
```

Specifying file name and first and last row number
```
(sample.rb:1) list sample2.rb:6,7
```

#### print command
Evaluating the string as source code and printing the value.

Usage:
```
print [expr]
p [expr]
```

expr: expression

To specify the expression is indispensableness.
The displayed expressions will be numbered in serial order from 1.
If an exception occurs, the exception information will be displayed and the debugging will be continued.

Example:
```
(sample.rb:1) print 1+2
$1 = 3
(sample.rb:1) print self
$2 = main
```

below is the case of the exception:
```
(sample.rb:1) print (1+2
$1 =  SyntaxError: line 1: syntax error, unexpected $end, expecting ')'
```

#### quit command
Quitting the debugger.

Usage:
```
quit
q
```

#### run command

Running the program and stopping at the first breakpoint.

Usage:
```
run
r
```

#### step command

Running the program step by step.
When the method and the block will be invoked, the program will be stop at the first row.
The program which is developed by C language will be ignored.

period
