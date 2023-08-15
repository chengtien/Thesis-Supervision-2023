# targets

```r
install.packages("targets")
```

In addition:
```
remotes::install_github("tpemartin/econDV2")
remotes::install_github("tpemartin/foodDelivery")
```

## Run the example

  1. Open `RStudio` as project using `targets/` folder.
  2. Source `R/pre_targets.R` file. This will create initial data sources in your `local-data/`  
  3. Run `tar_make()` in **Console**. 

## What is **targets** package

R **targets** package is a pipeline toolkit for statistics and data science. It is a tool for 
 
   * managing the workflow of an analysis project.  
   * ensuring reproducible research.  
  
## Why use **targets**

In our case it is **big data** that consumes too much local resources to handle. Normal analysis will look like this:

```r
# read data
df <- read.csv("data.csv")
df2 <- read.csv("data2.csv")

# do something
summarise_df <- df %>% summarise(...)
summarise_df2 <- df2 %>% summarise(...)

# do something on summarise_df's
summaries_comparison <- do_compare_summaries(summarise_df, summarise_df2)
```

When you run the code, it will produce the following objects in the global environment: `df`, `df2`, `summarise_df`, `summarise_df2`, `compare_summaries`. You probably only need `compare_summaries`, but the remaining objects still consume your memory. 

**targets** produce objects and cache them in a hidden directory. Nothing shown in the global environment. You can access the objects by their names. 

```r
targets::tar_load(summaries_comparison)
```

which only load the object `compare_summaries` into the global environment.

Another major reason of using **targets** is that your thesis develops over time. Its program will envolve with if not thousands, hundreds of code lines and hundreds of objects created. You probably split your code into several files, and you probably have a `main.R` file to run the whole program (managing which script run first, which second, etc). But it is still hard to manage the workflow. **targets** provides a way to manage the workflow of your project. Using **targets** you donot even have to think about which objects should be created first, which second, etc. **targets** will figure it out for you. On top of that, it also gives you visual network graph to show you the object workflow dependencies.Simply type:

```r
targets::tar_visnetwork()
```

## Usage

### 1. Create `_targets.R` file

Create a `_targets.R` file in the root directory of your project. This file contains the code to create the targets. 

Our codes line look like this:

```r
object1 <- command1
object2 <- command2
```

In targets, `object` is called your target, and `command` is still called your command. In `_targets.R` file, you will write the code to create your targets.

```r
library(targets)

# create list of targets
list(
    tar_target(object1, command1),
    tar_target(object2, command2)
    )
```

> If you command lines are many, you can use `{}` to wrap them up.

```r
tar_target(object1, {
    command1
    command2
    command3
    })
```

The last executed line in the `{}` will be the returned target value.

> econDV2 has a special function `%t=%`. You can write your `_targets.R` file like this:

```r
library(targets)
library(econDV2)

# create list of targets
list(
    object1 %t=% {command1},
    object2 %t=% {command2},
    object3 %t=% {
        command_a
        command_b
        command_c
    }
    )
```

> Be aware that `{}` is required when you use `%t=%` unless `command` is a pure one-step function call. 

Anything you need to have in your global environment, before targets are created should be created or sourced before the `list()` function.

```r
library(targets)
library(econDV2)
source("some_file.R")

# create list of targets
list(...)
```

> Other than those before `list()` function, the order of the targets does not matter. The following order will also work. Sorting out the targets in the order of their dependencies is the job of **targets**.

```r
library(targets)
library(econDV2)

# create list of targets
list(
    object3 %t=% {
        command_a
        command_b
        command_c
    },
    object1 %t=% command1,
    object2 %t=% command2,
    )
```


### 2. Run `_targets.R` file

In **Console**, run `tar_make()`. This will create a hidden directory `.targets` in your root directory. This directory contains the cached objects.

You won't see anything created in your global environment. That's the beauty of it.

### 3. Load targets

If you need some target to be loaded as an object in your global environment, you can use `tar_load()` function.

```r
tar_load(object1)
```

## Rules about commands

### 1. Commands should be pure

Commands should be pure functions. That is, the same input should always produce the same output. 

### 2. Commands should be side-effect free

Commands should be side-effect free. That is, commands should not change the state of the global environment, and should not change the value of other targets.

> This is very important. If some object in the global environment needs to be changed, then that object should be a target, sitting inside `list(...)`.  
> Also target values should not be altered by other target command. For example, consider `target1 %t=% list(a=1, b=2)`, `target2 %t=% {target1$a = 3; target$b}`. In thise case the `target1$a=3` command creates a side effect on `target1` value.  


## How to debug

Since `object %t=% command` is a counterpart of `object <- command`. You only need to load those object in `command` to the global environment through `tar_load()` function. Then you can debug your code as usual.
