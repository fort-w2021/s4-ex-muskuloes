library("methods")

# random pronouncable strings with length <length>
make_name <- function(length = 7) {
  vowels <- c("a", "e", "i", "o", "u")
  consonants <- setdiff(letters, vowels)
  name <- character(length)
  name[1] <- sample(toupper(consonants), 1)
  name[seq(3, length, by = 2)] <-
    sample(consonants, size = ceiling(length / 2) - 1, replace = TRUE)
  name[seq(2, length, by = 2)] <-
    sample(vowels, size = floor(length / 2), replace = TRUE)
  paste(name, collapse = "")
}

# animal class
setClass("animal",
  slots = list(name = "character", weight = "numeric", female = "logical"),
  validity = function(object) {
    invalids <- character(0)
    no_name <- nchar(object@name) == 0
    if (no_name) invalids <- "No <name> provided."
    if (length(invalids)) invalids else TRUE
  }
)
# attribute accessors definitions
setGeneric("name", function(object) standardGeneric("name"))
setGeneric("weight", function(object) standardGeneric("weight"))
setGeneric("female", function(object) standardGeneric("female"))

setMethod("name", "animal", function(object) object@name)
setMethod("weight", "animal", function(object) object@weight)
setMethod("female", "animal", function(object) object@female)

# prey class
setClass("prey",
  contains = "animal",
  slots = list(hide = "numeric"),
  validity = function(object) {
    invalids <- character(0)
    if (object@hide < 0 | object@hide > 1) {
      invalids <- "Wrong <hide> for prey, must be in [0, 1]"
    }
    if (length(invalids)) invalids else TRUE
  }
)
# attribute accessors definitions
setGeneric("hide", function(object) standardGeneric("hide"))
setMethod("hide", "prey", function(object) object@hide)

# print implementation for prey objects
setMethod(
  f = "show",
  signature = c(object = "prey"),
  function(object) {
    print(paste0(
      is(object)[[1]], " '",
      name(object), "' (",
      switch(female(object), "f", "m"), ")"
    ))
    print(paste0("  weight: ", weight(object)))
    print(paste0("  hide: ", hide(object)))
  }
)

# mouse class
setClass("mouse",
  contains = "prey",
  validity = function(object) {
    invalids <- character(0)
    if (object@weight < 0.5 | object@weight > 1) {
      invalids <- "Wrong <weight> for mouse, must be in [0.5, 1]"
    }
    if (object@hide < 0.6 | object@hide > 1) {
      invalids <- c(
        invalids,
        "Wrong <hide> for mouse, must be in [0.6, 1]"
      )
    }
    if (length(invalids)) invalids else TRUE
  }
)
# mouse object constructor
# inputs:
#   name: mouse's name
#   weight: mouse's week
#   hide: mouse's hide ability
#   female: sex, female or not
# return: mouse object
mouse <- function(name = make_name(5),
                  weight = round(runif(1, 0.5, 1), 4),
                  hide = round(runif(1, 0.6, 1), 4),
                  female = TRUE) {
  new("mouse", name = name, weight = weight, hide = hide, female = female)
}

# rabbit class
setClass("rabbit",
  contains = "prey",
  validity = function(object) {
    invalids <- character(0)
    if (object@weight < 1 | object@weight > 5) {
      invalids <- "Wrong <weight> for mouse, must be in [1, 5]"
    }
    if (object@hide < 0.3 | object@hide > 0.8) {
      invalids <- c(
        invalids,
        "Wrong <hide> for mouse, must be in [0.3, 0.8]"
      )
    }
    if (length(invalids)) invalids else TRUE
  }
)
# rabbit object constructor
# inputs:
#   name: rabbit's name
#   weight: rabbit's week
#   hide: rabbit's hide ability
#   female: sex, female or not
# return: rabbit object
rabbit <- function(name = make_name(6),
                   weight = round(runif(1, 1, 5), 4),
                   hide = round(runif(1, 0.3, 0.8), 4),
                   female = TRUE) {
  new("rabbit", name = name, weight = weight, hide = hide, female = female)
}

# deer constructor
setClass("deer",
  contains = "prey",
  validity = function(object) {
    invalids <- character(0)
    if (object@weight < 15 | object@weight > 30) {
      invalids <- "Wrong <weight> for mouse, must be in [15, 30]"
    }
    if (object@hide < 0.2 | object@hide > 0.7) {
      invalids <- c(
        invalids,
        "Wrong <hide> for mouse, must be in [0.2, 0.7]"
      )
    }
    if (length(invalids)) invalids else TRUE
  }
)

# deer object constructor
# inputs:
#   name: deer's name
#   weight: deer's week
#   hide: deer's hide ability
#   female: sex, female or not
# return: deer object
deer <- function(name = make_name(7),
                 weight = round(runif(1, 15, 30), 4),
                 hide = round(runif(1, 0.2, 0.7), 4),
                 female = TRUE) {
  new("deer", name = name, weight = weight, hide = hide, female = TRUE)
}

# predator class
setClass("predator",
  contains = "animal",
  slots = list(seek = "numeric"),
  validity = function(object) {
    invalids <- character(0)
    if (object@seek < 0 | object@seek > 1) {
      invalids <- "Wrong <seek> for prey, must be in [0, 1]"
    }
    if (length(invalids)) invalids else TRUE
  }
)
setGeneric("seek", function(object) standardGeneric("seek"))
setMethod("seek", "predator", function(object) object@seek)

setMethod(
  f = "show",
  signature = c(object = "predator"),
  function(object) {
    print(paste0(
      is(object)[[1]], " '",
      name(object), "' (",
      switch(female(object), "f", "m"), ")"
    ))
    print(paste0("  weight: ", weight(object)))
    print(paste0(" seek: ", seek(object)))
  }
)

# hawk class
setClass("hawk",
  contains = "predator",
  validity = function(object) {
    invalids <- character(0)
    if (object@weight < 3 | object@weight > 8) {
      invalids <- "Wrong <weight> for mouse, must be in [3, 8]"
    }
    if (object@seek < 0.6 | object@seek > 1) {
      invalids <- c(
        invalids,
        "Wrong <hide> for mouse, must be in [0.6, 1]"
      )
    }
    if (length(invalids)) invalids else TRUE
  }
)

# hawk object constructor
# inputs:
#   name: hawk's name
#   weight: hawk's week
#   seek: hawk's seek ability
#   female: sex, female or not
# return: hawk object
hawk <- function(name = make_name(4),
                 weight = round(runif(1, 3, 8), 4),
                 seek = round(runif(1, 0.6, 1), 4), female = TRUE) {
  new("hawk", name = name, weight = weight, seek = seek, female = female)
}

setClass("lynx",
  contains = "predator",
  validity = function(object) {
    invalids <- character(0)
    if (object@weight < 20 | object@weight > 60) {
      invalids <- "Wrong <weight> for mouse, must be in [20, 60]"
    }
    if (object@seek < 0.5 | object@seek > 0.9) {
      invalids <- c(
        invalids,
        "Wrong <hide> for mouse, must be in [0.5, 0.9]"
      )
    }
    if (length(invalids)) invalids else TRUE
  }
)

# lynx object constructor
# inputs:
#   name: lynx's name
#   weight: lynx's week
#   seek: lynx's seek ability
#   female: sex, female or not
# return: lynx object
lynx <- function(name = make_name(6),
                 weight = round(runif(1, 20, 60), 4),
                 seek = round(runif(1, 0.5, 0.9), 4), female = FALSE) {
  new("lynx", name = name, weight = weight, seek = seek, female = female)
}

# meet method
setGeneric(
  "meet",
  function(animal1, animal2, ...) {
    standardGeneric("meet")
  }
)

# helper for message construction
text_message <- function(animal1, animal2) {
  paste0(
    is(animal1)[[1]], " '",
    name(animal1), "'",
    " & ", is(animal2)[[1]], " '",
    name(animal2), "'"
  )
}

# animals ignore each other message
ignore_message <- function(animal1, animal2) {
  paste0(text_message(animal1, animal2), " ignore each other")
}

# animals make love message
love_message <- function(animal1, animal2) {
  paste0(text_message(animal1, animal2), " make sweet, sweet love")
}

# animals sniff each other message
sniff_message <- function(animal1, animal2) {
  paste0(
    text_message(animal1, animal2), " sniff each others' butts"
  )
}

# animals fight each other message
fight_message <- function(animal1, animal2) {
  paste0(
    text_message(animal1, animal2), " fight for territory"
  )
}

# animals kill each other message
kill_message <- function(animal1, animal2) {
  paste0(
    is(animal1)[[1]], " '",
    name(animal1), "' kills and eats ",
    is(animal2)[[1]], " '",
    name(animal2), "'"
  )
}

# gaze message
gaze_message <- function(animal1) {
  paste0(
    is(animal1)[[1]], " '", name(animal1),
    "' gazes at her reflection in a puddle"
  )
}

# prey escapes from predator message
escape_message <- function(prey, predator) {
  paste0(
    is(prey)[[1]], " '",
    name(prey), "' escapes from ",
    is(predator)[[1]], " '",
    name(predator), "'"
  )
}

# prey meets prey
setMethod("meet",
  signature = c(animal1 = "prey", animal2 = "prey"),
  function(animal1, animal2) {
    p <- runif(1)
    if ((is(animal1)[[1]] == is(animal2)[[1]])
    & (female(animal1) != female(animal2))) {
      if (p >= 1 / 2) {
        message(love_message(animal1, animal2))
      } else if (p >= 1 / 4) {
        message(ignore_message(animal1, animal2))
      } else {
        message(sniff_message(animal1, animal2))
      }
    }
    else {
      if (name(animal1) == name(animal2)) {
        message(gaze_message(animal1))
      } else if (p < 1 / 2) {
        message(ignore_message(animal1, animal2))
      } else {
        message(sniff_message(animal1, animal2))
      }
    }
  }
)

# predator meets predator
setMethod("meet",
  signature = c(animal1 = "predator", animal2 = "predator"),
  function(animal1, animal2) {
    p <- runif(1)
    if ((is(animal1)[[1]] == is(animal2))[[1]]
    & (female(animal1) != female(animal2))) {
      if (p >= 1 / 2) {
        message(love_message(animal1, animal2))
      } else {
        message(fight_message(animal1, animal2))
      }
    }
    else {
      if (name(animal1) == name(animal2)) {
        message(gaze_message(animal1))
      } else if (p < 1 / 3) {
        message(ignore_message(animal1, animal2))
      } else if (p >= 1 / 3 & p < 2 / 3) {
        message(sniff_message(animal1, animal2))
      } else {
        message(fight_message(animal1, animal2))
      }
    }
  }
)

# prey meets predator
setMethod("meet",
  signature = c(animal1 = "prey", animal2 = "predator"),
  function(animal1, animal2) {
    p <- runif(1)
    if ((weight(animal1) >= 0.05 * weight(animal2) &
      (weight(animal1) <= 0.7 * weight(animal2)))) {
      if (p >= min(1, max(0, 0.6 + seek(animal2) - hide(animal1)))) {
        message(kill_message(animal2, animal1))
      } else {
        message(escape_message(animal1, animal2))
      }
    }
    else {
      if (p < 1 / 2) {
        message(ignore_message(animal1, animal2))
      } else {
        message(sniff_message(animal1, animal2))
      }
    }
  }
)

# predator meets prey
setMethod("meet",
  signature = c(animal1 = "predator", animal2 = "prey"),
  function(animal1, animal2) {
    p <- runif(1)
    if ((weight(animal2) >= 0.05 * weight(animal1) &
      (weight(animal2) <= 0.7 * weight(animal1)))) {
      if (p >= min(1, max(0, 0.6 + seek(animal1) - hide(animal2)))) {
        message(kill_message(animal1, animal2))
      } else {
        message(escape_message(animal2, animal1))
      }
    }
    else {
      if (p < 1 / 2) {
        message(ignore_message(animal2, animal1))
      } else {
        message(sniff_message(animal2, animal1))
      }
    }
  }
)

# examples
set.seed(20191121)
animals <- list(
  mouse(female = TRUE),
  rabbit(),
  hawk(female = TRUE),
  deer(),
  lynx(female = TRUE),
  lynx(female = FALSE),
  deer(),
  mouse(female = FALSE),
  deer(female = TRUE)
)

for (animal1 in animals[1:5]) {
  for (animal2 in animals[9:5]) {
    cat(meet(animal1, animal2))
  }
}
