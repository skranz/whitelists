examples.getwhitelist = function() {
  get.default.whitelist(pkg="stats")
  get.default.whitelist(pkg="restorepoint")

}

#' Get a stored default whitelist for a specified package
#'
#' @param pkg the package name
#' @param color "white" for whitelist and "black" for blacklist
#' @param funvar "fun" for function list "var" for variable list
#' @param group possibly the name of a list group
#' @param return.type "vector" names of whitelisted functiin, "yaml" the yaml source
get.default.whitelist = function(pkg="base",color = c("white","black")[1], funvar=c("fun","var")[1], group="", return.type=c("vector","yaml","list")[1]) {
  restore.point("get.whitelist")


  dir = path.package("whitelists", quiet=TRUE)
  if (dir.exists(paste0(dir,"/inst"))) {
    dir =paste0(dir,"/inst/lists/pkg/",pkg)
  } else {
    dir =paste0(dir,"/lists/pkg/",pkg)
  }

  if (!dir.exists(dir)) return(NULL)

  var.str = if (funvar=="var") "var" else ""
  group.str = if (group=="") "" else paste0("__", group,)

  file = paste0(dir,"/",color,var.str,group.str,"__", pkg, ".yaml")
  if (!file.exists(file)) return(NULL)

  yaml = paste0(readLines(file,warn = FALSE), collapse="\n")

  if (return.type=="yaml") return(yaml)
  li =yaml.load(yaml)
  if (return.type=="list") return(li)
  unlist(li)
}