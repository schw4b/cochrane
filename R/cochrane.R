#' Reads all the data from a systematic review from a RevMan data file (XML) from
#'  the Cochrane Library, including both study data and meta-analytic data.
#'
#' @param path path where data file is located.
#' @param file  RevMan (.rm5) file name.
#'
#' @return a list with two objects (data.frames), a table of the study data, and
#' a table of the meta-analytic data.
#'
#' @export
read.review = function(file, path) {

  return(list(parse.studies(file, path),
              parse.ma(file, path)
  ))
}

#' Reads only study data.
#'
#' @param path path where data file is located.
#' @param file  RevMan (.rm5) file name.
#'
#' @return data frame with data from a study per row.
#'
#' @export
#' @importFrom xml2 read_xml
#' @importFrom rvest html_nodes html_node html_attr html_text
parse.studies = function(file, path) {
  # Structure of the Cochrane library
  # COMPARISON - NAME
  #            - IV_/CONT_|DICH_OUTCOME (k) - NAME
  #                                         - CONT_DATA (k)
  #                                         - CONT_SUBGROUP - NAME (1)
  #                                                         - CONT_DATA (k)

  # Examples
  # empty: file="CD005507.xml"
  # html:  file="CD005602.xml"
  # xml:   file="CD005601.xml"

  # check file exits
  if (!file.exists(file.path(path, file))) {
    stop(paste("File not found:", file))
  }

  # check file is empty
  info = file.info(file.path(path, file))
  if (info$size == 0) {
    stop(paste("Empty file:", file))
  }

  # check file is XML
  con = file(file.path(path, file), "r")
  first_line = readLines(con, n = 1, warn = FALSE)
  close(con)
  if (!grepl("xml version", substr(first_line,1,20))) {
    stop(paste("File is not XML:", file))
  }

  xmlsrc = read_xml(file.path(path, file))

  # get total number of outcomes and create data frame
  l = length(html_nodes(css=c("DICH_DATA,CONT_DATA,IV_DATA,IPD_DATA"), x=xmlsrc))

  if (l == 0) { # in case there is no outcome data in the file
    stop(paste("No outcome data found in:", file))
  }

  tab = data.frame(id=rep(NA, l), comparison.nr = rep(NA, l), comparison.name = rep(NA, l), comparison.id = rep(NA, l),
                   outcome.nr = rep(NA, l), outcome.name = rep(NA, l), outcome.measure = rep(NA, l), outcome.id = rep(NA, l), outcome.flag = rep(NA, l),
                   subgroup.nr = rep(NA, l), subgroup.name = rep(NA, l), subgroup.id = rep(NA, l),
                   study.id = rep(NA, l), study.name = rep(NA, l), study.year = rep(NA, l), study.data_source = rep(NA, l),
                   effect.size = rep(NA, l), se = rep(NA, l), ci.lower = rep(NA, l), ci.upper = rep(NA, l), weight = rep(NA, l), order = rep(NA, l),
                   events1 = rep(NA, l), total1 = rep(NA, l), mean1 = rep(NA, l), sd1 = rep(NA, l),
                   events2 = rep(NA, l), total2 = rep(NA, l), mean2 = rep(NA, l), sd2 = rep(NA, l))

  # 1. Table with study information
  xml.study = html_nodes(css=c("STUDIES STUDY"), x=xmlsrc)
  j = length(xml.study)
  table.studies = data.frame(id=rep(NA,j), name=rep(NA,j), year=rep(NA,j), data_source=rep(NA,j))
  table.studies$id = html_attr(x = xml.study, name="ID")
  table.studies$name = html_attr(x = xml.study, name="NAME")
  table.studies$year = gsub("\\.", "", html_attr(x = xml.study, name="YEAR")) # 2006. -> 2006
  table.studies$data_source = html_attr(x = xml.study, name="DATA_SOURCE")
  table.studies$id = gsub("--", "-", table.studies$id) # fix study id
  table.studies$name = gsub("  ", " ", table.studies$name) # fix study id

  # 2. Parse single study data from comparisons, outcomes, subgroups, and data nodes
  #    We have to loop across comparison, outcome and subgroups as the data from single
  #    studies have no ID but are hierarchically structured.
  k = 1 # counter
  xml.comparison = html_nodes(css=c("COMPARISON"), x=xmlsrc)
  for (c in 1:length(xml.comparison)) {
    # print(paste("Comparison:", c))
    comparison.nr = html_attr(x = xml.comparison[c], name="NO")
    comparison.name = cleanstr(html_text(html_node(css=c("NAME"), x=xml.comparison[c])))
    comparison.id = html_attr(x = xml.comparison[c], name="ID")

    xml.outcome = html_nodes(css=c("DICH_OUTCOME,CONT_OUTCOME,IV_OUTCOME,IPD_OUTCOME"), x=xml.comparison[c])

    # there are five type of outcomes, see
    #
    # CONT_OUTCOME - continuous data with mean and SD
    # DICH_OUTCOME - dichotomous data with no. of events
    # IV_OUTCOME   - inverse variance meta-analysis in case summary data for groups
    #                is unavailable. Inludes effects and SE only.
    # IPD_OUTCOME  - individual patient data
    # OTHER_OUTCOME - Additional informationn, notes, etc.
    if (length(xml.outcome) > 0) { # handle if xml node is empty
      for (o in 1:length(xml.outcome)) {
        # print(paste("Outcome:", o))
        outcome.nr = html_attr(x = xml.outcome[o], name="NO")
        outcome.name = cleanstr(html_text(html_node(css=c("NAME"), x=xml.outcome[o])))
        outcome.measure = html_attr(x = xml.outcome[o], name="EFFECT_MEASURE")
        outcome.id = html_attr(x = xml.outcome[o], name="ID")
        outcome.flag = NA
        if (grepl("^<CONT_OUTCOME", xml.outcome[o]))  { outcome.flag = "CONT" }
        if (grepl("^<DICH_OUTCOME", xml.outcome[o]))  { outcome.flag = "DICH" }
        if (grepl("^<IV_OUTCOME", xml.outcome[o]))    { outcome.flag = "IV" }
        if (grepl("^<IPD_OUTCOME", xml.outcome[o]))   { outcome.flag = "IPD" }
        if (grepl("^<OTHER_OUTCOME", xml.outcome[o])) { outcome.flag = "OTHER" }

        if (is.na(outcome.measure)) { # DICH and CONT have a attribute, IV and IPD have a node for effect measure.
          outcome.measure = html_text(html_nodes(css=c("EFFECT_MEASURE"), x=xml.outcome[o]))
        }

        has.subgroup = FALSE
        xml.subgroup = 1
        subgroup.nr = 0
        subgroup.name = ""
        subgroup.id = NA
        if (html_attr(name = "SUBGROUPS", x = xml.outcome[o]) == "YES") {
          has.subgroup = TRUE
          xml.subgroup = html_nodes(css=c("DICH_SUBGROUP,CONT_SUBGROUP,IV_SUBGROUP,IPD_SUBGROUP"), x=xml.outcome[o])
        }
        # print(paste("Subgroup:", has.subgroup))
        for (s in 1:length(xml.subgroup)) {
          # print(paste("Subgroup:", s))
          if (has.subgroup) {
            subgroup.nr = html_attr(x = xml.subgroup[s], name="NO")
            subgroup.name = cleanstr(html_text(html_node(css=c("NAME"), x=xml.subgroup[s])))
            xml.data = html_nodes(css=c("DICH_DATA,CONT_DATA,IV_DATA,IPD_DATA"), x=xml.subgroup[s])
            subgroup.id = html_attr(x = xml.subgroup[s], name="ID")
          } else {
            xml.data = html_nodes(css=c("DICH_DATA,CONT_DATA,IV_DATA,IPD_DATA"), x=xml.outcome[o])
          }

          if (length(xml.data) > 0) {
            for (d in 1:length(xml.data)) {
              # print(paste("Data:", d, "k:", k))
              tab$id[k] = sub('(CD[0-9]{6}).*', '\\1', file)

              tab$comparison.nr[k] = comparison.nr
              tab$comparison.name[k] = comparison.name
              tab$comparison.id[k] = comparison.id

              tab$outcome.nr[k] = outcome.nr
              tab$outcome.name[k] = outcome.name
              tab$outcome.measure[k] = outcome.measure
              tab$outcome.id[k] = outcome.id
              tab$outcome.flag[k] = outcome.flag

              tab$subgroup.nr[k] = subgroup.nr
              tab$subgroup.name[k] = subgroup.name
              tab$subgroup.id[k] = subgroup.id

              tab$study.id[k] = gsub("--", "-", html_attr(x = xml.data[d], name="STUDY_ID")) # replce "--" with "-" in study id
              idx = which(table.studies$id %in% tab$study.id[k])[1]
              tab$study.name[k] = table.studies$name[idx]
              tab$study.year[k] = table.studies$year[idx]
              tab$study.data_source[k] = table.studies$data_source[idx]

              tab$effect.size[k]= as.numeric(html_attr(x = xml.data[d], name="EFFECT_SIZE"))
              tab$se[k]         = as.numeric(html_attr(x = xml.data[d], name="SE"))
              tab$ci.lower[k]   = as.numeric(html_attr(x = xml.data[d], name="CI_START"))
              tab$ci.upper[k]   = as.numeric(html_attr(x = xml.data[d], name="CI_END"))
              tab$weight[k]     = as.numeric(html_attr(x = xml.data[d], name="WEIGHT"))
              tab$order [k]     = as.numeric(html_attr(x = xml.data[d], name="ORDER"))
              tab$events1[k]    = as.numeric(html_attr(x = xml.data[d], name="EVENTS_1"))
              tab$total1[k]     = as.numeric(html_attr(x = xml.data[d], name="TOTAL_1"))
              tab$mean1[k]      = as.numeric(html_attr(x = xml.data[d], name="MEAN_1"))
              tab$sd1[k]        = as.numeric(html_attr(x = xml.data[d], name="SD_1"))
              tab$events2[k]    = as.numeric(html_attr(x = xml.data[d], name="EVENTS_2"))
              tab$total2[k]     = as.numeric(html_attr(x = xml.data[d], name="TOTAL_2"))
              tab$mean2[k]      = as.numeric(html_attr(x = xml.data[d], name="MEAN_2"))
              tab$sd2[k]        = as.numeric(html_attr(x = xml.data[d], name="SD_2"))

              k = k + 1
            } # data loop
          }
        } # subgroup loop
      } # outcome loop
    }
  } # comparison loop
  return(tab)
}

#' Reads only meta-analytic data.
#'
#' @param path path where data file is located.
#' @param file  RevMan (.rm5) file name.
#'
#' @return data frame with data from a meta-analysis per row.
#'
#' @export
#' @importFrom xml2 read_xml
#' @importFrom rvest html_nodes html_node html_attr html_text
parse.ma = function(file, path) {

  # check file exits
  if (!file.exists(file.path(path, file))) {
    stop(paste("File not found", file))
  }

  # check file is empty
  info = file.info(file.path(path, file))
  if (info$size == 0) {
    stop(paste("Empty file", file))
  }

  # check file is XML
  con = file(file.path(path, file), "r")
  first_line = readLines(con, n = 1, warn = FALSE)
  close(con)
  if (!grepl("xml version", substr(first_line,1,20))) {
    stop(paste("File is not XML", file))
  }

  xmlsrc = read_xml(file.path(path, file))

  # get total number of outcomes and create data frame
  xml.overall = html_nodes(css=c("DICH_OUTCOME,CONT_OUTCOME,IV_OUTCOME,IPD_OUTCOME,
                                  DICH_SUBGROUP,CONT_SUBGROUP,IV_SUBGROUP,IPD_SUBGROUP"), x=xmlsrc)

  l = length(xml.overall)

  if (l == 0) { # in case there is no outcome data in the file
    stop(paste("No outcome data found for", file))
  }

  tab = data.frame(id=rep(NA, l),
                   outcome.id = rep(NA, l), name = rep(NA, l), outcome.flag = rep(NA, l),
                   hasSubgroups = rep(NA, l), isSubgroup = rep(FALSE, l), effect.measure = rep(FALSE, l),
                   effect.size = rep(NA, l), ci_start = rep(NA, l), ci_end = rep(NA, l),
                   z = rep(NA, l), p_z = rep(NA, l), estimable = rep(NA, l), studies = rep(NA, l),
                   hetero.chi2 = rep(NA, l), hetero.df = rep(NA, l), hetero.p_chi2 = rep(NA, l),
                   I2 = rep(NA, l), tau2 = rep(NA, l), Q = rep(NA, l), p_Q = rep(NA, l), # tau is missing in XML, e.g. CD012282 CMP-004.01
                   total1 = rep(NA, l), total2 = rep(NA, l), random = rep(NA, l), totals = rep(NA, l),
                   subgroups = rep(NA, l), subgroup.test = rep(NA, l))

  for (o in 1:length(xml.overall)) {

    tab$id[o] = sub('(CD[0-9]{6}).*', '\\1', file)
    tab$outcome.id[o] = html_attr(x = xml.overall[o], name="ID") # both outcome and subgroup id
    tab$name[o] = cleanstr(html_text(html_node(css=c("NAME"), x=xml.overall[o]))) # both outcome and subgroup names
    tab$outcome.flag[o] = NA
    if (grepl("^<CONT_(OUTCOME|SUBGROUP)", xml.overall[o]))  { tab$outcome.flag[o] = "CONT" }
    if (grepl("^<DICH_(OUTCOME|SUBGROUP)", xml.overall[o]))  { tab$outcome.flag[o] = "DICH" }
    if (grepl("^<IV_(OUTCOME|SUBGROUP)", xml.overall[o]))    { tab$outcome.flag[o] = "IV" }
    if (grepl("^<IPD_(OUTCOME|SUBGROUP)", xml.overall[o]))   { tab$outcome.flag[o] = "IPD" }
    if (grepl("^<OTHER_(OUTCOME|SUBGROUP)", xml.overall[o])) { tab$outcome.flag[o] = "OTHER" }

    tab$hasSubgroups[o] = html_attr(x = xml.overall[o], name="SUBGROUPS") == "YES"
    if (grepl("^<(DICH|CONT|IV|IPD)_SUBGROUP", xml.overall[o])) {
      tab$isSubgroup[o] = TRUE
    }

    tab$effect.measure[o] = html_attr(x = xml.overall[o], name="EFFECT_MEASURE")
    tab$effect.size[o] = as.numeric(html_attr(x = xml.overall[o], name="EFFECT_SIZE"))
    tab$ci_start[o] = as.numeric(html_attr(x = xml.overall[o], name="CI_START"))
    tab$ci_end[o] = as.numeric(html_attr(x = xml.overall[o], name="CI_END"))
    tab$z[o] = as.numeric(html_attr(x = xml.overall[o], name="Z"))
    tab$p_z[o] = as.numeric(html_attr(x = xml.overall[o], name="P_Z"))
    tab$estimable[o] = html_attr(x = xml.overall[o], name="ESTIMABLE")

    tab$studies[o] = as.numeric(html_attr(x = xml.overall[o], name="STUDIES"))
    tab$hetero.chi2[o] = as.numeric(html_attr(x = xml.overall[o], name="CHI2"))
    tab$hetero.df[o] = as.numeric(html_attr(x = xml.overall[o], name="DF"))
    tab$hetero.p_chi2[o] = as.numeric(html_attr(x = xml.overall[o], name="P_CHI2"))

    tab$I2[o] = as.numeric(html_attr(x = xml.overall[o], name="I2"))
    tab$tau2[o] = as.numeric(html_attr(x = xml.overall[o], name="TAU2"))

    tab$Q[o] = as.numeric(html_attr(x = xml.overall[o], name="Q"))
    tab$p_Q[o] = as.numeric(html_attr(x = xml.overall[o], name="P_Q"))

    tab$total1[o] = as.numeric(html_attr(x = xml.overall[o], name="TOTAL_1"))
    tab$total2[o] = as.numeric(html_attr(x = xml.overall[o], name="TOTAL_2"))

    tab$random[o] = html_attr(x = xml.overall[o], name="RANDOM")
    tab$totals[o] = html_attr(x = xml.overall[o], name="TOTALS")
    tab$subgroups[o] = html_attr(x = xml.overall[o], name="SUBGROUPS")
    tab$subgroup.test[o] = html_attr(x = xml.overall[o], name="SUBGROUP_TEST")
  }

  # cleaning
  tab$hasSubgroups[is.na(tab$hasSubgroups)] = FALSE

  return(tab)
}

cleanstr = function(string) {
  return(gsub("\n|\"|\\\\|[[:space:]]*$", "", string))
}

#' Gets the data directly from the Cochrane Library with a html session.
#'
#' @param doi  doi address (full http URL or short version).
#' @param path path to store data file.
#' @param show.terms whether to show link to Cochrane's Terms and Conditions.
#'
#' @export
#' @importFrom httr content user_agent
#' @importFrom rvest session_jump_to session
get.review = function(doi, path, show.terms = TRUE) {

  msg = "The data available are protected by copyright and may only be used in accordance with\nthe Terms and Conditions, see https://www.cochranelibrary.com/about/data-download.\nBy using the downloaded data you agree to these terms and conditions."

  if (grepl("https://doi.org/", doi)) {
    doi = sub('(^https://doi.org/)(.*)', '\\2', doi)
  }

  id = sub('.*(CD[0-9]{6}).*', '\\1', doi)
  file = paste0(id, "StatsDataOnly.rm5")
  url = paste0("https://www.cochranelibrary.com/cdsr/doi/", doi)
  url_data = paste0(url, "/media/CDSR/", id, "/table_n/", file)

  # create www session
  #ua = user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")
  ua = httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36")
  session_with_ua = rvest::session(url, ua)
  if (!session_with_ua$response$status_code == 200) {
    message(paste(Sys.time(), "error: could not open html session for", id, "\n"))
  } else {
    session_data = rvest::session_jump_to(session_with_ua, url_data) # jump to dataset

    # write to file if session is ok
    if (session_data$response$status_code == 200) {

      if (show.terms) {
        message(msg)
      }

      bin = httr::content(session_data$response, as = "raw")
      writeBin(bin, file.path(path, file))
      message(paste(Sys.time(), "ok: download successful for", id, "\n"))

    } else {
      message(paste(Sys.time(), "error: could not download data for", id, "\n"))
    }
  }
}
