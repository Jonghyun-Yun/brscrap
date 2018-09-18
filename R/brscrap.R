convertusd = function(var){
  temp = gsub( "\\$", "", var)
  out = as.numeric(gsub( ",", "", temp))
  return(out)
}

convertp = function(var){
  as.numeric(gsub( "*%", "", var))/100
}

#' @export
br_app = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/", year, "-appearances-fielding.shtml")
  out = url %>%
    read_html %>%
    html_nodes('table') %>%
    html_table %>%
    data.frame %>%
    filter(row_number() < n()) %>%
    arrange(Tm) %>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_bat = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/", year, "-standard-batting.shtml")
  out = url %>%
    read_html %>%
    html_nodes('table') %>%
    html_table %>%
    data.frame %>%
    filter(row_number() < n() - 2) %>%
    arrange(Tm) %>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_bat_pitch = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/", year, "-batting-pitching.shtml")
  out <- url %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table %>%
    data.frame %>%
    filter(row_number() < n() - 2) %>%
    arrange(Tm) %>%
    select(-one_of(c("PAu"))) %>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_batbr = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/",year,"-baserunning-batting.shtml")
  out = url %>%
    read_html %>%
    html_nodes('table') %>%
    html_table %>%
    data.frame %>%
    filter(row_number() < n() - 2) %>%
    arrange(Tm) %>%
    mutate_at(vars(one_of("RS.","SB.","XBT.")), funs(convertp))%>%
    mutate_at(vars(-one_of("Tm")),funs(as.numeric))%>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_batpv = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/",year,"-value-batting.shtml")
  out = url %>%
    read_html %>%
    html_nodes('table') %>%
    html_table %>%
    data.frame %>%
    filter(row_number() < n() - 1) %>%
    arrange(Tm) %>%
    select(-one_of(c("G")))%>%
    mutate_at(vars(one_of("Salary")), funs(convertusd)) %>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_batrat = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/",year,"-ratio-batting.shtml")
  out = url %>%
    read_html %>%
    html_nodes('table') %>%
    html_table %>%
    data.frame %>%
    filter(row_number() < n() - 2) %>%
    arrange(Tm) %>%
    mutate_at(vars(one_of("IP.","HR.","SO.","BB.","XBH.","X.H.","LD.","HR.FB","IF.FB")), funs(convertp)) %>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_batsit = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/", year, "-situational-batting.shtml")
  out = url %>%
    read_html %>%
    html_nodes('table') %>%
    html_table %>%
    data.frame() %>%
    filter((row_number() < n() - 2)) %>%
    setnames(old = c("Var.1","Var.2","Var.3","Var.4","Hits","Hits.1","Hits.2","Pinch.Hitting","Pinch.Hitting.1","Pinch.Hitting.2","Pinch.Hitting.3","Pinch.Hitting.4","Home.Runs","Home.Runs.1","Home.Runs.2","Home.Runs.3","Home.Runs.4","Home.Runs.5","Home.Runs.6","Home.Runs.7","SH","SH.1","SH.2","GIDP","GIDP.1","GIDP.2","PrdOut","PrdOut.1","PrdOut.2","BaseRunners","BaseRunners.1","BaseRunners.2","Advances","Advances.1","Advances.2","Advances.3","Advances.4","Advances.5","Var.39"), new = c("Tm","R.G","PA","Ptn.","Hits","Hits.Inf","Hits.Bnt","PH.AB","PH.H","PH.HR","PH.RBI","PH.PHlev","HR.All","HR.GS","HR.GSo","HR.vRH","HR.vLH","HR.Hm","HR.Rd","HR.IP","SH.Att","SH.Suc","SH.percent","GIDP.Opp","GIDP.DP","GIDP.percent","PrdOut.Opp","PrdOut.Suc","PrdOut.percent","BR","BRS","BRS.","Adv.2.3B","Adv.Scr","Adv.","Adv.0.2B","Adv","Adv.2","PAu"))%>%
    arrange(Tm) %>%
    select(-one_of(c("PAu"))) %>%
    mutate_at(vars(one_of("Ptn.","SH.percent","GIDP.percent","PrdOut.percent","BRS.","Adv.","Adv.2")), funs(convertp))%>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    remove.na.rows()%>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_fld = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/", year, "-standard-fielding.shtml")
  out = url %>%
    read_html %>%
    html_nodes('table') %>%
    html_table %>%
    data.frame %>%
    filter(row_number() < n() - 2) %>%
    arrange(Tm) %>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_fld1b = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/",year,"-specialpos_1b-fielding.shtml")
  out = url %>%
    read_html() %>%
    html_nodes('#all_teams_standard_fielding') %>%
    html_node('table') %>%
    html_table() %>%
    data.frame() %>%
    filter(row_number() < n() - 2)%>%
    arrange(Tm)%>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_fld2b = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/",year,"-specialpos_2b-fielding.shtml")
  out = url %>%
    read_html() %>%
    html_nodes('#all_teams_standard_fielding') %>%
    html_node('table') %>%
    html_table() %>%
    data.frame() %>%
    filter(row_number() < n() - 2)%>%
    arrange(Tm)%>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_fld3b = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/",year,"-specialpos_3b-fielding.shtml")
  out = url %>%
    read_html() %>%
    html_nodes('#all_teams_standard_fielding') %>%
    html_node('table') %>%
    html_table() %>%
    data.frame() %>%
    filter(row_number() < n() - 2)%>%
    arrange(Tm)%>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_fldc = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/",year,"-specialpos_c-fielding.shtml")
  out = url %>%
    read_html() %>%
    html_nodes('#all_teams_standard_fielding') %>%
    html_node('table') %>%
    html_table() %>%
    data.frame() %>%
    filter(row_number() < n() - 2)%>%
    arrange(Tm)%>%
    mutate_at(vars(one_of("CS.")), funs(convertp))%>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_fldcf = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/",year,"-specialpos_cf-fielding.shtml")
  out = url %>%
    read_html() %>%
    html_nodes('#all_teams_standard_fielding') %>%
    html_node('table') %>%
    html_table() %>%
    data.frame() %>%
    select(-one_of(c("Rhr")))%>%
    filter(row_number() < n() - 2)%>%
    arrange(Tm)%>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_fldlf = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/",year,"-specialpos_lf-fielding.shtml")
  out = url %>%
    read_html() %>%
    html_nodes('#all_teams_standard_fielding') %>%
    html_node('table') %>%
    html_table() %>%
    data.frame() %>%
    select(-one_of(c("Rhr")))%>%
    filter(row_number() < n() - 2)%>%
    arrange(Tm)%>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_fldof = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/",year,"-specialpos_of-fielding.shtml")
  out = url %>%
    read_html() %>%
    html_nodes('#all_teams_standard_fielding') %>%
    html_node('table') %>%
    html_table() %>%
    data.frame() %>%
    filter(row_number() < n() - 2)%>%
    arrange(Tm)%>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_fldp = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/",year,"-specialpos_p-fielding.shtml")
  out = url %>%
    read_html() %>%
    html_nodes('#all_teams_standard_fielding') %>%
    html_node('table') %>%
    html_table() %>%
    data.frame() %>%
    filter(row_number() < n() - 2)%>%
    arrange(Tm)%>%
    mutate_at(vars(one_of("CS.")), funs(convertp))%>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_fldrf = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/",year,"-specialpos_rf-fielding.shtml")
  out = url %>%
    read_html() %>%
    html_nodes('#all_teams_standard_fielding') %>%
    html_node('table') %>%
    html_table() %>%
    data.frame() %>%
    select(-one_of(c("Rhr")))%>%
    filter(row_number() < n() - 2)%>%
    arrange(Tm)%>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_fldss = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/",year,"-specialpos_ss-fielding.shtml")
  out = url %>%
    read_html() %>%
    html_nodes('#all_teams_standard_fielding') %>%
    html_node('table') %>%
    html_table() %>%
    data.frame() %>%
    filter(row_number() < n() - 2)%>%
    arrange(Tm)%>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_pitch = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/", year, "-standard-pitching.shtml")
  out <- url %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table %>%
    data.frame %>%
    filter(row_number() < n() - 2) %>%
    arrange(Tm) %>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_pitch2 = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/",year,"-pitches-pitching.shtml")
  out = url %>%
    read_html %>%
    html_nodes('table') %>%
    html_table %>%
    data.frame %>%
    select(-one_of(c("PAu","Pitu","Stru")))%>%
    filter(row_number() < n() - 2) %>%
    arrange(Tm) %>%
    mutate_at(vars(one_of("Str.","L.Str","S.Str","F.Str","I.Str","AS.Str","I.Bll","AS.Pit","Con","X1st.","X30.","X02.","L.SO.")), funs(convertp))%>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_pitchrat = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/",year,"-ratio-pitching.shtml")
  out = url %>%
    read_html %>%
    html_nodes('table') %>%
    html_table %>%
    data.frame %>%
    select(-one_of(c("PAu")))%>%
    filter(row_number() < n() - 2) %>%
    arrange(Tm) %>%
    mutate_at(vars(one_of("Ptn.","HR.","SO.","BB.","SO.BB.","XBH.","IP.","LD.","HR.FB","IF.FB","X.","X.H.")), funs(convertp))%>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_pitchrel = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/",year,"-reliever-pitching.shtml")
  out = url %>%
    read_html %>%
    html_nodes('table') %>%
    html_table %>%
    data.frame %>%
    select(-one_of(c("X1stIP")))%>%
    filter(row_number() < n() - 2) %>%
    arrange(Tm) %>%
    mutate_at(vars(one_of("SV.","IS.")), funs(convertp))%>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_pitchst = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/",year,"-starter-pitching.shtml")
  out = url %>%
    read_html %>%
    html_nodes('table') %>%
    html_table %>%
    data.frame %>%
    filter(row_number() < n() - 2) %>%
    arrange(Tm) %>%
    mutate_at(vars(one_of("QS.")), funs(convertp))%>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_pitchpv = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/",year,"-value-pitching.shtml")
  out = url %>%
    read_html %>%
    html_nodes('table') %>%
    html_table %>%
    data.frame %>%
    filter(row_number() < n() - 1) %>%
    arrange(Tm) %>%
    mutate_at(vars(one_of("Salary")), funs(convertusd))%>%
    mutate_at(vars(-one_of("Tm")), funs(as.numeric)) %>%
    mutate(yearID = year)
  return(out)
}

#' @export
br_stand = function(year){
  url = paste0("https://www.baseball-reference.com/leagues/MLB/",year,"-standings.shtml")
  br = url %>% read_html() # parse html

  post = br %>%
    html_nodes('#all_postseason') %>%     # select node with comment
    html_nodes(xpath = 'comment()') %>%   # select comments within node
    html_text() %>%                       # return contents as text
    read_html() %>%                       # parse text as html
    html_node('table') %>%                # select table node
    html_table() %>%
    separate(X3, into = paste0("V", 1:2), sep = " over ")

  lt = post$V2
  wt = post$V1

  stand = br %>%
    html_nodes('#all_expanded_standings_overall') %>%     # select node with comment
    html_nodes(xpath = 'comment()') %>%   # select comments within node
    html_text() %>%                       # return contents as text
    read_html() %>%                       # parse text as html
    html_node('table') %>%                # select table node
    html_table() %>%
    filter(row_number()!=n())

  tn = br %>%
    html_nodes("#all_expanded_standings_overall") %>%
    html_nodes(xpath = 'comment()') %>%   # select comments within node
    html_text() %>%                       # return contents as text
    read_html() %>%                       # parse text as html
    html_nodes('a')                # select table node

  teamID = tn %>% html_text()
  team = tn %>% html_attr('title')

  playoff = character(length(team))
  playoff[team == wt[1]] = "WSWin"
  playoff[team %in% lt] = "yes"
  playoff[team == wt[2]] = "LgWin"

  stand = stand %>%
    mutate(Playoff = playoff, Team = team) %>%
    arrange(Tm)

  url = paste0("https://www.baseball-reference.com/leagues/MLB/",year,"-misc.shtml")

  misc = url %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table(fill = T) %>%
    data.frame

  x = misc$'Est..Payroll'
  misc = misc %>%
    mutate(payroll = as.numeric(gsub('[$,]', '', x))) %>%
    arrange(Tm)

  stand = stand %>%
    mutate(Payroll = misc$payroll
           , yearID = year
    )
  return(stand)
}

#' @export
br_team = function(years){
  temp_bat = temp_pitch = temp_bat_pitch = temp_stand = list()
  bat = pitch = bat_pitch = stand = data.frame()
  k = 0
  for (t in years) {
    k = k + 1
    cat(sprintf("Scraping %i MLB data sets from www.baseball-reference.com...\n",t))
    temp_bat[[k]] = br_bat(t)
    temp_pitch[[k]] = br_pitch(t)
    temp_bat_pitch[[k]] = br_bat_pitch(t)
    temp_stand[[k]] = br_stand(t)
  }
  bat = bind_rows(temp_bat)
  pitch = bind_rows(temp_pitch)
  bat_pitch = bind_rows(temp_bat_pitch)
  stand = bind_rows(temp_stand)
  return(
    list(stand = stand, bat = bat, pitch = pitch, bat_pitch = bat_pitch)
  )
}
