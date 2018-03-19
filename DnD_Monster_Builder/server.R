# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)

# Functions ----------
ability_score_display <- function(data, ability){
  ability <- 
    data %>% 
    filter(Ability == ability)
  
  paste0(
    ability$Score
    ," ("
    ,if_else(ability$Score >= 10, "+", "-")
    ,abs(ability$Modifier)
    ,")"
  )
}

# Variables ----------
monster_sizes <- 
  tibble(
    Long =
      c(
        "Tiny"
        ,"Small"
        ,"Medium"
        ,"Large"
        ,"Huge"
        ,"Gargantuan"
      )
    ,Short = 
      c(
        "t"
        ,"s"
        ,"m"
        ,"l"
        ,"h"
        ,"g"
      )
    ,`Hit Die` = c(2 * (2:6), 20 )
    ,`Extra Damage Dice` = 
      c(
        rep(x = 1, times = 3)
        ,2:4
      )
  ) %>% 
  mutate(
    `Average HP` = (`Hit Die` + 1)/2
  )

armor_table <-
  tibble(
    Type =
      c(
        "Natural Armor"
        ,"Padded"
        ,"Leather"
        ,"Studded Leather"
        ,"Hide"
        ,"Chain Shirt"
        ,"Scale Mail"
        ,"Breastplate"
        ,"Halfplate"
        ,"Ring Mail"
        ,"Chain Mail"
        ,"Split"
        ,"Plate"
      )
    ,Weight =
      c(
        rep(x = "Natural", times = 1)
        ,rep(x = "Light", times = 3)
        ,rep(x = "Medium", times = 5)
        ,rep(x = "Heavy", times = 4)
      )
    ,Base =
      c(
        10
        , 11, 11, 12
        , 12, 13, 14, 14, 15
        , 14, 16, 17, 18
      )
    ,`Max Dex` =
      c(
        rep(NA, times = 4)
        ,rep(2, times = 5)
        ,rep(0, times = 4)
      )
  )

skills_table <-
  list(
    Str = "Athletics"
    ,Dex = 
      c(
        "Acrobatics"
        ,"Sleight of Hand"
        ,"Stealth"
      )
    ,Int =
      c(
        "Arcana"
        ,"History"
        ,"Investigation"
        ,"Nature"
        ,"Religion"
      )
    ,Wis = 
      c(
        "Animal Handling"
        ,"Insight"
        ,"Medicine"
        ,"Perception"
        ,"Survival"
      )
    ,Cha = 
      c(
        "Deception"
        ,"Intimidation"
        ,"Performance"
        ,"Persuasion"
      )
  ) %>% 
  do.call(cbind, .) %>% 
  as.tibble() %>% 
  gather(
    key = Ability
    ,value = Skill
  ) %>% 
  unique()

cr_table <-
  tibble(
    CR =
      c(
        0
        ,1/8
        ,1/4
        ,1/2
        ,1:30
      )
    ,XP =
      c(
        0
        ,25
        ,50
        ,100
        ,200
        ,450
        ,700
        ,1100
        ,1800
        ,2300
        ,2900
        ,3900
        ,5000
        ,5900
        ,7200
        ,8400
        ,10000
        ,11500
        ,13000
        ,15000
        ,18000
        ,20000
        ,22000
        ,25000
        ,33000
        ,41000
        ,50000
        ,62000
        ,75000
        ,90000
        ,105000
        ,120000
        ,135000
        ,155000
      )
    ,Prof = 
      rep(
        x = 2:9
        ,times = 
          c(
            8
            ,rep(x = 4, times = 6)
            ,2
          )
      )
    ,AC =
      rep(
        x = 13:19
        ,times = 
          c(
            7
            ,1
            ,3
            ,2
            ,3
            ,4
            ,14
          )
      )
    ,HP_LB =
      c(
        1
        ,7
        ,36
        ,50
        ,71
        ,86
        ,101
        ,116
        ,131
        ,146
        ,161
        ,176
        ,191
        ,206
        ,221
        ,236
        ,251
        ,266
        ,281
        ,296
        ,311
        ,326
        ,341
        ,356
        ,401
        ,446
        ,491
        ,536
        ,581
        ,626
        ,671
        ,716
        ,761
        ,806
      )
    ,HP_UB =
      c(
        1 + 5
        ,7 + 28
        ,36 + 13
        ,50 + 20
        ,71 + 14
        ,86 + 14
        ,101 + 14
        ,116 + 14
        ,131 + 14
        ,146 + 14
        ,161 + 14
        ,176 + 14
        ,191 + 14
        ,206 + 14
        ,221 + 14
        ,236 + 14
        ,251 + 14
        ,266 + 14
        ,281 + 14
        ,296 + 14
        ,311 + 14
        ,326 + 14
        ,341 + 14
        ,356 + 44
        ,401 + 44
        ,446 + 44
        ,491 + 44
        ,536 + 44
        ,581 + 44
        ,626 + 44
        ,671 + 44
        ,716 + 44
        ,761 + 44
        ,806 + 44
      )
    ,Attack =
      rep(
        x = 3:14
        ,times = 
          c(
            6
            ,1
            ,1
            ,3
            ,3
            ,5
            ,1
            ,4
            ,3
            ,3
            ,3
            ,1
          )
      )
    ,Damage_LB =
      c(
        0
        ,2
        ,4
        ,6
        ,9
        ,15
        ,21
        ,27
        ,33
        ,39
        ,45
        ,51
        ,57
        ,63
        ,69
        ,75
        ,81
        ,87
        ,93
        ,99
        ,105
        ,111
        ,117
        ,123
        ,141
        ,159
        ,177
        ,195
        ,213
        ,231
        ,249
        ,267
        ,285
        ,303
      )
    ,Damage_UB =
      c(
        0 + 1
        ,2 + 1
        ,4 + 1
        ,6 + 2
        ,9 + 5
        ,15 + 5
        ,21 + 5
        ,27 + 5
        ,33 + 5
        ,39 + 5
        ,45 + 5
        ,51 + 5
        ,57 + 5
        ,63 + 5
        ,69 + 5
        ,75 + 5
        ,81 + 5
        ,87 + 5
        ,93 + 5
        ,99 + 5
        ,105 + 5
        ,111 + 5
        ,117 + 5
        ,123 + 17
        ,141 + 17
        ,159 + 17
        ,177 + 17
        ,195 + 17
        ,213 + 17
        ,231 + 17
        ,249 + 17
        ,267 + 17
        ,285 + 17
        ,303 + 17
      )
    ,DC =
      rep(
        x = 13:23
        ,times = 
          c(
            7
            ,1
            ,3
            ,3
            ,2
            ,4
            ,4
            ,3
            ,3
            ,3
            ,1
          )
      )
  )

e_hp_resist_immune <-
  tibble(
    CR = 1:30
    ,Resist =
      c(
        rep(x = 2, times = 4)
        ,rep(x = 1.5, times = 6)
        ,rep(x = 1.25, times = 6)
        ,rep(x = 1, times = 14)
      )
    ,Immune =
      c(
        rep(x = 2, times = 10)
        ,rep(x = 1.5, times = 6)
        ,rep(x = 1.25, times = 14)
      )
  )

weapon_table <- 
  tibble(
    Name =
      c(
        "Club"
        ,"Dagger"
        ,"Greatclub"
        ,"Handaxe"
        ,"Javelin"
        ,"Light Hammer"
        ,"Mace"
        ,"Quarterstaff"
        ,"Sickle"
        ,"Spear"
        ,"Light Crossbow"
        ,"Dart"
        ,"Shortbow"
        ,"Sling"
        ,"Battleaxe"
        ,"Flail"
        ,"Glaive"
        ,"Greataxe"
        ,"Greatsword"
        ,"Halberd"
        ,"Lance"
        ,"Longsword"
        ,"Maul"
        ,"Morningstar"
        ,"Pike"
        ,"Rapier"
        ,"Scimitar"
        ,"Shortsword"
        ,"Trident"
        ,"War Pick"
        ,"Warhammer"
        ,"Whip"
        ,"Blowgun"
        ,"Hand Crossbow"
        ,"Heavy Crossbow"
        ,"Longbow"
        ,"Net"
      )
    ,Category = 
      rep(x = c("Simple", "Martial"), times = c(14, 23))
    ,Class =
      rep(
        x = rep(x = c("Melee", "Ranged"), times = 2)
        ,times = c(10, 4, 18, 5)
      )
    ,`Die Size` =
      c(
        rep(x = 4, times = 2)
        ,rep(x = 8, times = 1)
        ,rep(x = 6, times = 2)
        ,rep(x = 4, times = 1)
        ,rep(x = 6, times = 2)
        ,rep(x = 4, times = 1)
        ,rep(x = 6, times = 1)
        ,rep(x = 8, times = 1)
        ,rep(x = 4, times = 1)
        ,rep(x = 6, times = 1)
        ,rep(x = 4, times = 1)
        ,rep(x = 8, times = 2)
        ,rep(x = 10, times = 1)
        ,rep(x = 12, times = 1)
        ,rep(x = 6, times = 1)
        ,rep(x = 10, times = 1)
        ,rep(x = 12, times = 1)
        ,rep(x = 8, times = 1)
        ,rep(x = 6, times = 1)
        ,rep(x = 8, times = 1)
        ,rep(x = 10, times = 1)
        ,rep(x = 8, times = 1)
        ,rep(x = 6, times = 3)
        ,rep(x = 8, times = 2)
        ,rep(x = 4, times = 1)
        ,rep(x = 1, times = 1)
        ,rep(x = 6, times = 1)
        ,rep(x = 10, times = 1)
        ,rep(x = 8, times = 1)
        ,rep(x = 0, times = 1)
      )
    ,`Number of Dice` =
      c(
        rep(x = 1, times = 18)
        ,rep(x = 2, times = 1)
        ,rep(x = 1, times = 3)
        ,rep(x = 2, times = 1)
        ,rep(x = 1, times = 13)
        ,rep(x = 0, times = 1)
      )
    ,`Damage Type` =
      c(
        rep(x = "Bludgeoning", times = 1)
        ,rep(x = "Piercing", times = 1)
        ,rep(x = "Bludgeoning", times = 1)
        ,rep(x = "Slashing", times = 1)
        ,rep(x = "Piercing", times = 1)
        ,rep(x = "Bludgeoning", times = 3)
        ,rep(x = "Slashing", times = 1)
        ,rep(x = "Piercing", times = 4)
        ,rep(x = "Bludgeoning", times = 1)
        ,rep(x = "Slashing", times = 1)
        ,rep(x = "Bludgeoning", times = 1)
        ,rep(x = "Slashing", times = 4)
        ,rep(x = "Piercing", times = 1)
        ,rep(x = "Slashing", times = 1)
        ,rep(x = "Bludgeoning", times = 1)
        ,rep(x = "Piercing", times = 3)
        ,rep(x = "Slashing", times = 1)
        ,rep(x = "Piercing", times = 3)
        ,rep(x = "Bludgeoning", times = 1)
        ,rep(x = "Slashing", times = 1)
        ,rep(x = "Piercing", times = 4)
        ,rep(x = NA_character_, times = 1)
      )
    ,Ammunition =
      c(
        rep(x = FALSE, times = 10)
        ,rep(x = TRUE, times = 1)
        ,rep(x = FALSE, times = 1)
        ,rep(x = TRUE, times = 2)
        ,rep(x = FALSE, times = 18)
        ,rep(x = TRUE, times = 4)
        ,rep(x = FALSE, times = 1)
      )
    # ,Finesse
    # ,Heavy
    # ,Light
    # ,Loading
    ,`1st Range` =
      c(
        rep(x = 0, times = 1)
        ,rep(x = 20, times = 1)
        ,rep(x = 0, times = 1)
        ,rep(x = 20, times = 1)
        ,rep(x = 30, times = 1)
        ,rep(x = 20, times = 1)
        ,rep(x = 0, times = 3)
        ,rep(x = 20, times = 1)
        ,rep(x = 80, times = 1)
        ,rep(x = 20, times = 1)
        ,rep(x = 80, times = 1)
        ,rep(x = 30, times = 1)
        ,rep(x = 0, times = 14)
        ,rep(x = 20, times = 1)
        ,rep(x = 0, times = 3)
        ,rep(x = 25, times = 1)
        ,rep(x = 30, times = 1)
        ,rep(x = 100, times = 1)
        ,rep(x = 150, times = 1)
        ,rep(x = 5, times = 1)
      )
    ,`2nd Range` =
      c(
        rep(x = 0, times = 1)
        ,rep(x = 60, times = 1)
        ,rep(x = 0, times = 1)
        ,rep(x = 60, times = 1)
        ,rep(x = 120, times = 1)
        ,rep(x = 60, times = 1)
        ,rep(x = 0, times = 3)
        ,rep(x = 60, times = 1)
        ,rep(x = 320, times = 1)
        ,rep(x = 60, times = 1)
        ,rep(x = 320, times = 1)
        ,rep(x = 120, times = 1)
        ,rep(x = 0, times = 14)
        ,rep(x = 60, times = 1)
        ,rep(x = 0, times = 3)
        ,rep(x = 100, times = 1)
        ,rep(x = 120, times = 1)
        ,rep(x = 400, times = 1)
        ,rep(x = 600, times = 1)
        ,rep(x = 15, times = 1)
      )
    ,Reach =
      c(
        rep(x = FALSE, times = 16)
        ,rep(x = TRUE, times = 1)
        ,rep(x = FALSE, times = 2)
        ,rep(x = TRUE, times = 2)
        ,rep(x = FALSE, times = 3)
        ,rep(x = TRUE, times = 1)
        ,rep(x = FALSE, times = 6)
        ,rep(x = TRUE, times = 1)
        ,rep(x = FALSE, times = 5)
      )
    # ,Special
    # ,Thrown
    # ,`Two-Handed`
    # ,Versatile
  ) %>% 
  mutate(`Average Roll` = (`Die Size` + 1)/2)


# Server ----------
shinyServer(
  function(input, output, session) {
    # Description ----------
    output$name <- renderText(input$name)
    
    output$sub_name <- renderText({
      size <- 
        ifelse(
          input$size == ""
          ,""
          ,monster_sizes %>% 
            filter(Short == input$size) %>% 
            select(Long) %>% 
            as.character()
        )
      
      type <- tolower(input$type)
      
      law_chaos <- input$law_chaos
      good_evil <- input$good_evil
      
      alignment <- tolower(
        if(law_chaos == "" & good_evil == ""){
          "Unaligned"
        } else if(law_chaos == "Neutral" & good_evil == "Neutral"){
          "Neutral"
        } else{
          paste(law_chaos, good_evil)
        }
      )
      
      paste0(size, " ", type, ", ", alignment)
    })
    
    # Physical ----------
    ac_calc <- reactive({
      armor_chosen <- 
        armor_table %>% 
        filter(Type == input$armor_type)
      
      dex_mod <-
        ability_scores() %>% 
        filter(Ability == "Dex") %>% 
        select(Modifier) %>% 
        as.numeric()
      
      ac <- 
        armor_chosen$Base + 
        min(dex_mod, armor_chosen$`Max Dex`, na.rm = TRUE) + 
        2*input$shield +
        if_else(armor_chosen$Type == "Natural Armor", input$nat_armor, as.integer(0))
    })
    output$ac <- renderText({
      armor_chosen <- 
        armor_table %>% 
        filter(Type == input$armor_type)
      
      paste0("Armor Class ", ac_calc(), " (", tolower(armor_chosen$Type), ")")
    })
    
    hp_calc <- reactive({
      hit_die <- 
        monster_sizes %>% 
        filter(Short == input$size) %>% 
        select(`Hit Die`, `Average HP`)
      
      con_mod <-  
        ability_scores() %>% 
        filter(Ability == "Con") %>% 
        select(Modifier) %>% 
        as.numeric()
      
      hp <- max(floor((hit_die$`Average HP` + con_mod) * input$hit_dice), 1)
    })
    output$hp <- renderText({
      hit_die <- 
        monster_sizes %>% 
        filter(Short == input$size) %>% 
        select(`Hit Die`, `Average HP`)
      
      con_mod <-  
        ability_scores() %>% 
        filter(Ability == "Con") %>% 
        select(Modifier) %>% 
        as.numeric()
      
      paste0(
        "Hit Points "
        ,hp_calc()
        ," ("
        ,input$hit_dice
        ,"d"
        ,hit_die$`Hit Die`
        ," + "
        ,input$hit_dice * con_mod
        ,")"
      )
    })
    
    output$speed <- renderText(
      paste0(
        paste("Speed", input$speed, "ft.")
        ,if_else(input$burrow == 0, "", paste(", burrow", input$burrow, "ft."))
        ,if_else(input$climb == 0, "", paste(", climb", input$climb, "ft."))
        ,if_else(input$fly == 0, "", paste(", fly", input$fly, "ft."))
        ,if_else(input$swim == 0, "", paste(", swim", input$swim, "ft."))
      )
    )
    
    # Ability ----------
    ability_scores <- reactive({
      tibble(
        Ability = 
          c(
            "Str"
            ,"Dex"
            ,"Con"
            ,"Int"
            ,"Wis"
            ,"Cha"
          )
        ,Score =
          c(
            input$str
            ,input$dex
            ,input$con
            ,input$int
            ,input$wis
            ,input$cha
          )
      ) %>% 
        mutate(Modifier = (Score - 10)%/%2)
    })
    
    output$str <- renderText("STR")
    output$str_num <- renderText(ability_score_display(ability_scores(), "Str"))
    output$dex <- renderText("DEX")
    output$dex_num <- renderText(ability_score_display(ability_scores(), "Dex"))
    output$con <- renderText("CON")
    output$con_num <- renderText(ability_score_display(ability_scores(), "Con"))
    output$int <- renderText("INT")
    output$int_num <- renderText(ability_score_display(ability_scores(), "Int"))
    output$wis <- renderText("WIS")
    output$wis_num <- renderText(ability_score_display(ability_scores(), "Wis"))
    output$cha <- renderText("CHA")
    output$cha_num <- renderText(ability_score_display(ability_scores(), "Cha"))
    
    # Other ----------
    prof_bonus <- reactive(
      cr_table %>% 
        filter(CR == input$expected_cr) %>% 
        select(Prof) %>% 
        as.numeric()
    )
    
    output$save_throw <- renderPrint({
      scores <- 
        ability_scores() %>% 
        filter(Ability %in% input$saves)
      
      if(is.null(input$saves)){
        cat("")
      } else{
        cat("Saving Throws:", paste(scores$Ability, scores$Modifier + prof_bonus(), collapse = ", "))
      }
    })
    
    output$skill_prof <- renderPrint({
      skill_scores <- 
        ability_scores() %>% 
        left_join(
          skills_table %>% 
            filter(Skill %in% input$skills)
          ,by = "Ability"
        ) %>% 
        filter(!is.na(Skill)) %>% 
        mutate(Modifier = Modifier + prof_bonus())

            
      if(is.null(input$skills)){
        cat("")
      } else{
        cat("Skills:", paste(skill_scores$Skill, skill_scores$Modifier, collapse = ", "))
      }
    })
    
    output$d_vuln <- renderPrint(
      if(is.null(input$vuln)){
        cat("")
      } else{
        cat("Damage Vulnerabilities:", input$vuln[order(input$vuln)])
      }
    )
    output$d_resist <- renderPrint(
      if(is.null(input$resist)){
        cat("")
      } else{
        cat("Damage Resistances:", input$resist[order(input$resist)])
      }
    )
    output$d_immune <- renderPrint(
      if(is.null(input$immune)){
        cat("")
      } else{
        cat("Damage Immunities:", input$immune[order(input$immune)])
      }
    )
    output$c_immune <- renderPrint(
      if(is.null(input$cond_immune)){
        cat("")
      } else{
        cat("Condition Immunities:", input$cond_immune[order(input$cond_immune)])
      }
    )
    
    output$senses <- renderText({
      passive_perception <- 
        10 +
        filter(ability_scores(), Ability == "Wis")$Modifier +
        (prof_bonus() * if_else("Perception" %in% input$skills, TRUE, FALSE))
      
      paste0(
        "Senses: "
        ,if_else(input$blindsight == 0, "", paste("Blindsight", input$blindsight, "ft., "))
        ,if_else(input$darkvision == 0, "", paste("Darkvision", input$darkvision, "ft., "))
        ,if_else(input$tremorsense == 0, "", paste("Tremorsense", input$tremorsense, "ft., "))
        ,if_else(input$truesight == 0, "", paste("Truesight", input$truesight, "ft., "))
        ,paste("Passive Perception", passive_perception)
      )
    })
    
    output$languages <- renderPrint(
      if(is.null(input$lang)){
        cat("")
      } else{
        cat("Languages", paste0(input$lang[order(input$lang)], sep = ","))
      }
    )
    
    output$cr <- renderText({
      cr <- input$expected_cr
      xp <- 
        cr_table %>% 
        filter(CR == cr) %>% 
        select(XP) %>% 
        as.numeric()
      
      paste0("Challenge ", cr, " (", prettyNum(xp, big.mark = ","), "XP)")
    })
    
    # Abilities ----------
    
    # Actions ----------
    output$multiattack <- renderText({
      paste(
        "The"
        ,tolower(input$name)
        ,"makes"
        ,input$multiattack
        ,tolower(input$weapon_type)
        ,"attacks"
      )
    })
    
    weapons_chosen <- reactive({
      die_multi <- 
        monster_sizes %>% 
        filter(Short == input$size) %>% 
        select(`Extra Damage Dice`) %>% 
        as.numeric()
      
      ability_mod <- 
        ability_scores() %>% 
        filter(Ability %in% c("Str", "Dex")) %>% 
        summarize(Modifier = max(Modifier)) %>% 
        as.numeric()
      
      weapon_table %>% 
        filter(Name %in% input$weapon_type) %>% 
        mutate(
          `Number of Dice` = `Number of Dice` * die_multi
          ,`Average Damage` = floor(`Average Roll` * `Number of Dice`) + ability_mod
        )
    })
    output$weapons <- renderTable(weapons_chosen())
    
    ab_calc <- reactive({
      ability_scores() %>% 
        filter(Ability %in% c("Str", "Dex")) %>% 
        summarize(Modifier = max(Modifier)) %>% 
        as.numeric() + prof_bonus()
    })
    output$ab <- renderText(ab_calc())
    
    damage_calc <- reactive({
      weapons_chosen() %>% 
        summarize(`Average Damage` = sum(`Average Damage`)) %>% 
        as.numeric() * input$multiattack
    })
    # Determined CR ----------
    ## Defensive ----------
    effective_hp <- reactive({
      num_resist_immune <- 
        if_else(
          length(input$resist) + length(input$immune) == 0
          ,1
          ,as.numeric(length(input$resist) + length(input$immune))
        )
      
      hp_multi <- 
        e_hp_resist_immune %>% 
        filter(CR == input$expected_cr) %>% 
        transmute(
          Multiplier = (Resist*length(input$resist) + Immune*length(input$immune))/num_resist_immune
        ) %>% 
        as.numeric()
        
      
      hp_calc() *
        if_else(
          num_resist_immune >= 3
          ,hp_multi
          ,1
        )
    })
    output$e_hp <- renderText(paste("Effective HP", effective_hp()))
    
    effective_ac <- reactive({
      ac_calc() +
        if_else(
          input$fly > 0 &
            #Ranged Attacks &
            input$expected_cr <= 10
          ,2
          ,0
        ) +
        if_else(
          length(input$saves) >= 3
          ,2
          ,0
        ) +
        if_else(
          length(input$saves) >= 5
          ,2
          ,0
        )
    })
    output$e_ac <- renderText(paste("Effective AC", effective_ac()))
    
    defensive_cr <- reactive({
      defensive_cr <- 
        cr_table %>% 
        filter(
          HP_LB <= effective_hp()
          ,HP_UB >= effective_hp()
        )
      ac_adj <- (effective_ac() - defensive_cr$AC)%/%2
      max(defensive_cr$CR + ac_adj, 0)
    })
    output$defensive_cr <- renderText(paste("Defensive CR", defensive_cr()))
    
    ## Offensive ----------
    effective_damage <- reactive({
      damage_calc()
    })
    output$e_damage <- renderText(paste("Effective Damage", effective_damage()))
    
    effective_ab <- reactive({
      ab_calc()
    })
    output$e_ab <- renderText(paste("Effective Attack Bonus", effective_ab()))
    
    offensive_cr <- reactive({
      offensive_cr <-
        cr_table %>%
        filter(
          Damage_LB <= effective_damage()
          ,Damage_UB >= effective_damage()
        )
      ab_adj <- (effective_ab() - offensive_cr$Attack)%/%2
      max(offensive_cr$CR + ab_adj, 0)
    })
    output$offensive_cr <- renderText(paste("Offensive CR", offensive_cr()))
    
    ## Final CR ----------
    final_cr <- reactive({
      final_cr <- mean(c(offensive_cr(), defensive_cr()))
      cr_table$CR[which.min(abs(cr_table$CR - final_cr - 0.0000001))]
    })
    output$final_cr <- renderText(paste("Final CR", final_cr()))
  })
