# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)

# Variables ----------
monster_size <- 
  c(
    "Tiny" = "t"
    ,"Small" = "s"
    ,"Medium" = "m"
    ,"Large" = "l"
    ,"Huge" = "h"
    ,"Gargantuan" = "g"
  )

armor_type <-
  list(
    `Natural Armor` = "Natural Armor"
    ,`Light Armor` =
      c(
        "Padded"
        ,"Leather"
        ,"Studded Leather"
      )
    ,`Medium Armor` =
      c(
        "Hide"
        ,"Chain Shirt"
        ,"Scale Mail"
        ,"Breastplate"
        ,"Halfplate"
      )
    ,`Heavy Armor` =
      c(
        "Ring Mail"
        ,"Chain Mail"
        ,"Split"
        ,"Plate"
      )
  )

weapon_type <- 
  list(
    `Simple Melee` = 
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
      )
    ,`Simple Ranged` =
      c(
        "Light Crossbow"
        ,"Dart"
        ,"Shortbow"
        ,"Sling"
      )
    ,`Martial Melee` =
      c(
        "Battleaxe"
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
      )
    ,`Martial Ranged` =
      c(
        "Blowgun"
        ,"Hand Crossbow"
        ,"Heavy Crossbow"
        ,"Longbow"
        ,"Net"
      )
  )

monster_type <- 
  c(
    "Aberration"
    ,"Beast"
    ,"Celestial"
    ,"Construct"
    ,"Dragon"
    ,"Elemental"
    ,"Fey"
    ,"Fiend"
    ,"Giant"
    ,"Humanoid"
    ,"Monstrocity"
    ,"Ooze"
    ,"Plant"
    ,"Undead"
  )

abilities <- 
  c(
    "Str"
    ,"Dex"
    ,"Con"
    ,"Int"
    ,"Wis"
    ,"Cha"
  )

skills <- 
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
  )

damage_types <- 
  c(
    "Acid"
    ,"Bludgeoning"
    ,"Cold"
    ,"Fire"
    ,"Force"
    ,"Lightning"
    ,"Necrotic"
    ,"Piercing"
    ,"Poison"
    ,"Psychic"
    ,"Radiant"
    ,"Slashing"
    ,"Thunder"
  )

condition_types <- 
  c(
    "Blinded"
    ,"Charmed"
    ,"Deafened"
    ,"Exhaustion"
    ,"Fatigued"
    ,"Frightened"
    ,"Grappled"
    ,"Paralyzed"
    ,"Petrified"
    ,"Poisoned"
    ,"Prone"
    ,"Restrained"
    ,"Stunned"
    ,"Unconscious"
  )

languages <- 
  c(
    "Abyssal"
    ,"Aquan"
    ,"Auran"
    ,"Celestial"
    ,"Common"
    ,"Deep Speech"
    ,"Draconic"
    ,"Druidic"
    ,"Dwarvish"
    ,"Elvish"
    ,"Giant"
    ,"Gnomish"
    ,"Goblin"
    ,"Gnoll"
    ,"Halfling"
    ,"Ignan"
    ,"Orc"
    ,"Primordial"
    ,"Sylvan"
    ,"Terran"
    ,"Thieves' Cant"
    ,"Undercommon"
  )

# UI ----------
function(request) {
  fluidPage(
    
    # Application title ----------
    # theme = shinytheme("lumen")
    titlePanel(title = "D&D Monster Builder")
    
    ,fluidRow(
      
      # Inputs ----------
      column(
        width = 4
        ,offset = 0
        ,wellPanel(
          bookmarkButton()
          ,textInput(
            inputId = "name"
            ,label = "Name:"
          )
          ,numericInput(
            inputId = "expected_cr"
            ,label = "Expected CR"
            ,min = 0
            ,max = 30
            ,value = 0
          )
          ,selectizeInput(
            inputId = "size"
            ,label = "Size"
            ,choices = c("Choose One" = "", monster_size)
          )
          ,selectizeInput(
            inputId = "type"
            ,label = "Type"
            ,choices = c("Choose One" = "", monster_type)
          )
          ,selectizeInput(
            inputId = "law_chaos"
            ,label = "Law/Chaos"
            ,choices =
              c(
                "Choose One" = ""
                ,"Lawful"
                ,"Neutral"
                ,"Chaotic"
              )
          )
          ,selectizeInput(
            inputId = "good_evil"
            ,label = "Good/Evil"
            ,choices =
              c(
                "Choose One" = ""
                ,"Good"
                ,"Neutral"
                ,"Evil"
              )
          )
          ,selectizeInput(
            inputId = "armor_type"
            ,label = "Type of Armor"
            ,choices = armor_type
          )
          ,checkboxInput(
            inputId = "shield"
            ,label = "Has Shield?"
            ,value = FALSE
          )
          ,numericInput(
            inputId = "nat_armor"
            ,label = "Natural Armor Bonus"
            ,value = 0
          )
          ,numericInput(
            inputId = "hit_dice"
            ,label = "Hit Dice"
            ,min = 1
            ,value = 1
          )
          ,sliderInput(
            inputId = "speed"
            ,label = "Speed"
            ,min = 0
            ,max = 100
            ,value = 30
            ,step = 5
          )
          ,sliderInput(
            inputId = "burrow"
            ,label = "Burrowing Speed"
            ,min = 0
            ,max = 100
            ,value = 0
            ,step = 5
          )
          ,sliderInput(
            inputId = "climb"
            ,label = "Climbing Speed"
            ,min = 0
            ,max = 100
            ,value = 0
            ,step = 5
          )
          ,sliderInput(
            inputId = "fly"
            ,label = "Flying Speed"
            ,min = 0
            ,max = 100
            ,value = 0
            ,step = 5
          )
          ,sliderInput(
            inputId = "swim"
            ,label = "Swimming Speed"
            ,min = 0
            ,max = 100
            ,value = 0
            ,step = 5
          )
          ,column(
            width = 4
            ,offset = 0
            ,numericInput(
              inputId = "str"
              ,label = "Str"
              ,min = 1
              ,max = 30
              ,value = 10
            )
            ,numericInput(
              inputId = "int"
              ,label = "Int"
              ,min = 1
              ,max = 30
              ,value = 10
            )
          )
          ,column(
            width = 4
            ,offset = 0
            ,numericInput(
              inputId = "dex"
              ,label = "Dex"
              ,min = 1
              ,max = 30
              ,value = 10
            )
            ,numericInput(
              inputId = "wis"
              ,label = "Wis"
              ,min = 1
              ,max = 30
              ,value = 10
            )
          )
          ,column(
            width = 4
            ,offset = 0
            ,numericInput(
              inputId = "con"
              ,label = "Con"
              ,min = 1
              ,max = 30
              ,value = 10
            )
            ,numericInput(
              inputId = "cha"
              ,label = "Cha"
              ,min = 1
              ,max = 30
              ,value = 10
            )
          )
          ,selectizeInput(
            inputId = "saves"
            ,label = "Saving Throw Proficiencies"
            ,choices = abilities
            ,multiple = TRUE
          )
          ,selectizeInput(
            inputId = "skills"
            ,label = "Skill Proficiencies"
            ,choices = skills
            ,multiple = TRUE
          )
          ,selectizeInput(
            inputId = "vuln"
            ,label = "Damage Vulnerabilities"
            ,choices = damage_types
            ,multiple = TRUE
          )
          ,selectizeInput(
            inputId = "resist"
            ,label = "Damage Resistances"
            ,choices = damage_types
            ,multiple = TRUE
          )
          ,selectizeInput(
            inputId = "immune"
            ,label = "Damage Immunities"
            ,choices = damage_types
            ,multiple = TRUE
          )
          ,selectizeInput(
            inputId = "cond_immune"
            ,label = "Condition Immunities"
            ,choices = condition_types
            ,multiple = TRUE
          )
          ,sliderInput(
            inputId = "blindsight"
            ,label = "Blindsight Distance:"
            ,min = 0
            ,max = 120
            ,value = 0
            ,step = 5
          )
          ,sliderInput(
            inputId = "darkvision"
            ,label = "Darkvision Distance:"
            ,min = 0
            ,max = 120
            ,value = 0
            ,step = 5
          )
          ,sliderInput(
            inputId = "tremorsense"
            ,label = "Tremorsense Distance:"
            ,min = 0
            ,max = 120
            ,value = 0
            ,step = 5
          )
          ,sliderInput(
            inputId = "truesight"
            ,label = "Truesight Distance:"
            ,min = 0
            ,max = 120
            ,value = 0
            ,step = 5
          )
          ,selectizeInput(
            inputId = "lang"
            ,label = "Languages"
            ,choices = languages
            ,multiple = TRUE
          )
          
          
          ,sliderInput(
            inputId = "multiattack"
            ,label = "Number of Attacks"
            ,min = 1
            ,max = 5
            ,value = 1
            ,step = 1
          )
          ,selectizeInput(
            inputId = "weapon_type"
            ,label = "Type of Weapon"
            ,choices = weapon_type
            ,multiple = TRUE
          )
        )
      )
      
      # Main ----------
      # Displays the generated insult
      ,column(
        width = 8
        ,offset = 0
        ,h3(textOutput("name"))
        ,em(textOutput("sub_name"))
        ,strong(textOutput("ac"))
        ,strong(textOutput("hp"))
        ,strong(textOutput("speed"))
        ,column(
          width = 2
          ,offset = 0
          ,strong(textOutput("str"))
          ,textOutput("str_num")
        )
        ,column(
          width = 2
          ,offset = 0
          ,strong(textOutput("dex"))
          ,textOutput("dex_num")
        )
        ,column(
          width = 2
          ,offset = 0
          ,strong(textOutput("con"))
          ,textOutput("con_num")
        )
        ,column(
          width = 2
          ,offset = 0
          ,strong(textOutput("int"))
          ,textOutput("int_num")
        )
        ,column(
          width = 2
          ,offset = 0
          ,strong(textOutput("wis"))
          ,textOutput("wis_num")
        )
        ,column(
          width = 2
          ,offset = 0
          ,strong(textOutput("cha"))
          ,textOutput("cha_num")
        )
        ,textOutput("save_throw")
        ,textOutput("skill_prof")
        ,textOutput("d_vuln")
        ,textOutput("d_resist")
        ,textOutput("d_immune")
        ,textOutput("c_immune")
        ,textOutput("senses")
        ,textOutput("languages")
        ,textOutput("cr")
        ,h4("Actions")
        ,textOutput("multiattack")
        ,tableOutput("weapons")
        ,textOutput("e_hp")
        ,textOutput("e_ac")
        ,textOutput("e_damage")
        ,textOutput("e_ab")
        ,textOutput("defensive_cr")
        ,textOutput("offensive_cr")
        ,textOutput("final_cr")
        )
      
    )
    
  )
}