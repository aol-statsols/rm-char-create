#' rolemasterCharacterCreator.
#'
#' @name rolemasterCharacterCreator
#' @docType package
#' @import png gridExtra ggplot2 jsonlite DBI RSQLite grDevices graphics stats
NULL

#' Background Options
#'
#' A dataset containing all background options
#'
#' \itemize{
#'   item Number. Placeholder row
#'   item Option. Name of each option
#' }
#'
#' @docType data
#' @keywords datasets
#' @name background_Options
#' @usage background_Options
#' @format A data frame with 16 rows and 2 variables
NULL

#' Special Abilities Table
#'
#' A dataset containing the chances of getting a certain ability from the special abilities table during background creation
#'
#' \itemize{
#'   item Roll. Upper bound of roll to be in this row
#'   item Ability. What is their special ability
#'   item Description. Description of what this special ability means
#' }
#'
#' @docType data
#' @keywords datasets
#' @name background_Special_Abilities
#' @usage background_Special_Abilities
#' @format A data frame with 23 rows and 3 variables
NULL

#' Special Curse Table
#'
#' A dataset containing the chances of getting a certain curse from the special curses table during background creation
#'
#' \itemize{
#'   item Roll. Upper bound of roll to be in this row
#'   item Family.Tie. Resulting curse
#' }
#'
#' @docType data
#' @keywords datasets
#' @name background_Special_Curses
#' @usage background_Special_Curses
#' @format A data frame with 15 rows and 2 variables
NULL

#' Special Family Tie Table
#'
#' A dataset containing the chances of getting a certain family tie from the special family ties table during background creation
#'
#' \itemize{
#'   item Roll. Upper bound of roll to be in this row
#'   item Family.Tie. Resulting family tie
#' }
#'
#' @docType data
#' @keywords datasets
#' @name background_Special_Family_Ties
#' @usage background_Special_Family_Ties
#' @format A data frame with 11 rows and 2 variables
NULL

#' Special Item Table
#'
#' A dataset containing the chances of getting a certain item from the special items table during background creation
#'
#' \itemize{
#'   item Roll. Upper bound of roll to be in this row
#'   item Item. Resulting item
#' }
#'
#' @docType data
#' @keywords datasets
#' @name background_Special_Items
#' @usage background_Special_Items
#' @format A data frame with 15 rows and 2 variables
NULL

#' Special Status Table
#'
#' A dataset containing the chances of getting a certain status from the special status table during background creation
#'
#' \itemize{
#'   item Roll. Upper bound of roll to be in this row
#'   item Wealth. Resulting status
#' }
#'
#' @docType data
#' @keywords datasets
#' @name background_Special_Status
#' @usage background_Special_Status
#' @format A data frame with 17 rows and 2 variables
NULL

#' Special Wealth Table
#'
#' A dataset containing the chances of getting certain wealth from the special wealth table during background creation
#'
#' \itemize{
#'   item Roll. Upper bound of roll to be in this row
#'   item Wealth. Resulting wealth
#' }
#'
#' @docType data
#' @keywords datasets
#' @name background_Special_Wealth
#' @usage background_Special_Wealth
#' @format A data frame with 17 rows and 2 variables
NULL

#' Character and Clan Relationship
#'
#' A dataset containing the chances of the clanhead being viewed in various ways by the clan
#'
#' \itemize{
#'   item Roll. Upper bound of roll to be in this row
#'   item Status. What is their status
#'   item Description. Description of what this status means
#' }
#'
#' @docType data
#' @keywords datasets
#' @name char_Clan_Status
#' @usage char_Clan_Status
#' @format A data frame with 6 rows and 3 variables
NULL

#' Character and Clanhead Relationship
#'
#' A dataset containing the chances of the clanhead being related in various ways or unrelated to the character
#'
#' \itemize{
#'   item Roll. Upper bound of roll to be in this row
#'   item Status. What is their status
#' }
#'
#' @docType data
#' @keywords datasets
#' @name char_Clanhead_Relationship
#' @usage char_Clanhead_Relationship
#' @format A data frame with 6 rows and 2 variables
NULL

#' Contact Identity
#'
#' A dataset containing the chances of each contact being from a certain social area
#'
#' \itemize{
#'   item Roll. Upper bound of roll to be in this row
#'   item Status. What is their status
#' }
#'
#' @docType data
#' @keywords datasets
#' @name char_Contacts_Identity
#' @usage char_Contacts_Identity
#' @format A data frame with 6 rows and 2 variables
NULL

#' Contact Relationship Status
#'
#' A dataset containing the chances of character being in different states with their contactss
#'
#' \itemize{
#'   item Race. Name of each race
#'   item Divisor. Divide character's starting age by this to work out their number of starting contacts
#' }
#'
#' @docType data
#' @keywords datasets
#' @name char_Contacts_Number
#' @usage char_Contacts_Number
#' @format A data frame with 12 rows and 2 variables
NULL

#' Contact Relationship Status
#'
#' A dataset containing the chances of character being in different states with their contactss
#'
#' \itemize{
#'   item Roll. Upper bound of roll to be in this row
#'   item Status. What is their status
#'   item Loyalty. Base loyalty plus a roll of an n sided die
#' }
#'
#' @docType data
#' @keywords datasets
#' @name char_Contacts_Relationship
#' @usage char_Contacts_Relationship
#' @format A data frame with 5 rows and 3 variables
NULL

#' Family Status
#'
#' A dataset containing the chances of character being in different family states
#'
#' \itemize{
#'   item Roll. Upper bound of roll to be in this row
#'   item Status. What is their status
#' }
#'
#' @docType data
#' @keywords datasets
#' @name char_Family_Status
#' @usage char_Family_Status
#' @format A data frame with 6 rows and 2 variables
NULL

#' Height and Weight Calcs
#'
#' A dataset containing the numbers needed to caculate height and weight
#'
#' \itemize{
#'   item Race. Name of each race
#'   item Gender. Split into sexes
#'   item Base.Height. Base height for each race
#'   item Height.Mod. Height mod for each race
#'   item Base.Weight. Base weight for each race
#'   item Weight.Mod. Weight mod for each race
#' }
#'
#' @docType data
#' @keywords datasets
#' @name char_Height_And_Weight
#' @usage char_Height_And_Weight
#' @format A data frame with 16 rows and 6 variables
NULL

#' Movement Mods from Height
#'
#' A dataset containing the mods to base speed for different heights
#'
#' \itemize{
#'   item Height. How tall is the character
#'   item Modification. Mod to base speed
#' }
#'
#' @docType data
#' @keywords datasets
#' @name char_Movement_Mods
#' @usage char_Movement_Mods
#' @format A data frame with 10 rows and 2 variables
NULL

#' Number of Siblings
#'
#' A dataset containing the chances having different numbers of siblings depending on race
#'
#' \itemize{
#'   item Race. Name of each race
#'   item Roll_Mod. Number to subtract (meaning you are more likely to have a lower number of siblings)
#'   item Sibling_Dice. Number of sides on the die which determines how many siblings you have
#'   item Chance_Alive. Target to roll under on a d100 for them to be alive
#' }
#'
#' @docType data
#' @keywords datasets
#' @name char_No_of_Siblings
#' @usage char_No_of_Siblings
#' @format A data frame with 12 rows and 4 variables
NULL

#' Orphan Status
#'
#' A dataset containing the chances of being fostered, adopted or living alone if you're an orphan
#'
#' \itemize{
#'   item Roll. Upper bound of roll to be in this row
#'   item Status. What is their status
#' }
#'
#' @docType data
#' @keywords datasets
#' @name char_Orphan_Status
#' @usage char_Orphan_Status
#' @format A data frame with 3 rows and 2 variables
NULL

#' Parent Status
#'
#' A dataset containing the chances of parents being in different states
#'
#' \itemize{
#'   item Roll. Upper bound of roll to be in this row
#'   item Status. What is their status
#' }
#'
#' @docType data
#' @keywords datasets
#' @name char_Parents_Status
#' @usage char_Parents_Status
#' @format A data frame with 6 rows and 2 variables
NULL

#' Profession Bonuses
#'
#' A dataset containing the bonuses to skill totals for each profession
#'
#' \itemize{
#'   item Profession. Name for each profession 
#'   item Profession.Bonuses. Skills which have bonuses for each profession
#'   item Bonuses. Bonuses for each profession
#' }
#'
#' @docType data
#' @keywords datasets
#' @name char_Profession_Bonus
#' @usage char_Profession_Bonus
#' @format A data frame with 19 rows and 3 variables
NULL 

#' Social class
#'
#' A dataset containing the chances of being in each social class
#'
#' \itemize{
#'   item Roll. Upper bound of roll to be in this row
#'   item Rank. Which child were you
#' }
#'
#' @docType data
#' @keywords datasets
#' @name char_Sibling_Rank
#' @usage char_Sibling_Rank
#' @format A data frame with 6 rows and 2 variables
NULL

#' Background Skills
#'
#' A dataset containing the skills from being raised in a certain background
#'
#' \itemize{
#'   item Skill. Name of each skills
#'   item Urban.Man. Skills for being raised by Tribal people
#'   item Rural.Man. Skills for being raised by Tribal people
#'   item Noble.Man. Skills for being raised by Tribal people
#'   item Tribal.Man. Skills for being raised by Tribal people
#'   item Wood.Elf. Skills for being raised by Wood.Elf people
#'   item High.Elf. Skills for being raised by High.Elf people
#'   item Dwarf. Skills for being raised by Dwarf people
#'   item Hobbit. Skills for being raised by Hobbit people
#'   item Gnome. Skills for being raised by Gnome people
#' }
#'
#' @docType data
#' @keywords datasets
#' @name char_Skills_Background
#' @usage char_Skills_Background
#' @format A data frame with 19 rows and 11 variables
NULL

#' Social class
#'
#' A dataset containing the chances of being in each social class
#'
#' \itemize{
#'   item Class. Name of each class
#'   item Tribal. Upper bound on roll to be in each class for Tribal people
#'   item Viking. Upper bound on roll to be in each class for Viking people
#'   item Feudal. Upper bound on roll to be in each class for Feudal people
#'   item Imperial. Upper bound on roll to be in each class for Imperial people
#'   item Elf. Upper bound on roll to be in each class for Elf people
#'   item Dwarf. Upper bound on roll to be in each class for Dwarf people
#'   item Hobbit. Upper bound on roll to be in each class for Hobbit people
#'   item Gnome. Upper bound on roll to be in each class for Gnome people
#' }
#'
#' @docType data
#' @keywords datasets
#' @name char_Social_Class
#' @usage char_Social_Class
#' @format A data frame with 5 rows and 9 variables
NULL

#' Skill Costs
#'
#' A dataset containing the cost for each skill, the stats associated with them and the number of age die associated with each profession because I'm bad at putting things where they should be.
#'
#' \itemize{
#'   item Skill. Name for each skill 
#'   item Stat. Stats associated with each skill
#'   item Adventurer. Skill cost for profession Adventurer
#'   item Commoner. Skill cost for profession Commoner
#'   item Fighter. Skill cost for profession Fighter
#'   item Thief. Skill cost for profession Thief
#'   item Warrior_Monk. Skill cost for profession Warrior_Monk
#'   item Bard. Skill cost for profession Bard
#'   item Monk. Skill cost for profession Monk
#'   item Ranger. Skill cost for profession Ranger
#'   item Illusionist. Skill cost for profession Illusionist
#'   item Magician. Skill cost for profession Magician
#'   item Alchemist. Skill cost for profession Alchemist
#'   item Cleric. Skill cost for profession Cleric
#'   item Paladin. Skill cost for profession Paladin
#'   item Animist. Skill cost for profession Animist
#'   item Healer. Skill cost for profession Healer
#'   item Mentalist. Skill cost for profession Mentalist
#'   item Seer. Skill cost for profession Seer
#'   item Lay_Healer. Skill cost for profession Lay_Healer
#'   item Astrologer. Skill cost for profession Astrologer
#'   item Mystic. Skill cost for profession Mystic
#'   item Sorcerer. Skill cost for profession Sorcerer
#' }
#'
#' @docType data
#' @keywords datasets
#' @name class_Costs_And_Info
#' @usage class_Costs_And_Info
#' @format A data frame with 139 rows and 23 variables
NULL

#' Professions
#'
#' A dataset containing each profession
#'
#' \itemize{
#'   item Number. Placeholder row
#'   item Profession. Name of each profession
#' }
#'
#' @docType data
#' @keywords datasets
#' @name classesDF
#' @usage classesDF
#' @format A data frame with 23 rows and 2 variables
NULL

#' Flaws
#'
#' A dataset describing the flaws a person can take to change their background options
#'
#' \itemize{
#'   item Roll. Upper bound on rolls to be assigned each flaw
#'   item Flaw. Name of the flaw
#'   item Description. Description of the flaw
#' }
#'
#' @docType data
#' @keywords datasets
#' @name flaws_Table
#' @usage flaws_Table
#' @format A data frame with 22 rows and 3 variables
NULL

#' Guilded Occupations
#'
#' A dataset describing the guilded occupation for each people and bonuses from them.
#'
#' \itemize{
#'   item Tribal. Upper bound on rolls to be assigned a job for Tribal people 
#'   item Viking. Upper bound on rolls to be assigned a job for Viking people 
#'   item Feudal. Upper bound on rolls to be assigned a job for Feudal people 
#'   item Imperial. Upper bound on rolls to be assigned a job for Imperial people 
#'   item Elf. Upper bound on rolls to be assigned a job for Elf people 
#'   item Dwarf. Upper bound on rolls to be assigned a job for Dwarf people 
#'   item Hobbit. Upper bound on rolls to be assigned a job for Hobbit people 
#'   item Gnome. Upper bound on rolls to be assigned a job for Gnome people 
#'   item Occupation. Name of an occupation
#'   item Ur.. Percentage chance of the profession being urban
#'   item Skills. Names of skills you gain ranks in
#'   item Ranks. Number of skill ranks you gain for each skill
#' }
#'
#' @docType data
#' @keywords datasets
#' @name occupation_Guilded
#' @usage occupation_Guilded
#' @format A data frame with 22 rows and 12 variables
NULL

#' Noble Occupations
#'
#' A dataset describing the noble occupation for each people and bonuses from them.
#'
#' \itemize{
#'   item Tribal. Upper bound on rolls to be assigned a job for Tribal people 
#'   item Viking. Upper bound on rolls to be assigned a job for Viking people 
#'   item Feudal. Upper bound on rolls to be assigned a job for Feudal people 
#'   item Imperial. Upper bound on rolls to be assigned a job for Imperial people 
#'   item Elf. Upper bound on rolls to be assigned a job for Elf people 
#'   item Dwarf. Upper bound on rolls to be assigned a job for Dwarf people 
#'   item Hobbit. Upper bound on rolls to be assigned a job for Hobbit people 
#'   item Gnome. Upper bound on rolls to be assigned a job for Gnome people 
#'   item Occupation. Name of an occupation
#'   item Ur.. Percentage chance of the profession being urban
#'   item Skills. Names of skills you gain ranks in
#'   item Ranks. Number of skill ranks you gain for each skill
#' }
#'
#' @docType data
#' @keywords datasets
#' @name occupation_Noble
#' @usage occupation_Noble
#' @format A data frame with 22 rows and 12 variables
NULL

#' Serf Occupations
#'
#' A dataset describing the serf occupation for each people and bonuses from them.
#'
#' \itemize{
#'   item Tribal. Upper bound on rolls to be assigned a job for Tribal people 
#'   item Viking. Upper bound on rolls to be assigned a job for Viking people 
#'   item Feudal. Upper bound on rolls to be assigned a job for Feudal people 
#'   item Imperial. Upper bound on rolls to be assigned a job for Imperial people 
#'   item Elf. Upper bound on rolls to be assigned a job for Elf people 
#'   item Dwarf. Upper bound on rolls to be assigned a job for Dwarf people 
#'   item Hobbit. Upper bound on rolls to be assigned a job for Hobbit people 
#'   item Gnome. Upper bound on rolls to be assigned a job for Gnome people 
#'   item Occupation. Name of an occupation
#'   item Ur.. Percentage chance of the profession being urban
#'   item Skills. Names of skills you gain ranks in
#'   item Ranks. Number of skill ranks you gain for each skill
#' }
#'
#' @docType data
#' @keywords datasets
#' @name occupation_Serf
#' @usage occupation_Serf
#' @format A data frame with 22 rows and 12 variables
NULL

#' Slave Occupations
#'
#' A dataset describing the slave occupation for each people and bonuses from them.
#'
#' \itemize{
#'   item Tribal. Upper bound on rolls to be assigned a job for Tribal people 
#'   item Viking. Upper bound on rolls to be assigned a job for Viking people 
#'   item Feudal. Upper bound on rolls to be assigned a job for Feudal people 
#'   item Imperial. Upper bound on rolls to be assigned a job for Imperial people 
#'   item Elf. Upper bound on rolls to be assigned a job for Elf people 
#'   item Dwarf. Upper bound on rolls to be assigned a job for Dwarf people 
#'   item Hobbit. Upper bound on rolls to be assigned a job for Hobbit people 
#'   item Gnome. Upper bound on rolls to be assigned a job for Gnome people 
#'   item Occupation. Name of an occupation
#'   item Ur.. Percentage chance of the profession being urban
#'   item Skills. Names of skills you gain ranks in
#'   item Ranks. Number of skill ranks you gain for each skill
#' }
#'
#' @docType data
#' @keywords datasets
#' @name occupation_Slave
#' @usage occupation_Slave
#' @format A data frame with 22 rows and 12 variables
NULL

#' Unguilded Occupations
#'
#' A dataset describing the unguilded occupation for each people and bonuses from them.
#'
#' \itemize{
#'   item Tribal. Upper bound on rolls to be assigned a job for Tribal people 
#'   item Viking. Upper bound on rolls to be assigned a job for Viking people 
#'   item Feudal. Upper bound on rolls to be assigned a job for Feudal people 
#'   item Imperial. Upper bound on rolls to be assigned a job for Imperial people 
#'   item Elf. Upper bound on rolls to be assigned a job for Elf people 
#'   item Dwarf. Upper bound on rolls to be assigned a job for Dwarf people 
#'   item Hobbit. Upper bound on rolls to be assigned a job for Hobbit people 
#'   item Gnome. Upper bound on rolls to be assigned a job for Gnome people 
#'   item Occupation. Name of an occupation
#'   item Ur.. Percentage chance of the profession being urban
#'   item Skills. Names of skills you gain ranks in
#'   item Ranks. Number of skill ranks you gain for each skill
#' }
#'
#' @docType data
#' @keywords datasets
#' @name occupation_Unguilded
#' @usage occupation_Unguilded
#' @format A data frame with 22 rows and 12 variables
NULL

#' Pace Multiplier
#'
#' A dataset describing what rates of movement are allowed or disallowed
#'
#' \itemize{
#'   item Pace. Title of each rate of movement
#'   item Multiplier. Multiplier of each rate of movement
#'   item Difficulty. Difficulty of each rate of movement
#'   item Armor.Qu.Penalty.Prohibited.Paces. Prohibited rates of movement for levels of quickness penalty
#'   item Encumbrance.Penalty.Prohibited.Paces. Prohibited rates of movement for levels of encumbrance
#' }
#'
#' @docType data
#' @keywords datasets
#' @name pace_Multiplier
#' @usage pace_Multiplier
#' @format A data frame with 6 rows and 5 variables
NULL

#' Potential Stats Generation
#'
#' A dataset describing how to generate potential stats for each temporary stat range
#'
#' \itemize{
#'   item Stat. Temporary stat value
#'   item Base_Pot. Base for the potential stat associated with each temporary stat
#'   item Number.of.Dice. Number of dice to roll associated with each temporary stat
#'   item Number.of.Sides. Number of sides for each die rolled associated with each temporary stat
#' }
#'
#' @docType data
#' @keywords datasets
#' @name potential_Stats_Generator_DF
#' @usage potential_Stats_Generator_DF
#' @format A data frame with 17 rows and 4 variables
NULL

#' Levels
#'
#' A dataset of experience needed for each level up
#'
#' \itemize{
#'   item Level. Placeholder row
#'   item rm_levels. Experience needed to reach each level 
#' }
#'
#' @docType data
#' @keywords datasets
#' @name rm_Levels_Table
#' @usage rm_Levels_Table
#' @format A data frame with 21 rows and 2 variables
NULL

#' Skill groups 
#'
#' A dataset of skill groups
#'
#' \itemize{
#'   item Number. Placeholder row
#'   item Groups. Skills groups which share half ranks
#' }
#'
#' @docType data
#' @keywords datasets
#' @name skills_GroupsDF
#' @usage skills_GroupsDF
#' @format A data frame with 4 rows and 2 variables
NULL

#' Species Description
#'
#' A dataset for bonuses and penalties and age relations for each species
#'
#' \itemize{
#'   item Race. The name of each race
#'   item Constitution. Co bonus for each race
#'   item Agility. Ag bonus for each race
#'   item Self_Discipline. SD bonus for each race
#'   item Reasoning. Re bonus for each race
#'   item Memory. Me bonus for each race
#'   item Strength. St bonus for each race
#'   item Quickness. Qu bonus for each race
#'   item Empathy. Em bonus for each race
#'   item Intuition. In bonus for each race
#'   item Presence. Pr bonus for each race
#'   item Essense. Realm bonus for each race
#'   item Channelling. Realm bonus for each race
#'   item Mentalism. Realm bonus for each race
#'   item Poison. Resistance bonus for each race
#'   item Disease. Resistance bonus for each race
#'   item Background. Background Options Points for each race
#'   item Body_Dev. Body development bonus for each race
#'   item Adolescent. Age when they reach adolescence
#'   item Middle_Aged. Age when they reach middle age
#'   item Old. Age when they reach old age
#'   item Venerable  Age when they reach adolescence
#'   item Maximum_Age. 
#' }
#'
#' @docType data
#' @keywords datasets
#' @name species_Description_DF
#' @usage species_Description_DF
#' @format A data frame with 12 rows and 25 variables
NULL

#' Base Spell Lists
#'
#' A dataset for the base spell lists for each profession
#'
#' \itemize{
#'   item Adventurer. Base spell lists for an Adventurer
#'   item Commoner. Base spell lists for a Commoner
#'   item Fighter. Base spell lists for a Fighter
#'   item Thief. Base spell lists for a Thief
#'   item Warrior_Monk. Base spell lists for a Warrior_Monk
#'   item Bard. Base spell lists for a Bard
#'   item Monk. Base spell lists for a Monk
#'   item Ranger. Base spell lists for a Ranger
#'   item Illusionist. Base spell lists for a Illusionist
#'   item Magician. Base spell lists for a Magician
#'   item Elementalist. Base spell lists for an Elementalist
#'   item Alchemist_Channeling. Base spell lists for an Alchemist
#'   item Alchemist. Base spell lists for an Alchemist
#'   item Cleric. Base spell lists for a Cleric
#'   item Paladin. Base spell lists for a Paladin
#'   item Animist. Base spell lists for an Animist
#'   item Healer. Base spell lists for a Healer
#'   item Mentalist. Base spell lists for a Mentalist
#'   item Seer. Base spell lists for a Seer
#'   item Lay_Healer. Base spell lists for a Lay_Healer
#'   item Astrologer. Base spell lists for an Astrologer
#'   item Mystic. Base spell lists for a Mystic
#'   item Sorcerer. Base spell lists for a Sorcerer 
#' }
#'
#' @docType data
#' @keywords datasets
#' @name spell_Lists_Base
#' @usage spell_Lists_Base
#' @format A data frame with 9 rows and 21 variables
NULL  

#' Closed Spell Lists
#'
#' A dataset for the closed spell lists in each realm
#'
#' \itemize{
#'   item Channeling. Name of each closed Channeling Spell list
#'   item Essence. Name of each closed Essence Spell list
#'   item Mentalism. Name of each closed Mentalism Spell list
#' }
#'
#' @docType data
#' @keywords datasets
#' @name spell_Lists_Closed
#' @usage spell_Lists_Closed
#' @format A data frame with 10 rows and 3 variables
NULL  

#' Evil Spell Lists
#'
#' A dataset for the evil spell lists in each realm
#'
#' \itemize{
#'   item Channeling. Name of each evil Channeling Spell list
#'   item Essence. Name of each evil Essence Spell list
#'   item Mentalism. Name of each evil Mentalism Spell list
#' }
#'
#' @docType data
#' @keywords datasets
#' @name spell_Lists_Evil
#' @usage spell_Lists_Evil
#' @format A data frame with 6 rows and 3 variables
NULL  

#' Spell lists for Fire Bolt
#'
#' A dataset for the spell lists you must know up to level n to learn directed fire spells 
#'
#' \itemize{
#'   item Spell.List. Name of each spell list
#'   item Level. Level you must have in it to learn directed spells
#' }
#'
#' @docType data
#' @keywords datasets
#' @name spell_Lists_Fire_Bolt
#' @usage spell_Lists_Fire_Bolt
#' @format A data frame with 5 rows and 2 variables
NULL  

#' Spell lists for Ice Bolt
#'
#' A dataset for the spell lists you must know up to level n to learn directed ice spells 
#'
#' \itemize{
#'   item Spell.List. Name of each spell list
#'   item Level. Level you must have in it to learn directed spells
#' }
#'
#' @docType data
#' @keywords datasets
#' @name spell_Lists_Ice_Bolt
#' @usage spell_Lists_Ice_Bolt
#' @format A data frame with 5 rows and 2 variables
NULL  

#' Spell lists for Lightning Bolt
#'
#' A dataset for the spell lists you must know up to level n to learn directed lightning spells 
#'
#' \itemize{
#'   item Spell.List. Name of each spell list
#'   item Level. Level you must have in it to learn directed spells
#' }
#'
#' @docType data
#' @keywords datasets
#' @name spell_Lists_Lightning_Bolt
#' @usage spell_Lists_Lightning_Bolt
#' @format A data frame with 5 rows and 2 variables
NULL  

#' Open Spell Lists
#'
#' A dataset for the open spell lists in each realm
#'
#' \itemize{
#'   item Channeling. Name of each open Channeling Spell list
#'   item Essence. Name of each open Essence Spell list
#'   item Mentalism. Name of each open Mentalism Spell list
#' }
#'
#' @docType data
#' @keywords datasets
#' @name spell_Lists_Open
#' @usage spell_Lists_Open
#' @format A data frame with 10 rows and 3 variables
NULL  

#' Spell lists for Shock Bolt
#'
#' A dataset for the spell lists you must know up to level n to learn directed shock spells 
#'
#' \itemize{
#'   item Spell.List. Name of each spell list
#'   item Level. Level you must have in it to learn directed spells
#' }
#'
#' @docType data
#' @keywords datasets
#' @name spell_Lists_Shock_Bolt
#' @usage spell_Lists_Shock_Bolt
#' @format A data frame with 5 rows and 2 variables
NULL  

#' Spell lists for Water Bolt
#'
#' A dataset for the spell lists you must know up to level n to learn directed water spells 
#'
#' \itemize{
#'   item Spell.List. Name of each spell list
#'   item Level. Level you must have in it to learn directed spells
#' }
#'
#' @docType data
#' @keywords datasets
#' @name spell_Lists_Water_Bolt
#' @usage spell_Lists_Water_Bolt
#' @format A data frame with 4 rows and 2 variables
NULL  
  
#' Stats Name Data
#'
#' A dataset containing realm stats for each profession
#'
#' \itemize{
#'   item Stats. Short name of each stat
#'   item FullStats. Full name of each stat
#' }
#'
#' @docType data
#' @keywords datasets
#' @name stat_Name_DF
#' @usage stat_Name_DF
#' @format A data frame with 10 rows and 2 variables
NULL  
  
#' Realm Stats Data
#'
#' A dataset containing realm stats for each profession
#'
#' \itemize{
#'   item c..Essence...Channeling...Mentalism... Realm 
#'   item stats_Realm Realm stats 
#' }
#'
#' @docType data
#' @keywords datasets
#' @name stat_Realm
#' @usage stat_Realm
#' @format A data frame with 3 rows and 2 variables
NULL
  
#' Primary Stats Data
#'
#' A dataset containing primary stats for each profession
#'
#' \itemize{
#'   item Adventurer. Primary stats for an Adventurer
#'   item Commoner. Primary stats for a Commoner
#'   item Fighter. Primary stats for a Fighter
#'   item Thief. Primary stats for a Thief
#'   item Warrior_Monk. Primary stats for a Warrior_Monk
#'   item Bard. Primary stats for a Bard
#'   item Monk. Primary stats for a Monk
#'   item Ranger. Primary stats for a Ranger
#'   item Illusionist. Primary stats for a Illusionist
#'   item Magician. Primary stats for a Magician
#'   item Alchemist. Primary stats for an Alchemist
#'   item Cleric. Primary stats for a Cleric
#'   item Paladin. Primary stats for a Paladin
#'   item Animist. Primary stats for an Animist
#'   item Healer. Primary stats for a Healer
#'   item Mentalist. Primary stats for a Mentalist
#'   item Seer. Primary stats for a Seer
#'   item Lay_Healer. Primary stats for a Lay_Healer
#'   item Astrologer. Primary stats for an Astrologer
#'   item Mystic. Primary stats for a Mystic
#'   item Sorcerer. Primary stats for a Sorcerer 
#' }
#'
#' @docType data
#' @keywords datasets
#' @name stats_Primary
#' @usage stats_Primary
#' @format A data frame with 2 rows and 22 variables
NULL

#' Stat Ranges Data
#'
#' A dataset ranges of stat values and the bonuses and DP associated with them.
#'
#' \itemize{
#'   item Stat_Values. Ranges of stats associated with different bonuses and development points
#'   item Bonus. Bonuses associated with different ranges of stats 
#'   item DP. Development points associated with different ranges of stats 
#' }
#'
#' @docType data
#' @keywords datasets
#' @name statVal_And_Bonus_And_DP
#' @usage statVal_And_Bonus_And_DP
#' @format A data frame with 32 rows and 3 variables
NULL

#' Training Packages Costs Data
#'
#' A dataset containing the gains associated with different Training Packages
#'
#' \itemize{
#'   item Training_Package. Name of the training package
#'   item Adventurer. Cost of each training package for an Adventurer
#'   item Commoner. Cost of each training package for a Commoner
#'   item Fighter. Cost of each training package for a Fighter
#'   item Thief. Cost of each training package for a Thief
#'   item Warrior_Monk. Cost of each training package for a Warrior_Monk
#'   item Bard. Cost of each training package for a Bard
#'   item Monk. Cost of each training package for a Monk
#'   item Ranger. Cost of each training package for a Ranger
#'   item Illusionist. Cost of each training package for a Illusionist
#'   item Magician. Cost of each training package for a Magician
#'   item Alchemist. Cost of each training package for an Alchemist
#'   item Cleric. Cost of each training package for a Cleric
#'   item Paladin. Cost of each training package for a Paladin
#'   item Animist. Cost of each training package for an Animist
#'   item Healer. Cost of each training package for a Healer
#'   item Mentalist. Cost of each training package for a Mentalist
#'   item Seer. Cost of each training package for a Seer
#'   item Lay_Healer. Cost of each training package for a Lay_Healer
#'   item Astrologer. Cost of each training package for an Astrologer
#'   item Mystic. Cost of each training package for a Mystic
#'   item Sorcerer. Cost of each training package for a Sorcerer 
#' }
#'
#' @docType data
#' @keywords datasets
#' @name training_Packages_Cost_DF
#' @usage training_Packages_Cost_DF
#' @format A data frame with 35 rows and 22 variables
NULL

#' Training Packages Gains Data
#'
#' A dataset containing the gains associated with different Training Packages
#'
#' \itemize{
#'   \item Name. Name of the training package
#'   \item Targets. Targets to roll under for rewards
#'   \item Rewards. Rewards
#'   \item Skills. Skills increased by the training package
#'   \item Ranks. Skill ranks from the training package
#'   \item Stats. Stat change rolls from the training package
#' }
#'
#' @docType data
#' @keywords datasets
#' @name training_Packages_Gains
#' @usage training_Packages_Gains
#' @format A data frame with 35 rows and 5 variables
NULL

#' Character Sheet Page 1
#'
#' RDA of a blank version of p1 of the character sheet.
#'
#' @docType Graphics
#' @keywords aplot
#' @name cs.p1
#' @usage cs.p1
#' @format An image, sshhhhh
NULL

#' Character Sheet Page 2
#'
#' RDA of a blank version of p2 of the character sheet.
#'
#' @docType Graphics
#' @keywords aplot
#' @name cs.p2
#' @usage cs.p2
#' @format An image, sshhhhh
NULL

#' Character Sheet Page 3
#'
#' RDA of a blank version of p3 of the character sheet.
#'
#' @docType Graphics
#' @keywords aplot
#' @name cs.p3
#' @usage cs.p3
#' @format An image, sshhhhh
NULL