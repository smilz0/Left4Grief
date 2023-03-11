//------------------------------------------------------
//     Author : smilzo
//     https://steamcommunity.com/id/smilz0
//------------------------------------------------------

Msg("Including left4grief...\n");

if (!IncludeScript("left4lib_utils"))
	error("[L4G][ERROR] Failed to include 'left4lib_utils', please make sure the 'Left 4 Lib' addon is installed and enabled!\n");
if (!IncludeScript("left4lib_timers"))
	error("[L4G][ERROR] Failed to include 'left4lib_timers', please make sure the 'Left 4 Lib' addon is installed and enabled!\n");

IncludeScript("left4grief_requirements");

/*
--- [m_activeIssueIndex] Vote commands ---
[0] callvote ChangeDifficulty Impossible
[0] callvote ChangeDifficulty Expert
[0] callvote ChangeDifficulty Hard
[0] callvote ChangeDifficulty Normal
[1] callvote RestartGame
[2] callvote Kick <userID>
[3] callvote ChangeMission
[4] callvote ReturnToLobby
[5] callvote ChangeChapter
[6] callvote ChangeAllTalk
--- End Vote commands ---
*/
enum VOTE_TYPE {
  ChangeDifficulty,
  RestartGame,
  Kick,
  ChangeMission,
  ReturnToLobby,
  ChangeChapter,
  ChangeAllTalk
}

const VOTE_IGNORETIME_ONABORTED = 3; // Number of seconds during which received yes/no votes are ignored after a vote cast has been aborted

const ANTIRUSH_CHECK_DELAY = 3;

const DMG_BURN = 8;
const DMG_BLAST = 64;

const MOVETYPE_LADDER = 9;

// Log levels
const LOG_LEVEL_NONE = 0; // Log always
const LOG_LEVEL_ERROR = 1;
const LOG_LEVEL_WARN = 2;
const LOG_LEVEL_INFO = 3;
const LOG_LEVEL_DEBUG = 4;


//if (!("Left4Grief" in getroottable()))
//{
	::Left4Grief <-
	{
		Initialized = false
		ModeName = ""
		MapName = ""
		Settings =
		{
			antirush_admins = 0
			antirush_users = 1
			antirush_distance = 2000
			antirush_punish_goback = 1
			antirush_punish_slowdown = 0
			antirush_punish_vomit = 0
			//antibhop // TODO?
			ricochet_admins = 0
			ricochet_users = 1
			ricochet_bots = 0
			ricochet_damagefactor = 1
			ricochet_burn_damagefactor = 0.1
			ricochet_blast_damagefactor = 0.1
			fault_admin_alert = 1
			fault_public_alert = 0
			friendly_fire = 1
			friendly_fire_admins = 1
			user_can_cast_vote = 1
			admin_can_abort_vote = 1
			vote_change_difficulty = 1
			vote_restart_game = 1
			vote_kick = 1
			vote_change_mission = 1
			vote_return_to_lobby = 1
			vote_change_chapter = 1
			vote_change_all_talk = 1
			revenge_kick = 0
			punish_heal_onladder = 0
			anti_disconnect_grief = 1
			pin_invulnerability = 1
			pin_invulnerability_time = 1.0
			loglevel = 3
		}
		Admins = {}
		OnlineAdmins = []
		VoteAbortedOn = 0
		VoteKickCaster = -1
		Events = {}
		Invulnerability = {}
		LastRusherID = null
		LastRusherTime = 0
		L4F = false
	}

	::Left4Grief.Log <- function (level, text)
	{
		if (level > Left4Grief.Settings.loglevel)
			return;
		
		if (level == LOG_LEVEL_DEBUG)
			printl("[L4G][DEBUG] " + text);
		else if (level == LOG_LEVEL_INFO)
			printl("[L4G][INFO] " + text);
		else if (level == LOG_LEVEL_WARN)
			error("[L4G][WARNING] " + text + "\n");
		else if (level == LOG_LEVEL_ERROR)
			error("[L4G][ERROR] " + text + "\n");
		else
			error("[L4G][" + level + "] " + text + "\n");
	}

	// Left4Grief main initialization function
	::Left4Grief.Initialize <- function (modename, mapname)
	{
		if (Left4Grief.Initialized)
		{
			Left4Grief.Log(LOG_LEVEL_DEBUG, "Already initialized");
			return;
		}
		
		Left4Grief.ModeName = modename;
		Left4Grief.MapName = mapname;
		
		Left4Grief.Log(LOG_LEVEL_INFO, "Loading settings...");
		Left4Utils.LoadSettingsFromFile("left4grief/cfg/settings.txt", "Left4Grief.Settings.", Left4Grief.Log);
		Left4Utils.SaveSettingsToFile("left4grief/cfg/settings.txt", ::Left4Grief.Settings, Left4Grief.Log);
		Left4Utils.PrintSettings(::Left4Grief.Settings, Left4Grief.Log, "[Settings] ");
		
		Left4Grief.Log(LOG_LEVEL_INFO, "Loading admins...");
		::Left4Grief.Admins = Left4Utils.LoadAdminsFromFile("left4grief/cfg/admins.txt", Left4Grief.Log);
		Left4Grief.Log(LOG_LEVEL_INFO, "Loaded " + Left4Grief.Admins.len() + " admins");
		
		Left4Grief.Initialized = true;
	}

	::Left4Grief.IsAdmin <- function (player)
	{
		if (!player)
			return false;

		local steamid = player.GetNetworkIDString();
		if (!steamid || steamid == "BOT")
			return false;

		if (steamid in ::Left4Grief.Admins)
			return true;
		
		if (GetListenServerHost() == player || Director.IsSinglePlayerGame())
		{
			Left4Grief.Admins[steamid] <- player.GetPlayerName();
			
			Left4Utils.SaveAdminsToFile("left4grief/cfg/admins.txt", ::Left4Grief.Admins);

			return true;
		}
		return false;
	}

	::Left4Grief.IsOnlineAdmin <- function (player)
	{
		if (!player)
			return false;
		
		if (Left4Grief.OnlineAdmins.find(player.GetPlayerUserId()) != null)
			return true;
		else
			return false;
	}

	::Left4Grief.PlayerIn <- function (player)
	{
		local userid = player.GetPlayerUserId().tointeger();
		
		if (Left4Grief.OnlineAdmins.find(userid) == null && Left4Grief.IsAdmin(player))
		{
			Left4Grief.Log(LOG_LEVEL_INFO, "Adding admin with userid: " + userid);
		
			Left4Grief.OnlineAdmins.push(userid);
			Left4Grief.OnlineAdmins.sort();
		}
	}

	::Left4Grief.PlayerOut <- function (userid, player)
	{
		local idx = Left4Grief.OnlineAdmins.find(userid);
		if (idx != null)
		{
			Left4Grief.OnlineAdmins.remove(idx);
			Left4Grief.Log(LOG_LEVEL_INFO, "OnlineAdmin removed with idx: " + idx);
		}
	}

	::Left4Grief.OnRoundStart <- function (player)
	{
		Left4Grief.Log(LOG_LEVEL_DEBUG, "Left4Grief.OnRoundStart");
		
		if ("Left4Fun" in getroottable() && "IsOnlineTroll" in ::Left4Fun)
		{
			Left4Grief.L4F = true;
			
			Left4Grief.Log(LOG_LEVEL_DEBUG, "L4F = true");
		}
		else
			Left4Grief.Log(LOG_LEVEL_DEBUG, "L4F = false");
		
		::ConceptsHub.SetHandler("Left4Grief", Left4Grief.OnConcept);
		
		Left4Timers.AddTimer("AntiRushCheck", ANTIRUSH_CHECK_DELAY, Left4Grief.AntiRushCheck, {}, true);
		//Left4Timers.AddThinker("InvulnerabilityHandler", 0.2, Left4Grief.InvulnerabilityHandler, {});
		Left4Timers.AddTimer("InvulnerabilityHandler", 0.2, Left4Grief.InvulnerabilityHandler, {}, true);
		
		foreach (player in ::Left4Utils.GetHumanPlayers())
			Left4Grief.PlayerIn(player);
	}

	::Left4Grief.OnRoundEnd <- function (winner, reason, message, time, params)
	{
		Left4Grief.Log(LOG_LEVEL_DEBUG, "Left4Grief.OnRoundEnd - winner: " + winner + " - reason: " + reason + " - message: " + message + " - time: " + time);
		
		Left4Timers.RemoveTimer("AntiRushCheck");
		//Left4Timers.RemoveThinker("InvulnerabilityHandler");
		Left4Timers.RemoveTimer("InvulnerabilityHandler");
	}

	::Left4Grief.OnMapTransition <- function (params)
	{
		Left4Grief.Log(LOG_LEVEL_DEBUG, "Left4Grief.OnMapTransition");
		
		Left4Timers.RemoveTimer("AntiRushCheck");
		//Left4Timers.RemoveThinker("InvulnerabilityHandler");
		Left4Timers.RemoveTimer("InvulnerabilityHandler");
	}

	::Left4Grief.AdminAlert <- function (text)
	{
		foreach (userid in Left4Grief.OnlineAdmins)
		{
			local player = g_MapScript.GetPlayerFromUserID(userid);
			if (player)
				ClientPrint(player, 3, "\x04" + text);
		}
	}

	::Left4Grief.OnConcept <- function (concept, query)
	{
		if (concept != "Fault" || (!Left4Grief.Settings.fault_admin_alert && !Left4Grief.Settings.fault_public_alert))
			return;
		
		local who = Left4Utils.GetSurvivorFromActor(query.who);
		if (!who || !who.IsValid() || !("IsPlayer" in who) || !who.IsPlayer())
			return;
		
		Left4Grief.Fault(who, query.faultname);
	}

	::Left4Grief.Fault <- function (player, fault)
	{
		local txt = "[FAULT]: " + player.GetPlayerName() + " -> " + fault;
			
		if (Left4Grief.Settings.fault_admin_alert)
			Left4Grief.AdminAlert(txt);
			
		if (Left4Grief.Settings.fault_public_alert)
			ClientPrint(null, 3, "\x04" + txt);
	}

	::Left4Grief.AntiRushCheck <- function (args)
	{
		if (Left4Grief.Settings.antirush_distance <= 0 || (!Left4Grief.Settings.antirush_admins && !Left4Grief.Settings.antirush_users))
			return;
	  
		// In most finales you just have to hold a position until the rescue comes or you need to collect gascans around the map. It's not a good idea to have an anti-rush in such finales.
		// The only finale where the anti-rush makes sense is The Parish's bridge finale (TODO: maybe others?).
		if (Left4Utils.IsFinale() && Left4Grief.MapName != "c5m5_bridge")
			return;
	  
		local rusher = Left4Utils.SurvivorWithHighestFlow();
		if (!rusher || IsPlayerABot(rusher))
			return;
	  
		if ((!Left4Grief.Settings.antirush_users && !Left4Grief.IsOnlineAdmin(rusher)) || (!Left4Grief.Settings.antirush_admins && Left4Grief.IsOnlineAdmin(rusher)))
			return;
	  
		local nearest = Left4Utils.GetNearestAliveSurvivor(rusher);
		if (!nearest)
			return;
	  
		local dist = (rusher.GetOrigin() - nearest.GetOrigin()).Length();
	  
		if (dist > Left4Grief.Settings.antirush_distance)
		{
			Left4Grief.PunishRusher(rusher);
			
			if (rusher.GetPlayerUserId() != Left4Grief.LastRusherID || (Time() - Left4Grief.LastRusherTime) > 60.0)
			{
				Left4Grief.LastRusherID = rusher.GetPlayerUserId();
				Left4Grief.LastRusherTime = Time();
				
				Left4Grief.Fault(rusher, "Rushing");
			}
		}
	}

	::Left4Grief.PunishRusher <- function (rusher)
	{
		Left4Grief.Log(LOG_LEVEL_INFO, rusher.GetPlayerName() + " is rushing");

		local txt = "";
			
		if (Left4Grief.Settings.antirush_punish_goback != 0)
		{
			local last = Left4Utils.SurvivorWithLowestFlow();
			if (last != null)
			{
				rusher.SetOrigin(last.GetOrigin());
			
				txt = "Rusher " + rusher.GetPlayerName() + " has been teleported back";
			}
		}
		
		if (Left4Grief.Settings.antirush_punish_slowdown != 0)
		{
			if (NetProps.GetPropFloatArray(rusher, "m_flFriction", 0) == 1)
			{
				if (txt == "")
					txt = "Rusher " + rusher.GetPlayerName() + " has been slowed down";
				else
					txt += ", slowed down";
			}
			else
			{
				if (txt == "")
					txt = "Rusher " + rusher.GetPlayerName() + " has been re-slowed down";
				else
					txt += ", re-slowed down";
			}
			
			rusher.OverrideFriction(5, 3);
		}
		
		if (Left4Grief.Settings.antirush_punish_vomit != 0)
		{
			local lastVomitStart = NetProps.GetPropFloatArray(rusher, "m_vomitStart", 0);
			local elapsed = Time() - lastVomitStart;
			
			//Left4Grief.Log(LOG_LEVEL_DEBUG, "elapsed = " + elapsed);
			
			if (elapsed > 15)
			{
				rusher.HitWithVomit();
				NetProps.SetPropFloatArray(rusher, "m_itTimer.m_timestamp", Time() + 15.0, 0);
			
				if (txt == "")
					txt = "Rusher " + rusher.GetPlayerName() + " has been vomited";
				else
					txt += ", vomited";
			}
		}
		
		if (txt != "")
			Left4Grief.Log(LOG_LEVEL_INFO, txt);
	}
	
/*
	::Left4Grief.InvulnerabilityHandler <- function (args)
	{
		if (Left4Grief.Settings.pin_invulnerability)
		{
			foreach (player in ::Left4Utils.GetAliveSurvivors())
			{
				local id = player.GetPlayerUserId();
				if (Left4Utils.IsPlayerHeld(player))
					::Left4Grief.Invulnerability[id] <- { PE = player, TS = 0 };
				else
				{
					if (id in ::Left4Grief.Invulnerability && ::Left4Grief.Invulnerability[id].TS <= 0)
					{
						::Left4Grief.Invulnerability[id].TS = Time(); // Timestamp of when the survivor was freed from the special infected
						
						Left4Grief.Log(LOG_LEVEL_DEBUG, "InvulnerabilityHandler - player " + player.GetPlayerName() + " (" + id + ") freed (" + ::Left4Grief.Invulnerability[id].TS + ")");
					}
				}
			}
		}
		
		foreach (id, inv in ::Left4Grief.Invulnerability)
		{
			if (!Left4Grief.Settings.pin_invulnerability || (inv.TS > 0 && (Time() - inv.TS) > Left4Grief.Settings.pin_invulnerability_time) || !inv.PE || !inv.PE.IsValid())
			{
				delete ::Left4Grief.Invulnerability[id];
				
				Left4Grief.Log(LOG_LEVEL_DEBUG, "InvulnerabilityHandler - player with id " + id + " no longer invulnerable");
			}
		}
	}
*/

	::Left4Grief.PinStart <- function (player)
	{
		if (!Left4Grief.Settings.pin_invulnerability || NetProps.GetPropInt(player, "m_iTeamNum") != TEAM_SURVIVORS)
			return;
		
		Left4Grief.Log(LOG_LEVEL_DEBUG, "PinStart - " + player.GetPlayerName());
		
		::Left4Grief.Invulnerability[player.GetPlayerUserId()] <- { PE = player, TS = 0 };
		
		Left4Grief.Log(LOG_LEVEL_DEBUG, "PinStart - player with id " + player.GetPlayerUserId() + " (" + player.GetPlayerName() + ") is invulnerable");
	}
	
	::Left4Grief.PinStop <- function (player)
	{
		if (!Left4Grief.Settings.pin_invulnerability || NetProps.GetPropInt(player, "m_iTeamNum") != TEAM_SURVIVORS)
			return;
		
		Left4Grief.Log(LOG_LEVEL_DEBUG, "PinStop - " + player.GetPlayerName());
		
		::Left4Grief.Invulnerability[player.GetPlayerUserId()] <- { PE = player, TS = Time() };
		
		Left4Grief.Log(LOG_LEVEL_DEBUG, "PinStop - player with id " + player.GetPlayerUserId() + " (" + player.GetPlayerName() + ") is invulnerable");
	}

	::Left4Grief.TriggeredCarAlarm <- function (player)
	{
		Left4Grief.Log(LOG_LEVEL_DEBUG, "TriggeredCarAlarm - " + player.GetPlayerName());
		
		Left4Grief.Fault(player, "TriggeredCarAlarm");
	}

	::Left4Grief.CreatePanicEvent <- function (player)
	{
		Left4Grief.Log(LOG_LEVEL_DEBUG, "CreatePanicEvent - " + player.GetPlayerName());
		
		Left4Grief.Fault(player, "CreatePanicEvent");
	}

	::Left4Grief.InvulnerabilityHandler <- function (args)
	{
		foreach (id, inv in ::Left4Grief.Invulnerability)
		{
			if (!Left4Grief.Settings.pin_invulnerability || (inv.TS > 0 && (Time() - inv.TS) > Left4Grief.Settings.pin_invulnerability_time) || !inv.PE || !inv.PE.IsValid())
			{
				delete ::Left4Grief.Invulnerability[id];
				
				Left4Grief.Log(LOG_LEVEL_DEBUG, "InvulnerabilityHandler - player with id " + id + " no longer invulnerable");
			}
		}
	}
	
	::Left4Grief.OnDamage <- function (victim, attacker, damageDone, damageTable)
	{
		if (damageDone <= 0 || victim == null || !victim.IsPlayer())
			return damageDone;
		
		if (attacker == null)
		{
			Left4Grief.Log(LOG_LEVEL_DEBUG, "OnDamage - attacker is null!!! victim: " + victim.GetPlayerName());
			return damageDone;
		}

		if (!attacker.IsValid())
		{
			Left4Grief.Log(LOG_LEVEL_DEBUG, "OnDamage - attacker is invalid!!! victim: " + victim.GetPlayerName());
			return damageDone;
		}

		if (!attacker.IsPlayer())
			return damageDone;

		//Left4Grief.Log(LOG_LEVEL_DEBUG, "OnDamage - attacker: " + attacker.GetPlayerName() + " - victim: " + victim.GetPlayerName() + " - damage: " + damageDone + " - type: " + damageTable.DamageType);

		if (attacker.GetPlayerUserId() == victim.GetPlayerUserId())
			return damageDone; // self damage
		
		local attackerTeam = NetProps.GetPropInt(attacker, "m_iTeamNum");
		local victimTeam = NetProps.GetPropInt(victim, "m_iTeamNum");
		
		if (attackerTeam == TEAM_SPECTATORS)
		{
			Left4Grief.Log(LOG_LEVEL_WARN, "Blocked bugged damage from spectator: " + attacker.GetPlayerName());
			return -1;
		}
		
		if (attackerTeam != victimTeam)
			return damageDone;
		
		// Friendly fire!

		if (Left4Grief.IsOnlineAdmin(attacker))
		{
			if (Left4Grief.Settings.friendly_fire_admins == 0)
				return -1; // Friendly fire OFF
			
			if (Left4Grief.L4F && Left4Fun.IsOnlineTroll(victim))
				return damageDone;
			
			// Friendly fire towards a survivor who is being ridden by a jockey or pummeled by a charger is already blocked by the game
			// but, for some reason, friendly fire goes through during the carry phase of the charger attack and when the survivor is being
			// pounced by a hunter or trapped by the smoker's tongue... Here i block friendly fire for these too.
			//if (NetProps.GetPropInt(victim, "m_pounceAttacker") > 0 || NetProps.GetPropInt(victim, "m_tongueOwner") > 0 || NetProps.GetPropInt(victim, "m_carryAttacker") > 0)
			//	return -1;
			if (victim.GetPlayerUserId() in ::Left4Grief.Invulnerability)
				return -1;
			
			if (Left4Grief.Settings.ricochet_admins)
			{
				// Ricochet
				/*
				if ((damageTable.DamageType & DMG_BURN) != 0)
					attacker.TakeDamage(damageDone * Left4Grief.Settings.ricochet_burn_damagefactor, damageTable.DamageType, attacker);
				else if ((damageTable.DamageType & DMG_BLAST) != 0)
					attacker.TakeDamage(damageDone * Left4Grief.Settings.ricochet_blast_damagefactor, damageTable.DamageType, attacker);
				else
					attacker.TakeDamage(damageDone * Left4Grief.Settings.ricochet_damagefactor, damageTable.DamageType, attacker);
				*/

				if ((damageTable.DamageType & DMG_BURN) != 0)
					attacker.TakeDamageEx(damageTable.Inflictor, attacker, damageTable.Weapon, Vector(0,0,0), damageTable.Location, damageDone * Left4Grief.Settings.ricochet_burn_damagefactor, damageTable.DamageType);
				else if ((damageTable.DamageType & DMG_BLAST) != 0)
					attacker.TakeDamageEx(damageTable.Inflictor, attacker, damageTable.Weapon, Vector(0,0,0), damageTable.Location, damageDone * Left4Grief.Settings.ricochet_blast_damagefactor, damageTable.DamageType);
				else
					attacker.TakeDamageEx(damageTable.Inflictor, attacker, damageTable.Weapon, Vector(0,0,0), damageTable.Location, damageDone * Left4Grief.Settings.ricochet_damagefactor, damageTable.DamageType);

				return 0.0001; // Damage is pretty much next to 0 and totally insignificant but there are 2 pros of returning this instead of 0:
							   //  1. this will still be counted as friendly fire accident in the attacker's final stats (and in the console as "XXX attacked YYY")
							   //  2. the victim will still play friendly fire reactions so he can be aware of it
			}
		}
		else
		{
			if (Left4Grief.Settings.friendly_fire == 0)
				return -1; // Friendly fire OFF
			
			if (Left4Grief.L4F && Left4Fun.IsOnlineTroll(victim))
				return damageDone;
			
			// Friendly fire towards a survivor who is being ridden by a jockey or pummeled by a charger is already blocked by the game
			// but, for some reason, friendly fire goes through during the carry phase of the charger attack and when the survivor is being
			// pounced by a hunter or trapped by the smoker's tongue... Here i block friendly fire for these too.
			//if (NetProps.GetPropInt(victim, "m_pounceAttacker") > 0 || NetProps.GetPropInt(victim, "m_tongueOwner") > 0 || NetProps.GetPropInt(victim, "m_carryAttacker") > 0)
			//	return -1;
			if (victim.GetPlayerUserId() in ::Left4Grief.Invulnerability)
				return -1;
			
			if ((Left4Grief.Settings.ricochet_users && !IsPlayerABot(attacker)) || (Left4Grief.Settings.ricochet_bots && IsPlayerABot(attacker)))
			{
				// Ricochet
				/*
				if ((damageTable.DamageType & DMG_BURN) != 0)
					attacker.TakeDamage(damageDone * Left4Grief.Settings.ricochet_burn_damagefactor, damageTable.DamageType, attacker);
				else if ((damageTable.DamageType & DMG_BLAST) != 0)
					attacker.TakeDamage(damageDone * Left4Grief.Settings.ricochet_blast_damagefactor, damageTable.DamageType, attacker);
				else
					attacker.TakeDamage(damageDone * Left4Grief.Settings.ricochet_damagefactor, damageTable.DamageType, attacker);
				*/
				
				if ((damageTable.DamageType & DMG_BURN) != 0)
					attacker.TakeDamageEx(damageTable.Inflictor, attacker, damageTable.Weapon, Vector(0,0,0), damageTable.Location, damageDone * Left4Grief.Settings.ricochet_burn_damagefactor, damageTable.DamageType);
				else if ((damageTable.DamageType & DMG_BLAST) != 0)
					attacker.TakeDamageEx(damageTable.Inflictor, attacker, damageTable.Weapon, Vector(0,0,0), damageTable.Location, damageDone * Left4Grief.Settings.ricochet_blast_damagefactor, damageTable.DamageType);
				else
					attacker.TakeDamageEx(damageTable.Inflictor, attacker, damageTable.Weapon, Vector(0,0,0), damageTable.Location, damageDone * Left4Grief.Settings.ricochet_damagefactor, damageTable.DamageType);

				return 0.0001; // Damage is pretty much next to 0 and totally insignificant but there are 2 pros of returning this instead of 0:
							   //  1. this will still be counted as friendly fire accident in the attacker's final stats (and in the console as "XXX attacked YYY")
							   //  2. the victim will still play friendly fire reactions so he can be aware of it
			}
		}
		
		// Normal friendly fire
		return damageDone;
	}
	
	::Left4Grief.IdleTargetCheck <- function (params)
	{
		Left4Grief.Log(LOG_LEVEL_DEBUG, "IdleTargetCheck");
		
		// If we are here it means that someone casted a kick vote against an idle player (there is no automatic NO if the target is idle)
		// With no automatic NO there is no way to know who is the kick vote target so we'll abort the vote whenever there is at least 1 admin idle
		
		foreach (spec in ::Left4Utils.GetSpectators())
		{
			if (Left4Grief.IsOnlineAdmin(spec))
			{
				local voteControllerEnt = Entities.FindByClassname(null, "vote_controller");
				if (!voteControllerEnt)
				{
					Left4Grief.Log(LOG_LEVEL_ERROR, "vote_controller entity not found");
					return;
				}
			  
				NetProps.SetPropIntArray(voteControllerEnt, "m_votesYes", 0, 0);
				NetProps.SetPropIntArray(voteControllerEnt, "m_votesNo", NetProps.GetPropIntArray(voteControllerEnt, "m_potentialVotes", 0), 0);

				Left4Grief.VoteAbortedOn = Time();
				
				Left4Grief.Log(LOG_LEVEL_WARN, "Kick vote aborted due to a potentially idle admin target");
				
				return;
			}
		}
		
		// If no admin is idle, the vote keeps going
	}
	
	::Left4Grief.OnVoteCasted <- function (voteType, entIndex)
	{
		Left4Grief.Log(LOG_LEVEL_DEBUG, "OnVoteCasted - voteType: " + voteType + " - entIndex: " + entIndex);
		
		if (!entIndex)
		{
			Left4Grief.Log(LOG_LEVEL_ERROR, "OnVoteCasted - ERROR: invalid entIndex (voteType: " + voteType + " - entIndex: " + entIndex + ")!");
			return true;
		}
		
		//local player = g_MapScript.GetPlayerFromUserID(entIndex);
		local player = g_MapScript.PlayerInstanceFromIndex(entIndex);
		if (!player)
		{
			Left4Grief.Log(LOG_LEVEL_ERROR, "OnVoteCasted - ERROR: player not found (voteType: " + voteType + " - entIndex: " + entIndex + ")!");
			return true;
		}
		
		Left4Grief.Log(LOG_LEVEL_INFO, "Vote casted by: " + player.GetPlayerName() + " (voteType: " + voteType + ")");
		
		if (Left4Grief.IsOnlineAdmin(player))
			return true;
		else if (Left4Grief.Settings.user_can_cast_vote == 0)
			return false;
		
		switch (voteType)
		{
			case VOTE_TYPE.ChangeDifficulty:
			{
				Left4Grief.Log(LOG_LEVEL_DEBUG, "OnVoteCasted - ChangeDifficulty vote casted by " + player.GetPlayerName());
				return Left4Grief.Settings.vote_change_difficulty != 0;
			}
			case VOTE_TYPE.RestartGame:
			{
				Left4Grief.Log(LOG_LEVEL_DEBUG, "OnVoteCasted - RestartGame vote casted by " + player.GetPlayerName());
				return Left4Grief.Settings.vote_restart_game != 0;
			}
			case VOTE_TYPE.Kick:
			{
				Left4Grief.Log(LOG_LEVEL_DEBUG, "OnVoteCasted - Kick vote casted by " + player.GetPlayerName());
				
				if (Left4Grief.Settings.vote_kick)
				{
					Left4Timers.AddTimer("IdleTargetCheck", 0.2, Left4Grief.IdleTargetCheck, { });
					return true;
				}
				else
					return false;
			}
			case VOTE_TYPE.ChangeMission:
			{
				Left4Grief.Log(LOG_LEVEL_DEBUG, "OnVoteCasted - ChangeMission vote casted by " + player.GetPlayerName());
				return Left4Grief.Settings.vote_change_mission != 0;
			}
			case VOTE_TYPE.ReturnToLobby:
			{
				Left4Grief.Log(LOG_LEVEL_DEBUG, "OnVoteCasted - ReturnToLobby vote casted by " + player.GetPlayerName());
				return Left4Grief.Settings.vote_return_to_lobby != 0;
			}
			case VOTE_TYPE.ChangeChapter:
			{
				Left4Grief.Log(LOG_LEVEL_DEBUG, "OnVoteCasted - ChangeChapter vote casted by " + player.GetPlayerName());
				return Left4Grief.Settings.vote_change_chapter != 0;
			}
			case VOTE_TYPE.ChangeAllTalk:
			{
				Left4Grief.Log(LOG_LEVEL_DEBUG, "OnVoteCasted - ChangeAllTalk vote casted by " + player.GetPlayerName());
				return Left4Grief.Settings.vote_change_all_talk != 0;
			}
		}
		Left4Grief.Log(LOG_LEVEL_WARN, "OnVoteCasted - WARNING: Unknown voteType: " + voteType);
		return true;
	}

	::Left4Grief.OnKickVote <- function (casterIndex, targetIndex)
	{
		Left4Grief.Log(LOG_LEVEL_DEBUG, "OnKickVote - casterIndex: " + casterIndex + " - targetIndex: " + targetIndex);
		
		Left4Timers.RemoveTimer("IdleTargetCheck");
		
		if (!casterIndex)
		{
			Left4Grief.Log(LOG_LEVEL_ERROR, "OnKickVote - ERROR: invalid casterIndex (casterIndex: " + casterIndex + " - targetIndex: " + targetIndex + ")!");
			return true;
		}
		
		//local caster = g_MapScript.GetPlayerFromUserID(casterIndex);
		local caster = g_MapScript.PlayerInstanceFromIndex(casterIndex);
		if (!caster)
		{
			Left4Grief.Log(LOG_LEVEL_ERROR, "OnKickVote - ERROR: caster player not found (casterIndex: " + casterIndex + ")!");
			return true;
		}
		
		if (!targetIndex)
		{
			Left4Grief.Log(LOG_LEVEL_ERROR, "OnKickVote - ERROR: invalid targetIndex (casterIndex: " + casterIndex + " - targetIndex: " + targetIndex + ")!");
			return true;
		}
		
		local target = g_MapScript.PlayerInstanceFromIndex(targetIndex);
		if (!target)
		{
			Left4Grief.Log(LOG_LEVEL_ERROR, "OnKickVote - ERROR: target player not found (targetIndex: " + targetIndex + ")!");
			return true;
		}
		
		if (Left4Grief.IsOnlineAdmin(target))
		{
			if (Left4Grief.IsOnlineAdmin(caster))
				Left4Grief.Log(LOG_LEVEL_WARN, "Admin " + caster.GetPlayerName() + " tried to vote kick admin " + target.GetPlayerName());
			else
			{
				Left4Grief.Log(LOG_LEVEL_WARN, "User " + caster.GetPlayerName() + " tried to vote kick admin " + target.GetPlayerName());
				
				if (Left4Grief.Settings.revenge_kick != 0)
				{
					local steamid = caster.GetNetworkIDString();
					if (!steamid)
						Left4Grief.Log(LOG_LEVEL_ERROR, "Invalid steamid (" + steamid + ") in revenge kick");
					else
					{
						SendToServerConsole("kickid " + steamid + " Nice try! :D");
						Left4Grief.Log(LOG_LEVEL_WARN, caster.GetPlayerName() + " has been kicked with the reason: Nice try! :D");
					}
				}
			}
			
			return false;
		}
		return true;
	}

	::Left4Grief.OnHealStart <- function (player, healer, params)
	{
		if(!player || !healer)
			return;
		
		if (IsPlayerABot(healer))
			return;

		if (healer.GetPlayerUserId() == player.GetPlayerUserId() || NetProps.GetPropIntArray(player, "movetype", 0) != MOVETYPE_LADDER || Left4Grief.IsOnlineAdmin(healer))
			return;
		
		if (Left4Grief.Settings.punish_heal_onladder)
			Left4Utils.IncapacitatePlayer(healer, DMG_FALL);
		else
		{
			healer.Stagger(player.GetOrigin());
		
			Left4Timers.AddTimer(null, 0.1, Left4Grief.RemoveMedkit, { player = healer });
		}
		
		Left4Grief.Fault(healer, "HealOnLadder");
	}
	
	::Left4Grief.RemoveMedkit <- function (params)
	{
		local player = params["player"];
		
		Left4Grief.Log(LOG_LEVEL_DEBUG, "Left4Grief.RemoveMedkit - player: " + player.GetPlayerName());
				
		local table = {};
		GetInvTable(player, table);
		foreach(slot, item in table)
		{
			if (item.GetClassname() == "weapon_first_aid_kit")
				item.Kill();
		}
		
		Left4Timers.AddTimer(null, 0.1, Left4Grief.RestoreMedkit, { player = player });
	}
	
	::Left4Grief.RestoreMedkit <- function (params)
	{
		local player = params["player"];
		
		Left4Grief.Log(LOG_LEVEL_DEBUG, "Left4Grief.RestoreMedkit - player: " + player.GetPlayerName());
				
		player.GiveItem("weapon_first_aid_kit");
	}

	::Left4Grief.KillOwnedEntsByType <- function (entindex, enttype)
	{
		local ent = null;
		while (ent = Entities.FindByClassname(ent, enttype))
		{
			local owner = NetProps.GetPropEntity(ent, "m_hOwnerEntity");
			if (owner && owner.GetEntityIndex() == entindex)
			{
				Left4Grief.Log(LOG_LEVEL_DEBUG, "Removing entity of type: " + enttype + " that was owned by entity with index: " + entindex);
				ent.Kill();
			}
		}
	}

	::Left4Grief.RemoveOwnedEntities <- function (player)
	{
		if (!Left4Grief.Settings.anti_disconnect_grief)
			return;
		
		if (!player || !player.IsValid())
		{
			Left4Grief.Log(LOG_LEVEL_DEBUG, "Left4Grief.RemoveOwnedEntities - Invalid player!");
			return;
		}
		
		local entIndex = player.GetEntityIndex();
		
		Left4Grief.KillOwnedEntsByType(entIndex, "inferno");
		Left4Grief.KillOwnedEntsByType(entIndex, "molotov_projectile");
		Left4Grief.KillOwnedEntsByType(entIndex, "pipe_bomb_projectile");
		Left4Grief.KillOwnedEntsByType(entIndex, "grenade_launcher_projectile");
	}
	
	::Left4Grief.OnPlayerSay <- function (player, text, args, params)
	{
		if (player == null || !player.IsValid() || IsPlayerABot(player) || args.len() < 2)
			return;
		
		Left4Grief.Log(LOG_LEVEL_DEBUG, "Left4Grief.OnPlayerSay - " + player.GetPlayerName() + ": " + text);
		
		local arg0 = args[0].tolower();
		local arg1 = args[1].tolower();
		
		if (arg0 == "!l4gsettings" && Left4Grief.IsOnlineAdmin(player))
		{
			if (arg1 in Left4Grief.Settings)
			{
				if (args.len() < 3)
					ClientPrint(player, 3, "\x01 Current value for " + arg1 + ": " + Left4Grief.Settings[arg1]);
				else
				{
					try
					{
						local value = args[2].tointeger();
						::Left4Grief.Settings[arg1] <- value;
						Left4Utils.SaveSettingsToFile("left4grief/cfg/settings.txt", ::Left4Grief.Settings, Left4Grief.Log);
						ClientPrint(player, 3, "\x05 Changed value for " + arg1 + " to: " + value);
					}
					catch(exception)
					{
						Left4Grief.Log(LOG_LEVEL_ERROR, "Error changing settings value - option: " + arg1 + " - new value: " + args[2] + " - error: " + exception);
						ClientPrint(player, 3, "\x04 Error changing settings value for " + arg1);
					}
				}
			}
			else
				ClientPrint(player, 3, "\x04 Invalid settings option: " + arg1);
		}
	}
	
	//
//}

IncludeScript("left4grief_events");
