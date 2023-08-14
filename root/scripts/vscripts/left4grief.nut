//------------------------------------------------------
//     Author : smilzo
//     https://steamcommunity.com/id/smilz0
//------------------------------------------------------

/* TODO:
	antibhop?
*/

Msg("Including left4grief...\n");

if (!IncludeScript("left4lib_users"))
	error("[L4G][ERROR] Failed to include 'left4lib_users', please make sure the 'Left 4 Lib' addon is installed and enabled!\n");
if (!IncludeScript("left4lib_timers"))
	error("[L4G][ERROR] Failed to include 'left4lib_timers', please make sure the 'Left 4 Lib' addon is installed and enabled!\n");
if (!IncludeScript("left4lib_concepts"))
	error("[L4G][ERROR] Failed to include 'left4lib_concepts', please make sure the 'Left 4 Lib' addon is installed and enabled!\n");

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

// Log levels
const LOG_LEVEL_NONE = 0; // Log always
const LOG_LEVEL_ERROR = 1;
const LOG_LEVEL_WARN = 2;
const LOG_LEVEL_INFO = 3;
const LOG_LEVEL_DEBUG = 4;

::Left4Grief <-
{
	Initialized = false
	ModeName = ""
	MapName = ""
	Difficulty = "" // easy, normal, hard, impossible
	VoteAbortedOn = 0
	VoteKickCaster = -1
	Events = {}
	Invulnerability = {}
	LastRusherID = null
	LastRusherTime = 0
}

IncludeScript("left4grief_settings");

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
::Left4Grief.Initialize <- function ()
{
	if (Left4Grief.Initialized)
	{
		Left4Grief.Log(LOG_LEVEL_DEBUG, "Already initialized");
		return;
	}
	
	Left4Grief.ModeName = SessionState.ModeName;
	Left4Grief.MapName = SessionState.MapName;
	Left4Grief.Difficulty = Convars.GetStr("z_difficulty").tolower();

	Left4Grief.Log(LOG_LEVEL_INFO, "Initializing for game mode: " + Left4Grief.ModeName + " - map name: " + Left4Grief.MapName + " - difficulty: " + Left4Grief.Difficulty);
	
	Left4Grief.Log(LOG_LEVEL_INFO, "Loading settings...");
	Left4Utils.LoadSettingsFromFile("left4grief/cfg/settings.txt", "Left4Grief.Settings.", Left4Grief.Log);
	Left4Utils.SaveSettingsToFile("left4grief/cfg/settings.txt", ::Left4Grief.Settings, Left4Grief.Log);
	Left4Utils.PrintSettings(::Left4Grief.Settings, Left4Grief.Log, "[Settings] ");
	
	Left4Grief.Initialized = true;
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
		Left4Users.AdminNotice("\x04" + txt);
		
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
  
	local rl = Left4Users.GetOnlineUserLevel(rusher.GetPlayerUserId());
	if ((!Left4Grief.Settings.antirush_users && rl < L4U_LEVEL.Admin) || (!Left4Grief.Settings.antirush_admins && rl >= L4U_LEVEL.Admin))
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

::Left4Grief.AllowTakeDamage <- function (damageTable)
{
	if (damageTable.DamageDone < 0)
		return false;
	
	if (damageTable.DamageDone == 0 || damageTable.Victim == null || !damageTable.Victim.IsPlayer())
		return true;
	
	if (damageTable.Attacker == null)
	{
		Left4Grief.Log(LOG_LEVEL_DEBUG, "OnDamage - attacker is null!!! victim: " + damageTable.Victim.GetPlayerName());
		return true;
	}

	if (!damageTable.Attacker.IsValid())
	{
		Left4Grief.Log(LOG_LEVEL_DEBUG, "OnDamage - attacker is invalid!!! victim: " + damageTable.Victim.GetPlayerName());
		return true;
	}

	if (!damageTable.Attacker.IsPlayer())
		return true;

	//Left4Grief.Log(LOG_LEVEL_DEBUG, "OnDamage - attacker: " + damageTable.Attacker.GetPlayerName() + " - victim: " + damageTable.Victim.GetPlayerName() + " - damage: " + damageTable.DamageDone + " - type: " + damageTable.DamageType);

	if (damageTable.Attacker.GetPlayerUserId() == damageTable.Victim.GetPlayerUserId())
		return true; // self damage
	
	local attackerTeam = NetProps.GetPropInt(damageTable.Attacker, "m_iTeamNum");
	local victimTeam = NetProps.GetPropInt(damageTable.Victim, "m_iTeamNum");
	
	if (attackerTeam == TEAM_SPECTATORS)
	{
		Left4Grief.Log(LOG_LEVEL_WARN, "Blocked bugged damage from spectator: " + damageTable.Attacker.GetPlayerName());
		return false;
	}
	
	if (attackerTeam != victimTeam)
		return true;
	
	// Friendly fire!

	if (Left4Users.GetOnlineUserLevel(damageTable.Attacker.GetPlayerUserId()) >= L4U_LEVEL.Admin)
	{
		if (Left4Grief.Settings.friendly_fire_admins == 0)
			return false; // Friendly fire OFF
		
		if (Left4Users.GetOnlineUserLevel(damageTable.Victim.GetPlayerUserId()) < L4U_LEVEL.User)
			return true;
		
		// Friendly fire towards a survivor who is being ridden by a jockey or pummeled by a charger is already blocked by the game
		// but, for some reason, friendly fire goes through during the carry phase of the charger attack and when the survivor is being
		// pounced by a hunter or trapped by the smoker's tongue... Here i block friendly fire for these too.
		//if (NetProps.GetPropInt(damageTable.Victim, "m_pounceAttacker") > 0 || NetProps.GetPropInt(damageTable.Victim, "m_tongueOwner") > 0 || NetProps.GetPropInt(damageTable.Victim, "m_carryAttacker") > 0)
		//	return false;
		if (damageTable.Victim.GetPlayerUserId() in ::Left4Grief.Invulnerability)
			return false;
		
		if (Left4Grief.Settings.ricochet_admins)
		{
			// Ricochet
			/*
			if ((damageTable.DamageType & DMG_BURN) != 0)
				damageTable.Attacker.TakeDamage(damageTable.DamageDone * Left4Grief.Settings.ricochet_burn_damagefactor, damageTable.DamageType, damageTable.Attacker);
			else if ((damageTable.DamageType & DMG_BLAST) != 0)
				damageTable.Attacker.TakeDamage(damageTable.DamageDone * Left4Grief.Settings.ricochet_blast_damagefactor, damageTable.DamageType, damageTable.Attacker);
			else
				damageTable.Attacker.TakeDamage(damageTable.DamageDone * Left4Grief.Settings.ricochet_damagefactor, damageTable.DamageType, damageTable.Attacker);
			*/

			if ((damageTable.DamageType & DMG_BURN) != 0)
				damageTable.Attacker.TakeDamageEx(damageTable.Inflictor, damageTable.Attacker, damageTable.Weapon, Vector(0,0,0), damageTable.Location, damageTable.DamageDone * Left4Grief.Settings.ricochet_burn_damagefactor, damageTable.DamageType);
			else if ((damageTable.DamageType & DMG_BLAST) != 0)
				damageTable.Attacker.TakeDamageEx(damageTable.Inflictor, damageTable.Attacker, damageTable.Weapon, Vector(0,0,0), damageTable.Location, damageTable.DamageDone * Left4Grief.Settings.ricochet_blast_damagefactor, damageTable.DamageType);
			else
				damageTable.Attacker.TakeDamageEx(damageTable.Inflictor, damageTable.Attacker, damageTable.Weapon, Vector(0,0,0), damageTable.Location, damageTable.DamageDone * Left4Grief.Settings.ricochet_damagefactor, damageTable.DamageType);

			damageTable.DamageDone = 0.0001; // Damage is pretty much next to 0 and totally insignificant but there are 2 pros of returning this instead of 0:
											 //  1. this will still be counted as friendly fire accident in the attacker's final stats (and in the console as "XXX attacked YYY")
											 //  2. the victim will still play friendly fire reactions so he can be aware of it

			return true;
		}
	}
	else
	{
		if (Left4Grief.Settings.friendly_fire == 0)
			return false; // Friendly fire OFF
		
		if (Left4Users.GetOnlineUserLevel(damageTable.Victim.GetPlayerUserId()) < L4U_LEVEL.User)
			return true;
		
		// Friendly fire towards a survivor who is being ridden by a jockey or pummeled by a charger is already blocked by the game
		// but, for some reason, friendly fire goes through during the carry phase of the charger attack and when the survivor is being
		// pounced by a hunter or trapped by the smoker's tongue... Here i block friendly fire for these too.
		//if (NetProps.GetPropInt(damageTable.Victim, "m_pounceAttacker") > 0 || NetProps.GetPropInt(damageTable.Victim, "m_tongueOwner") > 0 || NetProps.GetPropInt(damageTable.Victim, "m_carryAttacker") > 0)
		//	return false;
		if (damageTable.Victim.GetPlayerUserId() in ::Left4Grief.Invulnerability)
			return false;
		
		if ((Left4Grief.Settings.ricochet_users && !IsPlayerABot(damageTable.Attacker)) || (Left4Grief.Settings.ricochet_bots && IsPlayerABot(damageTable.Attacker)))
		{
			// Ricochet
			/*
			if ((damageTable.DamageType & DMG_BURN) != 0)
				damageTable.Attacker.TakeDamage(damageTable.DamageDone * Left4Grief.Settings.ricochet_burn_damagefactor, damageTable.DamageType, damageTable.Attacker);
			else if ((damageTable.DamageType & DMG_BLAST) != 0)
				damageTable.Attacker.TakeDamage(damageTable.DamageDone * Left4Grief.Settings.ricochet_blast_damagefactor, damageTable.DamageType, damageTable.Attacker);
			else
				damageTable.Attacker.TakeDamage(damageTable.DamageDone * Left4Grief.Settings.ricochet_damagefactor, damageTable.DamageType, damageTable.Attacker);
			*/
			
			if ((damageTable.DamageType & DMG_BURN) != 0)
				damageTable.Attacker.TakeDamageEx(damageTable.Inflictor, damageTable.Attacker, damageTable.Weapon, Vector(0,0,0), damageTable.Location, damageTable.DamageDone * Left4Grief.Settings.ricochet_burn_damagefactor, damageTable.DamageType);
			else if ((damageTable.DamageType & DMG_BLAST) != 0)
				damageTable.Attacker.TakeDamageEx(damageTable.Inflictor, damageTable.Attacker, damageTable.Weapon, Vector(0,0,0), damageTable.Location, damageTable.DamageDone * Left4Grief.Settings.ricochet_blast_damagefactor, damageTable.DamageType);
			else
				damageTable.Attacker.TakeDamageEx(damageTable.Inflictor, damageTable.Attacker, damageTable.Weapon, Vector(0,0,0), damageTable.Location, damageTable.DamageDone * Left4Grief.Settings.ricochet_damagefactor, damageTable.DamageType);

			damageTable.DamageDone = 0.0001; // Damage is pretty much next to 0 and totally insignificant but there are 2 pros of returning this instead of 0:
											 //  1. this will still be counted as friendly fire accident in the attacker's final stats (and in the console as "XXX attacked YYY")
											 //  2. the victim will still play friendly fire reactions so he can be aware of it

			return true;
		}
	}
	
	// Normal friendly fire
	return true;
}

::Left4Grief.IdleTargetCheck <- function (params)
{
	Left4Grief.Log(LOG_LEVEL_DEBUG, "IdleTargetCheck");
	
	// If we are here it means that someone casted a kick vote against an idle player (there is no automatic NO if the target is idle)
	// With no automatic NO there is no way to know who is the kick vote target so we'll abort the vote whenever there is at least 1 admin idle
	
	foreach (spec in ::Left4Utils.GetSpectators())
	{
		if (Left4Users.GetOnlineUserLevel(spec.GetPlayerUserId()) >= L4U_LEVEL.Admin)
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
	
	if (Left4Users.GetOnlineUserLevel(player.GetPlayerUserId()) >= L4U_LEVEL.Admin)
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
	
	if (Left4Users.GetOnlineUserLevel(target.GetPlayerUserId()) >= L4U_LEVEL.Admin)
	{
		if (Left4Users.GetOnlineUserLevel(caster.GetPlayerUserId()) >= L4U_LEVEL.Admin)
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

	if (healer.GetPlayerUserId() == player.GetPlayerUserId() || NetProps.GetPropIntArray(player, "movetype", 0) != MOVETYPE_LADDER || Left4Users.GetOnlineUserLevel(healer.GetPlayerUserId()) >= L4U_LEVEL.Admin)
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

::Left4Grief.HandleCommand <- function (player, cmd, args, text)
{
	if (player == null || !player.IsValid() || IsPlayerABot(player) || Left4Users.GetOnlineUserLevel(player.GetPlayerUserId()) < L4U_LEVEL.Admin)
		return;
	
	Left4Grief.Log(LOG_LEVEL_DEBUG, "Left4Grief.HandleCommand - " + player.GetPlayerName() + " - cmd: " + cmd + " - args: " + args.len());
	
	local numArgs = args.len() - 2; // args[0] = handler, args[1] = command; Only count the remaining args
	switch (cmd)
	{
		case "settings":
		{
			if (numArgs > 0)
			{
				local arg1 = args[2].tolower();
				
				if (arg1 in Left4Grief.Settings)
				{
					if (numArgs < 2)
						ClientPrint(player, 3, "\x01 Current value for " + arg1 + ": " + Left4Grief.Settings[arg1]);
					else
					{
						try
						{
							local value = args[3].tointeger();
							::Left4Grief.Settings[arg1] <- value;
							Left4Utils.SaveSettingsToFile("left4grief/cfg/settings.txt", ::Left4Grief.Settings, Left4Grief.Log);
							ClientPrint(player, 3, "\x05 Changed value for " + arg1 + " to: " + value);
						}
						catch(exception)
						{
							Left4Grief.Log(LOG_LEVEL_ERROR, "Error changing value for: " + arg1 + " - new value: " + args[3] + " - error: " + exception);
							ClientPrint(player, 3, "\x04 Error changing value for " + arg1);
						}
					}
				}
				else
					ClientPrint(player, 3, "\x04 Invalid setting: " + arg1);
			}
			
			break;
		}
	}
}

IncludeScript("left4grief_events");

HooksHub.SetAllowTakeDamage("L4G", ::Left4Grief.AllowTakeDamage);
HooksHub.SetChatCommandHandler("l4g", ::Left4Grief.HandleCommand);
HooksHub.SetConsoleCommandHandler("l4g", ::Left4Grief.HandleCommand);

Left4Grief.Initialize();
