//------------------------------------------------------
//     Author : smilzo
//     https://steamcommunity.com/id/smilz0
//------------------------------------------------------

Msg("Including left4grief_events...\n");

::Left4Grief.Events.OnGameEvent_round_start <- function (params)
{
	Left4Grief.Log(LOG_LEVEL_DEBUG, "OnGameEvent_round_start");
	
	// Apparently, when scriptedmode is enabled and this director option isn't set, there is a big stutter (for the host)
	// when a witch is chasing a survivor and that survivor enters the saferoom. Simply having a value for this key, removes the stutter
	if (!("AllowWitchesInCheckpoints" in DirectorScript.GetDirectorOptions()))
		DirectorScript.GetDirectorOptions().AllowWitchesInCheckpoints <- false;
	
	::ConceptsHub.SetHandler("Left4Grief", Left4Grief.OnConcept);
	
	Left4Timers.AddTimer("AntiRushCheck", ANTIRUSH_CHECK_DELAY, Left4Grief.AntiRushCheck, {}, true);
	//Left4Timers.AddThinker("InvulnerabilityHandler", 0.2, Left4Grief.InvulnerabilityHandler, {});
	Left4Timers.AddTimer("InvulnerabilityHandler", 0.2, Left4Grief.InvulnerabilityHandler, {}, true);
}

::Left4Grief.Events.OnGameEvent_round_end <- function (params)
{
	local winner = params["winner"];
	local reason = params["reason"];
	local message = params["message"];
	local time = params["time"];
	
	Left4Grief.Log(LOG_LEVEL_DEBUG, "OnGameEvent_round_end - winner: " + winner + " - reason: " + reason + " - message: " + message + " - time: " + time);
	
	Left4Timers.RemoveTimer("AntiRushCheck");
	//Left4Timers.RemoveThinker("InvulnerabilityHandler");
	Left4Timers.RemoveTimer("InvulnerabilityHandler");
}

::Left4Grief.Events.OnGameEvent_map_transition <- function (params)
{
	Left4Grief.Log(LOG_LEVEL_DEBUG, "OnGameEvent_map_transition");
	
	Left4Timers.RemoveTimer("AntiRushCheck");
	//Left4Timers.RemoveThinker("InvulnerabilityHandler");
	Left4Timers.RemoveTimer("InvulnerabilityHandler");
}

::Left4Grief.Events.OnGameEvent_vote_cast_yes <- function (params)
{
	local entityIndex = null;
	if ("entityid" in params)
		entityIndex = params["entityid"];
	
	Left4Grief.Log(LOG_LEVEL_DEBUG, "OnGameEvent_vote_cast_yes - " + entityIndex);
  
	Left4Grief.Log(LOG_LEVEL_DEBUG, "Time() = " + Time());
  
	local voteControllerEnt = Entities.FindByClassname(null, "vote_controller");
	if (!voteControllerEnt)
	{
		Left4Grief.Log(LOG_LEVEL_ERROR, "vote_controller entity not found");
		return;
	}
  
	local m_activeIssueIndex = NetProps.GetPropIntArray(voteControllerEnt, "m_activeIssueIndex", 0);
	local m_onlyTeamToVote = NetProps.GetPropIntArray(voteControllerEnt, "m_onlyTeamToVote", 0);
	local m_votesYes = NetProps.GetPropIntArray(voteControllerEnt, "m_votesYes", 0);
	local m_votesNo = NetProps.GetPropIntArray(voteControllerEnt, "m_votesNo", 0);
	local m_potentialVotes = NetProps.GetPropIntArray(voteControllerEnt, "m_potentialVotes", 0);
	
	Left4Grief.Log(LOG_LEVEL_DEBUG, "m_activeIssueIndex: " + m_activeIssueIndex + " - m_onlyTeamToVote: " + m_onlyTeamToVote + " - m_votesYes: " + m_votesYes + " - m_votesNo: " + m_votesNo + " - m_potentialVotes: " + m_potentialVotes);
	
	if (m_votesYes == 0)
	{
		// First vote cast - entityIndex is the caster
		if (!Left4Grief.OnVoteCasted(m_activeIssueIndex, entityIndex))
		{
			Left4Grief.Log(LOG_LEVEL_INFO, "Vote cast aborted");
			
			// Abort the vote
			NetProps.SetPropIntArray(voteControllerEnt, "m_votesYes", 0, 0);
			NetProps.SetPropIntArray(voteControllerEnt, "m_votesNo", m_potentialVotes, 0);
			
			Left4Grief.VoteAbortedOn = Time();
			Left4Grief.VoteKickCaster = -1;
		}
		else
		{
			Left4Grief.Log(LOG_LEVEL_INFO, "Vote cast accepted");
			
			Left4Grief.VoteAbortedOn = 0;
			if (m_activeIssueIndex == VOTE_TYPE.Kick)
				Left4Grief.VoteKickCaster = entityIndex;
			else
				Left4Grief.VoteKickCaster = -1;
		}
	}
	else
	{
		if (Left4Grief.VoteAbortedOn > 0 && (Time() - Left4Grief.VoteAbortedOn) <= VOTE_IGNORETIME_ONABORTED)
			Left4Grief.Log(LOG_LEVEL_DEBUG, "Vote ignored");
		else
		{
			Left4Grief.Log(LOG_LEVEL_INFO, "Vote accepted");
			
			Left4Grief.VoteAbortedOn = 0;
			Left4Grief.VoteKickCaster = -1;
			
			// No error, it happens when the target player is IDLE
			//if (m_activeIssueIndex == VOTE_TYPE.Kick && m_votesNo == 0)
			//	Left4Grief.Log(LOG_LEVEL_ERROR, "Something went wrong, Kick vote had no automatic NO from the target?");
		}
	}
}

::Left4Grief.Events.OnGameEvent_vote_cast_no <- function (params)
{
	local entityIndex = null;
	if ("entityid" in params)
		entityIndex = params["entityid"];
	
	Left4Grief.Log(LOG_LEVEL_DEBUG, "OnGameEvent_vote_cast_no - " + entityIndex);
	
	Left4Grief.Log(LOG_LEVEL_DEBUG, "Time() = " + Time());
	
	local voteControllerEnt = Entities.FindByClassname(null, "vote_controller");
	if (!voteControllerEnt)
	{
		Left4Grief.Log(LOG_LEVEL_ERROR, "vote_controller entity not found");
		return;
	}
  
	local m_activeIssueIndex = NetProps.GetPropIntArray(voteControllerEnt, "m_activeIssueIndex", 0);
	local m_onlyTeamToVote = NetProps.GetPropIntArray(voteControllerEnt, "m_onlyTeamToVote", 0);
	local m_votesYes = NetProps.GetPropIntArray(voteControllerEnt, "m_votesYes", 0);
	local m_votesNo = NetProps.GetPropIntArray(voteControllerEnt, "m_votesNo", 0);
	local m_potentialVotes = NetProps.GetPropIntArray(voteControllerEnt, "m_potentialVotes", 0);
	
	Left4Grief.Log(LOG_LEVEL_DEBUG, "m_activeIssueIndex: " + m_activeIssueIndex + " - m_onlyTeamToVote: " + m_onlyTeamToVote + " - m_votesYes: " + m_votesYes + " - m_votesNo: " + m_votesNo + " - m_potentialVotes: " + m_potentialVotes);
	
	if (Left4Grief.VoteAbortedOn > 0 && (Time() - Left4Grief.VoteAbortedOn) <= VOTE_IGNORETIME_ONABORTED)
		Left4Grief.Log(LOG_LEVEL_DEBUG, "Vote ignored");
	else
	{
		local casterIndex = Left4Grief.VoteKickCaster;
		Left4Grief.VoteAbortedOn = 0;
		Left4Grief.VoteKickCaster = -1;
		
		Left4Grief.Log(LOG_LEVEL_INFO, "Vote accepted");
		
		if (m_activeIssueIndex == VOTE_TYPE.Kick && m_votesYes == 1 && m_votesNo == 0)
		{
			// Automatic NO from the target of a vote kick - entityIndex is the target
			if (casterIndex < 0)
				Left4Grief.Log(LOG_LEVEL_ERROR, "Something went wrong, Kick vote had no caster after the automatic NO from the target?");
			else
			{
				if (!Left4Grief.OnKickVote(casterIndex, entityIndex))
				{
					// Abort the vote
					NetProps.SetPropIntArray(voteControllerEnt, "m_votesYes", 0, 0);
					NetProps.SetPropIntArray(voteControllerEnt, "m_votesNo", m_potentialVotes, 0);
			
					Left4Grief.VoteAbortedOn = Time();
				}
			}
		}
		else
		{
			// Abort the vote if an admin voted NO
			//local target = g_MapScript.GetPlayerFromUserID(entityIndex);
			local target = g_MapScript.PlayerInstanceFromIndex(entityIndex);
			if (Left4Grief.Settings.admin_can_abort_vote != 0 && Left4Users.GetOnlineUserLevel(target.GetPlayerUserId()) >= L4U_LEVEL.Admin)
			{
				// Abort the vote
				NetProps.SetPropIntArray(voteControllerEnt, "m_votesYes", 1, 0);
				NetProps.SetPropIntArray(voteControllerEnt, "m_votesNo", m_potentialVotes - 1, 0);
			
				Left4Grief.VoteAbortedOn = Time();
				Left4Grief.VoteKickCaster = -1;
			}
		}
	}
}

// TODO: remove
::Left4Grief.Events.OnGameEvent_vote_started <- function (params)
{
	Left4Grief.Log(LOG_LEVEL_DEBUG, "OnGameEvent_vote_started");
}

::Left4Grief.Events.OnGameEvent_vote_changed <- function (params)
{
	Left4Grief.Log(LOG_LEVEL_DEBUG, "OnGameEvent_vote_changed");
	
	local yesVotes = params["yesVotes"];
	local noVotes = params["noVotes"];
	local potentialVotes = params["potentialVotes"];
	
	Left4Grief.Log(LOG_LEVEL_DEBUG, "yesVotes: " + yesVotes + " - noVotes: " + noVotes + " - potentialVotes: " + potentialVotes);
}

::Left4Grief.Events.OnGameEvent_vote_passed <- function (params)
{
	Left4Grief.Log(LOG_LEVEL_DEBUG, "OnGameEvent_vote_passed");
}

::Left4Grief.Events.OnGameEvent_vote_failed <- function (params)
{
	Left4Grief.Log(LOG_LEVEL_DEBUG, "OnGameEvent_vote_failed");
}
//

::Left4Grief.Events.OnGameEvent_heal_begin <- function (params)
{
	local healer = g_MapScript.GetPlayerFromUserID(params["userid"]);
	local healee = g_MapScript.GetPlayerFromUserID(params["subject"]);

	Left4Grief.Log(LOG_LEVEL_DEBUG, "OnGameEvent_heal_begin - healer: " + healer.GetPlayerName() + " - healee: " + healee.GetPlayerName());
	
	Left4Grief.OnHealStart(healee, healer, params);
}

::Left4Grief.Events.OnGameEvent_lunge_pounce <- function (params)
{
	if (!("victim" in params))
		return;

	local victim = g_MapScript.GetPlayerFromUserID(params["victim"]);
	if (victim && victim.IsValid())
		Left4Grief.PinStart(victim);
}

::Left4Grief.Events.OnGameEvent_pounce_end <- function (params)
{
	if (!("victim" in params))
		return;
	
	local victim = g_MapScript.GetPlayerFromUserID(params["victim"]);
	if (victim && victim.IsValid())
		Left4Grief.PinStop(victim);
}

::Left4Grief.Events.OnGameEvent_tongue_grab <- function (params)
{
	if (!("victim" in params))
		return;
	
	local victim = g_MapScript.GetPlayerFromUserID(params["victim"]);
	if (victim && victim.IsValid())
		Left4Grief.PinStart(victim);
}

::Left4Grief.Events.OnGameEvent_tongue_release <- function (params)
{
	if (!("victim" in params))
		return;
	
	local victim = g_MapScript.GetPlayerFromUserID(params["victim"]);
	if (victim && victim.IsValid())
		Left4Grief.PinStop(victim);
}

::Left4Grief.Events.OnGameEvent_jockey_ride <- function (params)
{
	if (!("victim" in params))
		return;
	
	local victim = g_MapScript.GetPlayerFromUserID(params["victim"]);
	if (victim && victim.IsValid())
		Left4Grief.PinStart(victim);
}

::Left4Grief.Events.OnGameEvent_jockey_ride_end <- function (params)
{
	if (!("victim" in params))
		return;
	
	local victim = g_MapScript.GetPlayerFromUserID(params["victim"]);
	if (victim && victim.IsValid())
		Left4Grief.PinStop(victim);
}

::Left4Grief.Events.OnGameEvent_charger_carry_start <- function (params)
{
	if (!("victim" in params))
		return;
	
	local victim = g_MapScript.GetPlayerFromUserID(params["victim"]);
	if (victim && victim.IsValid())
		Left4Grief.PinStart(victim);
}

::Left4Grief.Events.OnGameEvent_charger_carry_end <- function (params)
{
	if (!("victim" in params))
		return;
	
	local victim = g_MapScript.GetPlayerFromUserID(params["victim"]);
	if (victim && victim.IsValid())
		Left4Grief.PinStop(victim);
}

::Left4Grief.Events.OnGameEvent_charger_pummel_start <- function (params)
{
	if (!("victim" in params))
		return;
	
	local victim = g_MapScript.GetPlayerFromUserID(params["victim"]);
	if (victim && victim.IsValid())
		Left4Grief.PinStart(victim);
}

::Left4Grief.Events.OnGameEvent_charger_pummel_end <- function (params)
{
	if (!("victim" in params))
		return;
	
	local victim = g_MapScript.GetPlayerFromUserID(params["victim"]);
	if (victim && victim.IsValid())
		Left4Grief.PinStop(victim);
}

::Left4Grief.Events.OnGameEvent_triggered_car_alarm <- function (params)
{
	if (!("userid" in params))
		return;
	
	local player = g_MapScript.GetPlayerFromUserID(params["userid"]);
	if (player && player.IsValid())
		Left4Grief.TriggeredCarAlarm(player);
}

::Left4Grief.Events.OnGameEvent_create_panic_event <- function (params)
{
	if (!("userid" in params))
		return;
	
	local player = g_MapScript.GetPlayerFromUserID(params["userid"]);
	if (player && player.IsValid())
		Left4Grief.CreatePanicEvent(player);
}

__CollectEventCallbacks(::Left4Grief.Events, "OnGameEvent_", "GameEventCallbacks", RegisterScriptGameEventListener);
