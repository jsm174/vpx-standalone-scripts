' ****************************************************************
'                       VISUAL PINBALL X
'                		Punchout
'                       Version 1.0.0
'						started 2024 
' ****************************************************************

'DOF Config by Outhere
'101 Left Flipper
'102 Right Flipper
'103 Left Slingshot
'104 Right Slingshot
'105 
'106 
'107 
'108 
'109 Bumper Center
'110 
'111 AutoPlunger
'112 
'113 RightScoop
'114 LeftScoop
'115 Resetdrop Left
'116 Resetdrop Right
'117 TopScoop
'118 Youreupagain
'119 Spinner001
'120 Spinner002
'121 
'122 Knocker
'123 Ball Release
'124 
'125 
'126 
'127 
'128 
'129 
'130 Beacon = Left, Right and Top Scoops
'131 
'132 
'133 
'134 
'135 
'136 
'137 
'138 
'139 
'140 
'141 
'142 
'143 
'144 
'145 
'146 
'147 
'148 
'149 
'150 
'151 
'152 
'153 
'154 
'155 
'*******

Option Explicit
Randomize

Const BallSize = 50    ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1
Const SongVolume = 0.1 ' 1 is full volume. Value is from 0 to 1


' Load the core.vbs for supporting Subs and functions

On Error Resume Next
ExecuteGlobal GetTextFile("core.vbs")
If Err Then MsgBox "Can't open core.vbs"
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow, 1 = enable dynamic ball shadow

'****************************************************************
'	VR Stuff
'****************************************************************
'///////////////////////---- VR Room ----////////////////////////
Dim VRRoomChoice : VRRoomChoice = 3					'0 - VR Room Off, 1 - CabOnly, 2 - Minimal Room, 3 - BoxingRing
Dim VRTest : VRTest = False

'//////////////F12 Menu//////////////
Dim dspTriggered : dspTriggered = False
Sub Table1_OptionEvent(ByVal eventId)
	If eventId = 1 And Not dspTriggered Then dspTriggered = True : DisableStaticPreRendering = True : End If

    ' VRRoom
	VRRoomChoice = Table1.Option("VR Room", 0, 3, 1, 3, 0, Array("Off", "CabOnly", "Minimal", "BoxingRing"))
	LoadVRRoom

	If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If
End Sub


' Define any Constants
Const cGameName = "punchout"
Const TableName = "punchout"
Const myVersion = "1.0.0"
Const MaxPlayers = 4     ' from 1 to 4
Const BallSaverTime = 20 ' in seconds
Const MaxMultiplier = 3  ' limit to 3x in this game, both bonus multiplier and playfield multiplier
Const BallsPerGame = 3   ' usually 3 or 5
Const MaxMultiballs = 4  ' max number of balls during multiballs

' Use FlexDMD if in FS mode
Dim UseFlexDMD
If Table1.ShowDT = True then
    UseFlexDMD = False
Else
    UseFlexDMD = True
End If

Sub startB2S(aB2S) 
If B2SOn Then 
Controller.B2SSetData 1,0
Controller.B2SSetData 2,0
Controller.B2SSetData 3,0
Controller.B2SSetData 4,0
Controller.B2SSetData 5,0
Controller.B2SSetData 6,0
Controller.B2SSetData 7,0
Controller.B2SSetData 8,0
Controller.B2SSetData 9,0
Controller.B2SSetData 10,0
Controller.B2SSetData 11,0
Controller.B2SSetData 12,0
Controller.B2SSetData 13,0
Controller.B2SSetData 14,0
Controller.B2SSetData 15,0
Controller.B2SSetData 16,0
Controller.B2SSetData 17,0
Controller.B2SSetData 18,0
Controller.B2SSetData 19,0
Controller.B2SSetData 20,0
Controller.B2SSetData 21,0
Controller.B2SSetData 22,0
Controller.B2SSetData 23,0
Controller.B2SSetData 24,0
Controller.B2SSetData 25,0
Controller.B2SSetData 26,0
Controller.B2SSetData 27,0
Controller.B2SSetData 28,0
Controller.B2SSetData 29,0
Controller.B2SSetData 30,0
Controller.B2SSetData aB2S,1
End If
End Sub

' Define Global Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BonusPoints(4)
Dim BonusMultiplier(4)
Dim bBonusHeld
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Special1Awarded(4)
Dim Special2Awarded(4)
Dim Special3Awarded(4)
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim bAttractMode
Dim mBalls2Eject
Dim bAutoPlunger
Dim RandomModeState
Dim Training1
Dim Training2
Dim TrainingLevel
Dim Gtready
Dim GtreadyCount
dim Tleftcount
Dim Trightcount
dim TrainingLevelLock
Dim Status
dim timesMup
dim BonusFlippersActive
dim PFMultiplier
dim countr5
dim countr8
dim countr10
dim countr11
dim countr12
dim countr13
dim countr14
dim countr15
dim countr16
dim countr17
dim countr18
dim countr19
dim countr20
dim countr21
dim countr22
dim countr23
dim countr30
dim countr31
dim countr32
dim countr33
dim countr34
dim countr35
dim countr36
dim countr37
dim countr38
dim countr39
dim countr40
dim intraining
dim fightynight
dim prehit
dim Pgutpunch
dim Pstar
dim Pknockout
dim flashpic
dim hitdipointy
dim goforit
dim nextfght
dim youwonthegamy
dim fightswong
dim wizzscore
dim checkballyleft
dim onetimy

' Define Game Control Variables
Dim BallsOnPlayfield
Dim BallsInLock
Dim BallsInHole

' Define Game Flags
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim bBallSaverReady
Dim bMultiBallMode
'Dim Multiball
Dim bMusicOn
Dim bJustStarted
Dim bJackpot
Dim plungerIM
Dim LastSwitchHit

' core.vbs variables

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
	Dim i
	'Randomize

'reset HighScore
'Reseths

'Impulse Plunger as autoplunger
    Const IMPowerSetting = 36 ' Plunger Power
    Const IMTime = 1.1        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
        .CreateEvents "plungerIM"
    End With

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    ' load saved values, highscore, names, jackpot
    Loadhs

    'Init main variables
    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 0
        BonusMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
    Next

	' Turn off the bumper lights
	FlBumperFadeTarget(1) = 0

    ' Initalise the DMD display
    DMD_Init

    ' freeplay or coins
    bFreePlay = False 'we want coins

    'if bFreePlay = false Then DOF 125, DOFOn

    ' Init main variables and any other flags
    bAttractMode = False
    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bBallSaverActive = False
    bBallSaverReady = False
    bGameInPlay = False
    bMusicOn = true
    BallsOnPlayfield = 0
	Training1 = 0
	Training2 = 0 
	TrainingLevel = 0
	Gtready = 0
	GtreadyCount = 0
	Tleftcount = 0
	Trightcount = 0
	TrainingLevelLock = 0
	Status = "Normal"
	timesMup = 3
	BonusFlippersActive = False
	PFMultiplier = 1
	intraining = 0
	fightynight = 0
	prehit = 0
	Pgutpunch = 0
	Pstar = 0
    Pknockout = 0
	flashpic = 0
	hitdipointy = 0
    goforit = 0
    nextfght = 0
	countr10 = 0
	countr11 = 0
	countr12 = 0
	countr13 = 0
	countr14 = 0
	countr15 = 0
	countr16 = 0
	countr17 = 0
	countr18 = 0
	countr19 = 0
	countr20 = 0
	countr21 = 0
	countr22 = 0
	countr23 = 0
	countr30 = 0
	countr31 = 0
	countr32 = 0
	countr33 = 0
	countr34 = 0
	countr35 = 0
	countr36 = 0
	countr37 = 0
	countr38 = 0
	countr39 = 0
	countr40 = 0
	youwonthegamy = 0
	fightswong = 0
	wizzscore = 0
	checkballyleft = 3
	onetimy = 0

	bMultiBallMode = False
	RandomModeState = False
	'Multiball=false
	bAutoPlunger = False
    BallsInLock = 0
    BallsInHole = 0
	LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bJustStarted = True
    ' set any lights for the attract mode
    GiOff
    StartAttractMode
	'EndOfGame()
End Sub

'****************************************
' Real Time updatess using the GameTimer
'****************************************
'used for all the real time updates

Sub GameTimer_Timer
    RollingUpdate
    ' add any other real time update subs, like gates or diverters
    FlipperLSh.Rotz = LeftFlipper.CurrentAngle
    FlipperRSh.Rotz = RightFlipper.CurrentAngle
	LF1Logo.RotZ = LeftFlipper.CurrentAngle
	RF1logo.RotZ = RightFlipper.CurrentAngle
End Sub

Sub TimerPlunger_Timer

  If VR_PlungerGlove.Y < 2119.804 then
      VR_PlungerGlove.Y = VR_PlungerGlove.Y + 5
  End If
  If VR_PlungerStem.Y < 2103.142 then
      VR_PlungerStem.Y = VR_PlungerStem.Y + 5
  End If
End Sub

Sub TimerPlunger2_Timer
    VR_PlungerGlove.Y = 2019.804 + (5* Plunger.Position) -20
    VR_PlungerStem.Y = 2003.142 + (5* Plunger.Position) -20
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)
    If Keycode = AddCreditKey Then
        Credits = Credits + 1
        if bFreePlay = False Then
            DOF 125, DOFOn
            If(Tilted = False) Then
                DMDFlush
                DMD "_", CL(1, "CREDITS: " & Credits), "", eNone, eNone, eNone, 500, True, "fx_coin2"
            End If
        End If
    End If

    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
        PlaySoundAt "fx_reload", plunger
    End If

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

	If keycode = RightFlipperKey Then 
		VR_FlipperRight.x = VR_FlipperRight.x - 10
	End if
	If keycode = LeftFlipperKey Then 
		VR_FlipperLeft.x = VR_FlipperLeft.x + 10
	End If

	If keycode = StartGameKey Then 
		VR_Cab_StartButton.y = VR_Cab_StartButton.y - 2
	End if

	If keycode = PlungerKey Then
		TimerPlunger.Enabled = True
		TimerPlunger2.Enabled = False
	End If

    ' Normal flipper action

    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
        If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1

        If keycode = StartGameKey Then
            If((PlayersPlayingGame <MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, "so_fanfare1"
                Else
                    If(Credits> 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, "so_fanfare1"
                        If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
                        Else
                            ' Not Enough Credits to start a game.
                            DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, "so_nocredits"
                    End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = True) Then
                    If(BallsOnPlayfield = 0) Then
                        ResetForNewGame()
                    End If
                Else
                    If(Credits> 0) Then
                        If(BallsOnPlayfield = 0) Then
                            Credits = Credits - 1
                            If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
                            ResetForNewGame()
							stopsong
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, "so_nocredits"
                    End If
                End If
            End If
    End If ' If (GameInPlay)
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If keycode = PlungerKey Then
        Plunger.Fire
        PlaySoundAt "fx_plunger", plunger
        If bBallInPlungerLane Then PlaySoundAt "fx_fire", plunger
    End If

    If hsbModeActive Then
        Exit Sub
    End If

	If keycode = RightFlipperKey Then 
		VR_FlipperRight.x = VR_FlipperRight.x + 10
	End if
	If keycode = LeftFlipperKey Then 
		VR_FlipperLeft.x = VR_FlipperLeft.x - 10
	End If
	
	If keycode = StartGameKey Then 
		VR_Cab_StartButton.y = VR_Cab_StartButton.y + 2
	End if

	If keycode = PlungerKey Then
        TimerPlunger.Enabled = False
        TimerPlunger2.Enabled = True
        VR_PlungerGlove.Y = 2019.804
        VR_PlungerStem.Y = 2003.142
    end if

    ' Table specific

    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then
            SolLFlipper 0
        End If
        If keycode = RightFlipperKey Then
            SolRFlipper 0
        End If
    End If

    If BonusFlippersActive Then
		If keycode = RightFlipperkey then
		    'PlaySound "fx_flipperup", 0, 1, 0, 0.25
            'RightFlipper.RotateTostart
            SolRFlipper 1
			pushup = pushup + 1
		end If
			If keycode = LeftFlipperkey then
		    'PlaySound "fx_flipperup", 0, 1, 0, 0.25
            'LeftFlipper.RotateTostart
            SolLFlipper 1
			pushup = pushup + 1
		end If
		Else
        'If keycode = LeftFlipperKey Then SolLFlipper 1
        'If keycode = RightFlipperKey Then SolRFlipper 1
    End If

End Sub

'*************
' Pause Table
'*************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub Table1_Exit
    Savehs
    If B2SOn = true Then Controller.Stop
End Sub

'********************
'     Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
		If BonusFlippersActive Then
		    PlaySound "fx_flipperup", 0, .50, 0, 0.25
            LeftFlipper.RotateTostart
			pushup = pushup + 1
		Else
			PlaySoundAt SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
			LeftFlipper.RotateToEnd
'			Flipper1.RotateToEnd 'Adds To End Movement for Flipper1
			RotateLaneLightsLeft
			'RotateLaneLightsLeft2
		End If
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
'		Flipper1.RotateToStart 'Adds To End Movement for Flipper1
    End If
End Sub




Sub SolRFlipper(Enabled)
    If Enabled Then
		If BonusFlippersActive Then
		    PlaySound "fx_flipperup", 0, .50, 0, 0.25
            RightFlipper.RotateTostart
			pushup = pushup + 1
		Else
			PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
			RightFlipper.RotateToEnd
			RotateLaneLightsRight
			'RotateLaneLightsRight2
		End If
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
    End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub


Sub RotateLaneLightsLeft
    Dim TempState
    TempState = LeftOutlane.State
    LeftOutlane.State = LeftInlane.State
    LeftInlane.State = RightInlane.State
    RightInlane.State = RightOutlane.State
    RightOutlane.State = TempState
End Sub

Sub RotateLaneLightsRight
    Dim TempState
    TempState = RightOutlane.State
    RightOutlane.State = RightInlane.State
    RightInlane.State = LeftInlane.State
    LeftInlane.State = LeftOutlane.State
    LeftOutlane.State = TempState
End Sub

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                    'Called when table is nudged
    Tilt = Tilt + TiltSensitivity                'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt> TiltSensitivity) AND(Tilt <15) Then 'show a warning
        DMD "_", CL(1, "CAREFUL!"), "", eNone, eBlinkFast, eNone, 500, True, ""
    End if
    If Tilt> 15 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
        DMDFlush
        DMD "", "", "TILT", eNone, eNone, eBlink, 200, False, ""
		startB2S(15)
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt> 0 Then
        Tilt = Tilt - 0.1
    Else
        TiltDecreaseTimer.Enabled = False
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
        'turn off GI and turn off all the lights
        GiOff
        LightSeqTilt.Play SeqAllOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        'clean up the buffer display
        DMDFlush
    End If
End Sub

' GI light sequence effects

Sub GiEffect(n)
    Select Case n
        Case 0 'all blink
            LightSeqGi.UpdateInterval = 8
            LightSeqGi.Play SeqBlinking, , 5, 50
        Case 1 'random
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqRandom, 5, , 1000
        Case 2 'upon
            LightSeqGi.UpdateInterval = 4
            LightSeqGi.Play SeqUpOn, 5, 1
    End Select
End Sub

Sub LightEffect(n)
    Select Case n
        Case 0 'all blink
            LightSeqInserts.UpdateInterval = 8
            LightSeqInserts.Play SeqBlinking, , 5, 50
        Case 1 'random
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqRandom, 5, , 1000
        Case 2 'upon
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 10, 1
        Case 3 ' left-right-left
            LightSeqInserts.UpdateInterval = 5
            LightSeqInserts.Play SeqLeftOn, 10, 1
            LightSeqInserts.UpdateInterval = 5
            LightSeqInserts.Play SeqRightOn, 10, 1
    End Select
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield = 0) Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub

'********************
' Music as wav sounds
'********************
Dim Song
Song = ""

Sub PlaySong(name)
    If bMusicOn Then
        If Song <> name Then
            StopSound Song
            Song = name
            PlaySound Song, -1, SongVolume
        End If
    End If
End Sub

Sub StopSong
    If bMusicOn Then
        StopSound Song
        Song = ""
    End If
End Sub

sub playsong1
PlaySong "1"
end Sub

sub playsong2
PlaySong "Match"
end Sub

sub playsong3
PlaySong "2"
end Sub

sub playsong4
PlaySong "3"
end Sub

sub playsongEnd
PlaySong "M_end"
end Sub


Sub Pin001_hit()
Playsound "Rubber_4"
end sub

Sub Pin002_hit()
Playsound "Rubber_4"
end sub

Sub Pin3_hit()
Playsound "Rubber_4"
end sub

Sub Pin4_hit()
Playsound "Rubber_4"
end sub


'********************
' Play random quotes
'********************

Sub PlayQuote
    Dim tmp
    tmp = INT(RND * 123) + 1
    PlaySound "HIT_" &tmp
End Sub

'**********************
'     GI effects
' independent routine
' it turns on the gi
' when there is a ball
' in play
'**********************

Dim OldGiState
OldGiState = -1   'start witht the Gi off

Sub ChangeGi(col) 'changes the gi color
    Dim bulb
    For each bulb in aGILights
        SetLightColor bulb, col, -1
    Next
End Sub

Sub GIUpdateTimer_Timer
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = 1 Then 'we have 2 captive balls on the table (-1 means no balls, 0 is the first ball, 1 is the second..)
            GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    DOF 127, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
    For each bulb in aBumperLights
        bulb.State = 1
    Next
' table1.ColorGradeImage = "ColorGradeLUT256x16_HalfSat"
End Sub

Sub GiOff
    DOF 127, DOFOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
    For each bulb in aBumperLights
        bulb.State = 0
    Next
' table1.ColorGradeImage = "ColorGradeLUT256x16_HalfSat-dark"
End Sub

' GI, light & flashers sequence effects

Sub GiEffect(n)
    Dim ii
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 15, 10
        Case 2 'random
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 10, 10
        Case 4 'all blink once
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 4, 1
    End Select
End Sub

Sub LightEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqInserts.Play SeqAlloff
        Case 1 'all blink
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqBlinking, , 15, 10
        Case 2 'random
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqBlinking, , 10, 10
        Case 4 'up 1 time
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 8, 1
        Case 5 'up 2 times
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 8, 2
        Case 6 'down 1 time
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqDownOn, 8, 1
        Case 7 'down 2 times
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqDownOn, 8, 2
    End Select
End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / Table1.height-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.06, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'********************************************
'   JP's VP10 Rolling Sounds
'********************************************

Const tnob = 11 ' total number of balls
Const lob = 0   'number of locked balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch, ballvol
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball
    For b = lob to UBound(BOT)

        If BallVel(BOT(b) )> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b) )
                ballvol = Vol(BOT(b) )
            Else
                ballpitch = Pitch(BOT(b) ) + 25000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b) ) * 10
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b) ), 0, ballpitch, 1, 0, AudioFade(BOT(b) )
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ <-1 and BOT(b).z <55 and BOT(b).z> 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound "fx_collide", 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'******************************
' Diverse Collection Hit Sounds
'******************************

Sub RHelp1_Hit()
    StopSound "fx_metalrolling"
    PlaySoundAtBall "fx_ballrampdrop"
End Sub

Sub RHelp2_Hit()
    StopSound "fx_metalrolling"
    PlaySoundAtBall"fx_ballrampdrop"
End Sub


'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

'****** INSTRUCTIONS please read ******

' The "DynamicBSUpdate" sub should be called with an interval of -1 (framerate)
' Place a toggleable variable (DynamicBallShadowsOn) in user options at the top of the script
' Import the "bsrtx7" and "ballshadow" images
' Import the shadow materials file (3 sets included) (you can also export the 3 sets from this table to create the same file)
' Copy in the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#, with at least as many objects each as there can be balls
'
' Create a collection called DynamicSources that includes all light sources you want to cast ball shadows
'***These must be organized in order, so that lights that intersect on the table are adjacent in the collection***
' This is because the code will only project two shadows if they are coming from lights that are consecutive in the collection
' The easiest way to keep track of this is to start with the group on the left slingshot and move clockwise around the table
'	For example, if you use 6 lights: A & B on the left slingshot and C & D on the right, with E near A&B and F next to C&D, your collection would look like EBACDF
'
'																E
'	A		 C													B
'	 B		D			your collection should look like		A		because E&B, B&A, etc. intersect; but B&D or E&F do not
'  E		  F													C
'																D
'																F
'
'Update shadow options in the code to fit your table and preference

'****** End Instructions ******

' *** Example timer sub

' The frame timer interval is -1, so executes at the display frame rate
Sub FrameTimer_Timer()
	If DynamicBallShadowsOn=1 Then DynamicBSUpdate 'update ball shadows
End Sub

' *** These are usually defined elsewhere (ballrolling), but activate here if necessary

'Const tnob = 10 ' total number of balls
'Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done

' *** Example "Top of Script" User Option
'Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow, 1 = enable dynamic ball shadow

' *** Shadow Options ***
Const fovY					= -2	'Offset y position under ball to account for layback or inclination (more pronounced need further back, -2 seems best for alignment at slings)
Const DynamicBSFactor 		= 0.99	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
Const Wideness				= 20	'Sets how wide the shadows can get (20 +5 thinness should be most realistic)
Const Thinness				= 5		'Sets minimum as ball moves away from source
' ***				 ***

Dim sourcenames, currentShadowCount

sourcenames = Array ("","","","","","","","","","","","")
currentShadowCount = Array (0,0,0,0,0,0,0,0,0,0,0,0)


dim objrtx1(20), objrtx2(20)
dim objBallShadow(20)
DynamicBSInit

sub DynamicBSInit()
	Dim iii

	for iii = 0 to tnob									'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = iii/1000 + 0.01
		objrtx1(iii).visible = 0
		'objrtx1(iii).uservalue=0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = (iii)/1000 + 0.02
		objrtx2(iii).visible = 0
		'objrtx2(iii).uservalue=0
		currentShadowCount(iii) = 0
		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		objBallShadow(iii).Z = iii/1000 + 0.04
	Next
end sub


Sub DynamicBSUpdate
	Dim falloff:	falloff = 150			'Max distance to light sources, can be changed if you have a reason
	Const AmbientShadowOn = 1				'Toggle for just the moving shadow primitive (ninuzzu's)
	Dim ShadowOpacity, ShadowOpacity2 
	Dim s, Source, LSd, b, currentMat, AnotherSource, BOT
	BOT = GetBalls

	'Hide shadow of deleted balls
	For s = UBound(BOT) + 1 to tnob
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
	Next

	If UBound(BOT) = lob - 1 Then Exit Sub		'No balls in play, exit

'The Magic happens here
	For s = lob to UBound(BOT)

' *** Normal "ambient light" ball shadow
		If AmbientShadowOn = 1 Then
			If BOT(s).X < tablewidth/2 Then
				objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/13)) + 5
			Else
				objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/13)) - 5
			End If
			objBallShadow(s).Y = BOT(s).Y + fovY

			If BOT(s).Z < 30 Then 'or BOT(s).Z > 105 Then		'Defining when (height-wise) you want ambient shadows
				objBallShadow(s).visible = 1
	'			objBallShadow(s).Z = BOT(s).Z - 25 + s/1000 + 0.04		'Uncomment if you want to add shadows to an upper/lower pf
			Else
				objBallShadow(s).visible = 0
			end if
		End If
' *** Dynamic shadows
		For Each Source in DynamicSources
			LSd=DistanceFast((BOT(s).x-Source.x),(BOT(s).y-Source.y))	'Calculating the Linear distance to the Source
			If BOT(s).Z < 30 Then 'Or BOT(s).Z > 105 Then				'Defining when (height-wise) you want dynamic shadows
				If LSd < falloff and Source.state=1 Then	    		'If the ball is within the falloff range of a light and light is on
					currentShadowCount(s) = currentShadowCount(s) + 1	'Within range of 1 or 2
					if currentShadowCount(s) = 1 Then					'1 dynamic shadow source
						sourcenames(s) = source.name
						currentMat = objrtx1(s).material
						objrtx2(s).visible = 0 : objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01							'Uncomment if you want to add shadows to an upper/lower pf
						objrtx1(s).rotz = AnglePP(Source.x, Source.y, BOT(s).X, BOT(s).Y) + 90
						ShadowOpacity = (falloff-LSd)/falloff									'Sets opacity/darkness of shadow by distance to light
						objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness						'Scales shape of shadow with distance/opacity
						UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^2,RGB(0,0,0),0,0,False,True,0,0,0,0
						'debug.print "update1" & source.name & " at:" & ShadowOpacity

						currentMat = objBallShadow(s).material
						UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-ShadowOpacity),RGB(0,0,0),0,0,False,True,0,0,0,0

					Elseif currentShadowCount(s) = 2 Then
																'Same logic as 1 shadow, but twice
						currentMat = objrtx1(s).material
						set AnotherSource = Eval(sourcenames(s))
						objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01							'Uncomment if you want to add shadows to an upper/lower pf
						objrtx1(s).rotz = AnglePP(AnotherSource.x, AnotherSource.y, BOT(s).X, BOT(s).Y) + 90
						ShadowOpacity = (falloff-(((BOT(s).x-AnotherSource.x)^2+(BOT(s).y-AnotherSource.y)^2)^0.5))/falloff
						objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness
						UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0

						currentMat = objrtx2(s).material
						objrtx2(s).visible = 1 : objrtx2(s).X = BOT(s).X : objrtx2(s).Y = BOT(s).Y + fovY
'						objrtx2(s).Z = BOT(s).Z - 25 + s/1000 + 0.02							'Uncomment if you want to add shadows to an upper/lower pf
						objrtx2(s).rotz = AnglePP(Source.x, Source.y, BOT(s).X, BOT(s).Y) + 90
						ShadowOpacity2 = (falloff-LSd)/falloff
						objrtx2(s).size_y = Wideness*ShadowOpacity2+Thinness
						UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
						'debug.print "update2: " & source.name & " at:" & ShadowOpacity & " and "  & Eval(sourcenames(s)).name & " at:" & ShadowOpacity2

						currentMat = objBallShadow(s).material
						UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
					end if
				Else
					currentShadowCount(s) = 0
				End If
			Else									'Hide dynamic shadows everywhere else
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
			End If
		Next
	Next
End Sub


Function DistanceFast(x, y)
	dim ratio, ax, ay
	'Get absolute value of each vector
	ax = abs(x)
	ay = abs(y)
	'Create a ratio
	ratio = 1 / max(ax, ay)
	ratio = ratio * (1.29289 - (ax + ay) * ratio * 0.29289)
	if ratio > 0 then
		DistanceFast = 1/ratio
	Else
		DistanceFast = 0
	End if
end Function

Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function
							'Enable these functions if they are not already present elswhere in your table
Dim PI: PI = 4*Atn(1)

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then 
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		end if
	ElseIf dx = 0 Then
		if dy = 0 Then
			Atn2 = 0
		else
			Atn2 = Sgn(dy) * pi / 2
		end if
	End If
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************


' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i

    bGameInPLay = True

    'resets the score display, and turn off attract mode
    StopAttractMode
    GiOn

    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = True
	'Multiball=false	
    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 0
		'BonusHeldPoints(i) = 0
        BonusMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
        Special1Awarded(i) = False
        Special2Awarded(i) = False
        Special3Awarded(i) = False
    Next

    ' initialise any other flags
    Tilt = 0

	'reset variables
	forballlgameover
	resetafterfight
	nextfght = 0
	BonusFlippersActive = False
	PFMultiplier = 1
	intraining = 0
	youwonthegamy = 0
	fightswong = 0
	wizzscore = 0
	checkballyleft = 3
	onetimy = 0
	Tleftcount = 0
	Trightcount = 0
	TrainingLevel = 0
	Gtready = 0
	resettLeft
	resettRight
	Lup012.enabled = False

    ' initialise Game variables
    Game_Init()
	
    ' you may wish to start some music, play a sound, do whatever at this point
StopSong
PlaySound "TRAININGSLEVEL"
LevelT0
    vpmtimer.addtimer 6000, "playsong1 '"
    vpmtimer.addtimer 6000, "FirstBall '"
End Sub


' This is used to delay the start of a game to allow any attract sequence to

' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
    ' reset the table for a new ball
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
    CreateNewBall()
	Lup013.enabled = true
	Lup014.enabled = true
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()
    ' make sure the correct display is upto date
    AddScore 0
	timesMup = 3
LeftTargetLight.state = 0
RightTargetLight.state = 0
m2x.state = 0
m3x.state = 0
m4x.state = 0
m5x.state = 0
PFMultiplier = 1
multipoints = 0
GetUpRight.state = 1
GetUpLeft.state = 1
checkwichflashernow


if checkballyleft = 3 then
	pupflasher1.imageA = "Fball1" 
end if
if checkballyleft = 2 then
	pupflasher1.imageA = "Fball2" 
end if
if checkballyleft = 1 then
	pupflasher1.imageA = "Fball3" 
end if

    ' set the current players bonus multiplier back down to 1X
    BonusMultiplier(CurrentPlayer) = 1
    'UpdateBonusXLights
	
' reset any drop targets, lights, game Mode etc..
    
   'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'Reset any table specific
	BumperBonus = 0
	HoleBonus = 0
	ALLRampBonus = 0
	RampBonus1 = 0
	RampBonus2 = 0
	RampBonus3 = 0
	MulitballBonus = 0
    ResetNewBallVariables
    ResetNewBallLights()
	'Multiball=false	
End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
    
	LightSeqAttract.StopPlay

	' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
    PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
    BallRelease.Kick 90, 4

	'only this tableDrain / Plunger Functions
	'ChangeBallImage

    If BallsOnPlayfield> 1 Then
        bMultiBallMode = True
        bAutoPlunger = True
    End If

End Sub


' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table

Sub AddMultiball(nballs)
    mBalls2Eject = mBalls2Eject + nballs
    CreateMultiballTimer.Enabled = True
    'and eject the first ball
    CreateMultiballTimer_Timer
End Sub

' Eject the ball after the delay, AddMultiballDelay
Sub CreateMultiballTimer_Timer()
    ' wait if there is a ball in the plunger lane
    If bBallInPlungerLane Then
        Exit Sub
    Else
        If BallsOnPlayfield < MaxMultiballs Then
            CreateNewBall()
            mBalls2Eject = mBalls2Eject -1
            If mBalls2Eject = 0 Then 'if there are no more balls to eject then stop the timer
                CreateMultiballTimer.Enabled = False
            End If
        Else 'the max number of multiballs is reached, so stop the timer
            mBalls2Eject = 0
            CreateMultiballTimer.Enabled = False
        End If
    End If
End Sub


' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded

Sub EndOfBall()
	Dim BonusDelayTime
	' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)

LightSeqTrophy.Play SeqRandom,30,,2000
	'LightSeqAttract.Play SeqBlinking, , 5, 150
startB2S(2)
'add dof here
pupflasher1.imageA = "Fballlost" 
DMD "", "", "balllost", eNone, eNone, eNone, 3000, True, ""
StopSong
'bonuscheckie
playsound "BALLYLOST"
wizzscore = 0
onetimy = 0

    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 0

    'If NOT Tilted Then
	If(Tilted = False) Then
		
        'Number of Target hits
'       AwardPoints = TargetBonus * 2000
'       TotalBonus = TotalBonus + AwardPoints
'       DMD CL(0, FormatScore(AwardPoints)), CL(1, "TARGET BONUS " & TargetBonus), "", eBlink, eNone, eNone, 300, False, "whip" <- with dmd scores otherwise only total bonus

        AwardPoints = TargetBonus * 2000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = Scoopbonus * 5000
        TotalBonus = TotalBonus + AwardPoints

        
		DMD CL(0, FormatScore(TotalBonus) ), CL(1, "TOTAL BONUS" & BonusMultiplier(CurrentPlayer) ), "", eBlinkFast, eNone, eNone, 1000, True, "po_bonus7"
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)
        
		AddScore TotalBonus

		' add a bit of a delay to allow for the bonus points to be shown & added up
		vpmtimer.addtimer 5200, "EndOfBall2 '"
    Else 'Si hay falta simplemente espera un momento y va directo a la segunta parte después de perder la bola
		BonusDelayTime = 100
		EndOfBall2
    End If
	'vpmtimer.addtimer BonusDelayTime, "EndOfBall2 '"
End Sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the CurrentPlayer)
'
Sub EndOfBall2()
    ' if were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful if we are changing player LOL
	checkballyleft = checkballyleft - 1
	Updatethemusico
    Tilted = False
    Tilt = 0
    DisableTable False 'enable again bumpers and slingshots

    ' has the player won an extra-ball ? (might be multiple outstanding)
    If(ExtraBallsAwards(CurrentPlayer) <> 0) Then
        'debug.print "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0) Then
            LightShootAgain.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
		DMD "", "", "shootagain", eNone, eNone, eNone, 1000, True, ""

		checkballyleft = checkballyleft + 1
		Updatethemusico

        ' reset the playfield for the new ball
        ResetForNewPlayerBall()
		
		' set the dropped wall for bonus
		
        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0) Then
            'debug.print "No More Balls, High Score Entry"

            ' Submit the CurrentPlayers score to the High Score system
            CheckHighScore()
        ' you may wish to play some music at this point

        Else

            ' not the last ball (for that player)
            ' if multiple players are playing then move onto the next one
            EndOfBallComplete()
        End If
    End If
End Sub

' This function is called when the end of bonus display
' (or high score entry finished) AND it either end the game or
' move onto the next player (or the next ball of the same player)
'
Sub EndOfBallComplete()
    Dim NextPlayer

    'debug.print "EndOfBall - Complete"

    ' are there multiple players playing this game ?
    If(PlayersPlayingGame> 1) Then
        ' then move to the next player
        NextPlayer = CurrentPlayer + 1
        ' are we going from the last player back to the first
        ' (ie say from player 4 back to player 1)
        If(NextPlayer> PlayersPlayingGame) Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' is it the end of the game ? (all balls been lost for all players)
    If((BallsRemaining(CurrentPlayer) <= 0) AND(BallsRemaining(NextPlayer) <= 0) ) Then
        ' you may wish to do some sort of Point Match free game award here
        ' generally only done when not in free play mode
		StopSong
'add dof here	
if youwonthegamy = 1 then 
startB2S(14)
pupflasher1.imageA = "Fprewizardmode"
else
pupflasher1.imageA = "Fgameover"
startB2S(16)
end if
 
		forballlgameover
		DMD CL(0, "TRAININGS"), CL(1, "LEVEL "&traininglevel), "", eNone, eNone, eNone, 1000, True, ""
		DMD CL(0, "FIGHTS"), CL(1, "WON "&fightswong), "", eNone, eNone, eNone, 1000, True, ""
		DMD "", "", "gameover", eNone, eNone, eNone, 7000, True, ""
		PlaySound "GAMEOVER"
        ' set the machine into game over mode
        vpmtimer.addtimer 9100, "EndOfGame() '"

    ' you may wish to put a Game Over message on the desktop/backglass

    Else
        ' set the next player
        CurrentPlayer = NextPlayer

        ' make sure the correct display is up to date
        DMDScoreNow

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
        CreateNewBall()

        ' play a sound if more than 1 player
        If PlayersPlayingGame> 1 Then
            PlaySound "vo_player" &CurrentPlayer
            DMD "_", CL(1, "PLAYER " &CurrentPlayer), "", eNone, eNone, eNone, 800, True, ""
        End If
    End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    LightSeqAttract.StopPlay
	'debug.print "End Of Game"
    bGameInPLay = False
    ' just ended your game then play the end of game tune
    If NOT bJustStarted Then
    End If

    bJustStarted = False
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0

    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball

    ' set any lights for the attract mode
    GiOff
    StartAttractMode
' you may wish to light any Game Over Light you may have
End Sub

Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp> BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function

' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

' lost a ball ;-( check to see how many balls are on the playfield.
' if only one then decrement the remaining count AND test for End of game
' if more than 1 ball (multi-ball) then kill of the ball but don't create
' a new one
'
Sub Drain_Hit()
if bBallSaverActive = False and bMultiBallMode = False and timesMup = 3 Then
dopushup
exit sub
end if 
if bBallSaverActive = False and bMultiBallMode = False and timesMup = 2 Then
dopushup
exit sub
end if 
if bBallSaverActive = False and bMultiBallMode = False and timesMup = 1 Then
dopushup
exit sub
end if 

    ' Destroy the ball
    Drain.DestroyBall
    BallsOnPlayfield = BallsOnPlayfield - 1 
	'If BallsOnPlayfield<2 Then
	'Multiball=false
	'end if
	
    ' pretend to knock the ball into the ball storage mech
    PlaySoundAt "fx_drain", Drain
    'if Tilted then end Ball Mode
    If Tilted Then
        StopEndOfBallMode
    End If

    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True) AND(Tilted = False) Then

        ' is the ball saver active,
        If(bBallSaverActive = True) Then
	AddMultiball 1
	bAutoPlunger = True
            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
	'DMD CL(0, "BALL SAVED"), CL(1, "SHOOT AGAIN"), "", eBlink, eBlink, eNone, 800, True, ""
     DMD "", "", "shootagain", eNone, eNone, eNone, 1000, True, ""
     'vpmtimer.addtimer 1250, "CreateNewBall() '"


           ' you may wish to put something on a display or play a sound at this point

            
        Else

			If(BallsOnPlayfield = 1)Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True)then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ' you may wish to change any music over at this point and
                    ' turn off any multiball specific lights
                End If
            End If
            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0) Then

                ' End Mode and timers
				StopSong
				PlaySound ""
                ' Show the end of ball animation
                ' and continue with the end of ball
                ' DMD something?
                StopEndOfBallMode
                vpmtimer.addtimer 200, "EndOfBall '" 'the delay is depending of the animation of the end of ball, since there is no animation then move to the end of ball
            End If
        End If
    End If
End Sub



' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.

Sub Trigger1_Hit()
If bAutoPlunger Then
        'debug.print "autofire the ball"
        PlungerIM.AutoFire
        DOF 111, DOFPulse
        PlaySoundAt "fx_fire", Trigger1
        bAutoPlunger = False
    End If	
'StopSong
    DMDScoreNow
    bBallInPlungerLane = True
    'DMD "_", CL(1, "SHOOT THE BALL"), "", eNone, eBlink, eNone, 1000, True, ""
    DMD "", "", "shootball", eNone, eNone, eNone, 1000, True, ""
	If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
        EnableBallSaver BallSaverTime
        Else
        ' show the message to shoot the ball in case the player has fallen sleep
        Trigger1.TimerEnabled = 1
    End If
End Sub

' The ball is released from the plunger

Sub Trigger1_UnHit()
'add dof here
    bBallInPlungerLane = False
End Sub


Sub Trigger1_Timer
    'DMD "_", CL(1, "SHOOT THE BALL"), "", eNone, eNone, eNone, 800, True, ""
    trigger1.TimerEnabled = 0
End Sub

Sub EnableBallSaver(seconds)
    'debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' start the timer
    BallSaverTimer.Interval = 1000 * seconds
    BallSaverTimer.Enabled = True
    BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
    BallSaverSpeedUpTimer.Enabled = True
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    LightShootAgain.BlinkInterval = 160
    LightShootAgain.State = 2
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag
'
Sub BallSaverTimer_Timer()
    'debug.print "Ballsaver ended"
    BallSaverTimer.Enabled = False
    ' clear the flag
    bBallSaverActive = False
    ' if you have a ball saver light then turn it off at this point
   LightShootAgain.State = 0
End Sub

Sub BallSaverSpeedUpTimer_Timer()
    'debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    LightShootAgain.BlinkInterval = 80
    LightShootAgain.State = 2
End Sub

' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board
Sub AddScore(points)
    If Tilted Then Exit Sub

    ' add the points to the current players score variable
    Score(CurrentPlayer) = Score(CurrentPlayer) + points

    ' play a sound for each score
	PlaySound "tone"&points

End Sub

' Add bonus to the bonuspoints AND update the score board
Sub AddBonus(points) 'not used in this table, since there are many different bonus items.
    If(Tilted = False) Then
        ' add the bonus to the current players bonus variable
        BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
    End if
End Sub

Sub AwardExtraBall()
   'DMD "_", CL(1, ("EXTRA BALL WON") ), "", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
DMD "", "", "extraball", eNone, eNone, eNone, 1000, True, ""
    DOF 121, DOFPulse
    ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
    LightShootAgain.State = 1
    LightEffect 2
End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
    x = LoadValue(TableName, "HighScore1")
    If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 100000 End If
    x = LoadValue(TableName, "HighScore1Name")
    If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(TableName, "HighScore2")
    If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 100000 End If
    x = LoadValue(TableName, "HighScore2Name")
    If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(TableName, "HighScore3")
    If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 100000 End If
    x = LoadValue(TableName, "HighScore3Name")
    If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(TableName, "HighScore4")
    If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 100000 End If
    x = LoadValue(TableName, "HighScore4Name")
    If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(TableName, "Credits")
    If(x <> "") then Credits = CInt(x) Else Credits = 0:If bFreePlay = False Then DOF 125, DOFOff:End If
    x = LoadValue(TableName, "TotalGamesPlayed")
    If(x <> "") then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 End If
End Sub

Sub Savehs
    SaveValue TableName, "HighScore1", HighScore(0)
    SaveValue TableName, "HighScore1Name", HighScoreName(0)
    SaveValue TableName, "HighScore2", HighScore(1)
    SaveValue TableName, "HighScore2Name", HighScoreName(1)
    SaveValue TableName, "HighScore3", HighScore(2)
    SaveValue TableName, "HighScore3Name", HighScoreName(2)
    SaveValue TableName, "HighScore4", HighScore(3)
    SaveValue TableName, "HighScore4Name", HighScoreName(3)
    SaveValue TableName, "Credits", Credits
    SaveValue TableName, "TotalGamesPlayed", TotalGamesPlayed
End Sub

Sub Reseths
    HighScoreName(0) = "AAA"
    HighScoreName(1) = "BBB"
    HighScoreName(2) = "CCC"
    HighScoreName(3) = "DDD"
    HighScore(0) = 100000
    HighScore(1) = 110000
    HighScore(2) = 120000
    HighScore(3) = 130000
    Savehs
End Sub

' ***********************************************************
'  High Score Initals Entry Functions - based on Black's code
' ***********************************************************

Dim hsbModeActive
Dim hsEnteredName
Dim hsEnteredDigits(3)
Dim hsCurrentDigit
Dim hsValidLetters
Dim hsCurrentLetter
Dim hsLetterFlash

Sub CheckHighscore()
    Dim tmp
    tmp = Score(1)
    If Score(2)> tmp Then tmp = Score(2)
    If Score(3)> tmp Then tmp = Score(3)
    If Score(4)> tmp Then tmp = Score(4)

    'If tmp > HighScore(1)Then 'add 1 credit for beating the highscore
    '    Credits = Credits + 1
    '    DOF 125, DOFOn
    'End If

    If tmp> HighScore(3) Then
        PlaySound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
        DOF 121, DOFPulse
        HighScore(3) = tmp
        'enter player's name
        HighScoreEntryInit()
    Else
        EndOfBallComplete()
    End If
End Sub

Sub HighScoreEntryInit()
    hsbModeActive = True
    playsong4
	PlaySound "crowdnoise"
    hsLetterFlash = 0

    hsEnteredDigits(0) = " "
    hsEnteredDigits(1) = " "
    hsEnteredDigits(2) = " "
    hsCurrentDigit = 0

    'hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ'<>*+-/=\^0123456789`" ' ` is back arrow
	hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789<" ' < is back arrow JP FLEX FIX
    hsCurrentLetter = 1
    DMDFlush()
    HighScoreDisplayNameNow()

    HighScoreFlashTimer.Interval = 250
    HighScoreFlashTimer.Enabled = True
End Sub

Sub EnterHighScoreKey(keycode)
    If keycode = LeftFlipperKey Then
        playsound "fx_Previous"
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0) then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = RightFlipperKey Then
        playsound "fx_Next"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter> len(hsValidLetters) ) then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = StartGameKey Then
        'if(mid(hsValidLetters, hsCurrentLetter, 1) <> "`") then
		if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<") then 'JP FLEX FIX
            playsound "fx_Enter"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3) then
                HighScoreCommitName()
            else
                HighScoreDisplayNameNow()
            end if
        else
            playsound "fx_Esc"
            hsEnteredDigits(hsCurrentDigit) = " "
            if(hsCurrentDigit> 0) then
                hsCurrentDigit = hsCurrentDigit - 1
            end if
            HighScoreDisplayNameNow()
        end if
    end if
End Sub

Sub HighScoreDisplayNameNow()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreDisplayName()
    Dim i
    Dim TempTopStr
    Dim TempBotStr

    TempTopStr = "YOUR NAME:"
    dLine(0) = ExpandLine(TempTopStr, 0)
    DMDUpdate 0

    TempBotStr = "    > "
    if(hsCurrentDigit> 0) then TempBotStr = TempBotStr & hsEnteredDigits(0)
    if(hsCurrentDigit> 1) then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit> 2) then TempBotStr = TempBotStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3) then
        if(hsLetterFlash <> 0) then
            TempBotStr = TempBotStr & "_"
        else
            TempBotStr = TempBotStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit <1) then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit <2) then TempBotStr = TempBotStr & hsEnteredDigits(2)

    TempBotStr = TempBotStr & " <    "
    dLine(1) = ExpandLine(TempBotStr, 1)
    DMDUpdate 1
End Sub

Sub HighScoreFlashTimer_Timer()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = hsLetterFlash + 1
    if(hsLetterFlash = 2) then hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreCommitName()
    HighScoreFlashTimer.Enabled = False
    hsbModeActive = False
    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ") then
        hsEnteredName = "YOU"
    end if

    HighScoreName(3) = hsEnteredName
    SortHighscore
    EndOfBallComplete()
End Sub

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 0 to 3
        For j = 0 to 2
            If HighScore(j) <HighScore(j + 1) Then
                tmp = HighScore(j + 1)
                tmp2 = HighScoreName(j + 1)
                HighScore(j + 1) = HighScore(j)
                HighScoreName(j + 1) = HighScoreName(j)
                HighScore(j) = tmp
                HighScoreName(j) = tmp2
            End If
        Next
    Next
End Sub

' *************************************************************************
'   JP's Reduced Display Driver Functions (based on script by Black)
' only 5 effects: none, scroll left, scroll right, blink and blinkfast
' 3 Lines, treats all 3 lines as text. 3rd line is just 1 character
' Example format:
' DMD "text1","text2","backpicture", eNone, eNone, eNone, 250, True, "sound"
' Short names:
' dq = display queue
' de = display effect
' *************************************************************************

Const eNone = 0        ' Instantly displayed
Const eScrollLeft = 1  ' scroll on from the right
Const eScrollRight = 2 ' scroll on from the left
Const eBlink = 3       ' Blink (blinks for 'TimeOn')
Const eBlinkFast = 4   ' Blink (blinks for 'TimeOn') at user specified intervals (fast speed)

Const dqSize = 64

Dim dqHead
Dim dqTail
Dim deSpeed
Dim deBlinkSlowRate
Dim deBlinkFastRate

Dim dCharsPerLine(2)
Dim dLine(2)
Dim deCount(2)
Dim deCountEnd(2)
Dim deBlinkCycle(2)

Dim dqText(2, 64)
Dim dqEffect(2, 64)
Dim dqTimeOn(64)
Dim dqbFlush(64)
Dim dqSound(64)

Dim FlexDMD
Dim DMDScene

Sub DMD_Init() 'default/startup values
    If UseFlexDMD Then
        Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
        If Not FlexDMD is Nothing Then
            FlexDMD.TableFile = Table1.Filename & ".vpx"
            FlexDMD.RenderMode = 2
            FlexDMD.Width = 128
            FlexDMD.Height = 32
            FlexDMD.Clear = True
            FlexDMD.GameName = cGameName
            FlexDMD.Run = True
            Set DMDScene = FlexDMD.NewGroup("Scene")
            DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.bkempty")
            DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
            For i = 0 to 35
                DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.dempty&dmd=2")
                Digits(i).Visible = False
            Next
            'digitgrid.Visible = False
            For i = 0 to 19 ' Top
                DMDScene.GetImage("Dig" & i).SetBounds 4 + i * 6, 3 + 16 + 2, 8, 8
            Next
            For i = 20 to 35 ' Bottom
                DMDScene.GetImage("Dig" & i).SetBounds 8 * (i - 20), 3, 8, 16
            Next
            FlexDMD.LockRenderThread
            FlexDMD.Stage.AddActor DMDScene
            FlexDMD.UnlockRenderThread
        End If
    End If


'Sub DMD_Init() 'default/startup values
    Dim i, j
    DMDFlush()
    deSpeed = 20
    deBlinkSlowRate = 5
    deBlinkFastRate = 2
    dCharsPerLine(0) = 16 'characters lower line
    dCharsPerLine(1) = 20 'characters top line
    dCharsPerLine(2) = 1  'characters back line
    For i = 0 to 2
        dLine(i) = Space(dCharsPerLine(i) )
        deCount(i) = 0
        deCountEnd(i) = 0
        deBlinkCycle(i) = 0
        dqTimeOn(i) = 0
        dqbFlush(i) = True
        dqSound(i) = ""
    Next
    For i = 0 to 2
        For j = 0 to 64
            dqText(i, j) = ""
            dqEffect(i, j) = eNone
        Next
    Next
    DMD dLine(0), dLine(1), dLine(2), eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDFlush()
    Dim i
    DMDTimer.Enabled = False
    DMDEffectTimer.Enabled = False
    dqHead = 0
    dqTail = 0
    For i = 0 to 2
        deCount(i) = 0
        deCountEnd(i) = 0
        deBlinkCycle(i) = 0
    Next
End Sub

Sub DMDScore()
    Dim tmp, tmp1, tmp2
    if(dqHead = dqTail) Then
        tmp = RL(0, FormatScore(Score(Currentplayer) ) )
        tmp1 = CL(1, "PLAYER " & CurrentPlayer & "  BALL " & Balls)
        tmp2 = "bkborder"
    End If
    DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDScoreNow
    DMDFlush
    DMDScore
End Sub

Sub DMD(Text0, Text1, Text2, Effect0, Effect1, Effect2, TimeOn, bFlush, Sound)
    if(dqTail <dqSize) Then
        if(Text0 = "_") Then
            dqEffect(0, dqTail) = eNone
            dqText(0, dqTail) = "_"
        Else
            dqEffect(0, dqTail) = Effect0
            dqText(0, dqTail) = ExpandLine(Text0, 0)
        End If

        if(Text1 = "_") Then
            dqEffect(1, dqTail) = eNone
            dqText(1, dqTail) = "_"
        Else
            dqEffect(1, dqTail) = Effect1
            dqText(1, dqTail) = ExpandLine(Text1, 1)
        End If

        if(Text2 = "_") Then
            dqEffect(2, dqTail) = eNone
            dqText(2, dqTail) = "_"
        Else
            dqEffect(2, dqTail) = Effect2
            dqText(2, dqTail) = Text2 'it is always 1 letter in this table
        End If

        dqTimeOn(dqTail) = TimeOn
        dqbFlush(dqTail) = bFlush
        dqSound(dqTail) = Sound
        dqTail = dqTail + 1
        if(dqTail = 1) Then
            DMDHead()
        End If
    End If
End Sub

Sub DMDHead()
    Dim i
    deCount(0) = 0
    deCount(1) = 0
    deCount(2) = 0
    DMDEffectTimer.Interval = deSpeed

    For i = 0 to 2
        Select Case dqEffect(i, dqHead)
            Case eNone:deCountEnd(i) = 1
            Case eScrollLeft:deCountEnd(i) = Len(dqText(i, dqHead) )
            Case eScrollRight:deCountEnd(i) = Len(dqText(i, dqHead) )
            Case eBlink:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
            Case eBlinkFast:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
        End Select
    Next
    if(dqSound(dqHead) <> "") Then
        PlaySound(dqSound(dqHead) )
    End If
    DMDEffectTimer.Enabled = True
End Sub

Sub DMDEffectTimer_Timer()
    DMDEffectTimer.Enabled = False
    DMDProcessEffectOn()
End Sub

Sub DMDTimer_Timer()
    Dim Head
    DMDTimer.Enabled = False
    Head = dqHead
    dqHead = dqHead + 1
    if(dqHead = dqTail) Then
        if(dqbFlush(Head) = True) Then
            DMDScoreNow()
        Else
            dqHead = 0
            DMDHead()
        End If
    Else
        DMDHead()
    End If
End Sub

Sub DMDProcessEffectOn()
    Dim i
    Dim BlinkEffect
    Dim Temp

    BlinkEffect = False

    For i = 0 to 2
        if(deCount(i) <> deCountEnd(i) ) Then
            deCount(i) = deCount(i) + 1

            select case(dqEffect(i, dqHead) )
                case eNone:
                    Temp = dqText(i, dqHead)
                case eScrollLeft:
                    Temp = Right(dLine(i), dCharsPerLine(i) - 1)
                    Temp = Temp & Mid(dqText(i, dqHead), deCount(i), 1)
                case eScrollRight:
                    Temp = Mid(dqText(i, dqHead), (dCharsPerLine(i) + 1) - deCount(i), 1)
                    Temp = Temp & Left(dLine(i), dCharsPerLine(i) - 1)
                case eBlink:
                    BlinkEffect = True
                    if((deCount(i) MOD deBlinkSlowRate) = 0) Then
                        deBlinkCycle(i) = deBlinkCycle(i) xor 1
                    End If

                    if(deBlinkCycle(i) = 0) Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(dCharsPerLine(i) )
                    End If
                case eBlinkFast:
                    BlinkEffect = True
                    if((deCount(i) MOD deBlinkFastRate) = 0) Then
                        deBlinkCycle(i) = deBlinkCycle(i) xor 1
                    End If

                    if(deBlinkCycle(i) = 0) Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(dCharsPerLine(i) )
                    End If
            End Select

            if(dqText(i, dqHead) <> "_") Then
                dLine(i) = Temp
                DMDUpdate i
            End If
        End If
    Next

    if(deCount(0) = deCountEnd(0) ) and(deCount(1) = deCountEnd(1) ) and(deCount(2) = deCountEnd(2) ) Then

        if(dqTimeOn(dqHead) = 0) Then
            DMDFlush()
        Else
            if(BlinkEffect = True) Then
                DMDTimer.Interval = 10
            Else
                DMDTimer.Interval = dqTimeOn(dqHead)
            End If

            DMDTimer.Enabled = True
        End If
    Else
        DMDEffectTimer.Enabled = True
    End If
End Sub

Function ExpandLine(TempStr, id) 'id is the number of the dmd line
    If TempStr = "" Then
        TempStr = Space(dCharsPerLine(id) )
    Else
        if(Len(TempStr)> Space(dCharsPerLine(id) ) ) Then
            TempStr = Left(TempStr, Space(dCharsPerLine(id) ) )
        Else
            if(Len(TempStr) <dCharsPerLine(id) ) Then
                TempStr = TempStr & Space(dCharsPerLine(id) - Len(TempStr) )
            End If
        End If
    End If
    ExpandLine = TempStr
End Function

Function FormatScore(ByVal Num) 'it returns a string with commas (as in Black's original font)
    dim i
    dim NumString

    NumString = CStr(abs(Num) )

    For i = Len(NumString) -3 to 1 step -3
        if IsNumeric(mid(NumString, i, 1) ) then
            NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1) ) + 48) & right(NumString, Len(NumString) - i)
        end if
    Next
    FormatScore = NumString
End function

Function CL(id, NumString)
    Dim Temp, TempStr
    Temp = (dCharsPerLine(id) - Len(NumString) ) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL = TempStr
End Function

Function RL(id, NumString)
    Dim Temp, TempStr
    Temp = dCharsPerLine(id) - Len(NumString)
    TempStr = Space(Temp) & NumString
    RL = TempStr
End Function

'**************
' Update DMD
'**************

Sub DMDUpdate(id)
    Dim digit, value
    If UseFlexDMD Then FlexDMD.LockRenderThread
    Select Case id
        Case 0 'top text line
            For digit = 20 to 35
                DMDDisplayChar mid(dLine(0), digit-19, 1), digit
            Next
        Case 1 'bottom text line
            For digit = 0 to 19
                DMDDisplayChar mid(dLine(1), digit + 1, 1), digit
            Next
        Case 2 ' back image - back animations
            If dLine(2) = "" OR dLine(2) = " " Then dLine(2) = "bkempty"
            DigitsBack(0).ImageA = dLine(2)
            If UseFlexDMD Then DMDScene.GetImage("Back").Bitmap = FlexDMD.NewImage("", "VPX." & dLine(2) & "&dmd=2").Bitmap
    End Select
    If UseFlexDMD Then FlexDMD.UnlockRenderThread
End Sub

Sub DMDDisplayChar(achar, adigit)
    If achar = "" Then achar = " "
    achar = ASC(achar)
    Digits(adigit).ImageA = Chars(achar)
    If UseFlexDMD Then DMDScene.GetImage("Dig" & adigit).Bitmap = FlexDMD.NewImage("", "VPX." & Chars(achar) & "&dmd=2&add").Bitmap
End Sub

'****************************
' JP's new DMD using flashers
'****************************

Dim Digits, DigitsBack, Chars(255), Images(255)

DMDInit

Sub DMDInit
    Dim i
    'If Table1.ShowDT = true then
        Digits = Array(digit0, digit1, digit2, digit3, digit4, digit5, digit6, digit7, digit8, digit9, digit10, digit11,                  _
            digit12, digit13, digit14, digit15, digit16, digit17, digit18, digit19, digit20, digit21, digit22, digit23, digit24, digit25, _
            digit26, digit27, digit28, digit29, digit30, digit31, digit32, digit33, digit34, digit35)
        DigitsBack = Array(digit36)

    For i = 0 to 255:Chars(i)  = "dempty":Next '= "dempty":Images(i) = "dempty":Next

    Chars(32) = "dempty"
    '    Chars(34) = '"
    '    Chars(36) = '$
    '    Chars(39) = ''
    '    Chars(42) = '*
    '    Chars(43) = '+
    '    Chars(45) = '-
    '    Chars(47) = '/
    Chars(48) = "d0"       '0
    Chars(49) = "d1"       '1
    Chars(50) = "d2"       '2
    Chars(51) = "d3"       '3
    Chars(52) = "d4"       '4
    Chars(53) = "d5"       '5
    Chars(54) = "d6"       '6
    Chars(55) = "d7"       '7
    Chars(56) = "d8"       '8
    Chars(57) = "d9"       '9
    Chars(60) = "dless"    '<
    Chars(61) = "dequal"   '=
    Chars(62) = "dgreater" '>
    '   Chars(64) = '@
    Chars(65) = "da" 'A
    Chars(66) = "db" 'B
    Chars(67) = "dc" 'C
    Chars(68) = "dd" 'D
    Chars(69) = "de" 'E
    Chars(70) = "df" 'F
    Chars(71) = "dg" 'G
    Chars(72) = "dh" 'H
    Chars(73) = "di" 'I
    Chars(74) = "dj" 'J
    Chars(75) = "dk" 'K
    Chars(76) = "dl" 'L
    Chars(77) = "dm" 'M
    Chars(78) = "dn" 'N
    Chars(79) = "do" 'O
    Chars(80) = "dp" 'P
    Chars(81) = "dq" 'Q
    Chars(82) = "dr" 'R
    Chars(83) = "ds" 'S
    Chars(84) = "dt" 'T
    Chars(85) = "du" 'U
    Chars(86) = "dv" 'V
    Chars(87) = "dw" 'W
    Chars(88) = "dx" 'X
    Chars(89) = "dy" 'Y
    Chars(90) = "dz" 'Z
    'Chars(91) = "dball" '[
    'Chars(92) = "dcoin" '|
    'Chars(93) = "dpika" ']
    '    Chars(94) = '^
    '    Chars(95) = '_
    Chars(96) = "d0a"  '0.
    Chars(97) = "d1a"  '1.
    Chars(98) = "d2a"  '2.
    Chars(99) = "d3a"  '3.
    Chars(100) = "d4a" '4.
    Chars(101) = "d5a" '5.
    Chars(102) = "d6a" '6.
    Chars(103) = "d7a" '7.
    Chars(104) = "d8a" '8.
    Chars(105) = "d9a" '9
End Sub

'********************************************************************************************
' Only for VPX 10.2 and higher.
' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Return to previous State
'********************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState) 'thanks gtxjoe for the first version

    If TypeName(MyLight) = "Light" Then

        If FinalState = 2 Then
            FinalState = MyLight.State 'Keep the current light state
        End If
        MyLight.BlinkInterval = BlinkPeriod
        MyLight.Duration 2, TotalPeriod, FinalState
    ElseIf TypeName(MyLight) = "Flasher" Then

        Dim steps

        ' Store all blink information
        steps = Int(TotalPeriod / BlinkPeriod + .5) 'Number of ON/OFF steps to perform
        If FinalState = 2 Then                      'Keep the current flasher state
            FinalState = ABS(MyLight.Visible)
        End If
        MyLight.UserValue = steps * 10 + FinalState 'Store # of blinks, and final state

        ' Start blink timer and create timer subroutine
        MyLight.TimerInterval = BlinkPeriod
        MyLight.TimerEnabled = 0
        MyLight.TimerEnabled = 1
        ExecuteGlobal "Sub " & MyLight.Name & "_Timer:" & "Dim tmp, steps, fstate:tmp=me.UserValue:fstate = tmp MOD 10:steps= tmp\10 -1:Me.Visible = steps MOD 2:me.UserValue = steps *10 + fstate:If Steps = 0 then Me.Visible = fstate:Me.TimerEnabled=0:End if:End Sub"
    End If
End Sub

' #####################################
' ###### Flashers flupper #####
' #####################################

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 1		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 1		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherOffBrightness = 0.5		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objlight(20)
Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
'InitFlasher 1, "green" : InitFlasher 2, "red" : InitFlasher 3, "white"
'InitFlasher 4, "green" : InitFlasher 5, "red" : InitFlasher 6, "white"
'InitFlasher 7, "green" : InitFlasher 8, "red"
InitFlasher 9, "blue" ': InitFlasher 10, "red" : InitFlasher 11, "white" 
' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 4,17 : RotateFlasher 5,0 : RotateFlasher 6,90
'RotateFlasher 7,0 : RotateFlasher 8,0 
'RotateFlasher 9,-45 : RotateFlasher 10,90 : RotateFlasher 11,90

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 60
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness
	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objlit(nr).visible = 1 : End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objlit(nr).visible = 0 : End If
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub 
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub 
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub 
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub 
Sub FlasherFlash5_Timer() : FlashFlasher(5) : End Sub 
Sub FlasherFlash6_Timer() : FlashFlasher(6) : End Sub 
Sub FlasherFlash7_Timer() : FlashFlasher(7) : End Sub
Sub FlasherFlash8_Timer() : FlashFlasher(8) : End Sub
Sub FlasherFlash9_Timer() : FlashFlasher(9) : End Sub
Sub FlasherFlash10_Timer() : FlashFlasher(10) : End Sub
Sub FlasherFlash11_Timer() : FlashFlasher(11) : End Sub

' ********************************
'   Table info & Attract Mode
' ********************************

Sub ShowTableInfo
    Dim ii
    'info goes in a loop only stopped by the credits and the startkey
    If Score(1) Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 1" &FormatScore(Score(1) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(2) Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 2 " &FormatScore(Score(2) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(3) Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 3 " &FormatScore(Score(3) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(4) Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 4 " &FormatScore(Score(4) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
     DMD CL(0, "GAME OVER"), CL(1, "TRY AGAIN"), "", eNone, eBlink, eNone, 2000, True, ""
    If bFreePlay Then
        DMD "", CL(1, "FREE PLAY"), "", eNone, eNone, eNone, 2000, False, ""
    Else
        If Credits> 0 Then
            DMD CL(0, "CREDITS " & Credits), CL(1, "PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
        Else
            DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
        End If
    End If
	'   Put here your intro DMD
	DMD "", "", "at1", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at2", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at3", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at4", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at5", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at6", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at7", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at8", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at9", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at10", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at11", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at12", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at13", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at14", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at15", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at16", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at17", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at18", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at19", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at20", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at21", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at22", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at23", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at24", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at25", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at26", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at27", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at28", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at29", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at30", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at31", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at32", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at33", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at34", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at35", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at36", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at37", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at38", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at39", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at40", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at41", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at42", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at43", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at44", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at45", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at46", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at47", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at48", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at49", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "at50", eNone, eNone, eNone, 100, True, ""
    DMD CL(0, "HIGHSCORES"), Space(dCharsPerLine(1) ), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL(0, "HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL(0, "HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(dCharsPerLine(0) ), Space(dCharsPerLine(1) ), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

Sub StartAttractMode
	startB2S(1)
    playsongEnd
	fightersnotvisible
	attractLightsupTimer.enabled = 1
	pupflasher1.imageA = "attract1" 
    StartLightSeq
    DMDFlush
    ShowTableInfo
End Sub

Sub StopAttractMode
    LightSeqAttract.StopPlay
attractLightsupTimer.enabled = 0
pupflasher1.imageA = "Fpressstart" 
    DMDScoreNow
End Sub

Sub StartLightSeq()
    'lights sequences
    LightSeqAttract.UpdateInterval = 25
    LightSeqAttract.Play SeqBlinking, , 5, 150
    LightSeqAttract.Play SeqRandom, 40, , 4000
    LightSeqAttract.Play SeqAllOff
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 40, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 40, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqRightOn, 30, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqLeftOn, 30, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqStripe1VertOn, 50, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
End Sub

Sub LightSeqAttract_PlayDone()
    StartLightSeq()
End Sub

Sub LightSeqTilt_PlayDone()
    LightSeqTilt.Play SeqAllOff
End Sub

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' droptargets, animations, etc
Sub VPObjects_Init
End Sub

' tables variables and Mode init
Dim HoleBonus, BumperBonus, ALLRampBonus, RampBonus1, RampBonus2, RampBonus3, MulitballBonus, TargetBonus, Scoopbonus   

Sub Game_Init() 'called at the start of a new game
    Dim i, j
	TargetBonus = 0
	'bumperHits = 100
	BumperBonus = 0
	ALLRampBonus = 0
	RampBonus1 = 0
	RampBonus2 = 0
	RampBonus3 =0
	MulitballBonus = 0
	Scoopbonus = 0
	'BallInHole = 0 
    TurnOffPlayfieldLights()
End Sub

Sub StopEndOfBallMode()     'this sub is called after the last ball is drained
End Sub

Sub ResetNewBallVariables() 'reset variables for a new ball or player
Dim i
TargetBonus = 0
Scoopbonus = 0
bBallSaverReady = True
End Sub

Sub ResetNewBallLights()    'turn on or off the needed lights before a new ball is released
    'TurnOffPlayfieldLights
    'li025.State = 1
    'li021.State = 1
    'li022.State = 1
    'li023.State = 1
    'li024.State = 1
	'li033.state = 1
	gi1.state = 1
	gi2.state = 1
	gi3.state = 1
	gi4.state = 1
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

' *********************************************************************
'                        Table Object Hit Events
'
' Any target hit Sub should do something like this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/Mode this trigger is a member of
' - set the "LastSwitchHit" variable in case it is needed later
' *********************************************************************

'************
' Slingshots
'************

Dim RStep, Lstep

Sub RightSlingShot_Slingshot
DMD "", "", "500", eNone, eNone, eNone, 300, True, ""
Score(CurrentPlayer) = Score(CurrentPlayer) + (500*PFMultiplier) 
	PlaySound SoundFXDOF("right_slingshot", 104, DOFPulse, DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    RSling.Visible = 0:RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	AddScore 210
	gi1.State = 0
	Gi2.State = 0	
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10:gi1.State = 0:Gi2.State = 0
		Case 1:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 5:gi1.State = 0:Gi2.State = 0
        Case 2:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:gi1.State = 1:Gi2.State = 1:RightSlingShot.TimerEnabled = False
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
DMD "", "", "500", eNone, eNone, eNone, 300, True, ""
Score(CurrentPlayer) = Score(CurrentPlayer) + (500*PFMultiplier)
    PlaySound SoundFXDOF("left_slingshot", 103, DOFPulse, DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    LSling.Visible = 0:LSling1.Visible = 1
    sling2.rotx = 20
	 LStep = 0
    LeftSlingShot.TimerEnabled = 1
	AddScore 210
	gi3.State = 0
	Gi4.State = 0
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10:gi3.State = 0:Gi4.State = 0
        Case 2:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 5:gi3.State = 0:Gi4.State = 0
        Case 3:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:gi3.State = 1:Gi4.State = 1:LeftSlingShot.TimerEnabled = False
    End Select
    LStep = LStep + 1
End Sub

'*****************
'triggers
'*****************

'bonuschecker for ball lost **************
Sub Bonuschecker_Hit
End Sub

'inlane/outlanes ***************

Sub TLeftInlane_Hit
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
    end if
LeftInlane.State=1
DMD "", "", "ringI", eNone, eNone, eNone, 500, True, ""  
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
PlaySound "INLANE"
Checkring
End Sub

Sub TLeftOutlane_Hit
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (2000000*PFMultiplier)
    end if
LeftOutlane.State=1
DMD "", "", "ringR", eNone, eNone, eNone, 500, True, ""  
Score(CurrentPlayer) = Score(CurrentPlayer) + (2000*PFMultiplier)
PlaySound "OUTLANE"
Checkring
End Sub

Sub TRightInlane_Hit
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
    end if
RightInlane.State=1
DMD "", "", "ringN", eNone, eNone, eNone, 500, True, ""  
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
PlaySound "INLANE"
Checkring
End Sub

Sub TRightOutlane_Hit
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (2000000*PFMultiplier)
    end if
RightOutlane.State=1
DMD "", "", "ringG", eNone, eNone, eNone, 500, True, ""  
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
PlaySound "OUTLANE"
Checkring
End Sub

Sub Checkring
    If(LeftInlane.State = 1) And(LeftOutlane.State = 1) And(RightInlane.State = 1) And(RightOutlane.State = 1) Then
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	PlaySound ""
	LeftInlane.State=0
	LeftOutlane.State=0
	RightInlane.State=0
	RightOutlane.State=0
	DMD "", "", "10k", eNone, eNone, eNone, 500, True, ""
	DMD "", "", "ringCOMPLETE", eNone, eNone, eNone, 500, True, ""
    End If
End Sub

'************************** 
'Bumpers 
'************************** 

Sub Bumper001_hit()
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
    end if
DMD "", "", "1k", eNone, eNone, eNone, 300, True, ""
FlashForMs BLight001, 1000, 50, 0
PlaySoundAt SoundFXDOF("SoundHook",109,DOFPulse,DOFContactors), Bumper001
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
FlBumperFadeTarget(1) = 1		'Flupper bumper demo
Bumper001.timerenabled = True
ObjLevel(9) = 1 : FlasherFlash9_Timer
End sub

Sub Bumper001_timer
	FlBumperFadeTarget(1) = 0
End Sub

'*****************
'Targets
'*****************

' multiplier targets ********************
Dim multipoints, newmultipoints

Sub Target001_hit
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
    end if
	PlaySoundAtball "fx_target"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
	if RightTargetLight.state = 1 then exit sub
    multipoints = multipoints + 1
	TargetBonus = TargetBonus + 1
	RightTargetLight.state = 1
	checkmultiponts
End Sub

Sub Target002_hit
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
    end if
	PlaySoundAtball "fx_target"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
	if LeftTargetLight.state = 1 then exit sub
    multipoints = multipoints + 1
	TargetBonus = TargetBonus + 1
	LeftTargetLight.state = 1
	checkmultiponts
End Sub

sub checkmultiponts
if PFMultiplier => 5 then exit sub 
if multipoints = 2 Then
'add dof here
PFMultiplier = PFMultiplier + 1
RightTargetLight.state = 0
LeftTargetLight.state = 0
checkPFMultiplier
end if
end sub

sub checkPFMultiplier
if PFMultiplier = 2 Then
DMD "", "", "2x", eNone, eNone, eNone, 1000, True, "MULTIE"
m2x.state = 1
multipoints = 0
exit sub
end if
if PFMultiplier = 3 Then
DMD "", "", "3x", eNone, eNone, eNone, 1000, True, "MULTIE"
m3x.state = 1
multipoints = 0
exit sub
end if
if PFMultiplier = 4 Then
DMD "", "", "4x", eNone, eNone, eNone, 1000, True, "MULTIE"
m4x.state = 1
multipoints = 0
exit sub
end if
if PFMultiplier = 5 Then
DMD "", "", "5x", eNone, eNone, eNone, 1000, True, "MULTIE"
m5x.state = 1
multipoints = 0
exit sub
end if
end sub

' fightnight targets ********************
Sub Target009_Hit
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
    end if
    DMD "", "", "nightt", eNone, eNone, eNone, 500, True, ""
TargetBonus = TargetBonus + 1
if Gtready = 1 Then
GtreadyCount = GtreadyCount + 1
Lt9.state = 1
PlaySoundAt "fx_droptarget", Target009
	Score(CurrentPlayer) = Score(CurrentPlayer) + (2000*PFMultiplier)
checkGtreadyCount
exit sub
end if
Tleftcount = Tleftcount + 1
PlaySoundAt "fx_droptarget", Target009
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
checkTleftcount
End Sub

Sub Target010_Hit
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
    end if
    DMD "", "", "nighth", eNone, eNone, eNone, 500, True, ""
TargetBonus = TargetBonus + 1
if Gtready = 1 Then
GtreadyCount = GtreadyCount + 1
Lt10.state = 1
PlaySoundAt "fx_droptarget", Target010
	Score(CurrentPlayer) = Score(CurrentPlayer) + (2000*PFMultiplier)
checkGtreadyCount
exit sub
end if
Tleftcount = Tleftcount + 1
PlaySoundAt "fx_droptarget", Target010
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
checkTleftcount
End Sub

Sub Target011_Hit
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
    end if
    DMD "", "", "nightg", eNone, eNone, eNone, 500, True, ""
TargetBonus = TargetBonus + 1
if Gtready = 1 Then
GtreadyCount = GtreadyCount + 1
Lt11.state = 1
PlaySoundAt "fx_droptarget", Target011
	Score(CurrentPlayer) = Score(CurrentPlayer) + (2000*PFMultiplier)
checkGtreadyCount
exit sub
end if
Tleftcount = Tleftcount + 1
PlaySoundAt "fx_droptarget", Target011
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
checkTleftcount
End Sub

Sub Target012_Hit
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
    end if
    DMD "", "", "nighti", eNone, eNone, eNone, 500, True, ""
TargetBonus = TargetBonus + 1
if Gtready = 1 Then
GtreadyCount = GtreadyCount + 1
Lt12.state = 1
PlaySoundAt "fx_droptarget", Target012
	Score(CurrentPlayer) = Score(CurrentPlayer) + (2000*PFMultiplier)
checkGtreadyCount
exit sub
end if
Tleftcount = Tleftcount + 1
PlaySoundAt "fx_droptarget", Target012
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
checkTleftcount
End Sub

Sub Target013_Hit
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
    end if
    DMD "", "", "nightf", eNone, eNone, eNone, 500, True, ""
TargetBonus = TargetBonus + 1
if Gtready = 1 Then
GtreadyCount = GtreadyCount + 1
Lt13.state = 1
PlaySoundAt "fx_droptarget", Target013
	Score(CurrentPlayer) = Score(CurrentPlayer) + (2000*PFMultiplier)
checkGtreadyCount
exit sub
end if
Tleftcount = Tleftcount + 1
PlaySoundAt "fx_droptarget", Target013
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
checkTleftcount
End Sub

Sub Target014_Hit
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
    end if
    DMD "", "", "ngihtn", eNone, eNone, eNone, 500, True, ""
TargetBonus = TargetBonus + 1
if Gtready = 1 Then
GtreadyCount = GtreadyCount + 1
Lt14.state = 1
PlaySoundAt "fx_droptarget", Target014
	Score(CurrentPlayer) = Score(CurrentPlayer) + (2000*PFMultiplier)
checkGtreadyCount
exit sub
end if
Trightcount = Trightcount + 1
PlaySoundAt "fx_droptarget", Target014
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
checkTrightcount
End Sub

Sub Target015_Hit
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
    end if
    DMD "", "", "nighti", eNone, eNone, eNone, 500, True, ""
TargetBonus = TargetBonus + 1
if Gtready = 1 Then
GtreadyCount = GtreadyCount + 1
Lt15.state = 1
PlaySoundAt "fx_droptarget", Target015
	Score(CurrentPlayer) = Score(CurrentPlayer) + (2000*PFMultiplier)
checkGtreadyCount
exit sub
end if
Trightcount = Trightcount + 1
PlaySoundAt "fx_droptarget", Target015
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
checkTrightcount
End Sub

Sub Target016_Hit
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
    end if
    DMD "", "", "nightg", eNone, eNone, eNone, 500, True, ""
TargetBonus = TargetBonus + 1
if Gtready = 1 Then
GtreadyCount = GtreadyCount + 1
Lt16.state = 1
PlaySoundAt "fx_droptarget", Target016
	Score(CurrentPlayer) = Score(CurrentPlayer) + (2000*PFMultiplier)
checkGtreadyCount
exit sub
end if
Trightcount = Trightcount + 1
PlaySoundAt "fx_droptarget", Target016
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
checkTrightcount
End Sub

Sub Target017_Hit
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
    end if
    DMD "", "", "nighth", eNone, eNone, eNone, 500, True, ""
TargetBonus = TargetBonus + 1
if Gtready = 1 Then
GtreadyCount = GtreadyCount + 1
Lt17.state = 1
PlaySoundAt "fx_droptarget", Target017
	Score(CurrentPlayer) = Score(CurrentPlayer) + (2000*PFMultiplier)
checkGtreadyCount
exit sub
end if
Trightcount = Trightcount + 1
PlaySoundAt "fx_droptarget", Target017
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
checkTrightcount
End Sub

Sub Target018_Hit
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
    end if
    DMD "", "", "nightt", eNone, eNone, eNone, 500, True, ""
TargetBonus = TargetBonus + 1
if Gtready = 1 Then
GtreadyCount = GtreadyCount + 1
Lt18.state = 1
PlaySoundAt "fx_droptarget", Target018
	Score(CurrentPlayer) = Score(CurrentPlayer) + (2000*PFMultiplier)
checkGtreadyCount
exit sub
end if
Trightcount = Trightcount + 1
PlaySoundAt "fx_droptarget", Target018
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
checkTrightcount
End Sub

sub checkGtreadyCount
if GtreadyCount => 10 then
'add dof here
Score(CurrentPlayer) = Score(CurrentPlayer) + (20000*PFMultiplier)
    DMD "", "", "fightnight", eNone, eNone, eNone, 1000, True, ""
TrainingLevel = TrainingLevel + 1
Lt18.state = 2
Lt17.state = 2
Lt16.state = 2
Lt15.state = 2
Lt14.state = 2
Lt13.state = 2
Lt12.state = 2
Lt11.state = 2
Lt10.state = 2
Lt9.state = 2
ArrowRight.state = 0
ArrowLeft.state = 0
UpperLeftKickerLight.state = 2
UpperRightKickerLight.state = 2
Gtready = 0
GtreadyCount = 0
goforit = 1
end if
end sub

sub checkTleftcount
If Tleftcount => 5 Then
'add dof here
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
    DMD "", "", "fight", eNone, eNone, eNone, 1000, True, ""
vpmtimer.addtimer 1000, "resettLeft'"
Tleftcount = 0
end if  
end sub

sub resettLeft
PlaySoundAt SoundFXDOF("fx_resetdrop",115,DOFPulse,DOFDropTargets), Target009
Target009.IsDropped = False
Target010.IsDropped = False
Target011.IsDropped = False
Target012.IsDropped = False
Target013.IsDropped = False
end sub

sub checkTrightcount
If Trightcount => 5 Then
'add dof here
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
    DMD "", "", "night", eNone, eNone, eNone, 1000, True, ""
vpmtimer.addtimer 1000, "resettRight'"
Trightcount = 0
end if 
end sub

sub resettRight
PlaySoundAt SoundFXDOF("fx_resetdrop",116,DOFPulse,DOFDropTargets), Target014
Target014.IsDropped = False
Target015.IsDropped = False
Target016.IsDropped = False
Target017.IsDropped = False
Target018.IsDropped = False
end sub

sub stopTlights
Lt18.state = 0
Lt17.state = 0
Lt16.state = 0
Lt15.state = 0
Lt14.state = 0
Lt13.state = 0
Lt12.state = 0
Lt11.state = 0
Lt10.state = 0
Lt9.state = 0
end sub


'*****************
'Kickers
'*****************

'left scoop*************

Sub LeftScoop_Hit()
	'add dof here
    DOF 130, DOFon
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
    end if
	Scoopbonus = Scoopbonus + 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
	PlaySoundAt "fx_kicker_enter", LeftScoop
   	If Training1 => 4 then
    DMD "", "", "trainingdone", eNone, eNone, eNone, 1000, True, ""
vpmTimer.AddTimer 1000, "leftscoopkickout '"
	exit sub
	end if 
	Training1 = Training1 + 1
	checkreps
End Sub

sub leftscoopkickout
	if training1 = 4 Then
	AddMultiball 1
	training1 = 5
	end if
	LeftScoop.Kick 145,15
'	PlaySoundAt "fx_kicker", LeftScoop
	PlaySoundAt "SoundPunch", LeftScoop
    DOF 114, DOFPulse
    DOF 130, DOFoff
end Sub

sub leftscoopkickout2
	LeftScoop.Kick 145,15
	PlaySoundAt SoundFX("fx_kicker", DOFContactors), LeftScoop
    DOF 130, DOFoff
'	PlaySoundAt "SoundPunch", LeftScoop
end Sub

'top scoop*************

Sub TopScoop_Hit()
	'add dof here
    DOF 130, DOFon
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
    end if
	Scoopbonus = Scoopbonus + 1
	DMDFlush
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
	PlaySoundAt "fx_kicker_enter", TopScoop
'	PlaySoundAt "SoundRingBell", TopScoop
	'playsound "BELLY"
	if Pknockout = 8 then
	PlaySound "KO"
	pupflasher1.imageA = "Fko"
    resetafterfight
	checkyafter
	vpmTimer.AddTimer 5000, "topscoopkickout '"
	exit sub
	end if
	if Pstar = 4 and Pgutpunch = 5 then
	PlaySound "TKO"
	pupflasher1.imageA = "Ftko"
	resetafterfight
	checkyafter
	vpmTimer.AddTimer 5000, "topscoopkickout '"
	exit sub
	end if
if TrainingLevelLock = 1 then 
	vpmTimer.AddTimer 1000, "topscoopkickout '"
	exit sub
end if 
	if TrainingLevel = 1 and goforit = 1 Then
	StopSong
	DMD "", "", "fight1", eNone, eNone, eNone, 7000, True, ""
	playsound "fight1intro"
	pupflasher1.imageA = "ffights1"
	StartFight1
	vpmTimer.AddTimer 7000, "releaseFightMultiball '"
	exit Sub
end if 
if TrainingLevel = 2 and goforit = 1 Then
	StopSong
	DMD "", "", "fight2", eNone, eNone, eNone, 4000, True, ""
	playsound "fight2intro"
	pupflasher1.imageA = "ffights2"
	StartFight2
	vpmTimer.AddTimer 4000, "releaseFightMultiball '"
	exit Sub
end if 
if TrainingLevel = 3 and goforit = 1 Then
	StopSong
	DMD "", "", "fight3", eNone, eNone, eNone, 6000, True, ""
	playsound "fight3intro"
	pupflasher1.imageA = "ffights3"
	StartFight3
	vpmTimer.AddTimer 6000, "releaseFightMultiball '"
	exit Sub
end if 
if TrainingLevel = 4 and goforit = 1 Then
	StopSong
	DMD "", "", "fight4", eNone, eNone, eNone, 7000, True, ""
	playsound "fight4intro"
	pupflasher1.imageA = "ffights4"
	StartFight4
	vpmTimer.AddTimer 7000, "releaseFightMultiball '"
	exit Sub
end if 
if TrainingLevel = 5 and goforit = 1 Then
	StopSong
	DMD "", "", "fight5", eNone, eNone, eNone, 5000, True, ""
	playsound "fight5intro"
	pupflasher1.imageA = "ffights5"
	StartFight5
	vpmTimer.AddTimer 5000, "releaseFightMultiball '"
	exit Sub
end if 
if TrainingLevel = 6 and goforit = 1 Then
	StopSong
	DMD "", "", "fight6", eNone, eNone, eNone, 3000, True, ""
	playsound "fightintro"
	pupflasher1.imageA = "ffights6"
	StartFight6
	vpmTimer.AddTimer 3000, "releaseFightMultiball '"
	exit Sub
end if 
if TrainingLevel = 7 and goforit = 1 Then
	StopSong
	DMD "", "", "fight7", eNone, eNone, eNone, 3000, True, ""
	playsound "fightintro"
	pupflasher1.imageA = "ffights7"
	StartFight7
	vpmTimer.AddTimer 3000, "releaseFightMultiball '"
	exit Sub
end if 
if TrainingLevel = 8 and goforit = 1 Then
	StopSong
	DMD "", "", "fight8", eNone, eNone, eNone, 6000, True, ""
	playsound "fight8intro"
	pupflasher1.imageA = "ffights8"
	StartFight8
	vpmTimer.AddTimer 6000, "releaseFightMultiball '"
	exit Sub
end if 
if TrainingLevel = 9 and goforit = 1 Then
	StopSong
	DMD "", "", "fight9", eNone, eNone, eNone, 3000, True, ""
	playsound "fightintro"
	pupflasher1.imageA = "ffights9"
	StartFight9
	vpmTimer.AddTimer 3000, "releaseFightMultiball '"
	exit Sub
end if 
if TrainingLevel = 10 and goforit = 1 Then
	StopSong
	DMD "", "", "fight10", eNone, eNone, eNone, 3000, True, ""
	playsound "fightintro"
	pupflasher1.imageA = "ffights10"
	StartFight10
	vpmTimer.AddTimer 3000, "releaseFightMultiball '"
	exit Sub
end if 
if TrainingLevel = 11 and goforit = 1 Then
	StopSong
	DMD "", "", "fight11", eNone, eNone, eNone, 4000, True, ""
	playsound "fight11intro"
	pupflasher1.imageA = "Ffigths11"
	StartFight11
	vpmTimer.AddTimer 4000, "releaseFightMultiball '"
	exit Sub
end if 
vpmTimer.AddTimer 1000, "topscoopkickout '"
End Sub

sub topscoopkickout
'add dof here
DOF 130, DOFoff
if wizzscore = 1 and onetimy = 0 then
playsong3
onetimy = 1
releaseFightMultiball
TopScoop.Kick 175,15
PlaySoundAt "fx_kicker", TopScoop
AddMultiball 1
exit sub
end if
	TopScoop.Kick 175,15
    PlaySoundAt "fx_kicker", TopScoop
	updatethemusico
	playsound "BELLY"
    DOF 117, DOFPulse
	'PlaySoundAt "SoundPunch", LeftScoop
end Sub

sub topscoopkickout2
	'add dof here
DOF 130, DOFoff
	TopScoop.Kick 175,15
    PlaySoundAt  SoundFX("fx_kicker", DOFContactors), TopScoop
	'PlaySong "MATCH"
	'playsound "BELLY"
	'PlaySoundAt "SoundPunch", LeftScoop
end Sub

sub checkyafter
if TrainingLevel = 1 Then
'add dof here
Lup001.enabled = False
LightP001.image = "green"
DMD "", "", "fightwon", eNone, eNone, eNone, 2500, True, ""
DMD "", "", "10k", eNone, eNone, eNone, 2000, True, ""
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
flashpic = 12
startB2S(18)
end if
if TrainingLevel = 2 Then
'add dof here
Lup002.enabled = False
LightP002.image = "green"
pupflasher1.imageA = "Fminorcircuittitlebout"
DMD "", "", "fightwon", eNone, eNone, eNone, 2000, True, ""
DMD "", "", "minorbout", eNone, eNone, eNone, 2000, True, ""
DMD "", "", "20k", eNone, eNone, eNone, 1000, True, ""
Score(CurrentPlayer) = Score(CurrentPlayer) + (20000*PFMultiplier)
flashpic = 13
startB2S(19)
end if
if TrainingLevel = 3 Then
'add dof here
Lup003.enabled = False
LightP003.image = "green"
pupflasher1.imageA = "Fminorwinner"
DMD "", "", "fightwon", eNone, eNone, eNone, 2000, True, ""
DMD "", "", "minorwinner", eNone, eNone, eNone, 2000, True, ""
DMD "", "", "30k", eNone, eNone, eNone, 1000, True, ""
Score(CurrentPlayer) = Score(CurrentPlayer) + (30000*PFMultiplier)
flashpic = 14
startB2S(20)
AwardExtraBall
end if
if TrainingLevel = 4 Then
'add dof here
Lup004.enabled = False
LightP004.image = "green"
DMD "", "", "fightwon", eNone, eNone, eNone, 2500, True, ""
DMD "", "", "40k", eNone, eNone, eNone, 2000, True, ""
Score(CurrentPlayer) = Score(CurrentPlayer) + (40000*PFMultiplier)
flashpic = 15
startB2S(21)
end if
if TrainingLevel = 5 Then
'add dof here
Lup005.enabled = False
LightP005.image = "green"
DMD "", "", "fightwon", eNone, eNone, eNone, 2500, True, ""
DMD "", "", "50k", eNone, eNone, eNone, 2000, True, ""
Score(CurrentPlayer) = Score(CurrentPlayer) + (50000*PFMultiplier)
flashpic = 16
startB2S(22)
end if
if TrainingLevel = 6 Then
'add dof here
Lup006.enabled = False
LightP006.image = "green"
pupflasher1.imageA = "Fmajorcircuitbout"
DMD "", "", "fightwon", eNone, eNone, eNone, 2000, True, ""
DMD "", "", "majorbout", eNone, eNone, eNone, 2000, True, ""
DMD "", "", "60k", eNone, eNone, eNone, 1000, True, ""
Score(CurrentPlayer) = Score(CurrentPlayer) + (60000*PFMultiplier)
flashpic = 17
startB2S(23)
end if
if TrainingLevel = 7 Then
'add dof here
Lup007.enabled = False
LightP007.image = "green"
pupflasher1.imageA = "Fmajorwinner"
DMD "", "", "fightwon", eNone, eNone, eNone, 2000, True, ""
DMD "", "", "majorwinner", eNone, eNone, eNone, 2000, True, ""
DMD "", "", "70k", eNone, eNone, eNone, 1000, True, ""
Score(CurrentPlayer) = Score(CurrentPlayer) + (70000*PFMultiplier)
flashpic = 18
startB2S(24)
AwardExtraBall
end if
if TrainingLevel = 8 Then
'add dof here
Lup008.enabled = False
LightP008.image = "green"
DMD "", "", "fightwon", eNone, eNone, eNone, 2500, True, ""
DMD "", "", "80k", eNone, eNone, eNone, 2000, True, ""
Score(CurrentPlayer) = Score(CurrentPlayer) + (80000*PFMultiplier)
flashpic = 19
startB2S(25)
end if
if TrainingLevel = 9 Then
'add dof here
Lup009.enabled = False
LightP009.image = "green"
DMD "", "", "fightwon", eNone, eNone, eNone, 2500, True, ""
DMD "", "", "90k", eNone, eNone, eNone, 2000, True, ""
Score(CurrentPlayer) = Score(CurrentPlayer) + (90000*PFMultiplier)
flashpic = 20
startB2S(26)
end if
if TrainingLevel = 10 Then
'add dof here
Lup010.enabled = False
LightP010.image = "green"
pupflasher1.imageA = "Fworldchampionbout"
DMD "", "", "fightwon", eNone, eNone, eNone, 2000, True, ""
DMD "", "", "worldbout", eNone, eNone, eNone, 2000, True, ""
DMD "", "", "100k", eNone, eNone, eNone, 1000, True, ""
Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
flashpic = 21
startB2S(27)
end if
if TrainingLevel = 11 Then
'add dof here
Lup011.enabled = False
LightP011.image = "green"
pupflasher1.imageA = "Fwizardmodeflasher"
DMD "", "", "fightwon", eNone, eNone, eNone, 2000, True, ""
DMD "", "", "worldwinner", eNone, eNone, eNone, 2000, True, ""
DMD "", "", "110k", eNone, eNone, eNone, 1000, True, ""
Score(CurrentPlayer) = Score(CurrentPlayer) + (110000*PFMultiplier)
TrainingLevel = 0
intraining = 0
youwonthegamy = 1
wizzscore = 1
FightMultiball
startB2S(28)
AwardExtraBall
end if
end sub


Sub Trigger001_Hit()
		If activeball.vely < 0 then
if TrainingLevel = 2 and hitdipointy = 1 Then
exit sub
end if
if TrainingLevel = 3 and hitdipointy = 2 Then
exit sub
end if
if TrainingLevel = 4 and hitdipointy = 3 Then
exit sub
end if
if TrainingLevel = 5 and hitdipointy = 4 Then
exit sub
end if
if TrainingLevel = 6 and hitdipointy = 5 Then
exit sub
end if
if TrainingLevel = 7 and hitdipointy = 6 Then
exit sub
end if
if TrainingLevel = 8 and hitdipointy = 7 Then
exit sub
end if
if TrainingLevel = 9 and hitdipointy = 8 Then
exit sub
end if
if TrainingLevel = 10 and hitdipointy = 9 Then
exit sub
end if
if TrainingLevel = 11 and hitdipointy = 10 Then
exit sub
end if
if fightynight => 1 then
hitdipointy = hitdipointy + 1
checkhitdipointy
Ttrigger001on.enabled = true
Trigger001.enabled = false
exit sub
end if
			else
'onder
		end if
End Sub

sub Ttrigger001on_timer
Trigger001.enabled = true
Ttrigger001on.enabled = False
end sub


sub checkhitdipointy
if hitdipointy = 1 Then
DMD "", "", "1gpp", eNone, eNone, eNone, 500, True, ""
end if
if hitdipointy = 2 Then
DMD "", "", "2gpp", eNone, eNone, eNone, 500, True, ""
end if
if hitdipointy = 3 Then
DMD "", "", "3gpp", eNone, eNone, eNone, 500, True, ""
end if
if hitdipointy = 4 Then
DMD "", "", "4gpp", eNone, eNone, eNone, 500, True, ""
end if
if hitdipointy = 5 Then
DMD "", "", "5gpp", eNone, eNone, eNone, 500, True, ""
end if
if hitdipointy = 6 Then
DMD "", "", "6gpp", eNone, eNone, eNone, 500, True, ""
end if
if hitdipointy = 7 Then
DMD "", "", "7gpp", eNone, eNone, eNone, 500, True, ""
end if
if hitdipointy = 8 Then
DMD "", "", "8gpp", eNone, eNone, eNone, 500, True, ""
end if
if hitdipointy = 9 Then
DMD "", "", "9gpp", eNone, eNone, eNone, 500, True, ""
end if
if hitdipointy = 10 Then
DMD "", "", "10gpp", eNone, eNone, eNone, 500, True, ""
end if
if TrainingLevel = 2 and hitdipointy = 1 Then
prehit = 1
Lup012.enabled = False
trigger001.enabled = False
lightyouty
end if
if TrainingLevel = 3 and hitdipointy = 2 Then
prehit = 1
Lup012.enabled = False
trigger001.enabled = False
lightyouty
end if
if TrainingLevel = 4 and hitdipointy = 3 Then
prehit = 1
Lup012.enabled = False
trigger001.enabled = False
lightyouty
end if
if TrainingLevel = 5 and hitdipointy = 4 Then
prehit = 1
Lup012.enabled = False
trigger001.enabled = False
lightyouty
end if
if TrainingLevel = 6 and hitdipointy = 5 Then
prehit = 1
Lup012.enabled = False
trigger001.enabled = False
lightyouty
end if
if TrainingLevel = 7 and hitdipointy = 6 Then
prehit = 1
Lup012.enabled = False
trigger001.enabled = False
lightyouty
end if
if TrainingLevel = 8 and hitdipointy = 7 Then
prehit = 1
Lup012.enabled = False
trigger001.enabled = False
lightyouty
end if
if TrainingLevel = 9 and hitdipointy = 8 Then
prehit = 1
Lup012.enabled = False
trigger001.enabled = False
lightyouty
end if
if TrainingLevel = 10 and hitdipointy = 9 Then
prehit = 1
Lup012.enabled = False
trigger001.enabled = False
lightyouty
end if
if TrainingLevel = 11 and hitdipointy = 10 Then
prehit = 1
Lup012.enabled = False
trigger001.enabled = False
lightyouty
end if
end sub

'right scoop*************

Sub RightScoop_Hit()
	'add dof here
    DOF 130, DOFon
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
    end if
	Scoopbonus = Scoopbonus + 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
	PlaySoundAt "fx_kicker_enter", RightScoop
	If Training2 => 4 then
    DMD "", "", "trainingdone", eNone, eNone, eNone, 1000, True, ""
	vpmTimer.AddTimer 1000, "rightscoopkickout '"
	exit sub
	end if 
	Training2 = Training2 + 1
	checkcore
End Sub

sub rightscoopkickout
'add dof here
    DOF 130, DOFoff
	if training1 = 4 Then
	AddMultiball 1
	training1 = 5
	end if
RightScoop.Kick 225,15
PlaySoundAt "SoundUppercut", RightScoop
DOF 113, DOFPulse
end sub

sub rightscoopkickout2
'add dof here
    DOF 130, DOFoff
RightScoop.Kick 225,15
PlaySoundAt SoundFX("fx_kicker", DOFContactors), RightScoop
end sub

'scoop checks*************

sub checkreps
If training1 = 1 Then
	FlagDir = 5 'set the direction to up
	FlagTimer.Enabled = 1
vpmTimer.AddTimer 2000, "leftscoopkickout '"
firstrep
Playsound "gymbarbell2"
DMD "", "", "repsR", eNone, eNone, eNone, 1000, True, ""
DMD "", "", "1k", eNone, eNone, eNone, 1000, True, ""
exit Sub
end if
If training1 = 2 Then
	FlagDir = 5 'set the direction to up
	FlagTimer.Enabled = 1
vpmTimer.AddTimer 2000, "leftscoopkickout '"
LLeftGreen.state = 1
Playsound "gymbarbell2"
DMD "", "", "repsRE", eNone, eNone, eNone, 1000, True, ""
DMD "", "", "1k", eNone, eNone, eNone, 1000, True, ""
exit Sub
end if
If training1 = 3 Then
	FlagDir = 5 'set the direction to up
	FlagTimer.Enabled = 1
vpmTimer.AddTimer 2000, "leftscoopkickout '"
LLeftRed.state = 1
Playsound "gymbarbell2"
DMD "", "", "repsREP", eNone, eNone, eNone, 1000, True, ""
DMD "", "", "1k", eNone, eNone, eNone, 1000, True, ""
exit Sub
end if
If training1 = 4 Then
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
intraining = intraining + 1
vpmTimer.AddTimer 2000, "checkintrainglevel '"
vpmTimer.AddTimer 6000, "leftscoopkickout '"
LLeftYellow.state = 1
Playsound "TRAININGSLEVEL"
DMD "", "", "repsREPS", eNone, eNone, eNone, 1000, True, ""
DMD "", "", "10k", eNone, eNone, eNone, 1000, True, ""
getready
exit Sub
end if
end Sub

sub firstrep
Lup013.enabled = False
LLeftBlue.state=1
LLeftGreen.state=0
LLeftRed.state=0
LLeftYellow.state=0
end sub           

sub checkcore
If training2 = 1 Then
BagShaker
vpmTimer.AddTimer 2000, "rightscoopkickout '"
firstcore
Playsound "bagwork"
DMD "", "", "spareS", eNone, eNone, eNone, 1000, True, ""
DMD "", "", "1k", eNone, eNone, eNone, 1000, True, ""
exit Sub
end if
If training2 = 2 Then
BagShaker
vpmTimer.AddTimer 2000, "rightscoopkickout '"
LRightGreen.state = 1
Playsound "bagwork"
DMD "", "", "spareSP", eNone, eNone, eNone, 1000, True, ""
DMD "", "", "1k", eNone, eNone, eNone, 1000, True, ""
exit Sub
end if
If training2 = 3 Then
BagShaker
vpmTimer.AddTimer 2000, "rightscoopkickout '"
LRightRed.state = 1
Playsound "bagwork"
DMD "", "", "spareSPA", eNone, eNone, eNone, 1000, True, ""
DMD "", "", "1k", eNone, eNone, eNone, 1000, True, ""
exit Sub
end if
If training2 = 4 Then
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
intraining = intraining + 1
vpmTimer.AddTimer 2000, "checkintrainglevel '"
vpmTimer.AddTimer 6000, "rightscoopkickout '"
LRightYellow.state = 1
Playsound "TRAININGSLEVEL"
DMD "", "", "spareSPAR", eNone, eNone, eNone, 1000, True, ""
DMD "", "", "10k", eNone, eNone, eNone, 1000, True, ""
getready
exit Sub
end if
end Sub

sub firstcore
Lup014.enabled = False
LRightBlue.state=1
LRightGreen.state=0
LRightRed.state=0
LRightYellow.state=0
end sub

'********************** baggyshake

Dim BaggyShake

Sub BagShaker()
    BaggyShake = 6
    BaggyTimer.Enabled = True
End Sub

Sub BaggyTimer_Timer()
    baggys.Transz = BaggyShake / 2
    If BaggyShake = 0 Then Me.Enabled = False:Exit Sub
    If BaggyShake <0 Then
        BaggyShake = ABS(BaggyShake)- 0.1
    Else
        BaggyShake = - BaggyShake + 0.1
    End If
End Sub


sub checkintrainglevel
if intraining = 1 Then
LevelT1
exit sub
end if
if intraining = 2 Then
LevelT2
exit sub
end if
if intraining = 3 Then
LevelT3
exit sub
end if
if intraining = 4 Then
LevelT4
exit sub
end if
if intraining = 5 Then
LevelT5
exit sub
end if
if intraining = 6 Then
LevelT6
exit sub
end if
if intraining = 7 Then
LevelT7
exit sub
end if
if intraining = 8 Then
LevelT8
exit sub
end if
if intraining = 9 Then
LevelT9
exit sub
end if
if intraining = 10 Then
LevelT10
exit sub
end if
if intraining = 11 Then
LevelT11
exit sub
end if
end sub

sub getready
Lup013.enabled = False
Lup014.enabled = False
nextfght = 0
Gtready = 1
pupflasher1.imageA = "Fmatchstart"'"Fprefightcard" 
resettargets
training1 = 4
training2 = 4
LLeftBlue.state = 1
LLeftGreen.state = 1
LLeftRed.state = 1
LLeftYellow.state = 1
LRightBlue.state = 1
LRightGreen.state = 1
LRightRed.state = 1
LRightYellow.state = 1
ArrowLeft.state = 2
ArrowRight.state = 2
end sub

Sub resettargets
Target009.IsDropped = False
Target010.IsDropped = False
Target011.IsDropped = False
Target012.IsDropped = False
Target013.IsDropped = False
Target014.IsDropped = False
Target015.IsDropped = False
Target016.IsDropped = False
Target017.IsDropped = False
Target018.IsDropped = False
PlaySoundAt SoundFXDOF("fx_resetdrop",116,DOFPulse,DOFDropTargets), Target009
end sub

'*****************
' fights ***********************
'*****************

sub StartFight1
'add dof here
startB2S(3)
Lup001.enabled = true
FightMultiball
flashpic = 1
prehit = 1
fightynight = 1
TrainingLevelLock = 1
playsong2
stopTlights
boxingtimer001.enabled =true
end sub

sub StartFight2
'add dof here
startB2S(4)
Lup002.enabled = true
Lup012.enabled = true
trigger001.enabled = True
FightMultiball
flashpic = 2
prehit = 0
fightynight = 2
TrainingLevelLock = 1
playsong2
stopTlights
boxingtimer002.enabled =true
end sub

sub StartFight3
'add dof here
startB2S(5)
Lup003.enabled = true
Lup012.enabled = true
trigger001.enabled = True
FightMultiball
flashpic = 3
prehit = 0
fightynight = 3
TrainingLevelLock = 1
playsong2
stopTlights
boxingtimer003.enabled =true
end sub

sub StartFight4
'add dof here
startB2S(6)
Lup004.enabled = true
Lup012.enabled = true
trigger001.enabled = True
FightMultiball
flashpic = 4
prehit = 0
fightynight = 4
TrainingLevelLock = 1
playsong2
stopTlights
boxingtimer004.enabled =true
end sub

sub StartFight5
'add dof here
startB2S(7)
Lup005.enabled = true
Lup012.enabled = true
trigger001.enabled = True
FightMultiball
flashpic = 5
prehit = 0
fightynight = 5
TrainingLevelLock = 1
playsong2
stopTlights
boxingtimer005.enabled =true
end sub

sub StartFight6
'add dof here
startB2S(8)
Lup006.enabled = true
Lup012.enabled = true
trigger001.enabled = True
FightMultiball
flashpic = 6
prehit = 0
fightynight = 6
TrainingLevelLock = 1
playsong2
stopTlights
boxingtimer006.enabled =true
end sub

sub StartFight7
'add dof here
startB2S(9)
Lup007.enabled = true
Lup012.enabled = true
trigger001.enabled = True
FightMultiball
flashpic = 7
prehit = 0
fightynight = 7
TrainingLevelLock = 1
playsong2
stopTlights
boxingtimer007.enabled =true
end sub

sub StartFight8
'add dof here
startB2S(30)
Lup008.enabled = true
Lup012.enabled = true
trigger001.enabled = True
FightMultiball
flashpic = 8
prehit = 0
fightynight = 8
TrainingLevelLock = 1
playsong2
stopTlights
boxingtimer008.enabled =true
end sub

sub StartFight9
'add dof here
startB2S(11)
Lup009.enabled = true
Lup012.enabled = true
trigger001.enabled = True
FightMultiball
flashpic = 9
prehit = 0
fightynight = 9
TrainingLevelLock = 1
playsong2
stopTlights
boxingtimer009.enabled =true
end sub

sub StartFight10
'add dof here
startB2S(12)
Lup010.enabled = true
Lup012.enabled = true
trigger001.enabled = True
FightMultiball
flashpic = 10
prehit = 0
fightynight = 10
TrainingLevelLock = 1
playsong2
stopTlights
boxingtimer010.enabled =true
end sub

sub StartFight11
'add dof here
Lup011.enabled = true
Lup012.enabled = true
trigger001.enabled = True
FightMultiball
flashpic = 11
prehit = 0
fightynight = 11
TrainingLevelLock = 1
playsong2
onetimy = 0
stopTlights
boxingtimer011.enabled =true
end sub

sub FightMultiball
BallsOnPlayfield = BallsOnPlayfield + 2
bMultiBallMode = True
LeftScoop.CreateSizedball BallSize / 2
RightScoop.CreateSizedball BallSize / 2
end sub

sub releaseFightMultiball
playsound "SoundRingBell"
leftscoopkickout2
vpmTimer.AddTimer 450, "rightscoopkickout2 '"
vpmTimer.AddTimer 900, "topscoopkickout2 '"
end sub

sub fightersnotvisible
boxingtimer001.enabled =False
boxingtimer002.enabled =False
boxingtimer003.enabled =False
boxingtimer004.enabled =False
boxingtimer005.enabled =False
boxingtimer006.enabled =False
boxingtimer007.enabled =False
boxingtimer008.enabled =False
boxingtimer009.enabled =False
boxingtimer010.enabled =False
boxingtimer011.enabled =False
fighter1001.Visible = False
fighter1002.Visible = False
fighter1003.Visible = False
fighter2001.Visible = False
fighter2002.Visible = False
fighter2003.Visible = False
fighter3001.Visible = False
fighter3002.Visible = False
fighter3003.Visible = False
fighter4001.Visible = False
fighter4002.Visible = False
fighter4003.Visible = False
fighter5001.Visible = False
fighter5002.Visible = False
fighter5003.Visible = False
fighter6001.Visible = False
fighter6002.Visible = False
fighter6003.Visible = False
fighter7001.Visible = False
fighter7002.Visible = False
fighter7003.Visible = False
fighter8001.Visible = False
fighter8002.Visible = False
fighter8003.Visible = False
fighter9001.Visible = False
fighter9002.Visible = False
fighter9003.Visible = False
fighter10001.Visible = False
fighter10002.Visible = False
fighter10003.Visible = False
fighter11001.Visible = False
fighter11002.Visible = False
fighter11003.Visible = False
end sub

'*****************
'Spinners + gates
'****************

'spinners*****************


Sub Spinner001_Spin
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
    end if
    PlaySoundAt "fx_spinner", Spinner001
    DOF 119, DOFPulse
	Score(CurrentPlayer) = Score(CurrentPlayer) + (100*PFMultiplier)
End Sub

Sub Spinner002_Spin
	if wizzscore = 1 then
    Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
    end if
	PlaySoundAt "fx_spinner", Spinner002
    DOF 120, DOFPulse
	Score(CurrentPlayer) = Score(CurrentPlayer) + (100*PFMultiplier)
End Sub

'gates*****************


sub Gate_Hit()
checkwichflashernow
End Sub

Sub Gate001_Hit 'stargate
	PlaySoundAt "Star", Gate001
if Pstar = 4 then 
exit sub
end if
if prehit = 1 and fightynight => 1 then
Pstar = Pstar + 1
checkPstar
end if
End Sub

Sub Gate002_Hit 'knockoutgate
    PlaySoundAt "SoundFall", Gate002
if Pknockout = 8 then 
exit sub
end if
if prehit = 1 and fightynight => 1 then
Pknockout = Pknockout + 1
checkPknockout
end if
End Sub

Sub Gate003_Hit' gutpunchgate
    PlaySoundAt "SoundStunnedHit", Gate003
if Pgutpunch = 5 then 
exit sub
end if
if prehit = 1 and fightynight => 1 and activeball.vely < 0 then
Pgutpunch = Pgutpunch + 1
checkPgutpunch
end if
End Sub

Sub Gate005_Hit 'below sccop1
    PlaySoundAt "Soundjab1", Gate005
checkwichflashernow
End Sub

Sub Gate004_Hit 'below sccop1
    PlaySoundAt "Soundjab2", Gate004
checkwichflashernow
End Sub

sub checkPstar
If Pstar = 1 Then
LRB.state = 1
exit Sub
end if
If Pstar = 2 Then
LAG.state = 1
exit Sub
end if
If Pstar = 3 Then
LTR.state = 1
exit Sub
end if
If Pstar = 4 Then
LSY.state = 1
exit Sub
end if
end sub

sub checkPknockout
if Pknockout = 1 then
LKO1.state = 1
exit Sub
end if
if Pknockout = 2 then
LKO2.state = 1
exit Sub
end if
if Pknockout = 3 then
LKO3.state = 1
exit Sub
end if
if Pknockout = 4 then
LKO4.state = 1
exit Sub
end if
if Pknockout = 5 then
LKO5.state = 1
exit Sub
end if
if Pknockout = 6 then
LKO6.state = 1
exit Sub
end if
if Pknockout = 7 then
LKO7.state = 1
exit Sub
end if
if Pknockout = 8 then
LKO8.state = 1
exit Sub
end if
end sub

sub checkPgutpunch
if Pgutpunch = 1 Then
LCenterBlue001.state = 1
exit Sub
end if
if Pgutpunch = 2 Then
LCenterBlue.state = 1
exit Sub
end if
if Pgutpunch = 3 Then
LCenterGreen.state = 1
exit Sub
end if
if Pgutpunch = 4 Then
LCenterRed.state = 1
exit Sub
end if
if Pgutpunch = 5 Then
LCenterYellow.state = 1
exit Sub
end if
end sub

' #####################################
' ###### Flupper Bumper #####
' #####################################

' prepare some global vars to dim/brighten objects when using day-night slider
Dim DayNightAdjust , DNA30, DNA45, DNA90
If NightDay < 10 Then
	DNA30 = 0 : DNA45 = (NightDay-10)/20 : DNA90 = 0 : DayNightAdjust = 0.4
Else
	DNA30 = (NightDay-10)/30 : DNA45 = (NightDay-10)/45 : DNA90 = (NightDay-10)/90 : DayNightAdjust = NightDay/25
End If

Dim FlBumperFadeActual(6), FlBumperFadeTarget(6), FlBumperColor(6), FlBumperTop(6), FlBumperSmallLight(6), Flbumperbiglight(6)
Dim FlBumperDisk(6), FlBumperBase(6), FlBumperBulb(6), FlBumperscrews(6), FlBumperActive(6), FlBumperHighlight(6)
Dim cnt : For cnt = 1 to 6 : FlBumperActive(cnt) = False : Next

' colors available are red, white, blue, orange, yellow, green, purple and blacklight

FlInitBumper 1, "red"

' ### uncomment the statement below to change the color for all bumpers ###
' Dim ind : For ind = 1 to 5 : FlInitBumper ind, "green" : next

Sub FlInitBumper(nr, col)
	FlBumperActive(nr) = True
	' store all objects in an array for use in FlFadeBumper subroutine
	FlBumperFadeActual(nr) = 1 : FlBumperFadeTarget(nr) = 1.1: FlBumperColor(nr) = col
	Set FlBumperTop(nr) = Eval("bumpertop" & nr) : FlBumperTop(nr).material = "bumpertopmat" & nr
	Set FlBumperSmallLight(nr) = Eval("bumpersmalllight" & nr) : Set Flbumperbiglight(nr) = Eval("bumperbiglight" & nr)
	Set FlBumperDisk(nr) = Eval("bumperdisk" & nr) : Set FlBumperBase(nr) = Eval("bumperbase" & nr)
	Set FlBumperBulb(nr) = Eval("bumperbulb" & nr) : FlBumperBulb(nr).material = "bumperbulbmat" & nr
	Set FlBumperscrews(nr) = Eval("bumperscrews" & nr): FlBumperscrews(nr).material = "bumperscrew" & col
	Set FlBumperHighlight(nr) = Eval("bumperhighlight" & nr)
	' set the color for the two VPX lights
	select case col
		Case "red"
			FlBumperSmallLight(nr).color = RGB(255,4,0) : FlBumperSmallLight(nr).colorfull = RGB(255,24,0)
			FlBumperBigLight(nr).color = RGB(255,32,0) : FlBumperBigLight(nr).colorfull = RGB(255,32,0)
			FlBumperHighlight(nr).color = RGB(64,255,0)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.98
			FlBumperSmallLight(nr).TransmissionScale = 0
		Case "blue"
			FlBumperBigLight(nr).color = RGB(32,80,255) : FlBumperBigLight(nr).colorfull = RGB(32,80,255)
			FlBumperSmallLight(nr).color = RGB(0,80,255) : FlBumperSmallLight(nr).colorfull = RGB(0,80,255)
			FlBumperSmallLight(nr).TransmissionScale = 0 : MaterialColor "bumpertopmat" & nr, RGB(8,120,255)
			FlBumperHighlight(nr).color = RGB(255,16,8)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "green"
			FlBumperSmallLight(nr).color = RGB(8,255,8) : FlBumperSmallLight(nr).colorfull = RGB(8,255,8)
			FlBumperBigLight(nr).color = RGB(32,255,32) : FlBumperBigLight(nr).colorfull = RGB(32,255,32)
			FlBumperHighlight(nr).color = RGB(255,32,255) : MaterialColor "bumpertopmat" & nr, RGB(16,255,16) 
			FlBumperSmallLight(nr).TransmissionScale = 0.005
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "orange"
			FlBumperHighlight(nr).color = RGB(255,130,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).color = RGB(255,130,0) : FlBumperSmallLight(nr).colorfull = RGB (255,90,0)
			FlBumperBigLight(nr).color = RGB(255,190,8) : FlBumperBigLight(nr).colorfull = RGB(255,190,8)
		Case "white"
			FlBumperBigLight(nr).color = RGB(255,230,190) : FlBumperBigLight(nr).colorfull = RGB(255,230,190)
			FlBumperHighlight(nr).color = RGB(255,180,100) : 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.99
		Case "blacklight"
			FlBumperBigLight(nr).color = RGB(32,32,255) : FlBumperBigLight(nr).colorfull = RGB(32,32,255)
			FlBumperHighlight(nr).color = RGB(48,8,255) : 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "yellow"
			FlBumperSmallLight(nr).color = RGB(255,230,4) : FlBumperSmallLight(nr).colorfull = RGB(255,230,4)
			FlBumperBigLight(nr).color = RGB(255,240,50) : FlBumperBigLight(nr).colorfull = RGB(255,240,50)
			FlBumperHighlight(nr).color = RGB(255,255,220)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1 
			FlBumperSmallLight(nr).TransmissionScale = 0
		Case "purple"
			FlBumperBigLight(nr).color = RGB(80,32,255) : FlBumperBigLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).color = RGB(80,32,255) : FlBumperSmallLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).TransmissionScale = 0 : 
			FlBumperHighlight(nr).color = RGB(32,64,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
	end select
End Sub

Sub FlFadeBumper(nr, Z)
	FlBumperBase(nr).BlendDisableLighting = 0.5 * DayNightAdjust
'	UpdateMaterial(string, float wrapLighting, float roughness, float glossyImageLerp, float thickness, float edge, float edgeAlpha, float opacity,
'               OLE_COLOR base, OLE_COLOR glossy, OLE_COLOR clearcoat, VARIANT_BOOL isMetal, VARIANT_BOOL opacityActive,
'               float elasticity, float elasticityFalloff, float friction, float scatterAngle) - updates all parameters of a material
	FlBumperDisk(nr).BlendDisableLighting = (0.5 - Z * 0.3 )* DayNightAdjust	

	select case FlBumperColor(nr)

		Case "blue" :
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(38-24*Z,130 - 98*Z,255), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20  + 500 * Z / (0.5 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 5000 * (0.03 * Z +0.97 * Z^3)
			Flbumperbiglight(nr).intensity = 45 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 10000 * (Z^3) / (0.5 + DNA90)

		Case "green"	
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(16 + 16 * sin(Z*3.14),255,16 + 16 * sin(Z*3.14)), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 10 + 150 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 2 * DayNightAdjust + 20 * Z
			FlBumperBulb(nr).BlendDisableLighting = 7 * DayNightAdjust + 6000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 6000 * (Z^3) / (1 + DNA90)
		
		Case "red" 
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 16 - 11*Z + 16 * sin(Z*3.14),0), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 100 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 18 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 20 * DayNightAdjust + 9000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,20 + Z*4,8-Z*8)
		
		Case "orange"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 100 - 22*z  + 16 * sin(Z*3.14),Z*32), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 250 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 2500 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 4000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,100 + Z*50, 0)

		Case "white"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255,230 - 100 * Z, 200 - 150 * Z), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 180 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 5 * DayNightAdjust + 30 * Z
			FlBumperBulb(nr).BlendDisableLighting = 18 * DayNightAdjust + 3000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 14 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z^3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255,255 - 20*Z,255-65*Z) : FlBumperSmallLight(nr).colorfull = RGB(255,255 - 20*Z,255-65*Z)
			MaterialColor "bumpertopmat" & nr, RGB(255,235 - z*36,220 - Z*90)

		Case "blacklight"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 1, RGB(30-27*Z^0.03,30-28*Z^0.01, 255), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 900 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 60 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 30000 * Z^3
			Flbumperbiglight(nr).intensity = 40 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z^3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255-240*(Z^0.1),255 - 240*(Z^0.1),255) : FlBumperSmallLight(nr).colorfull = RGB(255-200*z,255 - 200*Z,255)
			MaterialColor "bumpertopmat" & nr, RGB(255-190*Z,235 - z*180,220 + 35*Z)

		Case "yellow"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 180 + 40*z, 48* Z), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 200 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 40 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 2000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,200, 24 - 24 * z)

		Case "purple" :
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(128-118*Z - 32 * sin(Z*3.14), 32-26*Z ,255), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 15  + 200 * Z / (0.5 + DNA30) 
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 10000 * (0.03 * Z +0.97 * Z^3)
			Flbumperbiglight(nr).intensity = 50 * Z / (1 + DNA45) 
			FlBumperHighlight(nr).opacity = 4000 * (Z^3) / (0.5 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(128-60*Z,32,255)


	end select
End Sub

Sub BumperTimer_Timer
	dim nr
	For nr = 1 to 6
		If FlBumperFadeActual(nr) < FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.8
			If FlBumperFadeActual(nr) > 0.99 Then FlBumperFadeActual(nr) = 1 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
		If FlBumperFadeActual(nr) > FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.4 / (FlBumperFadeActual(nr) + 0.1)
			If FlBumperFadeActual(nr) < 0.01 Then FlBumperFadeActual(nr) = 0 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
	next
End Sub

'' ###################################
'' ###### copy script until here #####
'' ###################################

'DMD animations

sub LevelT0
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl0b", eNone, eNone, eNone, 100, True, ""
end sub

sub LevelT1
    DMD "", "", "tl1a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl1a", eNone, eNone, eNone, 100, True, ""
end sub

sub LevelT2
    DMD "", "", "tl2a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl2a", eNone, eNone, eNone, 100, True, ""
end sub

sub LevelT3
    DMD "", "", "tl3a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl3a", eNone, eNone, eNone, 100, True, ""
end sub


sub LevelT4
    DMD "", "", "tl4a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl4a", eNone, eNone, eNone, 100, True, ""
end sub

sub LevelT5
    DMD "", "", "tl5a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl5a", eNone, eNone, eNone, 100, True, ""
end sub

sub LevelT6
    DMD "", "", "tl6a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl6a", eNone, eNone, eNone, 100, True, ""
end sub

sub LevelT7
    DMD "", "", "tl7a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl7a", eNone, eNone, eNone, 100, True, ""
end sub

sub LevelT8
    DMD "", "", "tl8a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl8a", eNone, eNone, eNone, 100, True, ""
end sub

sub LevelT9
    DMD "", "", "tl9a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl9a", eNone, eNone, eNone, 100, True, ""
end sub

sub LevelT10
    DMD "", "", "tl10a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl10a", eNone, eNone, eNone, 100, True, ""
end sub

sub LevelT11
    DMD "", "", "tl11a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11a", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11b", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "tl11a", eNone, eNone, eNone, 100, True, ""
end sub


'********************Get up******************

dim pushup

sub dopushup()
DMDFlush
startB2S(17)
    DMD "", "", "getupy", eNone, eNone, eNone, 1000, True, ""	
    DMD "", "", "startgetup", eNone, eNone, eNone, 1000, True, ""
	BonusFlippersActive = true
countr5 = 0
	pushup = 0
	StopSong()
	PlaySong "getupmac"
	vpmTimer.AddTimer 2000, "dothepushup1 '"
end sub

sub dothepushup1()
showgetup
flashyTimer1.enabled = true
end sub

sub checkthepushuppy
if timesMup = 3 Then
checkdiepushup1
end if
if timesMup = 2 Then
checkdiepushup2	
end If
if timesMup = 1 Then
checkdiepushup3	
end If
end sub


sub checkdiepushup1
if pushup => 150 Then
stopbonusNOW1
exit Sub
end If
end sub

sub checkdiepushup2
if pushup => 200 Then
stopbonusNOW2
exit Sub
end If
end sub

sub checkdiepushup3
if pushup => 2000 Then
stopbonusNOW3
exit Sub
end If
end sub


sub checkthepushuppy2
if timesMup = 3 Then
stopbonusNOW1	
exit Sub
end if
if timesMup = 2 Then
stopbonusNOW2	
exit Sub
end if
if timesMup = 1 Then
stopbonusNOW3	
exit Sub
end if
end sub

Drain.timerinterval = 700
Sub Drain_Timer
	drain.timerenabled = False
	BonusFlippersActive = False
End Sub


sub stopbonusNOW1
timesMup = 2
drain.timerenabled = True
if pushup => 150 Then
flashyTimer1.enabled = False
DMDFlush
checkwichflashernow
DMD "", "", "youreupagain", eNone, eNone, eNone, 1000, True, ""
Drain.Kick 4,50
Updatethemusico
PlaySoundAt "fx_kicker", Drain
DOF 118, DOFPulse
GetUpLeft.state = 0
Else
vpmTimer.AddTimer 1200, "didnotgetup '"
end if
end sub

sub stopbonusNOW2
timesMup = 1
drain.timerenabled = True
if pushup => 200 Then
flashyTimer1.enabled = False
DMDFlush
checkwichflashernow
DMD "", "", "youreupagain", eNone, eNone, eNone, 1000, True, ""
Drain.Kick 4,50
Updatethemusico
PlaySoundAt "fx_kicker", Drain
DOF 118, DOFPulse
GetUpRight.state = 0
Else
vpmTimer.AddTimer 1200, "didnotgetup '"
end if
end sub

sub stopbonusNOW3
timesMup = 0
drain.timerenabled = True
if pushup => 2000 Then
flashyTimer1.enabled = False
DMDFlush
checkwichflashernow
DMD "", "", "youreupagain", eNone, eNone, eNone, 1000, True, ""
Drain.Kick 4,50
Updatethemusico
PlaySoundAt "fx_kicker", Drain
DOF 118, DOFPulse
Else
vpmTimer.AddTimer 1200, "didnotgetup '"
end if
end sub


sub Updatethemusico
if wizzscore = 1 then
playsong3
exit Sub
end if
if TrainingLevelLock = 1 then 
playsong2
exit Sub
end if
playsong1
end sub

sub didnotgetup
	flashyTimer1.enabled = False
    Drain.DestroyBall
    BallsOnPlayfield = BallsOnPlayfield - 1 
    PlaySoundAt "fx_drain", Drain
    StopEndOfBallMode
    EndOfBall()
'BonusFlippersActive = False
end sub

sub showgetup
DMD "", "", "gu1", eNone, eNone, eNone, 500, True, ""
DMD "", "", "gu2", eNone, eNone, eNone, 500, True, ""
DMD "", "", "gu3", eNone, eNone, eNone, 500, True, ""
DMD "", "", "gu4", eNone, eNone, eNone, 500, True, ""
DMD "", "", "gu5", eNone, eNone, eNone, 500, True, ""
DMD "", "", "gu6", eNone, eNone, eNone, 500, True, ""
DMD "", "", "gu1", eNone, eNone, eNone, 500, True, ""
DMD "", "", "gu2", eNone, eNone, eNone, 500, True, ""
DMD "", "", "gu3", eNone, eNone, eNone, 500, True, ""
DMD "", "", "gu4", eNone, eNone, eNone, 500, True, ""
DMD "", "", "gu5", eNone, eNone, eNone, 500, True, ""
DMD "", "", "gu6", eNone, eNone, eNone, 500, True, ""
DMD "", "", "gu1", eNone, eNone, eNone, 500, True, ""
DMD "", "", "gu2", eNone, eNone, eNone, 500, True, ""
DMD "", "", "gu3", eNone, eNone, eNone, 500, True, ""
DMD "", "", "gu4", eNone, eNone, eNone, 500, True, ""
DMD "", "", "gu5", eNone, eNone, eNone, 500, True, ""
DMD "", "", "gu6", eNone, eNone, eNone, 500, True, ""
DMD "", "", "gu1", eNone, eNone, eNone, 500, True, ""
'DMD "", "", "gu2", eNone, eNone, eNone, 500, True, ""
end sub


Sub flashyTimer1_Timer
		countr5 = countr5 + 1 ': If Countr > 30 then stopbonusNOW : end If
			select case countr5
				case 1 : pupflasher1.imageA = "Fcount1" : PlaySound "gtu1" : checkthepushuppy 
				case 2 : pupflasher1.imageA = "Fcount2" : PlaySound "gtu2" : checkthepushuppy  
				case 3 : pupflasher1.imageA = "Fcount3" : PlaySound "gtu3" : checkthepushuppy  
				case 4 : pupflasher1.imageA = "Fcount4" : PlaySound "gtu4" : checkthepushuppy   
				case 5 : pupflasher1.imageA = "Fcount5" : PlaySound "gtu5" : checkthepushuppy  
				case 6 : pupflasher1.imageA = "Fcount6" : PlaySound "gtu6" : checkthepushuppy  
				case 7 : pupflasher1.imageA = "Fcount7" : PlaySound "gtu7" : checkthepushuppy  
				case 8 : pupflasher1.imageA = "Fcount8" : PlaySound "gtu8" : checkthepushuppy  
				case 9 : pupflasher1.imageA = "Fcount9" : PlaySound "gtu9" : checkthepushuppy  
				case 10 : pupflasher1.imageA = "Fcount10": PlaySound "gtu10" : checkthepushuppy2  
'				case 11 : checkwichflashernow
			end Select
end Sub

'lights players backwall************************************** 
Sub attractLightsupTimer_Timer
		countr8 = countr8 + 1 : If Countr8 > 11 then Countr8 = 1 : end If
			select case countr8
				case 1
LightP001.image = "green"
LightP002.image = "red"
LightP003.image = "red"
LightP004.image = "red"
LightP005.image = "red"
LightP006.image = "red"
LightP007.image = "red"
LightP008.image = "red"
LightP009.image = "red"
LightP010.image = "red"
LightP011.image = "red"
				case 2
LightP001.image = "red"
LightP002.image = "green"
LightP003.image = "red"
LightP004.image = "red"
LightP005.image = "red"
LightP006.image = "red"
LightP007.image = "red"
LightP008.image = "red"
LightP009.image = "red"
LightP010.image = "red"
LightP011.image = "red"
				case 3
LightP001.image = "red"
LightP002.image = "red"
LightP003.image = "green"
LightP004.image = "red"
LightP005.image = "red"
LightP006.image = "red"
LightP007.image = "red"
LightP008.image = "red"
LightP009.image = "red"
LightP010.image = "red"
LightP011.image = "red"
				case 4
LightP001.image = "red"
LightP002.image = "red"
LightP003.image = "red"
LightP004.image = "green"
LightP005.image = "red"
LightP006.image = "red"
LightP007.image = "red"
LightP008.image = "red"
LightP009.image = "red"
LightP010.image = "red"
LightP011.image = "red"
				case 5
LightP001.image = "red"
LightP002.image = "red"
LightP003.image = "red"
LightP004.image = "red"
LightP005.image = "green"
LightP006.image = "red"
LightP007.image = "red"
LightP008.image = "red"
LightP009.image = "red"
LightP010.image = "red"
LightP011.image = "red"
				case 6
LightP001.image = "red"
LightP002.image = "red"
LightP003.image = "red"
LightP004.image = "red"
LightP005.image = "red"
LightP006.image = "green"
LightP007.image = "red"
LightP008.image = "red"
LightP009.image = "red"
LightP010.image = "red"
LightP011.image = "red"
				case 7
LightP001.image = "red"
LightP002.image = "red"
LightP003.image = "red"
LightP004.image = "red"
LightP005.image = "red"
LightP006.image = "red"
LightP007.image = "green"
LightP008.image = "red"
LightP009.image = "red"
LightP010.image = "red"
LightP011.image = "red"
				case 8
LightP001.image = "red"
LightP002.image = "red"
LightP003.image = "red"
LightP004.image = "red"
LightP005.image = "red"
LightP006.image = "red"
LightP007.image = "red"
LightP008.image = "green"
LightP009.image = "red"
LightP010.image = "red"
LightP011.image = "red"
				case 9
LightP001.image = "red"
LightP002.image = "red"
LightP003.image = "red"
LightP004.image = "red"
LightP005.image = "red"
LightP006.image = "red"
LightP007.image = "red"
LightP008.image = "red"
LightP009.image = "green"
LightP010.image = "red"
LightP011.image = "red"
				case 10
LightP001.image = "red"
LightP002.image = "red"
LightP003.image = "red"
LightP004.image = "red"
LightP005.image = "red"
LightP006.image = "red"
LightP007.image = "red"
LightP008.image = "red"
LightP009.image = "red"
LightP010.image = "green"
LightP011.image = "red"
				case 11
LightP001.image = "red"
LightP002.image = "red"
LightP003.image = "red"
LightP004.image = "red"
LightP005.image = "red"
LightP006.image = "red"
LightP007.image = "red"
LightP008.image = "red"
LightP009.image = "red"
LightP010.image = "red"
LightP011.image = "green"
			end Select
end Sub

'lights players backwall fighting mode************************************** 

Sub Lup001_Timer
		countr10 = countr10 + 1 : If countr10 > 2 then countr10 = 1 : end If
			select case countr10
				case 1 : LightP001.image = "green" 
				case 2 : LightP001.image = "red" 
			end Select
end Sub

Sub Lup002_Timer
		countr11 = countr11 + 1 : If countr11 > 2 then countr11 = 1 : end If
			select case countr11
				case 1 : LightP002.image = "green" 
				case 2 : LightP002.image = "red" 
			end Select
end Sub

Sub Lup003_Timer
		countr12 = countr12 + 1 : If countr12 > 2 then countr12 = 1 : end If
			select case countr12
				case 1 : LightP003.image = "green" 
				case 2 : LightP003.image = "red" 
			end Select
end Sub

Sub Lup004_Timer
		countr13 = countr13 + 1 : If countr13 > 2 then countr13 = 1 : end If
			select case countr13
				case 1 : LightP004.image = "green" 
				case 2 : LightP004.image = "red" 
			end Select
end Sub

Sub Lup005_Timer
		countr14 = countr14 + 1 : If countr14 > 2 then countr14 = 1 : end If
			select case countr14
				case 1 : LightP005.image = "green" 
				case 2 : LightP005.image = "red" 
			end Select
end Sub

Sub Lup006_Timer
		countr15 = countr15 + 1 : If countr15 > 2 then countr15 = 1 : end If
			select case countr15
				case 1 : LightP006.image = "green" 
				case 2 : LightP006.image = "red" 
			end Select
end Sub

Sub Lup007_Timer
		countr16 = countr16 + 1 : If countr16 > 2 then countr16 = 1 : end If
			select case countr16
				case 1 : LightP007.image = "green" 
				case 2 : LightP007.image = "red" 
			end Select
end Sub

Sub Lup008_Timer
		countr17 = countr17 + 1 : If countr17 > 2 then countr17 = 1 : end If
			select case countr17
				case 1 : LightP008.image = "green" 
				case 2 : LightP008.image = "red" 
			end Select
end Sub

Sub Lup009_Timer
		countr18 = countr18 + 1 : If countr18 > 2 then countr18 = 1 : end If
			select case countr18
				case 1 : LightP009.image = "green" 
				case 2 : LightP009.image = "red" 
			end Select
end Sub

Sub Lup010_Timer
		countr19 = countr19 + 1 : If countr19 > 2 then countr19 = 1 : end If
			select case countr19
				case 1 : LightP010.image = "green" 
				case 2 : LightP010.image = "red" 
			end Select
end Sub

Sub Lup011_Timer
		countr20 = countr20 + 1 : If countr20 > 2 then countr20 = 1 : end If
			select case countr20
				case 1 : LightP011.image = "green" 
				case 2 : LightP011.image = "red" 
			end Select
end Sub

' LIGHTS GAMEPLAY********************************

Sub Lup012_Timer
		countr21 = countr21 + 1 : If countr21 > 5 then countr21 = 1 : end If
			select case countr21
				case 1 : LCenterBlue001.state=1:LCenterBlue.state=0:LCenterGreen.state=0:LCenterRed.state=0:LCenterYellow.state=0
				case 2 : LCenterBlue001.state=0:LCenterBlue.state=1:LCenterGreen.state=0:LCenterRed.state=0:LCenterYellow.state=0
				case 3 : LCenterBlue001.state=0:LCenterBlue.state=0:LCenterGreen.state=1:LCenterRed.state=0:LCenterYellow.state=0
				case 4 : LCenterBlue001.state=0:LCenterBlue.state=0:LCenterGreen.state=0:LCenterRed.state=1:LCenterYellow.state=0
				case 5 : LCenterBlue001.state=0:LCenterBlue.state=0:LCenterGreen.state=0:LCenterRed.state=0:LCenterYellow.state=1
			end Select
end Sub

Sub Lup013_Timer
		countr22 = countr22 + 1 : If countr22 > 4 then countr22 = 1 : end If
			select case countr22
				case 1 : LLeftBlue.state=1:LLeftGreen.state=0:LLeftRed.state=0:LLeftYellow.state=0
				case 2 : LLeftBlue.state=0:LLeftGreen.state=1:LLeftRed.state=0:LLeftYellow.state=0
				case 3 : LLeftBlue.state=0:LLeftGreen.state=0:LLeftRed.state=1:LLeftYellow.state=0
				case 4 : LLeftBlue.state=0:LLeftGreen.state=0:LLeftRed.state=0:LLeftYellow.state=1
			end Select
end Sub

Sub Lup014_Timer
		countr23 = countr23 + 1 : If countr23 > 4 then countr23 = 1 : end If
			select case countr23
				case 1 : LRightBlue.state=1:LRightGreen.state=0:LRightRed.state=0:LRightYellow.state=0
				case 2 : LRightBlue.state=0:LRightGreen.state=1:LRightRed.state=0:LRightYellow.state=0
				case 3 : LRightBlue.state=0:LRightGreen.state=0:LRightRed.state=1:LRightYellow.state=0
				case 4 : LRightBlue.state=0:LRightGreen.state=0:LRightRed.state=0:LRightYellow.state=1
			end Select
end Sub

sub checkwichflashernow
if nextfght = 1 Then
	pupflasher1.imageA = "Fnext" 
'startB2S(1) 'attract
end if
if TrainingLevel = 0 then
	pupflasher1.imageA = "Ffirst" 
startB2S(29)  'before first match
forballlgameover
end if
if Gtready = 1 Then
	pupflasher1.imageA = "Fprefightcard" 
'startB2S(1) 'attract 
end if
if flashpic = 1 then
startB2S(3) 'Fight 1
pupflasher1.imageA = "Ffight1" 
end if
if flashpic = 2 then
startB2S(4) 'Fight 2
pupflasher1.imageA = "Ffight2" 
end if
if flashpic = 3 then
startB2S(5) 'Fight 3
pupflasher1.imageA = "Ffight3" 
end if
if flashpic = 4 then
startB2S(6) 'Fight 4
pupflasher1.imageA = "Ffight4" 
end if
if flashpic = 5 then
startB2S(7) 'Fight 5
pupflasher1.imageA = "Ffight5" 
end if
if flashpic = 6 then
startB2S(8) 'Fight 6
pupflasher1.imageA = "Ffight6" 
end if
if flashpic = 7 then
startB2S(9) 'Fight 7
pupflasher1.imageA = "Ffight7" 
end if
if flashpic = 8 then
startB2S(30) 'Fight 8
pupflasher1.imageA = "Ffight8" 
end if
if flashpic = 9 then
startB2S(11) 'Fight 9
pupflasher1.imageA = "Ffight9" 
end if
if flashpic = 10 then
startB2S(12) 'Fight 10
pupflasher1.imageA = "Ffight10" 
end if
if flashpic = 11 then
startB2S(13) 'Fight 11
pupflasher1.imageA = "Ffight11" 
end if
if flashpic = 12 then
startB2S(18) 'Won Fight 1
end if
if flashpic = 13 then
startB2S(19) 'Won Fight 2
end if
if flashpic = 14 then
startB2S(20) 'Won Fight 3
end if
if flashpic = 15 then
startB2S(21) 'Won Fight 4
end if
if flashpic = 16 then
startB2S(22) 'Won Fight 5
end if
if flashpic = 17 then
startB2S(23) 'Won Fight 6
end if
if flashpic = 18 then
startB2S(24) 'Won Fight 7
end if
if flashpic = 19 then
startB2S(25) 'Won Fight 8
end if
if flashpic = 20 then
startB2S(26) 'Won Fight 9
end if
if flashpic = 21 then
startB2S(27) 'Won Fight 10 
end if
if flashpic = 22 then
startB2S(28) 'Won Fight 11
end if
end sub

sub resetafterfight
	Lup013.enabled = true
	Lup014.enabled = true
    goforit = 0
	TrainingLevelLock = 0
	GtreadyCount = 0
	nextfght = 1
	training1 = 0
	training2 = 0
	fightynight = 0
	fightersnotvisible
	prehit = 0
	hitdipointy = 0
	Pstar = 0
	flashpic = 0
	Pgutpunch = 0
	Pknockout = 0
	LLeftBlue.state = 0
	LLeftGreen.state = 0
	LLeftRed.state = 0
	LLeftYellow.state = 0
	LRightBlue.state = 0
	LRightGreen.state = 0
	LRightRed.state = 0
	LRightYellow.state = 0
	UpperRightKickerLight.state = 0
	UpperLeftKickerLight.state = 0
LKO1.state = 0
LKO2.state = 0
LKO3.state = 0
LKO4.state = 0
LKO5.state = 0
LKO6.state = 0
LKO7.state = 0
LKO8.state = 0
LCenterBlue001.state = 0
LCenterBlue.state = 0
LCenterGreen.state = 0
LCenterRed.state = 0
LCenterYellow.state = 0
LRB.state = 0
LAG.state = 0
LTR.state = 0
LSY.state = 0
end Sub

sub forballlgameover
Lup001.enabled = false
Lup002.enabled = false
Lup003.enabled = false
Lup004.enabled = false
Lup005.enabled = false
Lup006.enabled = false
Lup007.enabled = false
Lup008.enabled = false
Lup009.enabled = false
Lup010.enabled = false
Lup011.enabled = false
Ttrigger001on.enabled = False
Trigger001.enabled = False
LightP001.image = "red"
LightP002.image = "red"
LightP003.image = "red"
LightP004.image = "red"
LightP005.image = "red"
LightP006.image = "red"
LightP007.image = "red"
LightP008.image = "red"
LightP009.image = "red"
LightP010.image = "red"
LightP011.image = "red"
end sub

sub lightyouty
LCenterBlue001.state = 0
LCenterBlue.state = 0
LCenterGreen.state = 0
LCenterRed.state = 0
LCenterYellow.state = 0
end sub

'*************
'VR Stuff
'*************
Dim VRRoom
DIM VRThings

	If RenderingMode = 2 or VRTest Then	
		'Move DMD to correct location
		Digit36.rotx = -86
		Digit36.x = Digit36.x + 835
		Digit36.y = Digit36.y - 420
		Digit36.height = Digit36.height + 90
		Dim VrObj
		For Each VrObj in DMDUpper
			VrObj.rotx = -86
			VrObj.x = VrObj.x +835
			VrObj.y = VrObj.y - 420
			VrObj.height = VrObj.height + 107 
		Next
		For Each VrObj in DMDLower
			VrObj.rotx = -86
			VrObj.x = VrObj.x + 835
			VrObj.y = VrObj.y - 425
			VrObj.height = VrObj.height + 82 
		Next
    End If
Sub LoadVRRoom
	for each VRThings in VR_Cab:VRThings.visible = 0:Next
	for each VRThings in VR_Min:VRThings.visible = 0:Next
	for each VRThings in VR_Mega:VRThings.visible = 0:Next
		Ramp17.visible = 1
		Ramp16.visible = 1
		Ramp15.visible = 1
		Wall001.sidevisible = 1
		Wall002.sidevisible = 1
		'Primitive012.visible = 1
		'Primitive013.visible = 1
		'VR_weights1.visible = 0
		'VR_weights2.visible = 0
		VR_LeftBlade.sidevisible = 0
		VR_RightBlade.sidevisible = 0
		VR_Lockdownbar2.visible = 0
	If RenderingMode = 2 or VRTest Then
		VRRoom = VRRoomChoice
		'disable table objects that should not be visible
		Ramp17.visible = 0
		Ramp16.visible = 0
		Ramp15.visible = 0
		Wall001.sidevisible = 0
		Wall002.sidevisible = 0
		'Primitive012.visible = 0
		'Primitive013.visible = 0
		'VR_weights1.visible = 1
		'VR_weights2.visible = 1
		VR_LeftBlade.sidevisible = 1
		VR_RightBlade.sidevisible = 1
		VR_Lockdownbar2.visible = 1
	Else
		VRRoom = 0
	End If
	If VRRoom = 1 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		for each VRThings in VR_Mega:VRThings.visible = 0:Next
	End If
	If VRRoom = 2 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 1:Next
		for each VRThings in VR_Mega:VRThings.visible = 0:Next
	End If
	If VRRoom = 3 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		for each VRThings in VR_Mega:VRThings.visible = 1:Next
	End If
End Sub

'********************************fighters 3d animated

'**fighter1**

Sub boxingtimer001_Timer
countr30 = countr30 + 1 : If Countr30 > 4 then Countr30 = 1 : end If
select case countr30
case 1 : Fighter1001.visible=true:Fighter1002.visible=False:Fighter1003.visible=False
case 2 : Fighter1001.visible=False:Fighter1002.visible=True:Fighter1003.visible=False
case 3 : Fighter1001.visible=False:Fighter1002.visible=False:Fighter1003.visible=True
case 4 : Fighter1001.visible=False:Fighter1002.visible=True:Fighter1003.visible=False
end Select
End Sub 

'**fighter2**

Sub boxingtimer002_Timer
countr31 = countr31 + 1 : If Countr31 > 4 then Countr31 = 1 : end If
select case countr31
case 1 : Fighter2001.visible=true:Fighter2002.visible=False:Fighter2003.visible=False
case 2 : Fighter2001.visible=False:Fighter2002.visible=True:Fighter2003.visible=False
case 3 : Fighter2001.visible=False:Fighter2002.visible=False:Fighter2003.visible=True
case 4 : Fighter2001.visible=False:Fighter2002.visible=True:Fighter2003.visible=False
end Select
End Sub 

'**fighter3**

Sub boxingtimer003_Timer
countr32 = countr32 + 1 : If Countr32 > 4 then Countr32 = 1 : end If
select case countr32
case 1 : Fighter3001.visible=true:Fighter3002.visible=False:Fighter3003.visible=False
case 2 : Fighter3001.visible=False:Fighter3002.visible=True:Fighter3003.visible=False
case 3 : Fighter3001.visible=False:Fighter3002.visible=False:Fighter3003.visible=True
case 4 : Fighter3001.visible=False:Fighter3002.visible=True:Fighter3003.visible=False
end Select
End Sub 

'**fighter4**

Sub boxingtimer004_Timer
countr33 = countr33 + 1 : If Countr33 > 4 then Countr33 = 1 : end If
select case countr33
case 1 : Fighter4001.visible=true:Fighter4002.visible=False:Fighter4003.visible=False
case 2 : Fighter4001.visible=False:Fighter4002.visible=True:Fighter4003.visible=False
case 3 : Fighter4001.visible=False:Fighter4002.visible=False:Fighter4003.visible=True
case 4 : Fighter4001.visible=False:Fighter4002.visible=True:Fighter4003.visible=False
end Select
End Sub 

'**fighter5**

Sub boxingtimer005_Timer
countr34 = countr34 + 1 : If Countr34 > 4 then Countr34 = 1 : end If
select case countr34
case 1 : Fighter5001.visible=true:Fighter5002.visible=False:Fighter5003.visible=False
case 2 : Fighter5001.visible=False:Fighter5002.visible=True:Fighter5003.visible=False
case 3 : Fighter5001.visible=False:Fighter5002.visible=False:Fighter5003.visible=True
case 4 : Fighter5001.visible=False:Fighter5002.visible=True:Fighter5003.visible=False
end Select
End Sub 

'**fighter6**

Sub boxingtimer006_Timer
countr35 = countr35 + 1 : If Countr35 > 4 then Countr35 = 1 : end If
select case countr35
case 1 : Fighter6001.visible=true:Fighter6002.visible=False:Fighter6003.visible=False
case 2 : Fighter6001.visible=False:Fighter6002.visible=True:Fighter6003.visible=False
case 3 : Fighter6001.visible=False:Fighter6002.visible=False:Fighter6003.visible=True
case 4 : Fighter6001.visible=False:Fighter6002.visible=True:Fighter6003.visible=False
end Select
End Sub 

'**fighter7**

Sub boxingtimer007_Timer
countr36 = countr36 + 1 : If Countr36 > 4 then Countr36 = 1 : end If
select case countr36
case 1 : Fighter7001.visible=true:Fighter7002.visible=False:Fighter7003.visible=False
case 2 : Fighter7001.visible=False:Fighter7002.visible=True:Fighter7003.visible=False
case 3 : Fighter7001.visible=False:Fighter7002.visible=False:Fighter7003.visible=True
case 4 : Fighter7001.visible=False:Fighter7002.visible=True:Fighter7003.visible=False
end Select
End Sub 

'**fighter8**

Sub boxingtimer008_Timer
countr37 = countr37 + 1 : If Countr37 > 4 then Countr37 = 1 : end If
select case countr37
case 1 : Fighter8001.visible=true:Fighter8002.visible=False:Fighter8003.visible=False
case 2 : Fighter8001.visible=False:Fighter8002.visible=True:Fighter8003.visible=False
case 3 : Fighter8001.visible=False:Fighter8002.visible=False:Fighter8003.visible=True
case 4 : Fighter8001.visible=False:Fighter8002.visible=True:Fighter8003.visible=False
end Select
End Sub 

'**fighter9**

Sub boxingtimer009_Timer
countr38 = countr38 + 1 : If Countr38 > 4 then Countr38 = 1 : end If
select case countr38
case 1 : Fighter9001.visible=true:Fighter9002.visible=False:Fighter9003.visible=False
case 2 : Fighter9001.visible=False:Fighter9002.visible=True:Fighter9003.visible=False
case 3 : Fighter9001.visible=False:Fighter9002.visible=False:Fighter9003.visible=True
case 4 : Fighter9001.visible=False:Fighter9002.visible=True:Fighter9003.visible=False
end Select
End Sub 

'**fighter10**

Sub boxingtimer010_Timer
countr39 = countr39 + 1 : If Countr39 > 4 then Countr39 = 1 : end If
select case countr39
case 1 : Fighter10001.visible=true:Fighter10002.visible=False:Fighter10003.visible=False
case 2 : Fighter10001.visible=False:Fighter10002.visible=True:Fighter10003.visible=False
case 3 : Fighter10001.visible=False:Fighter10002.visible=False:Fighter10003.visible=True
case 4 : Fighter10001.visible=False:Fighter10002.visible=True:Fighter10003.visible=False
end Select
End Sub 

'**fighter11**

Sub boxingtimer011_Timer
countr40 = countr40 + 1 : If Countr40 > 4 then Countr40 = 1 : end If
select case countr40
case 1 : Fighter11001.visible=true:Fighter11002.visible=False:Fighter11003.visible=False
case 2 : Fighter11001.visible=False:Fighter11002.visible=True:Fighter11003.visible=False
case 3 : Fighter11001.visible=False:Fighter11002.visible=False:Fighter11003.visible=True
case 4 : Fighter11001.visible=False:Fighter11002.visible=True:Fighter11003.visible=False
end Select
End Sub 


'*****************
' Flag
'*****************

Dim FlagDir
FlagDir = 5 'this is both the direction, if + goes up, if - goes down, and also the speed

Sub FlagTimer_Timer
    VR_Weights1.z = VR_Weights1.z + FlagDir
    If VR_Weights1.z > 110 Then FlagDir = - 5 'goes down
    If VR_Weights1.z < 60 Then FlagTimer.Enabled = 0
End Sub