' ****************************************************************
'                       VISUAL PINBALL X
'                		South Park Pinball
'                       Version 1.0.0
'						started 27-06-2021
' ****************************************************************

'DOF Some Config by Outhere
'101 Left Flipper
'102 Right Flipper
'103 Left Slingshot
'104 
'105 Right Slingshot
'106 
'107 Bumper Left
'108 Bumper Center
'109 Bumper Right
'110 Kicker001
'111 Reset drop Targets
'112 Church Kicker
'113 Shaker = Gate002 and Gate003
'114 Kick the Baby KickBack Right
'115 Kick the Baby kickBack Left
'116 
'117 
'118 
'119 
'120 Auto Plunger
'121 
'122 Knocker
'123 BallRelease

Option Explicit
Randomize

Const BallSize = 50    ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1
Const SongVolume = 0.1 ' 1 is full volume. Value is from 0 to 1

' Load the core.vbs for supporting Subs and functions
LoadCoreFiles

Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "Can't open controller.vbs"
    On Error Goto 0
End Sub

' Define any Constants
Const cGameName = "SPP"
Const TableName = "SPP"
Const myVersion = "1.0.0"
Const MaxPlayers = 4     ' from 1 to 4
Const BallSaverTime = 20 ' in seconds
Const MaxMultiplier = 3  ' limit to 3x in this game, both bonus multiplier and playfield multiplier
Const BallsPerGame = 5   ' usually 3 or 5
Const MaxMultiballs = 4  ' max number of balls during multiballs

Const Special1 = 10000000  ' High score to obtain an extra ball/game
Const Special2 = 20000000
Const Special3 = 30000000

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
	Controller.B2SSetData 31,0
	Controller.B2SSetData 32,0
	Controller.B2SSetData 33,0
	Controller.B2SSetData 34,0
	Controller.B2SSetData 35,0
	Controller.B2SSetData 36,0
	Controller.B2SSetData 37,0
	Controller.B2SSetData 38,0
	Controller.B2SSetData 39,0
	Controller.B2SSetData 40,0
	Controller.B2SSetData 41,0
	Controller.B2SSetData 42,0
	Controller.B2SSetData 43,0
	Controller.B2SSetData 44,0
	Controller.B2SSetData 45,0
	Controller.B2SSetData 46,0
	Controller.B2SSetData 47,0
	Controller.B2SSetData 48,0
	Controller.B2SSetData 49,0
	Controller.B2SSetData 50,0
	Controller.B2SSetData 51,0
	Controller.B2SSetData 52,0
	Controller.B2SSetData 53,0
	Controller.B2SSetData 54,0
	Controller.B2SSetData 55,0
	Controller.B2SSetData 56,0
	Controller.B2SSetData 57,0
	Controller.B2SSetData 58,0
	Controller.B2SSetData 59,0
	Controller.B2SSetData 60,0
	Controller.B2SSetData aB2S,1
	End If
End Sub

' Use FlexDMD if in FS mode
Dim UseFlexDMD
If Table1.ShowDT = True then
    UseFlexDMD = False
Else
    UseFlexDMD = True
End If

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
dim PFMultiplier
dim SPMultiplier
dim COWMultiplier
dim SPlights
dim tempbonusaf
dim kennymissionmodes
dim schoolmissionmodes
dim musicmodes
dim mode1TimerCount
dim schoolsout
dim SPMaultiplier
dim SPkMultiplier
dim KillKenny
Dim PlayerSelectActive
Dim PlayerSelectActive2
Dim SlotAward
Dim SlotAward2
Dim SelectCounter
dim SelectCounter2
Dim SlotValue
Dim SlotValue2
dim p1
dim p2
dim p3
dim p4
dim p5
dim p6
dim p7
dim p8
dim p9
dim p10
dim p11
dim p12
dim p13
dim p14
dim p15
dim p16
dim p17
dim p18
dim p19
dim p20
dim p21
dim p22
dim p23
dim p24
dim p25
dim p26
dim p27
dim p28
dim p29
dim p30
dim p31
dim p32
dim p33
dim p34
dim p35
dim p36
dim p37
dim timmyt
dim galt
dim SchoolSlot
dim ter1a
dim ter1b
dim ter2a
dim ter2b
dim ter3a
dim ter3b
dim bosssaddam
dim bossclitty
dim bosskim
dim bhit
dim bvul
dim bhurt
dim bosshitler
dim bossosama
dim bossclitoris
dim countr34
dim kickbabysonplay
dim csongs
dim Kcompleted
dim SMcompleted
dim Scompleted
dim Bcompleted
dim hitgate3
dim hitgate2
dim heavenjesus
dim heavensatan
dim churgepoints
dim weedpoints
dim TGW
dim mrhan
dim churchlocked
dim countr11
dim weedultimate
dim charcomplete
dim BonusDelayTime

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

'MovingTarget_Init

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
    bMusicOn = True
    BallsOnPlayfield = 0
	bMultiBallMode = False
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
Dim PullBack2:PullBack2 = 0
Sub GameTimer_Timer
    RollingUpdate
	checkmissionupdateschool
    ' add any other real time update subs, like gates or diverters
    FlipperLSh.Rotz = LeftFlipper.CurrentAngle
    FlipperRSh.Rotz = RightFlipper.CurrentAngle

    If Plunger.Position >= 4.2 Then
        PullBack2 = 1
        kanon.TransY = Plunger.Position * 3
    End If
    If Plunger.Position < 4.2 and BallShot = 1 Then
    If PullBack2 = 1 Then
            PullBack2 = 0
            kanonnormal()
    End If
end if
End Sub

sub checkmissionupdateschool
	if schoolmissionmodes = 0 then
		li086.state = 2
	end if
	if schoolmissionmodes = 2 then
		li086.state = 1
	end if
end sub

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
                DMD "_", CL(1, "CREDITS: " & Credits), "", eNone, eNone, eNone, 500, True, "coinsp"
            End If
        End If
    End If

    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySound "plungersp"', plunger
        'PlaySoundAt "fx_reload", plunger
    End If

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

    If PlayerSelectActive Then
        SelectPlayerStart(Keycode)
        Exit Sub
    End If

    If PlayerSelectActive2 Then
        SelectPlayerStart2(Keycode)
        Exit Sub
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
							PlaySound "NOCOIN"
                    End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = True) Then
                    If(BallsOnPlayfield = 0) Then
                        ResetForNewGame()
						UpdateMusicNow
                    End If
                Else
                    If(Credits> 0) Then
                        If(BallsOnPlayfield = 0) Then
                            Credits = Credits - 1
                            If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
                            ResetForNewGame()
							UpdateMusicNow
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, "so_nocredits"
						PlaySound "NOCOIN"
					End If
                End If
            End If
    End If ' If (GameInPlay)

'table keys
'If keycode = RightMagnaSave or keycode = LeftMagnasave Then ShowPost 
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If keycode = PlungerKey Then
        Plunger.Fire
        PlaySound "balllaunchsp"', plunger
        'If bBallInPlungerLane Then PlaySoundAt "fx_fire", plunger
    End If

    If hsbModeActive Then
        Exit Sub
    End If

    ' Table specific

    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then
            SolLFlipper 0
        End If
        If keycode = RightFlipperKey Then
            SolRFlipper 0
        End If
    End If
End Sub

sub kanonnormal()
	kanon.TransY = 0
end sub

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
        PlaySoundAt SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToEnd
'		Flipper1.RotateToEnd 'Adds To End Movement for Flipper1
		RotateLaneLightsLeft
		RotateLaneLightsLeft2
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
'		Flipper1.RotateToStart 'Adds To End Movement for Flipper1
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
		RotateLaneLightsRight
		RotateLaneLightsRight2
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

Sub RotateLaneLightsLeft2
    Dim TempState
    TempState = li073.State
    li073.State = li074.State
    li074.State = li075.State
    li075.state = TempState
End Sub

Sub RotateLaneLightsRight2
    Dim TempState
    TempState = li075.State
    li075.State = li074.State
    li074.State = li073.State
    li073.state = TempState
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
        DMD "", "", "dmdtilt", eNone, eNone, eNone, 200, False, ""
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
'       Bumper1.Force = 0
'       Bumper2.Force = 0
'		Bumper3.Force = 0
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
'        Bumper1.Force = 8
'        Bumper2.Force = 8
'		Bumper3.Force = 8
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

Dim Song, UpdateMusic
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

Sub ChangeSong
    If(BallsOnPlayfield = 0)Then
        PlaySong "M_end"
        Exit Sub
    End If

    If bAttractMode Then
        PlaySong "M_end"
        Exit Sub
    End If
    If bMultiBallMode Then
        PlaySong "MULTI"
    Else
        UpdateMusicNow
    end if
End Sub

'if you add more balls to the game use changesong then if bMultiBallMode = true, your multiball song will be played.

Sub UpdateMusicNow
    Select Case UpdateMusic
        Case 0:PlaySong "1"
        Case 1:PlaySong "S1"
        Case 2:PlaySong "S2"
        Case 3:PlaySong "S3"
        Case 4:PlaySong "S4"
        Case 5:PlaySong "S5"
        Case 6:PlaySong "S6"
        Case 7:PlaySong "S7"
        Case 8:PlaySong "S8"
        Case 9:PlaySong "S9"
        Case 10:PlaySong "S10"
        Case 11:PlaySong "S11"
        Case 12:PlaySong "S12"
        Case 13:PlaySong "S_saddam"
        Case 14:PlaySong "st"
        Case 15:PlaySong "sh"
        Case 16:PlaySong "so"
        Case 17:PlaySong "M_end"
		case 18:Playsong "TEGRIDY2"
		case 19:Playsong "battlemusic"
		case 20:Playsong "bclit"
		case 21:Playsong "kj"
    End Select
end sub

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

Sub Pin003_hit()
Playsound "fx_rubber_pin"
end sub 

Sub Pin004_hit()
Playsound "fx_rubber_pin"
end sub

Sub Rubber003_hit()
Playsound "fx_rubber2"
end sub

'********************
' Play random quotes
'********************

Sub PlayQuoteWendy
    Dim tmp
    tmp = INT(RND * 10) + 1
    PlaySound "Wen_" &tmp
End Sub

Sub PlayQuoteBarbrady
    Dim tmp
    tmp = INT(RND * 10) + 1
    PlaySound "Bar_" &tmp
End Sub

Sub PlayQuoteButters
    Dim tmp
    tmp = INT(RND * 10) + 1
    PlaySound "But_" &tmp
End Sub

Sub PlayQuoteGaridson
    Dim tmp
    tmp = INT(RND * 10) + 1
    PlaySound "Gar_" &tmp
End Sub

Sub PlayQuoteJimbo
    Dim tmp
    tmp = INT(RND * 10) + 1
    PlaySound "Jim_" &tmp
End Sub

Sub PlayQuoteNed
    Dim tmp
    tmp = INT(RND * 10) + 1
    PlaySound "Ned_" &tmp
End Sub

Sub PlayQuoteTowely
    Dim tmp
    tmp = INT(RND * 10) + 1
    PlaySound "Tow_" &tmp
End Sub

Sub PlayQuoteSelly
    Dim tmp
    tmp = INT(RND * 10) + 1
    PlaySound "Sel_" &tmp
End Sub

Sub PlayQuoteMackey
    Dim tmp
    tmp = INT(RND * 10) + 1
    PlaySound "Mac_" &tmp
End Sub

Sub PlayQuoteMephisto
    Dim tmp
    tmp = INT(RND * 10) + 1
    PlaySound "mep_" &tmp
End Sub

Sub PlayQuoteSatan
    Dim tmp
    tmp = INT(RND * 6) + 1
    PlaySound "satan_" &tmp
End Sub

Sub PlayQuoteJesus
    Dim tmp
    tmp = INT(RND * 6) + 1
    PlaySound "jesus_" &tmp
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
'   JP's VP10 Rolling Sounds + Ballshadow
' uses a collection of shadows, aBallShadow
'********************************************

Const tnob = 20 ' total number of balls
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

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y

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

'Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
'Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
'Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
'Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
'Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
'Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
'Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

Sub RHelp1_Hit()
    StopSound "fx_metalrolling"
    PlaySoundAtBall "fx_ballrampdrop"
End Sub

Sub RHelp2_Hit()
    StopSound "fx_metalrolling"
    PlaySoundAtBall"fx_ballrampdrop"
End Sub


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

'walls for moving target off
'wallALLOFF
disableWalls1
disableWalls2
resettargett2
resettargett1
resetikes
shot001timer.enabled = False
shot002timer.enabled = False
churchy.Image = "churchy"
stopsongquests

'reset variable and Lights
PFMultiplier = 1
SPMultiplier = 1
COWMultiplier = 1
kennymissionmodes = 1
schoolmissionmodes = 1
musicmodes = 1
schoolsout = 0
SPlights = 1
SPMaultiplier = 1
SPkMultiplier = 1
KillKenny = 1
SlotValue = 0
SlotValue2 = 0
SelectCounter = 0
SelectCounter2 = 0
Flag.image = "spflagkopie"
Flagdown.Enabled = 1
'SlotAward = Array("dmdkb", "dmdt1", "dmdk2", "dmdt2", "dmdk1", "dmdt3", "dmdk3", "dmds1", "dmds2", "dmds3", "dmds4", "dmd5", "dmds6", "dmds7", "dmds8", "dmds9", "dmds10", "dmds11", "dmds12")
p1 = 0
p2 = 0
p3 = 1
p4 = 1
p5 = 1
p6 = 1
p7 = 1
p8 = 0
p9 = 0
p10 = 0
p11 = 0
p12 = 0
p13 = 0
p14 = 0
p15 = 0
p16 = 0
p17 = 0
p18 = 0
p19 = 0
p20 = 0
p21 = 0
p22 = 0
p23 = 0
p24 = 0
p25 = 0
p26 = 0
p27 = 0
p28 = 0
p29 = 0
p30 = 0
p31 = 0
p32 = 0
p33 = 0
p34 = 0
p35 = 0
p36 = 1
p37 = 1
timmyt = 1
galt = 1
SchoolSlot = 0
ter1a = 5
ter1b = 5
ter2a = 5
ter2b = 5
ter3a = 5
ter3b = 5
bosssaddam = 0
bosskim = 0
bossclitty = 0
bhit = 0
bvul = 0
bhurt = 0
csongs = 0
churgepoints = 0
weedpoints = 0
TGW = 0
countr11 = 0
weedultimate = 0
charcomplete = 0
mrhan = 0
churchlocked = 0
hitgate3 = 0
hitgate2 = 0
heavensatan = 0
heavenjesus = 0
Kcompleted = 0
SMcompleted = 0
Scompleted = 0
Bcompleted = 0
bosshitler = 0
bossosama = 0
bossclitoris = 0
countr34 = 0
kickbabysonplay = 0
li079.state = 0
li080.state = 0
li081.state = 0
li082.state = 0
li083.state = 0
li084.state = 0
li085.state = 0
li057.state = 0
li058.state = 0
li059.state = 0
li060.state = 0
li061.state = 0
li062.state = 0
li063.state = 0
li064.state = 0
li065.state = 0
Target001.IsDropped = false
Target002.IsDropped = false
Target003.IsDropped = false
Target004.IsDropped = false
Target005.IsDropped = false
Target006.IsDropped = false
Target007.IsDropped = false
Target008.IsDropped = false
Target009.IsDropped = false
StopBallon
Wall027.Collidable = False
Wall027.IsDropped = True
stoptegridymode
ressetkennyT
resetbossys


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
	bumperHitsC = 25
	bumperHitsK = 25
	bumperHitsS = 25
	PFMultiplier = 1

    UpdateMusic = 0
    'UpdateMusic = UpdateMusic + 6
    UpdateMusicNow

    ' initialise Game variables
    Game_Init()
	
    ' you may wish to start some music, play a sound, do whatever at this point
StopSong
PlaySound ""


    vpmtimer.addtimer 1500, "FirstBall '"
End Sub

' This is used to delay the start of a game to allow any attract sequence to

' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
    ' reset the table for a new ball
    ResetForNewPlayerBall()
Tskillshot.enabled = true
    ' create a new ball in the shooters lane
    CreateNewBall()
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()
	LightSeq001.Play SeqUpOn, 25, 500
	LightSeq002.Play SeqRightOn, 25, 500
Table1.ColorGradeImage = "ColorGradeLUT256x16_1to1"
    ' make sure the correct display is upto date
    AddScore 0

    ' set the current players bonus multiplier back down to 1X
    BonusMultiplier(CurrentPlayer) = 1
    'UpdateBonusXLights
	
' reset any drop targets, lights, game Mode etc..
PFMultiplier = 1
li079.state = 0
li080.state = 0
li081.state = 0
li082.state = 0
li083.state = 0
li084.state = 0
li085.state = 0
    
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
        'ChangeSong
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

	'LightSeqAttract.Play SeqBlinking, , 5, 150
ObjLevel(9) = 1 : FlasherFlash9_Timer
ObjLevel(8) = 1 : FlasherFlash8_Timer
ObjLevel(7) = 1 : FlasherFlash7_Timer
StopSong
Table1.ColorGradeImage = "-30"
StopmodeEndofBall()
tempbonusaf = 10
lichtTimer.enabled = True
li078.state = 0
li077.state = 0
li076.state = 0


'StopSong
'bonuscheckie
		vpmtimer.addtimer 1005, "endofballcontinue '"
end sub

sub endofballcontinue
li001.state = 0
li003.state = 0
li005.state = 0
li004.state = 0
LightSeq003.Play SeqCircleOutOn, 25, 1
playsound "BALLLOST22"
DMD "", "", "spbl00", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spbl01", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spbl02", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spbl03", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spbl04", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spbl05", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spbl06", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spbl07", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spbl08", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spbl09", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spbl10", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spbl11", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spbl12", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spbl13", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spbl14", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spbl15", eNone, eNone, eNone, 100, True, ""
vpmtimer.addtimer 1505, "endofballcontinue2 '"
end sub

sub endofballcontinue2

    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 0

    'If NOT Tilted Then
	If(Tilted = False) Then
		
        AwardPoints = TargetBonus * 10000
        TotalBonus = TotalBonus + AwardPoints
 
        AwardPoints = BumperBonus * 100000
        TotalBonus = TotalBonus + AwardPoints
        
		DMD CL(0, FormatScore(TotalBonus) ), CL(1, "TOTAL BONUS" & BonusMultiplier(CurrentPlayer) ), "", eBlinkFast, eNone, eNone, 1000, True, ""
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)
        
		AddScore TotalBonus

		' add a bit of a delay to allow for the bonus points to be shown & added up
		vpmtimer.addtimer 1000, "EndOfBall2 '"
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
'    UpdateMusic = UpdateMusic + 1
	UpdateMusicNow	
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
        DMD CL(0, "EXTRA BALL"), CL(1, "SHOOT AGAIN"), "", eNone, eNone, eBlink, 1000, True, "vo_extraball"

'		UpdateMusic = UpdateMusic - 1
		UpdateMusicNow

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
		'DMD CL(0, "GAME OVER") "", eNone, 13000, True, ""
'DMD "", CL(1, "GAME OVER"), "", eNone, eNone, eNone, 9000, False, "gameover"
		PlaySound "gameover"
DMD CL(0, "KENNY"), CL(1, "KILLED " &Kcompleted), "", eNone, eNone, eNone, 1300, True, ""
DMD CL(0, "SCHOOL"), CL(1, "MISSIONS " &SMcompleted), "", eNone, eNone, eNone, 1400, True, ""
DMD CL(0, "SONGS"), CL(1, "COMPLETED " &Scompleted), "", eNone, eNone, eNone, 1400, True, ""
DMD CL(0, "BOSSES"), CL(1, "DEFEATED " &Bcompleted), "", eNone, eNone, eNone, 1400, True, ""
DMD "", "", "spgo00", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo01", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo02", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo03", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo04", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo05", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo06", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo07", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo08", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo09", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo10", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo11", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo12", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo13", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo14", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo15", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo16", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo17", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo18", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo19", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo20", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo21", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo22", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo23", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo24", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo25", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo26", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo27", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo28", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo29", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo30", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo31", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo32", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo33", eNone, eNone, eNone, 100, True, ""
DMD "", "", "spgo34", eNone, eNone, eNone, 100, True, ""

        ' set the machine into game over mode
        vpmtimer.addtimer 9000, "EndOfGame() '"

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
        ChangeSong
    End If

    bJustStarted = False
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0

    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball
    UpdateMusic = 0
    UpdateMusic = UpdateMusic + 14
    UpdateMusicNow
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
	DMD CL(0, "BALL SAVED"), CL(1, "SHOOT AGAIN"), "", eBlink, eBlink, eNone, 800, True, ""
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
				ChangeSong
                End If
            End If
            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0) Then
StartBallControl.enabled = True
Tskillshot.enabled = true
                ' End Mode and timers
				StopSong
				PlaySound ""
                'vpmtimer.addtimer 3000, "ChangeSong '"
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
if Tskillshot.enabled=true Then
playsound "busstarty2"
end if
If bAutoPlunger Then
        'debug.print "autofire the ball"
        PlungerIM.AutoFire
        DOF 120, DOFPulse
        PlaySoundAt "fx_fire", Trigger1
        bAutoPlunger = False
    End If	
'StopSong
    DMDScoreNow
    bBallInPlungerLane = True
 '   DMD "_", CL(1, "SHOOT THE BALL"), "", eNone, eBlink, eNone, 1000, True, ""
    If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
        EnableBallSaver BallSaverTime
        Else
        ' show the message to shoot the ball in case the player has fallen sleep
 '       Trigger1.TimerEnabled = 1
    End If
	BallShot = 0
End Sub

' The ball is released from the plunger
Dim BallShot:BallShot = 0
Sub Trigger1_UnHit()
    bBallInPlungerLane = False
	BallShot = 1
    'LightEffect 4
	'ChangeSong
End Sub


'Sub Trigger1_Timer
'    DMD "_", CL(1, "SHOOT THE BALL"), "", eNone, eNone, eNone, 800, True, ""
'    trigger1.TimerEnabled = 0
'End Sub

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

    ' you may wish to check to see if the player has gotten an extra ball by a high score
    If Score(CurrentPlayer) >= Special1 AND Special1Awarded(CurrentPlayer) = False Then
        AwardExtraBall
        Special1Awarded(CurrentPlayer) = True
    End If
    If Score(CurrentPlayer) >= Special2 AND Special2Awarded(CurrentPlayer) = False Then
        AwardExtraBall
        Special2Awarded(CurrentPlayer) = True
    End If
    If Score(CurrentPlayer) >= Special3 AND Special3Awarded(CurrentPlayer) = False Then
        AwardExtraBall
        Special3Awarded(CurrentPlayer) = True
    End If
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
	playsound "extraballcartman"
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
	Playsong "endingsp"
'    ChangeSong
	playsound "highscore"
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
    ChangeSong
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
InitFlasher 7, "green" : InitFlasher 8, "red" : InitFlasher 9, "blue" 
': InitFlasher 10, "red" : InitFlasher 11, "white" 
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
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 40
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

' ###################################
' ###### copy script until here #####
' ###################################

' ***      script for demoing flashers					***
' *** you should not need this in your table			***
' *** in your table start a flash with :				***
' *** ObjLevel(xx) = 1 : FlasherFlashxx_Timer			***
' *** for modulated flashers use 0-1 for ObjLevel(xx)	***

'dim countr : Randomize

'Sub Timer1_Timer
'	If TestFlashers = 0 Then
'		countr = countr + 1 : If Countr > 11 then Countr = 3 : end If
'		If rnd(1) < 0.04 Then
'			PlaySound "fx_relay_on",0,1
'			select case countr
				'case 1 : Objlevel(1) = 1 : FlasherFlash1_Timer
				'case 2 : Objlevel(2) = 1 : FlasherFlash2_Timer
				'case 3 : ObjLevel(3) = 1 : FlasherFlash3_Timer
				'case 4 : ObjLevel(4) = 1 : FlasherFlash4_Timer
				'case 5 : ObjLevel(5) = 1 : FlasherFlash5_Timer
				'case 6 : ObjLevel(6) = 1 : FlasherFlash6_Timer
				'case 7 : ObjLevel(7) = 1 : FlasherFlash7_Timer
				'case 8 : ObjLevel(8) = 1 : FlasherFlash8_Timer
'				case 9 : ObjLevel(9) = 1 : FlasherFlash9_Timer
				'case 10 : ObjLevel(10) = 1 : FlasherFlash10_Timer
				'case 11 : ObjLevel(11) = 1 : FlasherFlash11_Timer
'			end Select
'		End If
'	End If
'end Sub

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
	DMD "", "", "dmdintro1", eNone, eNone, eNone, 3000, True, ""
	DMD "", "", "spintros1", eNone, eNone, eNone, 1000, True, ""
	DMD "", "", "spintros2", eNone, eNone, eNone, 500, True, ""
	DMD "", "", "spintros3", eNone, eNone, eNone, 500, True, ""
	DMD "", "", "spintros4", eNone, eNone, eNone, 500, True, ""
	DMD "", "", "spintros5", eNone, eNone, eNone, 500, True, ""
	DMD "", "", "spintros5", eNone, eNone, eNone, 500, True, ""
	DMD "", "", "spintros6", eNone, eNone, eNone, 2000, True, ""
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
Table1.ColorGradeImage = "ColorGradeLUT256x16_1to1"
    ChangeSong
    StartLightSeq
    DMDFlush
    ShowTableInfo
	changebackwalls
AttractFlashLeftUP.enabled = true
AttractFlashLeftDOWN.enabled = true
AttractFlashRightUP.enabled = true
AttractFlashRightDOWN.enabled = true
End Sub

Sub StopAttractMode
    LightSeqAttract.StopPlay
    DMDScoreNow
AttractFlashLeftUP.enabled = false
AttractFlashLeftDOWN.enabled = false
AttractFlashRightUP.enabled = false
AttractFlashRightDOWN.enabled = false
Flasher001.ImageA = "sc0"
Flasher002.ImageA = "sc0"
Flasher003.ImageA = "good"
Flasher004.ImageA = "evil"
End Sub

Dim AttractFlashLeftUP1Pos, AttractLeftUP
AttractLeftUP = Array("evil", "good")

Sub AttractFlashLeftUP_Timer
    Flasher003.ImageA = AttractLeftUP(AttractFlashLeftUP1Pos)
    AttractFlashLeftUP1Pos = (AttractFlashLeftUP1Pos + 1) MOD 2
End Sub

Dim AttractFlashLeftDOWN1Pos, AttractLeftDOWN
AttractLeftDOWN = Array("sc0", "sc1", "sc2", "sc3", "sc4", "sc5", "sc6", "sc7", "sc8", "sc9", "sc10")

Sub AttractFlashLeftDOWN_Timer
    Flasher001.ImageA = AttractLeftDOWN(AttractFlashLeftDOWN1Pos)
    AttractFlashLeftDOWN1Pos = (AttractFlashLeftDOWN1Pos + 1) MOD 11
End Sub

Dim AttractFlashRightUP1Pos, AttractRightUP
AttractRightUP = Array("good", "evil")

Sub AttractFlashRightUP_Timer
    Flasher004.ImageA = AttractRightUP(AttractFlashRightUP1Pos)
    AttractFlashRightUP1Pos = (AttractFlashRightUP1Pos + 1) MOD 2
End Sub

Dim AttractFlashRightDOWN1Pos, AttractRightDOWN
AttractRightDOWN = Array("sc10", "sc9", "sc8", "sc7", "sc6", "sc5", "sc4", "sc3", "sc2", "sc1", "sc0")

Sub AttractFlashRightDOWN_Timer
    Flasher002.ImageA = AttractRightDOWN(AttractFlashRightDOWN1Pos)
    AttractFlashRightDOWN1Pos = (AttractFlashRightDOWN1Pos + 1) MOD 11
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
Dim HoleBonus, BumperBonus, ALLRampBonus, RampBonus1, RampBonus2, RampBonus3, MulitballBonus, TargetBonus    

Sub Game_Init() 'called at the start of a new game
    Dim i, j
    ChangeSong
	TargetBonus = 0
	'bumperHits = 100
	BumperBonus = 0
	ALLRampBonus = 0
	RampBonus1 = 0
	RampBonus2 = 0
	RampBonus3 =0
	MulitballBonus = 0
	'BallInHole = 0
    TurnOffPlayfieldLights()
End Sub

Sub StopEndOfBallMode()     'this sub is called after the last ball is drained
End Sub

Sub ResetNewBallVariables() 'reset variables for a new ball or player
Dim i
TargetBonus = 0
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
	li077.state = 1
	'li011.state = 1
	'li012.state = 1

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
if kickbabysonplay = 1 then
countr34 = countr34 + 1
checkkickthebaby
end if
phillipy.transX = 10
	PlaySound ("farttp"), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    DOF 105, DOFPulse
    RSling.Visible = 0:RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (210*PFMultiplier)
	gi1.State = 0
	Gi2.State = 0	
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10:gi1.State = 0:Gi2.State = 0:phillipy.transX = 5
		Case 1:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 5:gi1.State = 0:Gi2.State = 0:phillipy.transX = 0
        Case 2:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:gi1.State = 1:Gi2.State = 1:RightSlingShot.TimerEnabled = False
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
if kickbabysonplay = 1 then
countr34 = countr34 + 1
checkkickthebaby
end if
	Tarancy.TransX = 10
    PlaySound ("farttp"), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    DOF 103, DOFPulse
    LSling.Visible = 0:LSling1.Visible = 1
    sling2.rotx = 20
	 LStep = 0
    LeftSlingShot.TimerEnabled = 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (210*PFMultiplier)
	gi3.State = 0
	Gi4.State = 0
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10:gi3.State = 0:Gi4.State = 0:Tarancy.TransX = 5
        Case 2:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 5:gi3.State = 0:Gi4.State = 0: Tarancy.TransX = 0
        Case 3:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:gi3.State = 1:Gi4.State = 1:LeftSlingShot.TimerEnabled = False
    End Select
    LStep = LStep + 1
End Sub

'*****************
'triggers
'*****************

Sub Bonuschecker_Hit
'FlashForMs Flasher4, 1000, 50, 0
'FlashForMs Flasher5, 1000, 50, 0
'FlashForMs Flasher6, 1000, 50, 0
'FlashForMs Flasher7, 1000, 50, 0
'FlashForMs Flasher8, 1000, 50, 0
'FlashForMs Flasher9, 1000, 50, 0
End Sub

'**********************inner/outerlane*********************

Sub TLeftInlane_Hit
	'FlashForMs Flasher011, 1000, 50, 0
	LeftInlane.State = 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	PlaySound "twang"
	DMD "", "", "10k", eNone, eNone, eNone, 500, True, ""
	Checkbgrog
End Sub

Sub TLeftOutlane_Hit
	If li011.state = 1 Then
	KickbackPulse.enabled = 1
    kickbacktimer.interval = 800
	kickbacktimer.enabled = true
	end If
	If li011.state = 0 Then
	KickbackPulse.enabled = 0
	kickbacktimer.enabled = False
	end If
	LeftOutlane.State = 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (20000*PFMultiplier)
	PlaySound "outlane2"

	DMD "", "", "50k", eNone, eNone, eNone, 500, True, ""
	Checkbgrog
End Sub

Sub TRightInlane_Hit
'	FlashForMs Flasher012, 1000, 50, 0
	RightInlane.State = 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	PlaySound "twang"
	DMD "", "", "10k", eNone, eNone, eNone, 500, True, ""
	Checkbgrog
End Sub

Sub TRightOutlane_Hit
	If li012.state = 1 Then
	KickbackPulse2.enabled = 1
    kickbacktimer2.interval = 800
	kickbacktimer2.enabled = true
	end If
	If li012.state = 0 Then
	KickbackPulse2.enabled = 0
	kickbacktimer2.enabled = False
	end If
	RightOutlane.State = 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (20000*PFMultiplier)
	PlaySound "outlane2"

	DMD "", "", "50k", eNone, eNone, eNone, 500, True, ""
	Checkbgrog
End Sub

Sub Checkbgrog
	If(LeftInlane.State = 1) And(LeftOutlane.State = 1) And(RightInlane.State = 1) And(RightOutlane.State = 1) Then
	DMD "", "", "dmdanal", eNone, eNone, eNone, 1000, True, "anal"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (50000*PFMultiplier*SPMaultiplier)
	LeftInlane.State=0
	LeftOutlane.State=0
	RightInlane.State=0
	RightOutlane.State=0	
	SPMaultiplier = SPMaultiplier + 1  
	End If
End Sub

'**********************cow triggers*********************

sub Trigger001_hit
	PlaySound "cowbell"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (2000*COWMultiplier)
	if li075.state = 0 then 
	li075.state = 1
	end If
	if li078.state = 1 then
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*COWMultiplier)
	PlaySound "Sweet" 
	end If
	CheckMoo
end sub

sub Trigger002_hit
	PlaySound "cowbell"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (2000*COWMultiplier)
	if li074.state = 0 then 
	li074.state = 1
	end If
	if li077.state = 1 then
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*COWMultiplier)
	PlaySound "Sweet"  
	end If
	CheckMoo
end sub

sub Trigger003_hit
	PlaySound "cowbell"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (2000*COWMultiplier)
	if li073.state = 0 then 
	li073.state = 1
	end If
	if li076.state = 1 then
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*COWMultiplier)
	PlaySound "Sweet" 
	end If
	CheckMoo
end sub

Sub CheckMoo
if (li075.state=1)And (li074.state=1)And (li073.state=1) then
DMD "", "", "dmdmoo", eNone, eNone, eNone, 1000, True, ""
If COWMultiplier = 6 then
Playsound "Moo1"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (50000*COWMultiplier)
li073.state = 0
li074.state = 0
li075.state = 0
exit sub
end if
If COWMultiplier = 1 then
Playsound "Moo1"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (50000*COWMultiplier)
COWMultiplier = COWMultiplier + 1
li073.state = 0
li074.state = 0
li075.state = 0
exit sub
end if 
If COWMultiplier = 2 then
Playsound "Moo2"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (50000*COWMultiplier)
COWMultiplier = COWMultiplier + 1
li073.state = 0
li074.state = 0
li075.state = 0
exit sub
end if
If COWMultiplier = 3 then
Playsound "Moo3"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (50000*COWMultiplier)
COWMultiplier = COWMultiplier + 1
li073.state = 0
li074.state = 0
li075.state = 0
exit sub
end if
If COWMultiplier = 4 then
Playsound "Moo4"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (50000*COWMultiplier)
COWMultiplier = COWMultiplier + 1
li073.state = 0
li074.state = 0
li075.state = 0
exit sub
end if
If COWMultiplier = 5 then
Playsound "Moo5"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (50000*COWMultiplier)
COWMultiplier = COWMultiplier + 1
li073.state = 0
li074.state = 0
li075.state = 0
exit sub
end if
end if
end sub

Sub ChangeBonusCowLights
    Dim TempState
    TempState = li076.State
    li076.State = li077.State
    li077.State = li078.State
    li078.state = TempState
End Sub

'************************** 
'Bumpers 
'************************** 
Dim bumperHitsC, bumperHitsK, bumperHitsS

Sub Bumper001_hit()
ObjLevel(9) = 1 : FlasherFlash9_Timer
cartmanbump.Image = "cartman2"
fireCART.visible = 1
Playsound "fart"
    DOF 107, DOFPulse
CartTimer.Enabled = True
Score(CurrentPlayer) = Score(CurrentPlayer) + (500*PFMultiplier)
ChangeBonusCowLights
bumperHitsC = bumperHitsC - 1
CheckBumpersC
End sub

Sub CartTimer_Timer()
	cartmanbump.Image = "cartman1"
	fireCART.visible = 0
'	CartFart.IsDropped = True
	CartTimer.Enabled = False
End Sub


Sub Bumper002_hit()
ObjLevel(8) = 1 : FlasherFlash8_Timer
kylebump.Image = "kyle2"
Playsound "dude"
    DOF 108, DOFPulse
KyleTimer.Enabled = True
Score(CurrentPlayer) = Score(CurrentPlayer) + (500*PFMultiplier)
ChangeBonusCowLights
bumperHitsK = bumperHitsK - 1
CheckBumpersK
End sub

Sub KyleTimer_Timer()
	kylebump.Image = "kyle1"
'	CartFart.IsDropped = True
	KyleTimer.Enabled = False
End Sub


Sub Bumper003_hit()
ObjLevel(7) = 1 : FlasherFlash7_Timer
stanbump.Image = "stan2"
pukeSTAN.visible = 1
Playsound "puke"
    DOF 109, DOFPulse
StanTimer.Enabled = True
Score(CurrentPlayer) = Score(CurrentPlayer) + (500*PFMultiplier)
ChangeBonusCowLights
bumperHitsS = bumperHitsS - 1
CheckBumpersS
End sub

Sub StanTimer_Timer()
	stanbump.Image = "stan1"
	pukeSTAN.visible = 0
'	CartFart.IsDropped = True
	StanTimer.Enabled = False
End Sub

Sub CheckBumpersC
    If bumperHitsC <= 0 Then
        BumperBonus = BumperBonus + 1
		li005.state = 1
        bumperHitsC = 25
    End If
End Sub

Sub CheckBumpersK
    If bumperHitsK <= 0 Then
        BumperBonus = BumperBonus + 1
		li004.state = 1
        bumperHitsK = 25
    End If
End Sub


Sub CheckBumpersS
    If bumperHitsS <= 0 Then
        BumperBonus = BumperBonus + 1
		li003.state = 1
        bumperHitsS = 25
    End If
End Sub


'*****************
'Targets
'*****************

'*****************characters targets*****************

Sub Target011_Hit()
PlayQuoteWendy
wendyShaker
If Tilted Then Exit Sub
If li050.State = 0 then 
li050.State = 1
charcomplete = charcomplete + 1
CheckallChars
end if 
TargetBonus = TargetBonus + 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
end sub

Sub Target010_Hit()
PlayQuoteButters
buttersShaker
If Tilted Then Exit Sub
If li049.State = 0 then 
li049.State = 1
charcomplete = charcomplete + 1
CheckallChars
end if
TargetBonus = TargetBonus + 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
end sub

Sub Target012_Hit()
PlayQuoteTowely
towelShaker
If Tilted Then Exit Sub
If li051.State = 0 then 
li051.State = 1
charcomplete = charcomplete + 1
CheckallChars
end if
TargetBonus = TargetBonus + 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
end sub

Sub Target013_Hit()
PlayQuoteNed
nedShaker
If Tilted Then Exit Sub
If li052.State = 0 then 
li052.State = 1
charcomplete = charcomplete + 1 
CheckallChars
end if
TargetBonus = TargetBonus + 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
end sub

Sub Target014_Hit()
PlayQuoteJimbo
jimboShaker
If Tilted Then Exit Sub
If li053.State = 0 then 
li053.State = 1
charcomplete = charcomplete + 1
CheckallChars
end if
TargetBonus = TargetBonus + 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
end sub

Sub Target015_Hit()
PlayQuoteBarbrady
barbradyShaker
If Tilted Then Exit Sub
If li054.State = 0 then 
li054.State = 1
charcomplete = charcomplete + 1
CheckallChars
end if
TargetBonus = TargetBonus + 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
end sub

Sub Target021_Hit()
PlayQuoteSelly
shellyShaker
If Tilted Then Exit Sub
If li056.State = 0 then 
li056.State = 1
charcomplete = charcomplete + 1 
CheckallChars
end if
TargetBonus = TargetBonus + 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
end sub

Sub Target022_Hit()
PlayQuoteGaridson
garridsonShaker
If Tilted Then Exit Sub
If li055.State = 0 then 
li055.State = 1
charcomplete = charcomplete + 1 
CheckallChars
end if
TargetBonus = TargetBonus + 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
end sub

Sub Target023_Hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
If timmyt = 5 Then
Playsound "Tim_5"
li070.state = 1
timmyShaker
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
exit sub
end if
If timmyt = 1 Then
Playsound "Tim_1"
li066.state = 1
timmyShaker
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
timmyt = timmyt + 1
exit sub
end if
If timmyt = 2 Then
Playsound "Tim_2"
li067.state = 1
timmyShaker
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
timmyt = timmyt + 1
exit sub
end if
If timmyt = 3 Then
Playsound "Tim_3"
li068.state = 1
timmyShaker
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
timmyt = timmyt + 1
exit sub
end if
If timmyt = 4 Then
Playsound "Tim_4"
li069.state = 1
timmyShaker
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
timmyt = timmyt + 1
CheckallChars
exit sub
end if
end sub

Sub Target024_Hit()
PlayQuoteMackey
mackeyShaker
If Tilted Then Exit Sub
If li095.State = 0 then 
li095.State = 1 
charcomplete = charcomplete + 1 
CheckallChars
end if
TargetBonus = TargetBonus + 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
end sub

Sub Target025_Hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
If galt = 5 Then
Playsound "Gal_5"
li093.state = 1
gayalShaker
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
'DMD "", "", "dmd9x", eNone, eNone, eNone, 1400, True, ""
exit sub
end if
If galt = 1 Then
Playsound "Gal_1"
li089.state = 1
gayalShaker
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
galt = galt + 1
exit sub
end if
If galt = 2 Then
Playsound "Gal_2"
li090.state = 1
gayalShaker
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
galt = galt + 1
exit sub
end if
If galt = 3 Then
Playsound "Gal_3"
li091.state = 1
gayalShaker
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
galt = galt + 1
exit sub
end if
If galt = 4 Then
Playsound "Gal_4"
li092.state = 1
gayalShaker
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
galt = galt + 1
CheckallChars
exit sub
end if
end sub

Sub Target026_Hit()
PlayQuoteMephisto
mephistoShaker
If Tilted Then Exit Sub
If li094.State = 0 then 
li094.State = 1 
charcomplete = charcomplete + 1 
CheckallChars
end if
TargetBonus = TargetBonus + 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
end sub

Sub CheckallChars
if charcomplete = 10 Then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, "100k"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
li094.State = 0
li095.State = 0
li055.State = 0
li056.State = 0
li054.State = 0
li053.State = 0
li052.State = 0
li051.State = 0
li049.State = 0
li050.State = 0
charcomplete = 0
end if
End sub

'********************droptargets kenny********************
sub Target016_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Playsound "T1"
li044.state = 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
checkAllDropped2
end sub

sub Target017_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Playsound "T1"
li045.state = 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
checkAllDropped2
end sub

sub Target018_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Playsound "T1"
li046.state = 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
checkAllDropped2
end sub

sub Target019_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Playsound "T2"
li047.state = 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
checkAllDropped2
end sub

sub Target020_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Playsound "T2"
li048.state = 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
checkAllDropped2
end sub

sub checkAllDropped2
if (li044.state=1)And (li045.state=1)And (li046.state=1)And (li047.state=1)And (li048.state=1) then 
Playsound "kennyok"
Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*SPkMultiplier)
li071.state = 2
li072.state = 2
KillKenny = 2
SPkMultiplier = SPkMultiplier + 1
End if
end sub
'********************droptargets southpark********************

sub Target005_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Playsound "T1"
SPlights = SPlights + 1
li061.state = 1
li039.state = 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
checkAllDropped
end sub

sub Target004_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Playsound "T1"
SPlights = SPlights + 1
li060.state = 1
li038.state = 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
checkAllDropped
end sub

sub Target003_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Playsound "T1"
SPlights = SPlights + 1
li059.state = 1
li037.state = 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
checkAllDropped
end sub

sub Target002_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Playsound "T1"
SPlights = SPlights + 1
li058.state = 1
li036.state = 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
checkAllDropped
end sub

sub Target001_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Playsound "T1"
SPlights = SPlights + 1
li057.state = 1
li035.state = 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
checkAllDropped
end sub

sub Target006_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Playsound "T2"
SPlights = SPlights + 1
li062.state = 1
li040.state = 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
checkAllDropped
end sub

sub Target007_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Playsound "T2"
SPlights = SPlights + 1
li063.state = 1
li041.state = 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
checkAllDropped
end sub

sub Target008_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Playsound "T2"
SPlights = SPlights + 1
li064.state = 1
li042.state = 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
checkAllDropped
end sub

sub Target009_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Playsound "T2"
SPlights = SPlights + 1
li065.state = 1
li043.state = 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
checkAllDropped
end sub

sub checkAllDropped
If SPlights = 10 then
DMD "", "", "500k", eNone, eNone, eNone, 1000, True, "SOUTHPARK"
Score(CurrentPlayer) = Score(CurrentPlayer) + (500000*SPMultiplier)
vpmtimer.addtimer 2005, "Resettargets ' "
SPMultiplier = SPMultiplier + 1
end if
end sub

Sub Resettargets
Playsound SoundFXDOF("fx_resetdrop", 111, DOFPulse, DOFContactors)
'Playsound "fx_resetdrop"
SPlights = 1
li057.state = 0
li058.state = 0
li059.state = 0
li060.state = 0
li061.state = 0
li062.state = 0
li063.state = 0
li064.state = 0
li065.state = 0
Target001.IsDropped = false
Target002.IsDropped = false
Target003.IsDropped = false
Target004.IsDropped = false
Target005.IsDropped = false
Target006.IsDropped = false
Target007.IsDropped = false
Target008.IsDropped = false
Target009.IsDropped = false
end sub

'***********character shakers*************
Dim jimboShake
Dim nedShake
Dim barbradyShake
dim towelShake
dim wendyShake
dim buttersShake
dim gayalShake
dim shellyShake
dim garridsonShake
Dim mackeyShake
Dim timmyShake
Dim mephistoShake

Sub jimboShaker()
    jimboShake = 6
    jimboShakeTimer.Enabled = True
End Sub

Sub jimboShakeTimer_Timer()
    TJimbo.Transz = jimboShake / 2
    If jimboShake = 0 Then Me.Enabled = False:Exit Sub
    If jimboShake <0 Then
        jimboShake = ABS(jimboShake)- 0.1
    Else
        jimboShake = - jimboShake + 0.1
    End If
End Sub

Sub nedShaker()
    nedShake = 6
    nedShakeTimer.Enabled = True
End Sub

Sub nedShakeTimer_Timer()
    Tned.Transz = nedShake / 2
    If nedShake = 0 Then Me.Enabled = False:Exit Sub
    If nedShake <0 Then
        nedShake = ABS(nedShake)- 0.1
    Else
        nedShake = - nedShake + 0.1
    End If
End Sub

Sub barbradyShaker()
    barbradyShake = 6
    barbradyShakeTimer.Enabled = True
End Sub

Sub barbradyShakeTimer_Timer()
    Tbarbrady.Transz = barbradyShake / 2
    If barbradyShake = 0 Then Me.Enabled = False:Exit Sub
    If barbradyShake <0 Then
        barbradyShake = ABS(barbradyShake)- 0.1
    Else
        barbradyShake = - barbradyShake + 0.1
    End If
End Sub

Sub towelShaker()
    towelShake = 6
    towelShakeTimer.Enabled = True
End Sub

Sub towelShakeTimer_Timer()
    Ttowel.Transz = towelShake / 2
    If towelShake = 0 Then Me.Enabled = False:Exit Sub
    If towelShake <0 Then
        towelShake = ABS(towelShake)- 0.1
    Else
        towelShake = - towelShake + 0.1
    End If
End Sub

Sub wendyShaker()
    wendyShake = 6
    wendyShakeTimer.Enabled = True
End Sub

Sub wendyShakeTimer_Timer()
    Twendy.Transz = wendyShake / 2
    If wendyShake = 0 Then Me.Enabled = False:Exit Sub
    If wendyShake <0 Then
        wendyShake = ABS(wendyShake)- 0.1
    Else
        wendyShake = - wendyShake + 0.1
    End If
End Sub

Sub buttersShaker()
    buttersShake = 6
    buttersShakeTimer.Enabled = True
End Sub

Sub buttersShakeTimer_Timer()
    Tbutters.Transz = buttersShake / 2
    If buttersShake = 0 Then Me.Enabled = False:Exit Sub
    If buttersShake <0 Then
        buttersShake = ABS(buttersShake)- 0.1
    Else
        buttersShake = - buttersShake + 0.1
    End If
End Sub

Sub gayalShaker()
    gayalShake = 6
    gayalShakeTimer.Enabled = True
End Sub

Sub gayalShakeTimer_Timer()
    Tgayal.Transz = gayalShake / 2
    If gayalShake = 0 Then Me.Enabled = False:Exit Sub
    If gayalShake <0 Then
        gayalShake = ABS(gayalShake)- 0.1
    Else
        gayalShake = - gayalShake + 0.1
    End If
End Sub

Sub shellyShaker()
    shellyShake = 6
    shellyShakeTimer.Enabled = True
End Sub

Sub shellyShakeTimer_Timer()
    Tshelly.Transz = shellyShake / 2
    If shellyShake = 0 Then Me.Enabled = False:Exit Sub
    If shellyShake <0 Then
        shellyShake = ABS(shellyShake)- 0.1
    Else
        shellyShake = - shellyShake + 0.1
    End If
End Sub

Sub garridsonShaker()
    garridsonShake = 6
    garridsonShakeTimer.Enabled = True
End Sub

Sub garridsonShakeTimer_Timer()
    Tgarridson.Transz = garridsonShake / 2
    If garridsonShake = 0 Then Me.Enabled = False:Exit Sub
    If garridsonShake <0 Then
        garridsonShake = ABS(garridsonShake)- 0.1
    Else
        garridsonShake = - garridsonShake + 0.1
    End If
End Sub

Sub mackeyShaker()
    mackeyShake = 6
    mackeyShakeTimer.Enabled = True
End Sub

Sub mackeyShakeTimer_Timer()
    Tmackey.Transz = mackeyShake / 2
    If mackeyShake = 0 Then Me.Enabled = False:Exit Sub
    If mackeyShake <0 Then
        mackeyShake = ABS(mackeyShake)- 0.1
    Else
        mackeyShake = - mackeyShake + 0.1
    End If
End Sub

Sub timmyShaker()
    timmyShake = 6
    timmyShakeTimer.Enabled = True
End Sub

Sub timmyShakeTimer_Timer()
    Ttimmy.Transz = timmyShake / 2
    If timmyShake = 0 Then Me.Enabled = False:Exit Sub
    If timmyShake <0 Then
        timmyShake = ABS(timmyShake)- 0.1
    Else
        timmyShake = - timmyShake + 0.1
    End If
End Sub

Sub mephistoShaker()
    mephistoShake = 6
    mephistoShakeTimer.Enabled = True
End Sub

Sub mephistoShakeTimer_Timer()
    Tmephisto.Transz = mephistoShake / 2
    If mephistoShake = 0 Then Me.Enabled = False:Exit Sub
    If mephistoShake <0 Then
        mephistoShake = ABS(mephistoShake)- 0.1
    Else
        mephistoShake = - mephistoShake + 0.1
    End If
End Sub

'*****************
'Gates
'*****************
sub Gate003_hit()
playsound "gate"
end Sub

sub Gate002_hit()
playsound "gate"
end sub

sub Gate_Hit()
ObjLevel(8) = 1 : FlasherFlash8_Timer
playsound "turkeys"
Tturkey1up.Enabled = 1
vpmtimer.addtimer 250, "startturkey2 ' "
vpmtimer.addtimer 500, "startturkey3 ' "
if weedpoints = 6 then exit sub
if weedpoints = 5 and schoolmissionmodes = 2 or musicmodes = 2 or kennymissionmodes = 2 or bMultiBallMode = true or churchlocked = 2 then exit sub
weedpoints = weedpoints + 1
updateweedpoints
End Sub

sub startturkey2
Tturkey2up.Enabled = 1
end Sub

sub startturkey3
Tturkey3up.Enabled = 1
end Sub

sub updateweedpoints
    Select Case weedpoints
        Case 1 : weedys1
        Case 2 : weedys2
        Case 3 : weedys3
        Case 4 : weedys4
        Case 5 : weedys5
        Case 6 : weedys6
    End Select
end sub

sub weedys1
end sub

sub weedys2
end sub

sub weedys3
end sub

sub weedys4
end sub

sub weedys5
end sub

sub weedys6
StartWeeds
end sub

'********tegridy farm***************

Sub StartWeeds
if weedultimate = 0 then
li126.state = 0
li127.state = 0
li128.state = 0
end if
changewalls
turny3Timer.enabled = true
TGW = 1
UpdateMusic = 18
UpdateMusicNow
EnableWeedss()
WeedsChecker = 0
end sub

Dim WhichWeeds, WeedsChecker
WhichWeeds = 0
WeedsChecker = 0
sub EnableWeedss()
	If WeedsChecker = 31 Then
		CheckBonusWeeds()
		Exit Sub
	End If
	Randomize()
	WhichWeeds = INT(RND * 5) + 1
	Select Case WhichWeeds
		Case 3
			WhichWeeds = 4
		Case 4
			WhichWeeds = 8
		Case 5
			WhichWeeds = 16
	End Select
	Do While (WhichWeeds AND WeedsChecker) > 0
		WhichWeeds = INT(RND * 5) + 1
		Select Case WhichWeeds
			Case 3
				WhichWeeds = 4
			Case 4
				WhichWeeds = 8
			Case 5
				WhichWeeds = 16
		End Select
	Loop
	Select Case WhichWeeds
		Case 1
			tsweed001.enabled = 1
			weed010.Visible = 1
			weed010.X = tsweed001.X
			weed010.Y = tsweed001.Y
		Case 2
			tsweed002.enabled = 1
			weed010.Visible = 1
			weed010.X = tsweed002.X
			weed010.Y = tsweed002.Y
		Case 4
			tsweed003.enabled = 1
			weed010.Visible = 1
			weed010.X = tsweed003.X
			weed010.Y = tsweed003.Y
		Case 8
			tsweed004.enabled = 1
			weed010.Visible = 1
			weed010.X = tsweed004.X
			weed010.Y = tsweed004.Y
		Case 16
			tsweed005.enabled = 1
			weed010.Visible = 1
			weed010.X = tsweed005.X
			weed010.Y = tsweed005.Y
	End Select
end sub

sub tsweed001_hit()
	tsweed001.enabled = 0
	MoveWeedsDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "weedhurt"
	WeedsChecker = (WeedsChecker OR 1)
	EnableWeedss()
end sub

sub MoveWeedsDown()
	Dim X
	For Each X in Weedss
		X.Visible = 0
	Next
end sub

sub tsweed002_hit()
	tsweed002.enabled = 0
	MoveWeedsDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "weedhurt"
	WeedsChecker = (WeedsChecker OR 2)
	EnableWeedss()
end sub

sub tsweed003_hit()
	tsweed003.enabled = 0
	MoveWeedsDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "weedhurt"
	WeedsChecker = (WeedsChecker OR 4)
	EnableWeedss()
end sub

sub tsweed004_hit()
	tsweed004.enabled = 0
	MoveWeedsDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "weedhurt"
	WeedsChecker = (WeedsChecker OR 8)
	EnableWeedss()
end sub

sub tsweed005_hit()
	tsweed005.enabled = 0
	MoveWeedsDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "weedhurt"
	WeedsChecker = (WeedsChecker OR 16)
	EnableWeedss()
end sub

sub CheckBonusWeeds()
	If WeedsChecker = 31 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, "100k"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		playsound ""
		WeedsChecker = 0
		TGW = 0
		stoptegridymode
		weedpoints = 0
		UpdateMusic = 0
		UpdateMusicNow
		weedultimate = weedultimate + 1
		checkweedultimate
	end if
end sub

sub checkweedultimate
select case weedultimate
				case 1 : weedtodo1
				case 2 : weedtodo2
				case 3 : weedtodo3
			end Select
End Sub

sub weedtodo1
li126.state = 1
end sub

sub weedtodo2
li127.state = 1
end sub

sub weedtodo3
weedultimate = 0
li128.state = 1
	DMD "", "", "1mln", eNone, eNone, eNone, 1000, True, "1000000"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
end sub

sub changewalls
startB2S(25)
Wall001.Sideimage = "sidewallkopie2"
Wall002.Sideimage = "sidewallkopie2"
Wall027.Collidable = True
Wall027.IsDropped = False
tractor.visible = True
tree001.visible = False
end sub

sub changebackwalls
changeb2sback2begin
Wall001.Sideimage = "sidewallkopie"
Wall002.Sideimage = "sidewallkopie"
Wall027.Collidable = False
Wall027.IsDropped = True
tractor.visible = False
tree001.visible = True
end sub

sub stoptegridymode
dim X
turny3Timer.enabled = false
changebackwalls
tsweed001.enabled = 0
tsweed002.enabled = 0
tsweed003.enabled = 0
tsweed004.enabled = 0
tsweed005.enabled = 0
	For Each X in weedss
		X.Visible = 0
	Next
end sub

'*****************
'Kickers
'*****************

'*********************Church kicker************************

sub Kicker004_hit()
ObjLevel(7) = 1 : FlasherFlash7_Timer
if churgepoints = 8 then
vpmtimer.addtimer 2000, "kickeroutchurch ' "
exit sub
end if


if churgepoints = 4 and schoolmissionmodes = 2 or musicmodes = 2 or kennymissionmodes = 2 or bMultiBallMode = true then 
Playsound "church6"
DMD "", "", "dmdfaith", eNone, eNone, eNone, 2000, True, ""
	Score(CurrentPlayer) = Score(CurrentPlayer) + (30000*PFMultiplier)
vpmtimer.addtimer 2000, "kickeroutchurch ' "
exit sub
end if
if churgepoints = 5 or TGW = 1 Then
Playsound "church6"
DMD "", "", "dmdfaith", eNone, eNone, eNone, 2000, True, ""
	Score(CurrentPlayer) = Score(CurrentPlayer) + (30000*PFMultiplier)
vpmtimer.addtimer 2000, "kickeroutchurch ' "
exit sub
end if
churgepoints = churgepoints +1
updatechurgepoints
end sub

sub updatechurgepoints
    Select Case churgepoints
        Case 1: churge1
        Case 2 : churge2
        Case 3 : churge3
        Case 4 : churge4
        Case 5 : churge5
    End Select
end sub

sub churge1
startB2S(60)
	DMD "", "", "dmdfaith", eNone, eNone, eNone, 2100, True, ""
	Score(CurrentPlayer) = Score(CurrentPlayer) + (30000*PFMultiplier)
Playsound "church1"
vpmtimer.addtimer 2100, "kickeroutchurch ' "
end sub

sub churge2
startB2S(60)
	DMD "", "", "dmdfaith", eNone, eNone, eNone, 3000, True, ""
	Score(CurrentPlayer) = Score(CurrentPlayer) + (30000*PFMultiplier)
Playsound "church2"
vpmtimer.addtimer 3000, "kickeroutchurch ' "
end sub

sub churge3
startB2S(60)
	DMD "", "", "dmdfaith", eNone, eNone, eNone, 2000, True, ""
	Score(CurrentPlayer) = Score(CurrentPlayer) + (30000*PFMultiplier)
Playsound "church3"
vpmtimer.addtimer 2000, "kickeroutchurch ' "
end sub

sub churge4
startB2S(60)
Playsound "church4"
	DMD "", "", "dmdfaith", eNone, eNone, eNone, 3000, True, ""
	Score(CurrentPlayer) = Score(CurrentPlayer) + (30000*PFMultiplier)
vpmtimer.addtimer 3000, "kickeroutchurch ' "
end sub

sub churge5
Playsound "church5"
	DMD "", "", "dmdfaith", eNone, eNone, eNone, 2000, True, ""
	Score(CurrentPlayer) = Score(CurrentPlayer) + (30000*PFMultiplier)
vpmtimer.addtimer 2000, "startfigthy ' "
end sub

sub startfigthy
li025.state = 0
li026.state = 0
StopSong
playsound "battlejesus2"
Flasher006.ImageA = "ball-black"
Flasher006.Visible = true
vpmtimer.addtimer 2000, "changeflashimage1 ' "
vpmtimer.addtimer 9000, "changeflashimage2 ' "
vpmtimer.addtimer 10000, "changeflashimage3 ' "
vpmtimer.addtimer 11000, "changeflashimage2 ' "
vpmtimer.addtimer 12000, "changeflashimage4 ' "
vpmtimer.addtimer 13000, "changeflashimage5 ' "
vpmtimer.addtimer 13500, "changeflashimage4 ' "
vpmtimer.addtimer 14500, "changeflashimage6 ' "
vpmtimer.addtimer 15500, "changeflashimage7 ' "
vpmtimer.addtimer 18000, "startheavenfight ' "
end sub

sub changeflashimage1
Flasher006.ImageA = "battle01"
end sub

sub changeflashimage2
Flasher006.ImageA = "battle02"
end sub

sub changeflashimage3
Flasher006.ImageA = "battle03"
end sub

sub changeflashimage4
Flasher006.ImageA = "battle04"
end sub

sub changeflashimage5
Flasher006.ImageA = "battle05"
end sub

sub changeflashimage6
Flasher006.ImageA = "battle06"
end sub

sub changeflashimage7
Flasher006.ImageA = "battle07"
end sub

Sub kickeroutchurch
Playsound SoundFXDOF("fx_kicker", 112, DOFPulse, DOFContactors)
'Playsound "fx_kicker"
Kicker004.Kick 110, 24, 24
changeb2sback2begin
end sub

Dim HeavenFight : HeavenFight = False 

Sub startheavenfight
churchlocked = 2
Flasher006.Visible = False
UpdateMusic = 19
UpdateMusicNow
kickeroutchurch
li087.state = 1
li002.state = 1
hitgate3 = 0
hitgate2 = 0
heavensatan = 0
heavenjesus = 0
Jesus1.visible = True
Jesus2.visible = False
satan1.visible = True
satan2.visible = False
HeavenFight = True
startfirechurch
end sub

sub Gate002_hit
    DOF 113, DOFPulse
	If HeavenFight then
		If activeball.vely < 0 then
			Gate2.enabled=true
			if hitgate2 = 0 then
			changegate2jesus
			Exit sub
			end if
			if hitgate2 = 1 then
			changegate2satan
			Exit sub
			end if
		end if
	end if
end Sub

Sub Gate2_timer
	Gate2.enabled=False
	If Hitgate3 = 3 Then hitgate2=1 : hitgate3=1 	
	If Hitgate2 = 2 Then hitgate2=0 : hitgate3=0 
end Sub

sub changegate2jesus
heavensatan =  heavensatan + 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (25000*PFMultiplier)
		DMD "", "", "25k", eNone, eNone, eNone, 1000, True, ""
PlayQuoteSatan
Jesus1.visible = False
Jesus2.visible = True
satan1.visible = False
satan2.visible = True
Flasher003.ImageA = "evil"
Flasher004.ImageA = "good"
checkstatusheavenfight2
Hitgate3 = 3
Hitgate2 = 3
end sub

sub changegate2satan
heavenjesus =  heavenjesus + 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (25000*PFMultiplier)
		DMD "", "", "25k", eNone, eNone, eNone, 1000, True, ""
PlayQuoteJesus
Jesus1.visible = True
Jesus2.visible = False
satan1.visible = True
satan2.visible = False
Flasher003.ImageA = "good"
Flasher004.ImageA = "evil"
checkstatusheavenfight
Hitgate3 = 2
Hitgate2 = 2
end sub

sub Gate003_hit
    DOF 113, DOFPulse
	If HeavenFight then
		If activeball.vely < 0 then
			Gate3.enabled=true
			if hitgate3 = 0 then
			changegate3jesus
			Exit sub
			end if
			if hitgate3 = 1 then
			changegate3satan
			Exit sub
			end if
		end if
	end if
end Sub

Sub Gate3_timer
	Gate3.enabled=False
	If Hitgate3 = 3 Then hitgate2=1 : hitgate3=1 	
	If Hitgate2 = 2 Then hitgate2=0 : hitgate3=0 
end Sub

sub changegate3jesus
heavenjesus =  heavenjesus + 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (25000*PFMultiplier)
		DMD "", "", "25k", eNone, eNone, eNone, 1000, True, ""
PlayQuoteJesus
Jesus1.visible = False
Jesus2.visible = True
satan1.visible = False
satan2.visible = True
Flasher003.ImageA = "evil"
Flasher004.ImageA = "good"
checkstatusheavenfight
Hitgate3 = 3
Hitgate2 = 3
end sub

sub changegate3satan
heavensatan =  heavensatan + 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (25000*PFMultiplier)
		DMD "", "", "25k", eNone, eNone, eNone, 1000, True, ""
PlayQuoteSatan
Jesus1.visible = True
Jesus2.visible = False
satan1.visible = True
satan2.visible = False
Flasher003.ImageA = "good"
Flasher004.ImageA = "evil"
checkstatusheavenfight2
Hitgate3 = 2
Hitgate2 = 2
end sub


sub checkstatusheavenfight
    Select Case heavenjesus
        Case 1: Flasher001.ImageA="sc1"
        Case 2 : Flasher001.ImageA="sc2"
        Case 3 : Flasher001.ImageA="sc3"
        Case 4 : Flasher001.ImageA="sc4"
        Case 5 : Flasher001.ImageA="sc5"
        Case 6 : Flasher001.ImageA="sc6"
        Case 7 : Flasher001.ImageA="sc7"
        Case 8 : Flasher001.ImageA="sc8"
        Case 9 : Flasher001.ImageA="sc9"
        Case 10 : Flasher001.ImageA="sc10":jesuswon
    End Select
end sub

sub jesuswon
HeavenFight = False
stopfirechurch
	Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, "100k"
li087.state = 0
li002.state = 0
li025.state = 1
hitgate3 = 0
hitgate2 = 0
heavensatan = 0
heavenjesus = 0
Jesus1.visible = False
Jesus2.visible = False
satan1.visible = False
satan2.visible = False
Flasher002.ImageA="sc0"
Flasher001.ImageA="sc0"
churgepoints = 0
UpdateMusic = 0
UpdateMusicNow
churchlocked = 0
end sub

sub checkstatusheavenfight2
    Select Case heavensatan
        Case 1: Flasher002.ImageA="sc1"
        Case 2 : Flasher002.ImageA="sc2"
        Case 3 : Flasher002.ImageA="sc3"
        Case 4 : Flasher002.ImageA="sc4"
        Case 5 : Flasher002.ImageA="sc5"
        Case 6 : Flasher002.ImageA="sc6"
        Case 7 : Flasher002.ImageA="sc7"
        Case 8 : Flasher002.ImageA="sc8"
        Case 9 : Flasher002.ImageA="sc9"
        Case 10 : Flasher002.ImageA="sc10":satanwon
    End Select
end sub

sub satanwon
HeavenFight = False
stopfirechurch
	Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, "100k"
li087.state = 0
li002.state = 0
li026.state = 1
hitgate3 = 0
hitgate2 = 0
heavensatan = 0
heavenjesus = 0
Jesus1.visible = False
Jesus2.visible = False
satan1.visible = False
satan2.visible = False
Flasher002.ImageA="sc0"
Flasher001.ImageA="sc0"
churgepoints = 0
UpdateMusic = 0
UpdateMusicNow
churchlocked = 0
end sub

Dim FireChurch1Pos, ChurchFlames
ChurchFlames = Array("fire_1", "fire_2", "fire_3", "fire_4", "fire_5", "fire_6")

Sub FireChurchTimer_Timer
    'debug.print FireChurch1pos
    FireChurch1.ImageA = ChurchFlames(FireChurch1Pos)
    FireChurch1Pos = (FireChurch1Pos + 1) MOD 6
End Sub

sub stopfirechurch
FireChurch1.Visible = False
FireChurchTimer.Enabled = False
end sub

sub startfirechurch
FireChurch1.Visible = True
FireChurchTimer.Enabled = True
end sub

'*********************Chef sweet loving bonus************************

Sub Kicker001_hit()
ObjLevel(9) = 1 : FlasherFlash9_Timer
LightSeq001.StopPlay
LightSeq002.StopPlay
sweetlightsflash
if PFMultiplier = 8 then
Playsound "blackass"
vpmtimer.addtimer 1505, "kickeroutchef ' "
DMD "", "", "dmd9x", eNone, eNone, eNone, 1400, True, ""
exit sub
end if
if PFMultiplier = 1 then
li079.state = 1
Playsound "chf_1"
PFMultiplier = 2
vpmtimer.addtimer 5005, "kickeroutchef ' "
DMD "", "", "dmd2x", eNone, eNone, eNone, 4900, True, ""
exit sub
end if
if PFMultiplier = 2 then
li080.state = 1
Playsound "chf_2"
PFMultiplier = 3
vpmtimer.addtimer 2005, "kickeroutchef ' "
DMD "", "", "dmd3x", eNone, eNone, eNone, 1900, True, ""
exit sub
end if
if PFMultiplier = 3 then
li081.state = 1
Playsound "chf_3"
PFMultiplier = 4
vpmtimer.addtimer 1005, "kickeroutchef ' "
DMD "", "", "dmd4x", eNone, eNone, eNone, 1000, True, ""
exit sub
end if
if PFMultiplier = 4 then
li082.state = 1
Playsound "chf_4"
PFMultiplier = 5
vpmtimer.addtimer 2005, "kickeroutchef ' "
DMD "", "", "dmd5x", eNone, eNone, eNone, 1900, True, ""
exit sub
end if
if PFMultiplier = 5 then
li083.state = 1
Playsound "chf_5"
PFMultiplier = 6
vpmtimer.addtimer 3005, "kickeroutchef ' "
DMD "", "", "dmd6x", eNone, eNone, eNone, 2900, True, ""
exit sub
end if
if PFMultiplier = 6 then
li084.state = 1
Playsound "chf_6"
PFMultiplier = 7
vpmtimer.addtimer 2005, "kickeroutchef ' "
DMD "", "", "dmd7x", eNone, eNone, eNone, 1900, True, ""
exit sub
end if
if PFMultiplier = 7 then
li085.state = 1
Playsound "chf_7"
PFMultiplier = 8
vpmtimer.addtimer 3005, "kickeroutchef ' "
DMD "", "", "dmd8x", eNone, eNone, eNone, 2900, True, ""
exit sub
end if
End Sub

Sub kickeroutchef
Playsound SoundFXDOF("fx_kicker", 110, DOFPulse, DOFContactors)
Kicker001.Kick 0, 24, 24
sweetlightsstady
end sub

sub sweetlightsflash
li107.state = 2
li108.state = 2
li109.state = 2
li110.state = 2
li111.state = 2
li112.state = 2
li113.state = 2
li114.state = 2
li115.state = 2
li116.state = 2
li117.state = 2
li118.state = 2
li119.state = 2
li120.state = 2
li121.state = 2
li122.state = 2
end Sub

sub sweetlightsstady
li107.state = 1
li108.state = 1
li109.state = 1
li110.state = 1
li111.state = 1
li112.state = 1
li113.state = 1
li114.state = 1
li115.state = 1
li116.state = 1
li117.state = 1
li118.state = 1
li119.state = 1
li120.state = 1
li121.state = 1
li122.state = 1
end sub

'*********************kenny kicker************************

Sub Kicker002_hit()
if TGW = 1 then
	countr11 = 0
	DMD "", "", "dmdweedbonus", eNone, eNone, eNone, 1500, False, "yeehaw"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
	tractoromhoog.enabled = True
	vpmTimer.AddTimer 1000, "kickoutkennys'"
	exit Sub
	end if
if schoolmissionmodes = 2 or musicmodes = 2 or kennymissionmodes = 2 or bMultiBallMode = true or churchlocked = 2 then 
	vpmTimer.AddTimer 1000, "kickoutkennys'"
	exit Sub
	end if
if KillKenny = 2 Then
	shaketree
	exit Sub
end if
StopSong
playsong "kennymusic"
DMDFlush
PlayerSelectActive = True
DMD "", "", "dmdbs", eNone, eNone, eNone, 10000, False, ""
end sub

sub kickoutkennys
flushit
Playsound "fx_kicker"
Kicker002.Kick 170, 4, 12
end sub

sub flushit
DMDScoreNow
end sub

Sub tractoromhoog_Timer
countr11 = countr11 + 1 : If Countr11 > 7 then stoptractor 'Countr11 = 1 : end If 
select case countr11
				case 1 : tractor.RotZ=0
				case 2 : tractor.RotZ=20
				case 3 : tractor.RotZ=40
				case 4 : tractor.RotZ=60
				case 5 : tractor.RotZ=40
				case 6 : tractor.RotZ=20
				case 7 : tractor.RotZ=0
			end Select
End Sub

sub stoptractor
tractoromhoog.enabled = false
end sub

'*****************
' tree shaker
'*****************

sub shaketree
Kcompleted = Kcompleted + 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
Playsound "killedkenny"
BumperBonus = BumperBonus + 1
li001.state = 1
startkilldmd
tree1Shaker
KillKenny = 0
vpmTimer.AddTimer 4300, "ressetkennyT'"
vpmTimer.AddTimer 4700, "kickoutkennys'"
vpmtimer.addtimer 4500, "shoutpoints2 '"
end sub

Sub startkilldmd
DMD "", "", "dmdkillk1", eNone, eNone, eNone, 100, False, ""
DMD "", "", "dmdkillk2", eNone, eNone, eNone, 100, False, ""
DMD "", "", "dmdkillk1", eNone, eNone, eNone, 100, False, ""
DMD "", "", "dmdkillk2", eNone, eNone, eNone, 100, False, ""
DMD "", "", "dmdkillk1", eNone, eNone, eNone, 100, False, ""
DMD "", "", "dmdkillk2", eNone, eNone, eNone, 100, False, ""
DMD "", "", "dmdkillk1", eNone, eNone, eNone, 100, False, ""
DMD "", "", "dmdkillk2", eNone, eNone, eNone, 100, False, ""
DMD "", "", "dmdkillk1", eNone, eNone, eNone, 100, False, ""
DMD "", "", "dmdkillk2", eNone, eNone, eNone, 100, False, ""
DMD "", "", "dmdkillk1", eNone, eNone, eNone, 100, False, ""
DMD "", "", "dmdkillk2", eNone, eNone, eNone, 100, False, ""
DMD "", "", "dmdkillk1", eNone, eNone, eNone, 100, False, ""
DMD "", "", "dmdkillk2", eNone, eNone, eNone, 100, False, ""
DMD "", "", "1mln", eNone, eNone, eNone, 3100, False, ""
end sub

sub ressetkennyT
li071.state = 0
li072.state = 0
li044.state = 0
li045.state = 0
li046.state = 0
li047.state = 0
li048.state = 0
Target016.IsDropped = false
Target017.IsDropped = false
Target018.IsDropped = false
Target019.IsDropped = false
Target020.IsDropped = false
Playsound "fx_resetdrop"
end sub


Dim tree1Shake

Sub tree1Shaker()
    tree1Shake = 10
    tree1Timer.Enabled = True
End Sub

Sub tree1Timer_Timer()
    tree001.RotZ = tree1Shake
    If tree1Shake <= 0.1 AND tree1Shake >= -0.1 Then Me.Enabled = False:Exit Sub
    If tree1Shake < 0 Then
        tree1Shake = ABS(tree1Shake)- 0.1
    Else
        tree1Shake = - tree1Shake + 0.1
    End If
End Sub

Sub SelectPlayerStart(Keycode)
'    DMD "", "", SlotAward(SelectCounter), eNone, eNone, eNone, 500, False, ""
    If keycode = LeftFlipperKey then
		Playsound "beep"
        SelectCounter = SelectCounter - 1
       If SelectCounter = -1 Then SelectCounter = 20
   end If
    If keycode = RightFlipperKey then
		Playsound "beep"
        SelectCounter = SelectCounter + 1
        If SelectCounter = 21 Then SelectCounter = 0
    end If
    Select Case SelectCounter
        Case 0
			if p1 = 1 then
            DMDFlush
            DMD "", "", "dmdclck11", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 0
			end if
			if p1 = 0 then 
            DMDFlush
            DMD "", "", "dmdkb", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 0
			end if
        Case 1
			if p2 = 1 then
            DMDFlush
            DMD "", "", "terlck", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 1
			end if
			if p2 = 0 then 
            DMDFlush
            DMD "", "", "dmdt1", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 1
			end if
        Case 2
			if p3 = 1 then
            DMDFlush
            DMD "", "", "sdlck", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 2
			end if
			if p3 = 0 then 
            DMDFlush
            DMD "", "", "dmdk2", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 2
			end if
        Case 3
			if p4 = 1 then
            DMDFlush
            DMD "", "", "terlck2", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 3
			end if
			if p4 = 0 then 
            DMDFlush
            DMD "", "", "dmdt2", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 3
			end if
        Case 4
			if p5 = 1 then
            DMDFlush
            DMD "", "", "hitlck", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 4
			end if
			if p5 = 0 then 
            DMDFlush
            DMD "", "", "dmdk1", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 4
			end if
        Case 5
			if p6 = 1 then
            DMDFlush
            DMD "", "", "terlck3", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 5
			end if
			if p6 = 0 then 
            DMDFlush
            DMD "", "", "dmdt3", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 5
			end if
        Case 6
			if p7 = 1 then
            DMDFlush
            DMD "", "", "oslck", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 6
			end if
			if p7 = 0 then 
            DMDFlush
            DMD "", "", "dmdk3", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 6
			end if
        Case 7
			if p8 = 1 then
            DMDFlush
            DMD "", "", "dmdclck", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 7
			end if
			if p8 = 0 then 
            DMDFlush
            DMD "", "", "dmds1", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 7
			end if
        Case 8
			if p9 = 1 then
            DMDFlush
            DMD "", "", "dmdclck10", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 8
			end if
			if p9 = 0 then 
            DMDFlush
            DMD "", "", "dmds2", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 8
			end if
        Case 9
			if p10 = 1 then
            DMDFlush
            DMD "", "", "dmdclck", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 9
			end if
			if p10 = 0 then 
            DMDFlush
            DMD "", "", "dmds3", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 9
			end if
        Case 10
			if p11 = 1 then
            DMDFlush
            DMD "", "", "dmdclck8", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 10
			end if
			if p11 = 0 then 
            DMDFlush
            DMD "", "", "dmds4", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 10
			end if
        Case 11
			if p12 = 1 then
            DMDFlush
            DMD "", "", "dmdclck9", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 11
			end if
			if p12 = 0 then 
            DMDFlush
            DMD "", "", "dmds5", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 11
			end if
        Case 12
			if p13 = 1 then
            DMDFlush
            DMD "", "", "dmdclck8", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 12
			end if
			if p13 = 0 then 
            DMDFlush
            DMD "", "", "dmds6", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 12
			end if
        Case 13
			if p14 = 1 then
            DMDFlush
            DMD "", "", "dmdclck7", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 13
			end if
			if p14 = 0 then 
            DMDFlush
            DMD "", "", "dmds7", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 13
			end if
        Case 14
			if p15 = 1 then
            DMDFlush
            DMD "", "", "dmdclck6", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 14
			end if
			if p15 = 0 then 
            DMDFlush
            DMD "", "", "dmds8", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 14
			end if
        Case 15
			if p16 = 1 then
            DMDFlush
            DMD "", "", "dmdclck5", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 15
			end if
			if p16 = 0 then 
            DMDFlush
            DMD "", "", "dmds9", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 15
			end if
        Case 16
			if p17 = 1 then
            DMDFlush
            DMD "", "", "dmdclck4", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 16
			end if
			if p17 = 0 then 
            DMDFlush
            DMD "", "", "dmds10", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 16
			end if
        Case 17
			if p18 = 1 then
            DMDFlush
            DMD "", "", "dmdclck3", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 17
			end if
			if p18 = 0 then 
            DMDFlush
            DMD "", "", "dmds11", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 17
			end if
        Case 18
			if p19 = 1 then
            DMDFlush
            DMD "", "", "dmdclck2", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 18
			end if
			if p19 = 0 then 
            DMDFlush
            DMD "", "", "dmds12", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 18
			end if
        Case 19
			if p36 = 1 then
            DMDFlush
            DMD "", "", "dmdlclt", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 19
			end if
			if p36 = 0 then 
            DMDFlush
            DMD "", "", "dmdkclt", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 19
			end if
        Case 20
			if p37 = 1 then
            DMDFlush
            DMD "", "", "dmdlkjn", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 20
			end if
			if p37 = 0 then 
            DMDFlush
            DMD "", "", "dmddkjn", eNone, eNone, eNone, 10000, False, ""
            SlotValue = 20
			end if
    End Select

    If keycode = PlungerKey Then
        Select Case SlotValue
            Case 0
				if p1 = 1 then
				StopSong
				Kicker002_hit
				exit sub
				end if
				PlayerSelectActive = False
				DMDFlush
				kickbabysonplay = 1
				churchy.Image = "churchy"
				countr34 = countr34 + 1
				checkkickthebaby
				playsound "kickthebabystart"
				UpdateMusicNow
				p1 = 1
                vpmtimer.addtimer 2100, "kickoutkennys '"
            Case 1
				if p2 = 1 then
				Kicker002_hit
				exit sub
				end if
				startB2S(10)
				StopSong
				ter1a = 0
				ter1b = 0
				UpdateMusic = 0
				UpdateMusic = UpdateMusic + 14
				PlayerSelectActive = False
				playsound "Stopterrorist"
				DMDFlush
				Startballon
				shot003timer.enabled = False
				shot004timer.enabled = False
				terror1.image = "terror1a"
				terror2.image = "terror2a"
				movingshot001.z = 20
				terror1.visible = True
				movingshot002.z = 20
				terror2.visible = True
				shot001timer.enabled = True
				shot002timer.enabled = True
				UpdateMusicNow
				kennymissionmodes = 2
                vpmtimer.addtimer 2000, "kickoutkennys '"
            Case 2
				if p3 = 1 then
				Kicker002_hit
				exit sub
				end if
				startB2S(3)
				StopSong
				bosssaddam = 1
				bhit = 0
				bvul = 0
				bhurt = 0
				Startballon
				li010.state = 2
				tsboss001.enabled = true
				tsboss002.enabled = true
				dildo001.Visible = True
				dildo002.Visible = True
				UpdateMusic = 0
				UpdateMusic = UpdateMusic + 13
				PlayerSelectActive = False
				PlaySound "saddamstart"
				DMDFlush
				shot004timer.enabled = False
				terror2.image = "dssdm"
				movingshot002.z = 20
				terror2.visible = True
				shot002timer.enabled = True
				kennymissionmodes = 2
				turny3Timer.enabled = True
                vpmtimer.addtimer 3000, "UpdateMusicNow '"				
                vpmtimer.addtimer 3000, "kickoutkennys '"
            Case 3
				if p4 = 1 then
				Kicker002_hit
				exit sub
				end if
				startB2S(10)
				ter2a = 0
				ter2b = 0
				StopSong
				UpdateMusic = 0
				UpdateMusic = UpdateMusic + 14
				PlayerSelectActive = False
				playsound "Stopterrorist"
				DMDFlush
				Startballon
				shot003timer.enabled = False
				shot004timer.enabled = False
				terror1.image = "terror1a1"
				terror2.image = "terror2a2"
				movingshot001.z = 20
				terror1.visible = True
				movingshot002.z = 20
				terror2.visible = True
				shot001timer.enabled = True
				shot002timer.enabled = True
				UpdateMusicNow
				kennymissionmodes = 2
                vpmtimer.addtimer 2000, "kickoutkennys '"
            Case 4
				if p5 = 1 then
				Kicker002_hit
				exit sub
				end if
				startB2S(2)
				StopSong
				bosshitler = 1
				bhit = 0
				bvul = 0
				bhurt = 0
				Startballon
				li123.state = 2
				tsboss001.enabled = true
				tsboss002.enabled = true
				fhit1.Visible = True
				fhit2.Visible = True
				UpdateMusic = 0
				UpdateMusic = UpdateMusic + 15
				PlayerSelectActive = False
				PlaySound "hitlerstart"
				DMDFlush
				shot004timer.enabled = False
				terror2.image = "dshit"
				movingshot002.z = 20
				terror2.visible = True
				shot002timer.enabled = True
				kennymissionmodes = 2
				turny3Timer.enabled = True
                vpmtimer.addtimer 3000, "UpdateMusicNow '"				
                vpmtimer.addtimer 3000, "kickoutkennys '"
            Case 5
				if p6 = 1 then
				Kicker002_hit
				exit sub
				end if
				startB2S(10)
				ter3a = 0
				ter3b = 0
				StopSong
				UpdateMusic = 0
				UpdateMusic = UpdateMusic + 14
				PlayerSelectActive = False
				playsound "Stopterrorist"
				DMDFlush
				Startballon
				shot003timer.enabled = False
				shot004timer.enabled = False
				terror1.image = "terror1"
				terror2.image = "terror2"
				movingshot001.z = 20
				terror1.visible = True
				movingshot002.z = 20
				terror2.visible = True
				shot001timer.enabled = True
				shot002timer.enabled = True
				UpdateMusicNow
				kennymissionmodes = 2
                vpmtimer.addtimer 2000, "kickoutkennys '"
            Case 6
				if p7 = 1 then
				Kicker002_hit
				exit sub
				end if
				startB2S(11)
				StopSong
				PlaySound "BLOWYOURHEADOFF"
				bossosama = 1
				bhit = 0
				bvul = 0
				bhurt = 0
				li125.state = 2
				tsboss001.enabled = true
				tsboss002.enabled = true
				camel1.Visible = True
				camel2.Visible = True
				UpdateMusic = 0
				UpdateMusic = UpdateMusic + 16
				PlayerSelectActive = False
				DMDFlush
				Startballon
				shot004timer.enabled = False
				terror2.image = "dsosm"
				movingshot002.z = 20
				terror2.visible = True
				shot002timer.enabled = True
				kennymissionmodes = 2
				turny3Timer.enabled = True
                vpmtimer.addtimer 2000, "UpdateMusicNow '"				
                vpmtimer.addtimer 2000, "kickoutkennys '"
            Case 7
				if p8 = 1 then
				Kicker002_hit
				exit sub
				end if
				PlayerSelectActive = False
				UpdateMusic = 0
				UpdateMusic = UpdateMusic + 1
				DMDFlush
				UpdateMusicNow
				musicmodes = 2
				turny2Timer.enabled = True
				Ssong1
            Case 8
				if p9 = 1 then
				Kicker002_hit
				exit sub
				end if
				PlayerSelectActive = False
				UpdateMusic = 0
				UpdateMusic = UpdateMusic + 2
				DMDFlush
				UpdateMusicNow
				musicmodes = 2
				turny2Timer.enabled = True
				Ssong2
            Case 9
				if p10 = 1 then
				Kicker002_hit
				exit sub
				end if
				PlayerSelectActive = False
				UpdateMusic = 0
				UpdateMusic = UpdateMusic + 3
				DMDFlush
				UpdateMusicNow
				musicmodes = 2
				turny2Timer.enabled = True
				Ssong3
            Case 10
				if p11 = 1 then
				Kicker002_hit
				exit sub
				end if
				PlayerSelectActive = False
				UpdateMusic = 0
				UpdateMusic = UpdateMusic + 4
				DMDFlush
				UpdateMusicNow
				musicmodes = 2
				turny2Timer.enabled = True
				Ssong4
            Case 11
				if p12 = 1 then
				Kicker002_hit
				exit sub
				end if
				PlayerSelectActive = False
				UpdateMusic = 0
				UpdateMusic = UpdateMusic + 5
				DMDFlush
				UpdateMusicNow
				musicmodes = 2
				turny2Timer.enabled = True
				Ssong5
            Case 12
				if p13 = 1 then
				Kicker002_hit
				exit sub
				end if
				PlayerSelectActive = False
				UpdateMusic = 0
				UpdateMusic = UpdateMusic + 6
				DMDFlush
				UpdateMusicNow
				musicmodes = 2
				turny2Timer.enabled = True
				Ssong6
            Case 13
				if p14 = 1 then
				Kicker002_hit
				exit sub
				end if
				PlayerSelectActive = False
				UpdateMusic = 0
				UpdateMusic = UpdateMusic + 7
				DMDFlush
				UpdateMusicNow
				musicmodes = 2
				turny2Timer.enabled = True
				Ssong7
            Case 14
				if p15 = 1 then
				Kicker002_hit
				exit sub
				end if
				PlayerSelectActive = False
				UpdateMusic = 0
				UpdateMusic = UpdateMusic + 8
				DMDFlush
				UpdateMusicNow
				musicmodes = 2
				turny2Timer.enabled = True
				Ssong8
            Case 15
				if p16 = 1 then
				Kicker002_hit
				exit sub
				end if
				PlayerSelectActive = False
				UpdateMusic = 0
				UpdateMusic = UpdateMusic + 9
				DMDFlush
				UpdateMusicNow
				musicmodes = 2
				turny2Timer.enabled = True
				Ssong9
            Case 16
				if p17 = 1 then
				Kicker002_hit
				exit sub
				end if
				PlayerSelectActive = False
				UpdateMusic = 0
				UpdateMusic = UpdateMusic + 10
				DMDFlush
				UpdateMusicNow
				musicmodes = 2
				turny2Timer.enabled = True
				Ssong10
            Case 17
				if p18 = 1 then
				Kicker002_hit
				exit sub
				end if
				PlayerSelectActive = False
				UpdateMusic = 0
				UpdateMusic = UpdateMusic + 11
				DMDFlush
				UpdateMusicNow
				musicmodes = 2
				turny2Timer.enabled = True
				Ssong11
            Case 18
				if p19 = 1 then
				Kicker002_hit
				exit sub
				end if
				PlayerSelectActive = False
				UpdateMusic = 0
				UpdateMusic = UpdateMusic + 12
				DMDFlush
				UpdateMusicNow
				musicmodes = 2
				turny2Timer.enabled = True
				Ssong12
            Case 19
				if p36 = 1 then
				Kicker002_hit
				exit sub
				end if
				startB2S(8)
				StopSong
				bossclitty = 1
				bhit = 0
				bvul = 0
				bhurt = 0
				Startballon
				li009.state = 2 
				tsboss001.enabled = true
				tsboss002.enabled = true
				clita001.Visible = True
				clita002.Visible = True
				UpdateMusic = 0
				UpdateMusic = UpdateMusic + 20
				PlayerSelectActive = False
				PlaySound "clitstart"
				DMDFlush
				shot004timer.enabled = False
				terror2.image = "dsclit"
				movingshot002.z = 20
				terror2.visible = True
				shot002timer.enabled = True
				kennymissionmodes = 2
				turny3Timer.enabled = True
                vpmtimer.addtimer 2000, "UpdateMusicNow '"				
                vpmtimer.addtimer 2000, "kickoutkennys '"
            Case 20
				if p37 = 1 then
				Kicker002_hit
				exit sub
				end if
				startB2S(20)
				StopSong
				bosskim = 1
				bhit = 0
				bvul = 0
				bhurt = 0
				Startballon
				li124.state = 2
				tsboss001.enabled = true
				tsboss002.enabled = true
				bomba.Visible = True
				bombb.Visible = True
				UpdateMusic = 0
				UpdateMusic = UpdateMusic + 21
				PlayerSelectActive = False
				PlaySound "nkstart"
				DMDFlush
				shot004timer.enabled = False
				terror2.image = "dskim"
				movingshot002.z = 20
				terror2.visible = True
				shot002timer.enabled = True
				kennymissionmodes = 2
				turny3Timer.enabled = True
                vpmtimer.addtimer 3000, "UpdateMusicNow '"				
                vpmtimer.addtimer 3000, "kickoutkennys '"
        End Select
    end if
end sub

'********Ssong1 mode***************

sub Ssong1()
	enablesong1s()
	li013.state = 2
	song1Checker = 0
	kickoutkennys
end sub

Dim Whichsong1, song1Checker
Whichsong1 = 0
song1Checker = 0
sub enablesong1s()
	If song1Checker = 7 Then
		CheckBonussong1()
		Exit Sub
	End If
	Randomize()
	Whichsong1 = INT(RND * 3) + 1
	Select Case Whichsong1
		Case 3
			Whichsong1 = 4
	End Select
	Do While (Whichsong1 AND song1Checker) > 0
		Whichsong1 = INT(RND * 3) + 1
		Select Case Whichsong1
			Case 3
				Whichsong1 = 4
		End Select
	Loop
	Select Case Whichsong1
		Case 1
			tsong1001.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong1001.X
			Mnote001.Y = tsong1001.Y
		Case 2
			tsong1002.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong1002.X
			Mnote001.Y = tsong1002.Y
		Case 4
			tsong1003.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong1003.X
			Mnote001.Y = tsong1003.Y
	End Select
end sub

sub movesong1down()
	Dim X
	For Each X in songss
		X.Visible = 0
	Next
end sub

Sub tsong1001_Hit()
	tsong1001.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong1down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
	Playsound "songhurt1"
	song1Checker = (song1Checker OR 1)
	Enablesong1s()
end sub

Sub tsong1002_Hit()
	tsong1002.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong1down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt2"
	song1Checker = (song1Checker OR 2)
	Enablesong1s()
end sub

Sub tsong1003_Hit()
	tsong1003.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong1down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt3"
	song1Checker = (song1Checker OR 4)
	Enablesong1s()
end sub

sub checkbonussong1()
	If song1Checker = 7 then
		DMD "", "", "200k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (200000*PFMultiplier)
		li013.state = 1
		turny2Timer.enabled = True
		song1Checker = 0
		musicmodes = 0
		UpdateMusic = 0
		Scompleted = Scompleted + 1
		UpdateMusicNow
		csongs = csongs + 1
		checkcsongs
		turny2Timer.enabled = False
		p8 = 1
	end if
end sub

'********Ssong2 mode***************

sub Ssong2()
	enablesong2s()
	li014.state = 2
	song2Checker = 0
	kickoutkennys
end sub

Dim Whichsong2, song2Checker
Whichsong2 = 0
song2Checker = 0
sub enablesong2s()
	If song2Checker = 7 Then
		CheckBonussong2()
		Exit Sub
	End If
	Randomize()
	Whichsong2 = INT(RND * 3) + 1
	Select Case Whichsong2
		Case 3
			Whichsong2 = 4
	End Select
	Do While (Whichsong2 AND song2Checker) > 0
		Whichsong2 = INT(RND * 3) + 1
		Select Case Whichsong2
			Case 3
				Whichsong2 = 4
		End Select
	Loop
	Select Case Whichsong2
		Case 1
			tsong2001.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong2001.X
			Mnote001.Y = tsong2001.Y
		Case 2
			tsong2002.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong2002.X
			Mnote001.Y = tsong2002.Y
		Case 4
			tsong2003.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong2003.X
			Mnote001.Y = tsong2003.Y
	End Select
end sub

sub movesong2down()
	Dim X
	For Each X in songss
		X.Visible = 0
	Next
end sub

Sub tsong2001_Hit()
	tsong2001.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong2down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt1"
	song2Checker = (song2Checker OR 1)
	Enablesong2s()
end sub

Sub tsong2002_Hit()
	tsong2002.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong2down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt2"
	song2Checker = (song2Checker OR 2)
	Enablesong2s()
end sub

Sub tsong2003_Hit()
	tsong2003.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong2down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt3"
	song2Checker = (song2Checker OR 4)
	Enablesong2s()
end sub

sub checkbonussong2()
	If song2Checker = 7 then
		DMD "", "", "200k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (200000*PFMultiplier)
		li014.state = 1
		song2Checker = 0
		musicmodes = 0
		Scompleted = Scompleted + 1
		csongs = csongs + 1
		checkcsongs
		UpdateMusic = 0
		UpdateMusicNow
		turny2Timer.enabled = False
		p9 = 1
	end if
end sub

'********Ssong3 mode***************

sub Ssong3()
	enablesong3s()
	li015.state = 2
	song3Checker = 0
	kickoutkennys
end sub

Dim Whichsong3, song3Checker
Whichsong3 = 0
song3Checker = 0
sub enablesong3s()
	If song3Checker = 7 Then
		CheckBonussong3()
		Exit Sub
	End If
	Randomize()
	Whichsong3 = INT(RND * 3) + 1
	Select Case Whichsong3
		Case 3
			Whichsong3 = 4
	End Select
	Do While (Whichsong3 AND song3Checker) > 0
		Whichsong3 = INT(RND * 3) + 1
		Select Case Whichsong3
			Case 3
				Whichsong3 = 4
		End Select
	Loop
	Select Case Whichsong3
		Case 1
			tsong3001.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong3001.X
			Mnote001.Y = tsong3001.Y
		Case 2
			tsong3002.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong3002.X
			Mnote001.Y = tsong3002.Y
		Case 4
			tsong3003.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong3003.X
			Mnote001.Y = tsong3003.Y
	End Select
end sub

sub movesong3down()
	Dim X
	For Each X in songss
		X.Visible = 0
	Next
end sub

Sub tsong3001_Hit()
	tsong3001.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong3down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt1"
	song3Checker = (song3Checker OR 1)
	Enablesong3s()
end sub

Sub tsong3002_Hit()
	tsong3002.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong3down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt2"
	song3Checker = (song3Checker OR 2)
	Enablesong3s()
end sub

Sub tsong3003_Hit()
	tsong3003.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong3down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt3"
	song3Checker = (song3Checker OR 4)
	Enablesong3s()
end sub

sub checkbonussong3()
	If song3Checker = 7 then
		DMD "", "", "200k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (200000*PFMultiplier)
		li015.state = 1
		song3Checker = 0
		musicmodes = 0
		Scompleted = Scompleted + 1
		csongs = csongs + 1
		checkcsongs
		UpdateMusic = 0
		UpdateMusicNow
		turny2Timer.enabled = False
		p10 = 1
	end if
end sub

'********Ssong4 mode***************

sub Ssong4()
	enablesong4s()
	li016.state = 2
	song4Checker = 0
	kickoutkennys
end sub

Dim Whichsong4, song4Checker
Whichsong4 = 0
song4Checker = 0
sub enablesong4s()
	If song4Checker = 7 Then
		CheckBonussong4()
		Exit Sub
	End If
	Randomize()
	Whichsong4 = INT(RND * 3) + 1
	Select Case Whichsong4
		Case 3
			Whichsong4 = 4
	End Select
	Do While (Whichsong4 AND song4Checker) > 0
		Whichsong4 = INT(RND * 3) + 1
		Select Case Whichsong4
			Case 3
				Whichsong4 = 4
		End Select
	Loop
	Select Case Whichsong4
		Case 1
			tsong4001.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong4001.X
			Mnote001.Y = tsong4001.Y
		Case 2
			tsong4002.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong4002.X
			Mnote001.Y = tsong4002.Y
		Case 4
			tsong4003.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong4003.X
			Mnote001.Y = tsong4003.Y
	End Select
end sub

sub movesong4down()
	Dim X
	For Each X in songss
		X.Visible = 0
	Next
end sub

Sub tsong4001_Hit()
	tsong4001.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong4down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt1"
	song4Checker = (song4Checker OR 1)
	Enablesong4s()
end sub

Sub tsong4002_Hit()
	tsong4002.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong4down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt2"
	song4Checker = (song4Checker OR 2)
	Enablesong4s()
end sub

Sub tsong4003_Hit()
	tsong4003.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong4down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt3"
	song4Checker = (song4Checker OR 4)
	Enablesong4s()
end sub

sub checkbonussong4()
	If song4Checker = 7 then
		DMD "", "", "200k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (200000*PFMultiplier)
		li016.state = 1
		song4Checker = 0
		musicmodes = 0
		csongs = csongs + 1
		checkcsongs
		Scompleted = Scompleted + 1
		UpdateMusic = 0
		UpdateMusicNow
		turny2Timer.enabled = False
		p11 = 1
	end if
end sub

'********Ssong5 mode***************

sub Ssong5()
	enablesong5s()
	li017.state = 2
	song5Checker = 0
	kickoutkennys
end sub

Dim Whichsong5, song5Checker
Whichsong5 = 0
song5Checker = 0
sub enablesong5s()
	If song5Checker = 7 Then
		CheckBonussong5()
		Exit Sub
	End If
	Randomize()
	Whichsong5 = INT(RND * 3) + 1
	Select Case Whichsong5
		Case 3
			Whichsong5 = 4
	End Select
	Do While (Whichsong5 AND song5Checker) > 0
		Whichsong5 = INT(RND * 3) + 1
		Select Case Whichsong5
			Case 3
				Whichsong5 = 4
		End Select
	Loop
	Select Case Whichsong5
		Case 1
			tsong5001.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong5001.X
			Mnote001.Y = tsong5001.Y
		Case 2
			tsong5002.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong5002.X
			Mnote001.Y = tsong5002.Y
		Case 4
			tsong5003.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong5003.X
			Mnote001.Y = tsong5003.Y
	End Select
end sub

sub movesong5down()
	Dim X
	For Each X in songss
		X.Visible = 0
	Next
end sub

Sub tsong5001_Hit()
	tsong5001.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong5down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt1"
	song5Checker = (song5Checker OR 1)
	Enablesong5s()
end sub

Sub tsong5002_Hit()
	tsong5002.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong5down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt2"
	song5Checker = (song5Checker OR 2)
	Enablesong5s()
end sub

Sub tsong5003_Hit()
	tsong5003.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong5down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt3"
	song5Checker = (song5Checker OR 4)
	Enablesong5s()
end sub

sub checkbonussong5()
	If song5Checker = 7 then
		DMD "", "", "200k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (200000*PFMultiplier)
		li017.state = 1
		song5Checker = 0
		musicmodes = 0
		Scompleted = Scompleted + 1
		csongs = csongs + 1
		checkcsongs
		UpdateMusic = 0
		UpdateMusicNow
		turny2Timer.enabled = False
		p12 = 1
	end if
end sub

'********Ssong6 mode***************

sub Ssong6()
	enablesong6s()
	li018.state = 2
	song6Checker = 0
	kickoutkennys
end sub

Dim Whichsong6, song6Checker
Whichsong6 = 0
song6Checker = 0
sub enablesong6s()
	If song6Checker = 7 Then
		CheckBonussong6()
		Exit Sub
	End If
	Randomize()
	Whichsong6 = INT(RND * 3) + 1
	Select Case Whichsong6
		Case 3
			Whichsong6 = 4
	End Select
	Do While (Whichsong6 AND song6Checker) > 0
		Whichsong6 = INT(RND * 3) + 1
		Select Case Whichsong6
			Case 3
				Whichsong6 = 4
		End Select
	Loop
	Select Case Whichsong6
		Case 1
			tsong6001.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong6001.X
			Mnote001.Y = tsong6001.Y
		Case 2
			tsong6002.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong6002.X
			Mnote001.Y = tsong6002.Y
		Case 4
			tsong6003.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong6003.X
			Mnote001.Y = tsong6003.Y
	End Select
end sub

sub movesong6down()
	Dim X
	For Each X in songss
		X.Visible = 0
	Next
end sub

Sub tsong6001_Hit()
	tsong6001.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong6down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt1"
	song6Checker = (song6Checker OR 1)
	Enablesong6s()
end sub

Sub tsong6002_Hit()
	tsong6002.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong6down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt2"
	song6Checker = (song6Checker OR 2)
	Enablesong6s()
end sub

Sub tsong6003_Hit()
	tsong6003.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong6down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt3"
	song6Checker = (song6Checker OR 4)
	Enablesong6s()
end sub

sub checkbonussong6()
	If song6Checker = 7 then
		DMD "", "", "200k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (200000*PFMultiplier)
		li018.state = 1
		song6Checker = 0
		musicmodes = 0
		Scompleted = Scompleted + 1
		csongs = csongs + 1
		checkcsongs
		UpdateMusic = 0
		UpdateMusicNow
		turny2Timer.enabled = False
		p13 = 1
	end if
end sub

'********Ssong7 mode***************

sub Ssong7()
	enablesong7s()
	li019.state = 2
	song7Checker = 0
	kickoutkennys
end sub

Dim Whichsong7, song7Checker
Whichsong7 = 0
song7Checker = 0
sub enablesong7s()
	If song7Checker = 7 Then
		CheckBonussong7()
		Exit Sub
	End If
	Randomize()
	Whichsong7 = INT(RND * 3) + 1
	Select Case Whichsong7
		Case 3
			Whichsong7 = 4
	End Select
	Do While (Whichsong7 AND song7Checker) > 0
		Whichsong7 = INT(RND * 3) + 1
		Select Case Whichsong7
			Case 3
				Whichsong7 = 4
		End Select
	Loop
	Select Case Whichsong7
		Case 1
			tsong7001.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong7001.X
			Mnote001.Y = tsong7001.Y
		Case 2
			tsong7002.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong7002.X
			Mnote001.Y = tsong7002.Y
		Case 4
			tsong7003.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong7003.X
			Mnote001.Y = tsong7003.Y
	End Select
end sub

sub movesong7down()
	Dim X
	For Each X in songss
		X.Visible = 0
	Next
end sub

Sub tsong7001_Hit()
	tsong7001.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong7down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt1"
	song7Checker = (song7Checker OR 1)
	Enablesong7s()
end sub

Sub tsong7002_Hit()
	tsong7002.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong7down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt2"
	song7Checker = (song7Checker OR 2)
	Enablesong7s()
end sub

Sub tsong7003_Hit()
	tsong7003.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong7down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt3"
	song7Checker = (song7Checker OR 4)
	Enablesong7s()
end sub

sub checkbonussong7()
	If song7Checker = 7 then
		DMD "", "", "200k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (200000*PFMultiplier)
		li019.state = 1
		song7Checker = 0
		musicmodes = 0
		Scompleted = Scompleted + 1
		csongs = csongs + 1
		checkcsongs
		UpdateMusic = 0
		UpdateMusicNow
		turny2Timer.enabled = False
		p14 = 1
	end if
end sub

'********Ssong8 mode***************

sub Ssong8()
	enablesong8s()
	li020.state = 2
	song8Checker = 0
	kickoutkennys
end sub

Dim Whichsong8, song8Checker
Whichsong8 = 0
song8Checker = 0
sub enablesong8s()
	If song8Checker = 7 Then
		CheckBonussong8()
		Exit Sub
	End If
	Randomize()
	Whichsong8 = INT(RND * 3) + 1
	Select Case Whichsong8
		Case 3
			Whichsong8 = 4
	End Select
	Do While (Whichsong8 AND song8Checker) > 0
		Whichsong8 = INT(RND * 3) + 1
		Select Case Whichsong8
			Case 3
				Whichsong8 = 4
		End Select
	Loop
	Select Case Whichsong8
		Case 1
			tsong8001.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong8001.X
			Mnote001.Y = tsong8001.Y
		Case 2
			tsong8002.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong8002.X
			Mnote001.Y = tsong8002.Y
		Case 4
			tsong8003.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong8003.X
			Mnote001.Y = tsong8003.Y
	End Select
end sub

sub movesong8down()
	Dim X
	For Each X in songss
		X.Visible = 0
	Next
end sub

Sub tsong8001_Hit()
	tsong8001.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong8down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt1"
	song8Checker = (song8Checker OR 1)
	Enablesong8s()
end sub

Sub tsong8002_Hit()
	tsong8002.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong8down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt2"
	song8Checker = (song8Checker OR 2)
	Enablesong8s()
end sub

Sub tsong8003_Hit()
	tsong8003.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong8down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt3"
	song8Checker = (song8Checker OR 4)
	Enablesong8s()
end sub

sub checkbonussong8()
	If song8Checker = 7 then
		DMD "", "", "200k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (200000*PFMultiplier)
		li020.state = 1
		song8Checker = 0
		musicmodes = 0
		Scompleted = Scompleted + 1
		csongs = csongs + 1
		checkcsongs
		UpdateMusic = 0
		UpdateMusicNow
		turny2Timer.enabled = False
		p15 = 1
	end if
end sub

'********Ssong9 mode***************

sub Ssong9()
	enablesong9s()
	li021.state = 2
	song9Checker = 0
	kickoutkennys
end sub

Dim Whichsong9, song9Checker
Whichsong9 = 0
song9Checker = 0
sub enablesong9s()
	If song9Checker = 7 Then
		CheckBonussong9()
		Exit Sub
	End If
	Randomize()
	Whichsong9 = INT(RND * 3) + 1
	Select Case Whichsong9
		Case 3
			Whichsong9 = 4
	End Select
	Do While (Whichsong9 AND song9Checker) > 0
		Whichsong9 = INT(RND * 3) + 1
		Select Case Whichsong9
			Case 3
				Whichsong9 = 4
		End Select
	Loop
	Select Case Whichsong9
		Case 1
			tsong9001.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong9001.X
			Mnote001.Y = tsong9001.Y
		Case 2
			tsong9002.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong9002.X
			Mnote001.Y = tsong9002.Y
		Case 4
			tsong9003.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong9003.X
			Mnote001.Y = tsong9003.Y
	End Select
end sub

sub movesong9down()
	Dim X
	For Each X in songss
		X.Visible = 0
	Next
end sub

Sub tsong9001_Hit()
	tsong9001.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong9down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt1"
	song9Checker = (song9Checker OR 1)
	Enablesong9s()
end sub

Sub tsong9002_Hit()
	tsong9002.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong9down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt2"
	song9Checker = (song9Checker OR 2)
	Enablesong9s()
end sub

Sub tsong9003_Hit()
	tsong9003.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong9down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt3"
	song9Checker = (song9Checker OR 4)
	Enablesong9s()
end sub

sub checkbonussong9()
	If song9Checker = 7 then
		DMD "", "", "200k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (200000*PFMultiplier)
		li021.state = 1
		song9Checker = 0
		musicmodes = 0
		Scompleted = Scompleted + 1
		csongs = csongs + 1
		checkcsongs
		UpdateMusic = 0
		UpdateMusicNow
		turny2Timer.enabled = False
		p16 = 1
	end if
end sub

'********Ssong10 mode***************

sub Ssong10()
	enablesong10s()
	li022.state = 2
	song10Checker = 0
	kickoutkennys
end sub

Dim Whichsong10, song10Checker
Whichsong10 = 0
song10Checker = 0
sub enablesong10s()
	If song10Checker = 7 Then
		CheckBonussong10()
		Exit Sub
	End If
	Randomize()
	Whichsong10 = INT(RND * 3) + 1
	Select Case Whichsong10
		Case 3
			Whichsong10 = 4
	End Select
	Do While (Whichsong10 AND song10Checker) > 0
		Whichsong10 = INT(RND * 3) + 1
		Select Case Whichsong10
			Case 3
				Whichsong10 = 4
		End Select
	Loop
	Select Case Whichsong10
		Case 1
			tsong10001.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong10001.X
			Mnote001.Y = tsong10001.Y
		Case 2
			tsong10002.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong10002.X
			Mnote001.Y = tsong10002.Y
		Case 4
			tsong10003.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong10003.X
			Mnote001.Y = tsong10003.Y
	End Select
end sub

sub movesong10down()
	Dim X
	For Each X in songss
		X.Visible = 0
	Next
end sub

Sub tsong10001_Hit()
	tsong10001.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong10down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt1"
	song10Checker = (song10Checker OR 1)
	Enablesong10s()
end sub

Sub tsong10002_Hit()
	tsong10002.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong10down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt2"
	song10Checker = (song10Checker OR 2)
	Enablesong10s()
end sub

Sub tsong10003_Hit()
	tsong10003.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong10down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt3"
	song10Checker = (song10Checker OR 4)
	Enablesong10s()
end sub

sub checkbonussong10()
	If song10Checker = 7 then
		DMD "", "", "200k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (200000*PFMultiplier)
		li022.state = 1
		song10Checker = 0
		musicmodes = 0
		Scompleted = Scompleted + 1
		csongs = csongs + 1
		checkcsongs
		UpdateMusic = 0
		UpdateMusicNow
		turny2Timer.enabled = False
		p17 = 1
	end if
end sub

'********Ssong11 mode***************

sub Ssong11()
	enablesong11s()
	li023.state = 2
	song11Checker = 0
	kickoutkennys
end sub

Dim Whichsong11, song11Checker
Whichsong11 = 0
song11Checker = 0
sub enablesong11s()
	If song11Checker = 7 Then
		CheckBonussong11()
		Exit Sub
	End If
	Randomize()
	Whichsong11 = INT(RND * 3) + 1
	Select Case Whichsong11
		Case 3
			Whichsong11 = 4
	End Select
	Do While (Whichsong11 AND song11Checker) > 0
		Whichsong11 = INT(RND * 3) + 1
		Select Case Whichsong11
			Case 3
				Whichsong11 = 4
		End Select
	Loop
	Select Case Whichsong11
		Case 1
			tsong11001.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong11001.X
			Mnote001.Y = tsong11001.Y
		Case 2
			tsong11002.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong11002.X
			Mnote001.Y = tsong11002.Y
		Case 4
			tsong11003.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong11003.X
			Mnote001.Y = tsong11003.Y
	End Select
end sub

sub movesong11down()
	Dim X
	For Each X in songss
		X.Visible = 0
	Next
end sub

Sub tsong11001_Hit()
	tsong11001.enabled = 0
	DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong11down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt1"
	song11Checker = (song11Checker OR 1)
	Enablesong11s()
end sub

Sub tsong11002_Hit()
	tsong11002.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong11down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt2"
	song11Checker = (song11Checker OR 2)
	Enablesong11s()
end sub

Sub tsong11003_Hit()
	tsong11003.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong11down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt3"
	song11Checker = (song11Checker OR 4)
	Enablesong11s()
end sub

sub checkbonussong11()
	If song11Checker = 7 then
		DMD "", "", "200k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (200000*PFMultiplier)
		li023.state = 1
		song11Checker = 0
		musicmodes = 0
		Scompleted = Scompleted + 1
		csongs = csongs + 1
		checkcsongs
		UpdateMusic = 0
		UpdateMusicNow
		turny2Timer.enabled = False
		p18 = 1
	end if
end sub

'********Ssong12 mode***************

sub Ssong12()
	enablesong12s()
	li024.state = 2
	song12Checker = 0
	kickoutkennys
end sub

Dim Whichsong12, song12Checker
Whichsong12 = 0
song12Checker = 0
sub enablesong12s()
	If song12Checker = 7 Then
		CheckBonussong12()
		Exit Sub
	End If
	Randomize()
	Whichsong12 = INT(RND * 3) + 1
	Select Case Whichsong12
		Case 3
			Whichsong12 = 4
	End Select
	Do While (Whichsong12 AND song12Checker) > 0
		Whichsong12 = INT(RND * 3) + 1
		Select Case Whichsong12
			Case 3
				Whichsong12 = 4
		End Select
	Loop
	Select Case Whichsong12
		Case 1
			tsong12001.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong12001.X
			Mnote001.Y = tsong12001.Y
		Case 2
			tsong12002.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong12002.X
			Mnote001.Y = tsong12002.Y
		Case 4
			tsong12003.enabled = 1
			Mnote001.Visible = 1
			Mnote001.X = tsong12003.X
			Mnote001.Y = tsong12003.Y
	End Select
end sub

sub movesong12down()
	Dim X
	For Each X in songss
		X.Visible = 0
	Next
end sub

Sub tsong12001_Hit()
	tsong12001.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong12down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt1"
	song12Checker = (song12Checker OR 1)
	Enablesong12s()
end sub

Sub tsong12002_Hit()
	tsong12002.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong12down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt2"
	song12Checker = (song12Checker OR 2)
	Enablesong12s()
end sub

Sub tsong12003_Hit()
	tsong12003.enabled = 0
DMD "", "", "dmdmusicnote", eNone, eNone, eNone, 1000, True, ""
	movesong12down()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "songhurt3"
	song12Checker = (song12Checker OR 4)
	Enablesong12s()
end sub

sub checkbonussong12()
	If song12Checker = 7 then
		DMD "", "", "200k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (200000*PFMultiplier)
		li024.state = 1
		song12Checker = 0
		musicmodes = 0
		Scompleted = Scompleted + 1
		csongs = csongs + 1
		checkcsongs
		UpdateMusic = 0
		UpdateMusicNow
		turny2Timer.enabled = False
		p19 = 1
	end if
end sub


sub checkcsongs
if csongs = 12 then
   		DMD "", "", "1mln", eNone, eNone, eNone, 1000, True, "1000000"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
p8 = 0
p9 = 0
p10 = 0
p11 = 0
p12 = 0
p13 = 0
p14 = 0
p15 = 0
p16 = 0
p17 = 0
p18 = 0
p19 = 0
csongs = 0
end if
end sub

'*********************************************************
'**************school mission kicker**********************
'*********************************************************
sub Kicker003_hit()
ObjLevel(9) = 1 : FlasherFlash9_Timer
if mrhan = 2 Then
	StopSong
	TiChickenDance.Enabled = 0
	Mrhankey.x = 285
	Mrhankey.y = 194
	Mrhankey.z = 150
	Mrhankey.RotZ = 0
	DMD "", "", "1mln", eNone, eNone, eNone, 9000, False, "mrhanky"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
	playsound "mrhankey"
	vpmtimer.addtimer 1200, "startmrhankeyturn '"
	vpmtimer.addtimer 3500, "startmrhankeydownn '"	
	mrhan = 0
	li006.state = 1
	li086.state = 0
	vpmTimer.AddTimer 9000, "kickeroutschool'"
	vpmTimer.AddTimer 8800, "playmusicagain'"
	exit Sub
	end if
if schoolmissionmodes = 2 or kennymissionmodes = 2 or bMultiBallMode = true then 
	vpmTimer.AddTimer 1000, "kickeroutschool'"
	exit Sub
	end if
StopSong
Playsong "schoolmusic"
	Playsound "schoolmission"
	'StartChoisemachine
DMDFlush
PlayerSelectActive2 = True
DMD "", "", "dmdssmis", eNone, eNone, eNone, 10000, False, ""
turny1Timer.enabled = True
end sub

sub startmrhankeyturn
TiChickenturn.Enabled = 1
end sub

sub startmrhankeydownn
TiChickenDown.Enabled = 1
end sub

sub playmusicagain
UpdateMusicNow
end sub

Sub kickeroutschool
flushit2
Playsound "fx_kicker"
Kicker003.Kick 170, 4, 12
end sub

sub flushit2
DMDScoreNow
end sub

Sub SelectPlayerStart2(Keycode)
   If keycode = LeftFlipperKey then
		Playsound "beep"
        SelectCounter2 = SelectCounter2 - 1
        If SelectCounter2 = -1 Then SelectCounter2 = 15
    end If
    If keycode = RightFlipperKey then
		Playsound "beep"
        SelectCounter2 = SelectCounter2 + 1
        If SelectCounter2 = 16 Then SelectCounter2 = 0
    end If
    Select Case SelectCounter2
        Case 0
			if p20 = 1 then
            DMDFlush
            DMD "", "", "dmdsmcplt1", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 0
			end if
			if p20 = 0 then 
            DMDFlush
            DMD "", "", "dmdsm1", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 0
			end if
        Case 1
			if p21 = 1 then
            DMDFlush
            DMD "", "", "dmdsmcplt2", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 1
			end if
			if p21 = 0 then 
            DMDFlush
            DMD "", "", "dmdsm2", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 1
			end if
        Case 2
			if p22 = 1 then
            DMDFlush
            DMD "", "", "dmdsmcplt3", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 2
			end if
			if p22 = 0 then 
            DMDFlush
            DMD "", "", "dmdsm3", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 2
			end if
        Case 3
			if p23 = 1 then
            DMDFlush
            DMD "", "", "dmdsmcplt4", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 3
			end if
			if p23 = 0 then 
            DMDFlush
            DMD "", "", "dmdsm4", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 3
			end if
        Case 4
			if p24 = 1 then
            DMDFlush
            DMD "", "", "dmdsmcplt5", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 4
			end if
			if p24 = 0 then 
            DMDFlush
            DMD "", "", "dmdsm5", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 4
			end if
        Case 5
			if p25 = 1 then
            DMDFlush
            DMD "", "", "dmdsmcplt6", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 5
			end if
			if p25 = 0 then 
            DMDFlush
            DMD "", "", "dmdsm6", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 5
			end if
        Case 6
			if p26 = 1 then
            DMDFlush
            DMD "", "", "dmdsmcplt7", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 6
			end if
			if p26 = 0 then 
            DMDFlush
            DMD "", "", "dmdsm7", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 6
			end if
        Case 7
			if p27 = 1 then
            DMDFlush
            DMD "", "", "dmdsmcplt8", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 7
			end if
			if p27 = 0 then 
            DMDFlush
            DMD "", "", "dmdsm8", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 7
			end if
        Case 8
			if p28 = 1 then
            DMDFlush
            DMD "", "", "dmdsmcplt9", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 8
			end if
			if p28 = 0 then 
            DMDFlush
            DMD "", "", "dmdsm9", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 8
			end if
        Case 9
			if p29 = 1 then
            DMDFlush
            DMD "", "", "dmdsmcplt10", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 9
			end if
			if p29 = 0 then 
            DMDFlush
            DMD "", "", "dmdsm10", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 9
			end if
        Case 10
			if p30 = 1 then
            DMDFlush
            DMD "", "", "dmdsmcplt11", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 10
			end if
			if p30 = 0 then 
            DMDFlush
            DMD "", "", "dmdsm11", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 10
			end if
        Case 11
			if p31 = 1 then
            DMDFlush
            DMD "", "", "dmdsmcplt12", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 11
			end if
			if p31 = 0 then 
            DMDFlush
            DMD "", "", "dmdsm12", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 11
			end if
        Case 12
			if p32 = 1 then
            DMDFlush
            DMD "", "", "dmdsmcplt1", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 12
			end if
			if p32 = 0 then 
            DMDFlush
            DMD "", "", "dmdsm13", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 12
			end if
        Case 13
			if p33 = 1 then
            DMDFlush
            DMD "", "", "dmdsmcplt2", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 13
			end if
			if p33 = 0 then 
            DMDFlush
            DMD "", "", "dmdsm14", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 13
			end if
        Case 14
			if p34 = 1 then
            DMDFlush
            DMD "", "", "dmdsmcplt3", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 14
			end if
			if p34 = 0 then 
            DMDFlush
            DMD "", "", "dmdsm15", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 14
			end if
        Case 15
			if p35 = 1 then
            DMDFlush
            DMD "", "", "dmdsmcplt4", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 15
			end if
			if p35 = 0 then 
            DMDFlush
            DMD "", "", "dmdsm16", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 15
			end if
    End Select

    If keycode = PlungerKey Then
        Select Case SlotValue2
            Case 0
				if p20 = 1 then
				Kicker003_hit
				exit sub
				end if
				startB2S(5)
				PlayerSelectActive2 = False
				DMDFlush
				StartMrHankey
            Case 1
				if p21 = 1 then
				Kicker003_hit
				exit sub
				end if
				startB2S(6)
				PlayerSelectActive2 = False
				DMDFlush
				StartClitoris
            Case 2
				if p22 = 1 then
				Kicker003_hit
				exit sub
				end if
				startB2S(7)
				PlayerSelectActive2 = False
				DMDFlush
				StartJobs
            Case 3
				if p23 = 1 then
				Kicker003_hit
				exit sub
				end if
				startB2S(9)
				PlayerSelectActive2 = False
				DMDFlush
				StartFlag
            Case 4
				if p24 = 1 then
				Kicker003_hit
				exit sub
				end if
				startB2S(12)
				PlayerSelectActive2 = False
				DMDFlush
				StartAlien
            Case 5
				if p25 = 1 then
				Kicker003_hit
				exit sub
				end if
				startB2S(13)
				PlayerSelectActive2 = False
				DMDFlush
				StartStarving
            Case 6
				if p26 = 1 then
				Kicker003_hit
				exit sub
				end if
				startB2S(14)
				PlayerSelectActive2 = False
				DMDFlush
				StartAid
            Case 7
				if p27 = 1 then
				Kicker003_hit
				exit sub
				end if
				startB2S(15)
				PlayerSelectActive2 = False
				DMDFlush
				StartSupplies
            Case 8
				if p28 = 1 then
				Kicker003_hit
				exit sub
				end if
				startB2S(16)
				PlayerSelectActive2 = False
				DMDFlush
				StartBudha
            Case 9
				if p29 = 1 then
				Kicker003_hit
				exit sub
				end if
				startB2S(22)
				PlayerSelectActive2 = False
				DMDFlush
				StartJesus
            Case 10
				if p30 = 1 then
				Kicker003_hit
				exit sub
				end if
				startB2S(4)
				PlayerSelectActive2 = False
				DMDFlush
				StartMuhhamad
            Case 11
				if p31 = 1 then
				Kicker003_hit
				exit sub
				end if
				startB2S(23)
				PlayerSelectActive2 = False
				DMDFlush
				StartKrishna
            Case 12
				if p32 = 1 then
				Kicker003_hit
				exit sub
				end if
				startB2S(17)
				PlayerSelectActive2 = False
				DMDFlush
				StartJoseph
            Case 13
				if p33 = 1 then
				Kicker003_hit
				exit sub
				end if
				startB2S(18)
				PlayerSelectActive2 = False
				DMDFlush
				StartLao
            Case 14
				if p34 = 1 then
				Kicker003_hit
				exit sub
				end if
				startB2S(24)
				PlayerSelectActive2 = False
				DMDFlush
				StartSeaman
            Case 15
				if p35 = 1 then
				Kicker003_hit
				exit sub
				end if
				startB2S(19)
				PlayerSelectActive2 = False
				DMDFlush
				StartMoses
        End Select
    end if
end sub


Sub CheckSchool()
If SchoolSlot = 16 Then
     DMD CL(0, "MISSIONS"), CL(1, "COMPLETED"), "", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
p20 = 0
p21 = 0
p22 = 0
p23 = 0
p24 = 0
p25 = 0
p26 = 0
p27 = 0
p28 = 0
p29 = 0
p30 = 0
p31 = 0
p32 = 0
p33 = 0
p34 = 0
p35 = 0
SchoolSlot = 0
end if
End Sub
'********schoolmission 1***************

Sub StartMrHankey
StopSong
StartPoops
end sub

Sub StartPoops
schoolmissionmodes = 2
     DMD CL(0, "FIND 5 POOPIES"), CL(1, "COMPLETE THE MISSION"), "", eNone, eNone, eNone, 3000, True, "poopstarty"
	vpmTimer.AddTimer 3000, "startSmission1 '"
end sub

sub startSmission1
UpdateMusicNow
mode1TimerCount = 60
mode1timer.Enabled = 1
EnablePoopss()
PoopsChecker = 0
kickeroutschool
end sub

Dim WhichPoops, PoopsChecker
WhichPoops = 0
PoopsChecker = 0
sub EnablePoopss()
	If PoopsChecker = 31 Then
		CheckBonusPoops()
		Exit Sub
	End If
	Randomize()
	WhichPoops = INT(RND * 5) + 1
	Select Case WhichPoops
		Case 3
			WhichPoops = 4
		Case 4
			WhichPoops = 8
		Case 5
			WhichPoops = 16
	End Select
	Do While (WhichPoops AND PoopsChecker) > 0
		WhichPoops = INT(RND * 5) + 1
		Select Case WhichPoops
			Case 3
				WhichPoops = 4
			Case 4
				WhichPoops = 8
			Case 5
				WhichPoops = 16
		End Select
	Loop
	Select Case WhichPoops
		Case 1
			ts2item001.enabled = 1
			Poops001.Visible = 1
			Poops001.X = ts2item001.X
			Poops001.Y = ts2item001.Y
		Case 2
			ts2item002.enabled = 1
			Poops001.Visible = 1
			Poops001.X = ts2item002.X
			Poops001.Y = ts2item002.Y
		Case 4
			ts2item003.enabled = 1
			Poops001.Visible = 1
			Poops001.X = ts2item003.X
			Poops001.Y = ts2item003.Y
		Case 8
			ts2item004.enabled = 1
			Poops001.Visible = 1
			Poops001.X = ts2item004.X
			Poops001.Y = ts2item004.Y
		Case 16
			ts2item005.enabled = 1
			Poops001.Visible = 1
			Poops001.X = ts2item005.X
			Poops001.Y = ts2item005.Y
	End Select
end sub


sub ts2item001_hit()
	startB2S(46)
	chbkhankey.enabled = true
	ts2item001.enabled = 0
	MovePoopsDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "poophit"
	addextratime()
	PoopsChecker = (PoopsChecker OR 1)
	EnablePoopss()
end sub

sub MovePoopsDown()
	Dim X
	For Each X in Poopss
		X.Visible = 0
	Next
end sub

sub ts2item002_hit()
	startB2S(46)
	chbkhankey.enabled = true
	ts2item002.enabled = 0
	MovePoopsDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "poophit"
	addextratime()
	PoopsChecker = (PoopsChecker OR 2)
	EnablePoopss()
end sub

sub ts2item003_hit()
	startB2S(46)
	chbkhankey.enabled = true
	ts2item003.enabled = 0
	MovePoopsDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "poophit"
	addextratime()
	PoopsChecker = (PoopsChecker OR 4)
	EnablePoopss()
end sub

sub ts2item004_hit()
	startB2S(46)
	chbkhankey.enabled = true
	ts2item004.enabled = 0
	MovePoopsDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "poophit"
	addextratime()
	PoopsChecker = (PoopsChecker OR 8)
	EnablePoopss()
end sub

sub ts2item005_hit()
	startB2S(46)
	chbkhankey.enabled = true
	ts2item005.enabled = 0
	MovePoopsDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "poophit"
	addextratime()
	PoopsChecker = (PoopsChecker OR 16)
	EnablePoopss()
end sub

sub CheckBonusPoops()
	If PoopsChecker = 31 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, "100k"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		StopmodeEndofBall()
		SMcompleted = SMcompleted + 1
		PoopsChecker = 0
		SchoolSlot = SchoolSlot + 1
		p20 = 1
		li096.state = 1
		li006.state = 2
		Mrhankey.x = 285
		Mrhankey.y = 194
		Mrhankey.z = 150
		Mrhankey.RotZ = 0
		Mrhankey.Visible = true
		TiChickenDance.Enabled = 1
		mrhan = 2
		vpmTimer.AddTimer 3000, "changeb2sback2begin '"
		CheckSchool()
		Flagdown.Enabled = 1
		li086.state = 2
	end if
end sub

Sub	chbkhankey_timer
chbkhankey.enabled = false
chhb
end sub

Sub chhb
startB2S(5)
end sub

sub changeb2sback2begin
startB2S(1)
end sub

'********schoolmission 2***************

Sub StartClitoris
StopSong
schoolmissionmodes = 2
     DMD CL(0, "FIND 5 CLITORIS"), CL(1, "COMPLETE THE MISSION"), "", eNone, eNone, eNone, 5400, True, "findclitoris"
	vpmTimer.AddTimer 5400, "startSmission2 '"
end sub

sub startSmission2
UpdateMusicNow
mode1TimerCount = 60
mode1timer.Enabled = 1
EnableClitoriss()
ClitorisChecker = 0
kickeroutschool
end sub

Dim WhichClitoris, ClitorisChecker
WhichClitoris = 0
ClitorisChecker = 0
sub EnableClitoriss()
	If ClitorisChecker = 31 Then
		CheckBonusClitoris()
		Exit Sub
	End If
	Randomize()
	WhichClitoris = INT(RND * 5) + 1
	Select Case WhichClitoris
		Case 3
			WhichClitoris = 4
		Case 4
			WhichClitoris = 8
		Case 5
			WhichClitoris = 16
	End Select
	Do While (WhichClitoris AND ClitorisChecker) > 0
		WhichClitoris = INT(RND * 5) + 1
		Select Case WhichClitoris
			Case 3
				WhichClitoris = 4
			Case 4
				WhichClitoris = 8
			Case 5
				WhichClitoris = 16
		End Select
	Loop
	Select Case WhichClitoris
		Case 1
			tsitem001.enabled = 1
			Clitoris001.Visible = 1
			Clitoris001.X = tsitem001.X
			Clitoris001.Y = tsitem001.Y
			Clitoris001.z = 100
		Case 2
			tsitem002.enabled = 1
			Clitoris001.Visible = 1
			Clitoris001.X = tsitem002.X
			Clitoris001.Y = tsitem002.Y
			Clitoris001.z = 100
		Case 4
			tsitem003.enabled = 1
			Clitoris001.Visible = 1
			Clitoris001.X = tsitem003.X
			Clitoris001.Y = tsitem003.Y
			Clitoris001.z = 100
		Case 8
			tsitem004.enabled = 1
			Clitoris001.Visible = 1
			Clitoris001.X = tsitem004.X
			Clitoris001.Y = tsitem004.Y
			Clitoris001.z = 100
		Case 16
			tsitem005.enabled = 1
			Clitoris001.Visible = 1
			Clitoris001.X = tsitem005.X
			Clitoris001.Y = tsitem005.Y
			Clitoris001.z = 100
	End Select
end sub

sub tsitem001_hit()
	tsitem001.enabled = 0
	startB2S(38)
	chbkclif.enabled = True
	MoveClitorisDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "clitoris"
	addextratime()
	ClitorisChecker = (ClitorisChecker OR 1)
	vpmTimer.AddTimer 500, "EnableClitoriss() '"
end sub

sub MoveClitorisDown()
	Dim X
	For Each X in Clitoriss
		X.Visible = 0
	Next
end sub

sub tsitem002_hit()
	tsitem002.enabled = 0
	startB2S(38)
	chbkclif.enabled = True
	MoveClitorisDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "clitoris"
	addextratime()
	ClitorisChecker = (ClitorisChecker OR 2)
	vpmTimer.AddTimer 500, "EnableClitoriss() '"
end sub

sub tsitem003_hit()
	tsitem003.enabled = 0
	startB2S(38)
	chbkclif.enabled = True
	MoveClitorisDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "clitoris"
	addextratime()
	ClitorisChecker = (ClitorisChecker OR 4)
	vpmTimer.AddTimer 500, "EnableClitoriss() '"
end sub

sub tsitem004_hit()
	tsitem004.enabled = 0
	startB2S(38)
	chbkclif.enabled = True
	MoveClitorisDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "clitoris"
	addextratime()
	ClitorisChecker = (ClitorisChecker OR 8)
	vpmTimer.AddTimer 500, "EnableClitoriss() '"
end sub

sub tsitem005_hit()
	tsitem005.enabled = 0
	startB2S(38)
	chbkclif.enabled = True
	MoveClitorisDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "clitoris"
	addextratime()
	ClitorisChecker = (ClitorisChecker OR 16)
	vpmTimer.AddTimer 500, "EnableClitoriss() '"
end sub

sub CheckBonusClitoris()
	If ClitorisChecker = 31 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, "100k"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		StopmodeEndofBall()
		SMcompleted = SMcompleted + 1
		ClitorisChecker = 0
		SchoolSlot = SchoolSlot + 1
		p21 = 1
		p36 = 0
		li009.state = 1
		CheckSchool()
		vpmTimer.AddTimer 3000, "changeb2sback2begin '"
	end if
end sub

Sub chbkclif_timer
chbkclif.enabled = false
chhc
end sub

Sub chhc
startB2S(6)
end sub

'********schoolmission 3***************

Sub StartJobs
StopSong
schoolmissionmodes = 2
     DMD CL(0, "FIND 5 GARBAGE"), CL(1, "COMPLETE THE MISSION"), "", eNone, eNone, eNone, 3500, True, "tookerjobstart"
	vpmTimer.AddTimer 3500, "startSmission3 '"
end sub

Sub startSmission3
UpdateMusicNow
mode1TimerCount = 60
mode1timer.Enabled = 1
EnableJobss()
JobsChecker = 0
changeling001.image = "dspf2"
changeling002.image = "dspf3"
changeling003.image = "dspf1"
changeling004.image = "dspf5"
changeling005.image = "dspf4"
changeling001.Visible = true
changeling002.Visible  = true
changeling003.Visible  = true
changeling004.Visible  = true
changeling005.Visible  = true
kickeroutschool
end sub

Dim WhichJobs, JobsChecker
WhichJobs = 0
JobsChecker = 0
sub EnableJobss()
	If JobsChecker = 31 Then
		CheckBonusJobs()
		Exit Sub
	End If
	Randomize()
	WhichJobs = INT(RND * 5) + 1
	Select Case WhichJobs
		Case 3
			WhichJobs = 4
		Case 4
			WhichJobs = 8
		Case 5
			WhichJobs = 16
	End Select
	Do While (WhichJobs AND JobsChecker) > 0
		WhichJobs = INT(RND * 5) + 1
		Select Case WhichJobs
			Case 3
				WhichJobs = 4
			Case 4
				WhichJobs = 8
			Case 5
				WhichJobs = 16
		End Select
	Loop
	Select Case WhichJobs
		Case 1
			ts3item001.enabled = 1
			Jobs001.Visible = 1
			Jobs001.X = ts3item001.X
			Jobs001.Y = ts3item001.Y
		Case 2
			ts3item002.enabled = 1
			Jobs001.Visible = 1
			Jobs001.X = ts3item002.X
			Jobs001.Y = ts3item002.Y
		Case 4
			ts3item003.enabled = 1
			Jobs001.Visible = 1
			Jobs001.X = ts3item003.X
			Jobs001.Y = ts3item003.Y
		Case 8
			ts3item004.enabled = 1
			Jobs001.Visible = 1
			Jobs001.X = ts3item004.X
			Jobs001.Y = ts3item004.Y
		Case 16
			ts3item005.enabled = 1
			Jobs001.Visible = 1
			Jobs001.X = ts3item005.X
			Jobs001.Y = ts3item005.Y
	End Select
end sub


sub ts3item001_hit()
	ts3item001.enabled = 0
	startB2S(54)
	chbkjobs.enabled = True
	MoveJobsDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "tj1"
	addextratime()
	changeling005.Visible = false
	JobsChecker = (JobsChecker OR 1)
	EnableJobss()
end sub

sub MoveJobsDown()
	Dim X
	For Each X in Jobss
		X.Visible = 0
	Next
end sub

sub ts3item002_hit()
	ts3item002.enabled = 0
	startB2S(56)
	chbkjobs.enabled = True
	MoveJobsDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "tj2"
	addextratime()
	changeling003.Visible = false
	JobsChecker = (JobsChecker OR 2)
	EnableJobss()
end sub

sub ts3item003_hit()
	ts3item003.enabled = 0
	startB2S(53)
	chbkjobs.enabled = True
	MoveJobsDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "tj3"
	addextratime()
	changeling001.Visible = false
	JobsChecker = (JobsChecker OR 4)
	EnableJobss()
end sub

sub ts3item004_hit()
	ts3item004.enabled = 0
	startB2S(55)
	chbkjobs.enabled = True
	MoveJobsDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "tj4"
	addextratime()
	changeling004.Visible = false
	JobsChecker = (JobsChecker OR 8)
	EnableJobss()
end sub

sub ts3item005_hit()
	ts3item005.enabled = 0
	startB2S(52)
	chbkjobs.enabled = True
	MoveJobsDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "tj5"
	addextratime()
	changeling002.Visible = false
	JobsChecker = (JobsChecker OR 16)
	EnableJobss()
end sub

sub CheckBonusJobs()
	If JobsChecker = 31 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, "100k"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		StopmodeEndofBall()
		SMcompleted = SMcompleted + 1
		JobsChecker = 0
		SchoolSlot = SchoolSlot + 1
		p22 = 1
		li097.state = 1
		vpmTimer.AddTimer 3000, "changeb2sback2begin '"
		CheckSchool()
	end if
end sub

Sub chbkjobs_timer
chbkjobs.enabled = false
chhj
end sub

Sub chhj
startB2S(7)
end sub


'********schoolmission 4***************

Sub StartFlag
StopSong
schoolmissionmodes = 2
Flagup.Enabled = 1
     DMD CL(0, "FIND 5 FLAGS"), CL(1, "COMPLETE THE MISSION"), "", eNone, eNone, eNone, 6000, True, "flagstart"
	vpmTimer.AddTimer 6000, "startSmission4 '"
end Sub

sub startSmission4
UpdateMusicNow
mode1TimerCount = 60
mode1timer.Enabled = 1
EnableFlags()
FlagChecker = 0
changeling001.image = "dskkk"
changeling002.image = "dskkk"
changeling003.image = "dskkk"
changeling004.image = "dskkk"
changeling005.image = "dskkk"
changeling001.Visible = true
changeling002.Visible  = true
changeling003.Visible  = true
changeling004.Visible  = true
changeling005.Visible  = true
kickeroutschool
end sub

Dim WhichFlag, FlagChecker
WhichFlag = 0
FlagChecker = 0
sub EnableFlags()
	If FlagChecker = 31 Then
		CheckBonusFlag()
		Exit Sub
	End If
	Randomize()
	WhichFlag = INT(RND * 5) + 1
	Select Case WhichFlag
		Case 3
			WhichFlag = 4
		Case 4
			WhichFlag = 8
		Case 5
			WhichFlag = 16
	End Select
	Do While (WhichFlag AND FlagChecker) > 0
		WhichFlag = INT(RND * 5) + 1
		Select Case WhichFlag
			Case 3
				WhichFlag = 4
			Case 4
				WhichFlag = 8
			Case 5
				WhichFlag = 16
		End Select
	Loop
	Select Case WhichFlag
		Case 1
			ts4item001.enabled = 1
			Flag001.Visible = 1
			Flag001.X = ts4item001.X
			Flag001.Y = ts4item001.Y
		Case 2
			ts4item002.enabled = 1
			Flag001.Visible = 1
			Flag001.X = ts4item002.X
			Flag001.Y = ts4item002.Y
		Case 4
			ts4item003.enabled = 1
			Flag001.Visible = 1
			Flag001.X = ts4item003.X
			Flag001.Y = ts4item003.Y
		Case 8
			ts4item004.enabled = 1
			Flag001.Visible = 1
			Flag001.X = ts4item004.X
			Flag001.Y = ts4item004.Y
		Case 16
			ts4item005.enabled = 1
			Flag001.Visible = 1
			Flag001.X = ts4item005.X
			Flag001.Y = ts4item005.Y
	End Select
end sub


sub ts4item001_hit()
	ts4item001.enabled = 0
	startB2S(32)
	chbkflag.enabled = True
	MoveFlagDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "flgh_1"
	addextratime()
	changeling001.Visible = false
	FlagChecker = (FlagChecker OR 1)
	EnableFlags()
end sub

sub MoveFlagDown()
	Dim X
	For Each X in Flags
		X.Visible = 0
	Next
end sub

sub ts4item002_hit()
	ts4item002.enabled = 0
	startB2S(28)
	chbkflag.enabled = True
	MoveFlagDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "flgh_2"
	addextratime()
	changeling002.Visible = false
	FlagChecker = (FlagChecker OR 2)
	EnableFlags()
end sub

sub ts4item003_hit()
	ts4item003.enabled = 0
	startB2S(30)
	chbkflag.enabled = True
	MoveFlagDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "flgh_3"
	addextratime()
	changeling003.Visible = false
	FlagChecker = (FlagChecker OR 4)
	EnableFlags()
end sub

sub ts4item004_hit()
	ts4item004.enabled = 0
	startB2S(29)
	chbkflag.enabled = True
	MoveFlagDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "flgh_4"
	addextratime()
	changeling004.Visible = false
	FlagChecker = (FlagChecker OR 8)
	EnableFlags()
end sub

sub ts4item005_hit()
	ts4item005.enabled = 0
	startB2S(31)
	chbkflag.enabled = True
	MoveFlagDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "flgh_5"
	addextratime()
	changeling005.Visible = false
	FlagChecker = (FlagChecker OR 16)
	EnableFlags()
end sub

sub CheckBonusFlag()
	If FlagChecker = 31 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, "100k"
		Flag.image = "spflagkopie2"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		StopmodeEndofBall()
		SMcompleted = SMcompleted + 1
		FlagChecker = 0
		SchoolSlot = SchoolSlot + 1
		p23 = 1
		li098.state = 1
		vpmTimer.AddTimer 3000, "changeb2sback2begin '"
		CheckSchool()
	end if
end sub

Sub chbkflag_timer
chbkflag.enabled = false
chhf
end sub

Sub chhf
startB2S(9)
end sub

'********schoolmission 5***************

Sub StartAlien
StopSong
schoolmissionmodes = 2
     DMD CL(0, "HIT 5 ALIENS"), CL(1, "COMPLETE THE MISSION"), "", eNone, eNone, eNone, 3300, True, "aliensstarty"
	vpmTimer.AddTimer 3000, "startSmission5 '"
end sub

Sub startSmission5
UpdateMusicNow
mode1TimerCount = 60
mode1timer.Enabled = 1
EnableAliens()
AlienChecker = 0
kickeroutschool
end sub

Dim WhichAlien, AlienChecker
WhichAlien = 0
AlienChecker = 0
sub EnableAliens()
	If AlienChecker = 31 Then
		CheckBonusAlien()
		Exit Sub
	End If
	Randomize()
	WhichAlien = INT(RND * 5) + 1
	Select Case WhichAlien
		Case 3
			WhichAlien = 4
		Case 4
			WhichAlien = 8
		Case 5
			WhichAlien = 16
	End Select
	Do While (WhichAlien AND AlienChecker) > 0
		WhichAlien = INT(RND * 5) + 1
		Select Case WhichAlien
			Case 3
				WhichAlien = 4
			Case 4
				WhichAlien = 8
			Case 5
				WhichAlien = 16
		End Select
	Loop
	Select Case WhichAlien
		Case 1
			ts5item001.enabled = 1
			Alien001.Visible = 1
			Alien001.X = ts5item001.X
			Alien001.Y = ts5item001.Y
		Case 2
			ts5item002.enabled = 1
			Alien001.Visible = 1
			Alien001.X = ts5item002.X
			Alien001.Y = ts5item002.Y
		Case 4
			ts5item003.enabled = 1
			Alien001.Visible = 1
			Alien001.X = ts5item003.X
			Alien001.Y = ts5item003.Y
		Case 8
			ts5item004.enabled = 1
			Alien001.Visible = 1
			Alien001.X = ts5item004.X
			Alien001.Y = ts5item004.Y
		Case 16
			ts5item005.enabled = 1
			Alien001.Visible = 1
			Alien001.X = ts5item005.X
			Alien001.Y = ts5item005.Y
	End Select
end sub


sub ts5item001_hit()
	ts5item001.enabled = 0
	startB2S(26)
	chbkalien.enabled = True
	MoveAlienDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "ufo_hit"
	addextratime()
	AlienChecker = (AlienChecker OR 1)
	EnableAliens()
end sub

sub MoveAlienDown()
	Dim X
	For Each X in Aliens
		X.Visible = 0
	Next
end sub

sub ts5item002_hit()
	ts5item002.enabled = 0
	startB2S(26)
	chbkalien.enabled = True
	MoveAlienDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "ufo_hit"
	addextratime()
	AlienChecker = (AlienChecker OR 2)
	EnableAliens()
end sub

sub ts5item003_hit()
	ts5item003.enabled = 0
	startB2S(26)
	chbkalien.enabled = True
	MoveAlienDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "ufo_hit"
	addextratime()
	AlienChecker = (AlienChecker OR 4)
	EnableAliens()
end sub

sub ts5item004_hit()
	ts5item004.enabled = 0
	startB2S(26)
	chbkalien.enabled = True
	MoveAlienDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "ufo_hit"
	addextratime()
	AlienChecker = (AlienChecker OR 8)
	EnableAliens()
end sub

sub ts5item005_hit()
	ts5item005.enabled = 0
	startB2S(26)
	chbkalien.enabled = True
	MoveAlienDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "ufo_hit"
	addextratime()
	AlienChecker = (AlienChecker OR 16)
	EnableAliens()
end sub

sub CheckBonusAlien()
	If AlienChecker = 31 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, "100k"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		StopmodeEndofBall()
		SMcompleted = SMcompleted + 1
		AlienChecker = 0
		SchoolSlot = SchoolSlot + 1
		p24 = 1
		li008.state = 1
		vpmTimer.AddTimer 3000, "changeb2sback2begin '"
		CheckSchool()
	end if
end sub

Sub chbkalien_timer
chbkalien.enabled = false
chha
end sub

Sub chha
startB2S(12)
end sub

'********schoolmission 6***************

Sub StartStarving
StopSong
schoolmissionmodes = 2
     DMD CL(0, "HIT 5 STARVINGS"), CL(1, "COMPLETE THE MISSION"), "", eNone, eNone, eNone, 6000, True, "starvingstart"
	vpmTimer.AddTimer 6000, "startSmission6 '"
end sub

Sub startSmission6
UpdateMusicNow
mode1TimerCount = 60
mode1timer.Enabled = 1
EnableStarvings()
StarvingChecker = 0
kickeroutschool
end sub

Dim WhichStarving, StarvingChecker
WhichStarving = 0
StarvingChecker = 0
sub EnableStarvings()
	If StarvingChecker = 31 Then
		CheckBonusStarving()
		Exit Sub
	End If
	Randomize()
	WhichStarving = INT(RND * 5) + 1
	Select Case WhichStarving
		Case 3
			WhichStarving = 4
		Case 4
			WhichStarving = 8
		Case 5
			WhichStarving = 16
	End Select
	Do While (WhichStarving AND StarvingChecker) > 0
		WhichStarving = INT(RND * 5) + 1
		Select Case WhichStarving
			Case 3
				WhichStarving = 4
			Case 4
				WhichStarving = 8
			Case 5
				WhichStarving = 16
		End Select
	Loop
	Select Case WhichStarving
		Case 1
			ts6item001.enabled = 1
			Starving001.Visible = 1
			Starving001.X = ts6item001.X
			Starving001.Y = ts6item001.Y
		Case 2
			ts6item002.enabled = 1
			Starving001.Visible = 1
			Starving001.X = ts6item002.X
			Starving001.Y = ts6item002.Y
		Case 4
			ts6item003.enabled = 1
			Starving001.Visible = 1
			Starving001.X = ts6item003.X
			Starving001.Y = ts6item003.Y
		Case 8
			ts6item004.enabled = 1
			Starving001.Visible = 1
			Starving001.X = ts6item004.X
			Starving001.Y = ts6item004.Y
		Case 16
			ts6item005.enabled = 1
			Starving001.Visible = 1
			Starving001.X = ts6item005.X
			Starving001.Y = ts6item005.Y
	End Select
end sub


sub ts6item001_hit()
	ts6item001.enabled = 0
	startB2S(51)
	chbkstarf.enabled = True
	MoveStarvingDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "marvhit"
	addextratime()
	StarvingChecker = (StarvingChecker OR 1)
	EnableStarvings()
end sub

sub MoveStarvingDown()
	Dim X
	For Each X in Starvings
		X.Visible = 0
	Next
end sub

sub ts6item002_hit()
	ts6item002.enabled = 0
	startB2S(51)
	chbkstarf.enabled = True
	MoveStarvingDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "marvhit"
	addextratime()
	StarvingChecker = (StarvingChecker OR 2)
	EnableStarvings()
end sub

sub ts6item003_hit()
	ts6item003.enabled = 0
	startB2S(51)
	chbkstarf.enabled = True
	MoveStarvingDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "marvhit"
	addextratime()
	StarvingChecker = (StarvingChecker OR 4)
	EnableStarvings()
end sub

sub ts6item004_hit()
	ts6item004.enabled = 0
	startB2S(51)
	chbkstarf.enabled = True
	MoveStarvingDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "marvhit"
	addextratime()
	StarvingChecker = (StarvingChecker OR 8)
	EnableStarvings()
end sub

sub ts6item005_hit()
	ts6item005.enabled = 0
	startB2S(51)
	chbkstarf.enabled = True
	MoveStarvingDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "marvhit"
	addextratime()
	StarvingChecker = (StarvingChecker OR 16)
	EnableStarvings()
end sub

sub CheckBonusStarving()
	If StarvingChecker = 31 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, "100k"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		StopmodeEndofBall()
		SMcompleted = SMcompleted + 1
		StarvingChecker = 0
		SchoolSlot = SchoolSlot + 1
		p25 = 1
		li099.state = 1
		vpmTimer.AddTimer 3000, "changeb2sback2begin '"
		CheckSchool()
	end if
end sub

Sub chbkstarf_timer
chbkstarf.enabled = false
chhs
end sub

Sub chhs
startB2S(13)
end sub


'********schoolmission 7***************

Sub StartAid
StopSong
schoolmissionmodes = 2
     DMD CL(0, "FIND 5 CONDOMS"), CL(1, "COMPLETE THE MISSION"), "", eNone, eNone, eNone, 3500, True, "chefaidstart"
	vpmTimer.AddTimer 3500, "startSmission7 '"
end sub

sub startSmission7
UpdateMusicNow
mode1TimerCount = 60
mode1timer.Enabled = 1
EnableAids()
AidChecker = 0
kickeroutschool
end sub

Dim WhichAid, AidChecker
WhichAid = 0
AidChecker = 0
sub EnableAids()
	If AidChecker = 31 Then
		CheckBonusAid()
		Exit Sub
	End If
	Randomize()
	WhichAid = INT(RND * 5) + 1
	Select Case WhichAid
		Case 3
			WhichAid = 4
		Case 4
			WhichAid = 8
		Case 5
			WhichAid = 16
	End Select
	Do While (WhichAid AND AidChecker) > 0
		WhichAid = INT(RND * 5) + 1
		Select Case WhichAid
			Case 3
				WhichAid = 4
			Case 4
				WhichAid = 8
			Case 5
				WhichAid = 16
		End Select
	Loop
	Select Case WhichAid
		Case 1
			ts7item001.enabled = 1
			Aid001.Visible = 1
			Aid001.X = ts7item001.X
			Aid001.Y = ts7item001.Y
		Case 2
			ts7item002.enabled = 1
			Aid001.Visible = 1
			Aid001.X = ts7item002.X
			Aid001.Y = ts7item002.Y
		Case 4
			ts7item003.enabled = 1
			Aid001.Visible = 1
			Aid001.X = ts7item003.X
			Aid001.Y = ts7item003.Y
		Case 8
			ts7item004.enabled = 1
			Aid001.Visible = 1
			Aid001.X = ts7item004.X
			Aid001.Y = ts7item004.Y
		Case 16
			ts7item005.enabled = 1
			Aid001.Visible = 1
			Aid001.X = ts7item005.X
			Aid001.Y = ts7item005.Y
	End Select
end sub


sub ts7item001_hit()
	ts7item001.enabled = 0
	startB2S(33)
	chbkchefa.enabled = True
	MoveAidDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "CA_hit1"
	addextratime()
	AidChecker = (AidChecker OR 1)
	EnableAids()
end sub

sub MoveAidDown()
	Dim X
	For Each X in Aids
		X.Visible = 0
	Next
end sub

sub ts7item002_hit()
	ts7item002.enabled = 0
	startB2S(37)
	chbkchefa.enabled = True
	MoveAidDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "CA_hit2"
	addextratime()
	AidChecker = (AidChecker OR 2)
	EnableAids()
end sub

sub ts7item003_hit()
	ts7item003.enabled = 0
	startB2S(36)
	chbkchefa.enabled = True
	MoveAidDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "CA_hit4"
	addextratime()
	AidChecker = (AidChecker OR 4)
	EnableAids()
end sub

sub ts7item004_hit()
	ts7item004.enabled = 0
	startB2S(35)
	chbkchefa.enabled = True
	MoveAidDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "CA_hit3"
	addextratime()
	AidChecker = (AidChecker OR 8)
	EnableAids()
end sub

sub ts7item005_hit()
	ts7item005.enabled = 0
	startB2S(34)
	chbkchefa.enabled = True
	MoveAidDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "CA_hit5"
	addextratime()
	AidChecker = (AidChecker OR 16)
	EnableAids()
end sub

sub CheckBonusAid()
	If AidChecker = 31 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, "100k"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		StopmodeEndofBall()
		SMcompleted = SMcompleted + 1
		AidChecker = 0
		SchoolSlot = SchoolSlot + 1
		p26 = 1
		li007.state = 1
		vpmTimer.AddTimer 3000, "changeb2sback2begin '"
		CheckSchool()
	end if
end sub

Sub chbkchefa_timer
chbkchefa.enabled = false
chhca
end sub

Sub chhca
startB2S(14)
end sub

'********schoolmission 8***************

Sub StartSupplies
StopSong
schoolmissionmodes = 2
     DMD CL(0, "FIND 5 MARKERS"), CL(1, "COMPLETE THE MISSION"), "", eNone, eNone, eNone, 4000, True, "supplystarty"
	vpmTimer.AddTimer 4000, "startSmission8 '"
end Sub

Sub startSmission8
UpdateMusicNow
mode1TimerCount = 60
mode1timer.Enabled = 1
EnableSuppliess()
SuppliesChecker = 0
kickeroutschool
end sub

Dim WhichSupplies, SuppliesChecker
WhichSupplies = 0
SuppliesChecker = 0
sub EnableSuppliess()
	If SuppliesChecker = 31 Then
		CheckBonusSupplies()
		Exit Sub
	End If
	Randomize()
	WhichSupplies = INT(RND * 5) + 1
	Select Case WhichSupplies
		Case 3
			WhichSupplies = 4
		Case 4
			WhichSupplies = 8
		Case 5
			WhichSupplies = 16
	End Select
	Do While (WhichSupplies AND SuppliesChecker) > 0
		WhichSupplies = INT(RND * 5) + 1
		Select Case WhichSupplies
			Case 3
				WhichSupplies = 4
			Case 4
				WhichSupplies = 8
			Case 5
				WhichSupplies = 16
		End Select
	Loop
	Select Case WhichSupplies
		Case 1
			ts8item001.enabled = 1
			Supplies001.Visible = 1
			Supplies001.X = ts8item001.X
			Supplies001.Y = ts8item001.Y
		Case 2
			ts8item002.enabled = 1
			Supplies001.Visible = 1
			Supplies001.X = ts8item002.X
			Supplies001.Y = ts8item002.Y
		Case 4
			ts8item003.enabled = 1
			Supplies001.Visible = 1
			Supplies001.X = ts8item003.X
			Supplies001.Y = ts8item003.Y
		Case 8
			ts8item004.enabled = 1
			Supplies001.Visible = 1
			Supplies001.X = ts8item004.X
			Supplies001.Y = ts8item004.Y
		Case 16
			ts8item005.enabled = 1
			Supplies001.Visible = 1
			Supplies001.X = ts8item005.X
			Supplies001.Y = ts8item005.Y
	End Select
end sub


sub ts8item001_hit()
	ts8item001.enabled = 0
	startB2S(59)
	chbksuplie.enabled = True
	MoveSuppliesDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "markerhit"
	addextratime()
	SuppliesChecker = (SuppliesChecker OR 1)
	EnableSuppliess()
end sub

sub MoveSuppliesDown()
	Dim X
	For Each X in Suppliess
		X.Visible = 0
	Next
end sub

sub ts8item002_hit()
	ts8item002.enabled = 0
	startB2S(59)
	chbksuplie.enabled = True
	MoveSuppliesDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "markerhit"
	addextratime()
	SuppliesChecker = (SuppliesChecker OR 2)
	EnableSuppliess()
end sub

sub ts8item003_hit()
	ts8item003.enabled = 0
	startB2S(59)
	chbksuplie.enabled = True
	MoveSuppliesDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "markerhit"
	addextratime()
	SuppliesChecker = (SuppliesChecker OR 4)
	EnableSuppliess()
end sub

sub ts8item004_hit()
	ts8item004.enabled = 0
	startB2S(59)
	chbksuplie.enabled = True
	MoveSuppliesDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "markerhit"
	addextratime()
	SuppliesChecker = (SuppliesChecker OR 8)
	EnableSuppliess()
end sub

sub ts8item005_hit()
	ts8item005.enabled = 0
	startB2S(59)
	chbksuplie.enabled = True
	MoveSuppliesDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "markerhit"
	addextratime()
	SuppliesChecker = (SuppliesChecker OR 16)
	EnableSuppliess()
end sub

sub CheckBonusSupplies()
	If SuppliesChecker = 31 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, "100k"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		StopmodeEndofBall()
		SMcompleted = SMcompleted + 1
		SuppliesChecker = 0
		SchoolSlot = SchoolSlot + 1
		p27 = 1
		li100.state = 1
		vpmTimer.AddTimer 3000, "changeb2sback2begin '"
		CheckSchool()
	end if
end sub

Sub chbksuplie_timer
chbksuplie.enabled = false
chhsp
end sub

Sub chhsp
startB2S(15)
end sub

'********schoolmission 9***************

Sub StartBudha
StopSong
schoolmissionmodes = 2
     DMD CL(0, "COLLECT 125 GRAM"), CL(1, "COMPLETE THE MISSION"), "", eNone, eNone, eNone, 5000, True, "budhastart"
	vpmTimer.AddTimer 5000, "startSmission9 '"
end Sub

Sub startSmission9
UpdateMusicNow
mode1TimerCount = 60
mode1timer.Enabled = 1
EnableBudhas()
BudhaChecker = 0
kickeroutschool
end sub

Dim WhichBudha, BudhaChecker
WhichBudha = 0
BudhaChecker = 0
sub EnableBudhas()
	If BudhaChecker = 31 Then
		CheckBonusBudha()
		Exit Sub
	End If
	Randomize()
	WhichBudha = INT(RND * 5) + 1
	Select Case WhichBudha
		Case 3
			WhichBudha = 4
		Case 4
			WhichBudha = 8
		Case 5
			WhichBudha = 16
	End Select
	Do While (WhichBudha AND BudhaChecker) > 0
		WhichBudha = INT(RND * 5) + 1
		Select Case WhichBudha
			Case 3
				WhichBudha = 4
			Case 4
				WhichBudha = 8
			Case 5
				WhichBudha = 16
		End Select
	Loop
	Select Case WhichBudha
		Case 1
			ts9item001.enabled = 1
			Budha001.Visible = 1
			Budha001.X = ts9item001.X
			Budha001.Y = ts9item001.Y
		Case 2
			ts9item002.enabled = 1
			Budha001.Visible = 1
			Budha001.X = ts9item002.X
			Budha001.Y = ts9item002.Y
		Case 4
			ts9item003.enabled = 1
			Budha001.Visible = 1
			Budha001.X = ts9item003.X
			Budha001.Y = ts9item003.Y
		Case 8
			ts9item004.enabled = 1
			Budha001.Visible = 1
			Budha001.X = ts9item004.X
			Budha001.Y = ts9item004.Y
		Case 16
			ts9item005.enabled = 1
			Budha001.Visible = 1
			Budha001.X = ts9item005.X
			Budha001.Y = ts9item005.Y
	End Select
end sub

sub ts9item001_hit()
	ts9item001.enabled = 0
	startB2S(27)
	chbkbudha.enabled = True
	MoveBudhaDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "budhahitt"
	addextratime()
	BudhaChecker = (BudhaChecker OR 1)
	EnableBudhas()
end sub

sub MoveBudhaDown()
	Dim X
	For Each X in Budhas
		X.Visible = 0
	Next
end sub

sub ts9item002_hit()
	ts9item002.enabled = 0
	startB2S(27)
	chbkbudha.enabled = True
	MoveBudhaDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "budhahitt"
	addextratime()
	BudhaChecker = (BudhaChecker OR 2)
	EnableBudhas()
end sub

sub ts9item003_hit()
	ts9item003.enabled = 0
	startB2S(27)
	chbkbudha.enabled = True
	MoveBudhaDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "budhahitt"
	addextratime()
	BudhaChecker = (BudhaChecker OR 4)
	EnableBudhas()
end sub

sub ts9item004_hit()
	ts9item004.enabled = 0
	startB2S(27)
	chbkbudha.enabled = True
	MoveBudhaDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "budhahitt"
	addextratime()
	BudhaChecker = (BudhaChecker OR 8)
	EnableBudhas()
end sub

sub ts9item005_hit()
	ts9item005.enabled = 0
	startB2S(27)
	chbkbudha.enabled = True
	MoveBudhaDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "budhahitt"
	addextratime()
	BudhaChecker = (BudhaChecker OR 16)
	EnableBudhas()
end sub

sub CheckBonusBudha()
	If BudhaChecker = 31 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, "100k"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		StopmodeEndofBall()
		SMcompleted = SMcompleted + 1
		BudhaChecker = 0
		SchoolSlot = SchoolSlot + 1
		p28 = 1
		li028.state = 1
		vpmTimer.AddTimer 3000, "changeb2sback2begin '"
		CheckSchool()
	end if
end sub

Sub chbkbudha_timer
chbkbudha.enabled = false
chhbd
end sub

Sub chhbd
startB2S(16)
end sub

'********schoolmission 10***************

Sub StartJesus
StopSong
     DMD CL(0, "FIND 5 HOLYBOOKS"), CL(1, "COMPLETE THE MISSION"), "", eNone, eNone, eNone, 5900, True, "jesusstart"
	vpmTimer.AddTimer 5900, "startSmission10 '"
end sub

sub startSmission10
UpdateMusicNow
schoolmissionmodes = 2
mode1TimerCount = 60
mode1timer.Enabled = 1
EnableJesuss()
JesusChecker = 0
kickeroutschool
end sub

Dim WhichJesus, JesusChecker
WhichJesus = 0
JesusChecker = 0
sub EnableJesuss()
	If JesusChecker = 31 Then
		CheckBonusJesus()
		Exit Sub
	End If
	Randomize()
	WhichJesus = INT(RND * 5) + 1
	Select Case WhichJesus
		Case 3
			WhichJesus = 4
		Case 4
			WhichJesus = 8
		Case 5
			WhichJesus = 16
	End Select
	Do While (WhichJesus AND JesusChecker) > 0
		WhichJesus = INT(RND * 5) + 1
		Select Case WhichJesus
			Case 3
				WhichJesus = 4
			Case 4
				WhichJesus = 8
			Case 5
				WhichJesus = 16
		End Select
	Loop
	Select Case WhichJesus
		Case 1
			ts10item001.enabled = 1
			Jesus001.Visible = 1
			Jesus001.X = ts10item001.X
			Jesus001.Y = ts10item001.Y
		Case 2
			ts10item002.enabled = 1
			Jesus001.Visible = 1
			Jesus001.X = ts10item002.X
			Jesus001.Y = ts10item002.Y
		Case 4
			ts10item003.enabled = 1
			Jesus001.Visible = 1
			Jesus001.X = ts10item003.X
			Jesus001.Y = ts10item003.Y
		Case 8
			ts10item004.enabled = 1
			Jesus001.Visible = 1
			Jesus001.X = ts10item004.X
			Jesus001.Y = ts10item004.Y
		Case 16
			ts10item005.enabled = 1
			Jesus001.Visible = 1
			Jesus001.X = ts10item005.X
			Jesus001.Y = ts10item005.Y
	End Select
end sub

sub ts10item001_hit()
	ts10item001.enabled = 0
	startB2S(40)
	chbkjesus.enabled = True
	MoveJesusDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "jesushit"
	addextratime()
	JesusChecker = (JesusChecker OR 1)
	EnableJesuss()
end sub

sub MoveJesusDown()
	Dim X
	For Each X in Jesuss
		X.Visible = 0
	Next
end sub

sub ts10item002_hit()
	ts10item002.enabled = 0
	startB2S(40)
	chbkjesus.enabled = True
	MoveJesusDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "jesushit"
	addextratime()
	JesusChecker = (JesusChecker OR 2)
	EnableJesuss()
end sub

sub ts10item003_hit()
	ts10item003.enabled = 0
	startB2S(40)
	chbkjesus.enabled = True
	MoveJesusDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "jesushit"
	addextratime()
	JesusChecker = (JesusChecker OR 4)
	EnableJesuss()
end sub

sub ts10item004_hit()
	ts10item004.enabled = 0
	startB2S(40)
	chbkjesus.enabled = True
	MoveJesusDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "jesushit"
	addextratime()
	JesusChecker = (JesusChecker OR 8)
	EnableJesuss()
end sub

sub ts10item005_hit()
	ts10item005.enabled = 0
	startB2S(40)
	chbkjesus.enabled = True
	MoveJesusDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "jesushit"
	addextratime()
	JesusChecker = (JesusChecker OR 16)
	EnableJesuss()
end sub

sub CheckBonusJesus()
	If JesusChecker = 31 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, "100k"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		StopmodeEndofBall()
		SMcompleted = SMcompleted + 1
		JesusChecker = 0
		SchoolSlot = SchoolSlot + 1
		p29 = 1
		li027.state = 1
		vpmTimer.AddTimer 3000, "changeb2sback2begin '"
		CheckSchool()
	end if
end sub

Sub chbkjesus_timer
chbkjesus.enabled = false
chhjs
end sub

Sub chhjs
startB2S(22)
end sub

'********schoolmission 11***************

Sub StartMuhhamad
StopSong
     DMD CL(0, "FIND 5 HOLYBOOKS"), CL(1, "COMPLETE THE MISSION"), "", eNone, eNone, eNone, 4000, True, "mohambomb"
	vpmTimer.AddTimer 5400, "startSmission11 '"
end sub

Sub startSmission11
UpdateMusicNow
schoolmissionmodes = 2
mode1TimerCount = 60
mode1timer.Enabled = 1
EnableMuhhamads()
MuhhamadChecker = 0
kickeroutschool
end sub

Dim WhichMuhhamad, MuhhamadChecker
WhichMuhhamad = 0
MuhhamadChecker = 0
sub EnableMuhhamads()
	If MuhhamadChecker = 31 Then
		CheckBonusMuhhamad()
		Exit Sub
	End If
	Randomize()
	WhichMuhhamad = INT(RND * 5) + 1
	Select Case WhichMuhhamad
		Case 3
			WhichMuhhamad = 4
		Case 4
			WhichMuhhamad = 8
		Case 5
			WhichMuhhamad = 16
	End Select
	Do While (WhichMuhhamad AND MuhhamadChecker) > 0
		WhichMuhhamad = INT(RND * 5) + 1
		Select Case WhichMuhhamad
			Case 3
				WhichMuhhamad = 4
			Case 4
				WhichMuhhamad = 8
			Case 5
				WhichMuhhamad = 16
		End Select
	Loop
	Select Case WhichMuhhamad
		Case 1
			ts11item001.enabled = 1
			Muhhamad001.Visible = 1
			Muhhamad001.X = ts11item001.X
			Muhhamad001.Y = ts11item001.Y
		Case 2
			ts11item002.enabled = 1
			Muhhamad001.Visible = 1
			Muhhamad001.X = ts11item002.X
			Muhhamad001.Y = ts11item002.Y
		Case 4
			ts11item003.enabled = 1
			Muhhamad001.Visible = 1
			Muhhamad001.X = ts11item003.X
			Muhhamad001.Y = ts11item003.Y
		Case 8
			ts11item004.enabled = 1
			Muhhamad001.Visible = 1
			Muhhamad001.X = ts11item004.X
			Muhhamad001.Y = ts11item004.Y
		Case 16
			ts11item005.enabled = 1
			Muhhamad001.Visible = 1
			Muhhamad001.X = ts11item005.X
			Muhhamad001.Y = ts11item005.Y
	End Select
end sub


sub ts11item001_hit()
	ts11item001.enabled = 0
	startB2S(47)
	chbkmo.enabled = True
	MoveMuhhamadDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "mohit"
	addextratime()
	MuhhamadChecker = (MuhhamadChecker OR 1)
	EnableMuhhamads()
end sub

sub MoveMuhhamadDown()
	Dim X
	For Each X in Muhhamads
		X.Visible = 0
	Next
end sub

sub ts11item002_hit()
	ts11item002.enabled = 0
	startB2S(47)
	chbkmo.enabled = True
	MoveMuhhamadDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "mohit"
	addextratime()
	MuhhamadChecker = (MuhhamadChecker OR 2)
	EnableMuhhamads()
end sub

sub ts11item003_hit()
	ts11item003.enabled = 0
	startB2S(47)
	chbkmo.enabled = True
	MoveMuhhamadDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "mohit"
	addextratime()
	MuhhamadChecker = (MuhhamadChecker OR 4)
	EnableMuhhamads()
end sub

sub ts11item004_hit()
	ts11item004.enabled = 0
	startB2S(47)
	chbkmo.enabled = True
	MoveMuhhamadDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "mohit"
	addextratime()
	MuhhamadChecker = (MuhhamadChecker OR 8)
	EnableMuhhamads()
end sub

sub ts11item005_hit()
	ts11item005.enabled = 0
	startB2S(47)
	chbkmo.enabled = True
	MoveMuhhamadDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "mohit"
	addextratime()
	MuhhamadChecker = (MuhhamadChecker OR 16)
	EnableMuhhamads()
end sub

sub CheckBonusMuhhamad()
	If MuhhamadChecker = 31 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, "100k"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		StopmodeEndofBall()
		SMcompleted = SMcompleted + 1
		MuhhamadChecker = 0
		SchoolSlot = SchoolSlot + 1
		p30 = 1
		li029.state = 1
		vpmTimer.AddTimer 3000, "changeb2sback2begin '"
		CheckSchool()
	end if
end sub

Sub chbkmo_timer
chbkmo.enabled = false
chhmo
end sub

Sub chhmo
startB2S(4)
end sub

'********schoolmission 12***************

Sub StartKrishna
StopSong
     DMD CL(0, "FIND 5 HOLYBOOKS"), CL(1, "COMPLETE THE MISSION"), "", eNone, eNone, eNone, 2300, True, "krishnastart"
	vpmTimer.AddTimer 2300, "startSmission12 '"
end sub

sub startSmission12
UpdateMusicNow
schoolmissionmodes = 2
mode1TimerCount = 60
mode1timer.Enabled = 1
EnableKrishnas()
KrishnaChecker = 0
kickeroutschool
end sub

Dim WhichKrishna, KrishnaChecker
WhichKrishna = 0
KrishnaChecker = 0
sub EnableKrishnas()
	If KrishnaChecker = 31 Then
		CheckBonusKrishna()
		Exit Sub
	End If
	Randomize()
	WhichKrishna = INT(RND * 5) + 1
	Select Case WhichKrishna
		Case 3
			WhichKrishna = 4
		Case 4
			WhichKrishna = 8
		Case 5
			WhichKrishna = 16
	End Select
	Do While (WhichKrishna AND KrishnaChecker) > 0
		WhichKrishna = INT(RND * 5) + 1
		Select Case WhichKrishna
			Case 3
				WhichKrishna = 4
			Case 4
				WhichKrishna = 8
			Case 5
				WhichKrishna = 16
		End Select
	Loop
	Select Case WhichKrishna
		Case 1
			ts12item001.enabled = 1
			Krishna001.Visible = 1
			Krishna001.X = ts12item001.X
			Krishna001.Y = ts12item001.Y
		Case 2
			ts12item002.enabled = 1
			Krishna001.Visible = 1
			Krishna001.X = ts12item002.X
			Krishna001.Y = ts12item002.Y
		Case 4
			ts12item003.enabled = 1
			Krishna001.Visible = 1
			Krishna001.X = ts12item003.X
			Krishna001.Y = ts12item003.Y
		Case 8
			ts12item004.enabled = 1
			Krishna001.Visible = 1
			Krishna001.X = ts12item004.X
			Krishna001.Y = ts12item004.Y
		Case 16
			ts12item005.enabled = 1
			Krishna001.Visible = 1
			Krishna001.X = ts12item005.X
			Krishna001.Y = ts12item005.Y
	End Select
end sub


sub ts12item001_hit()
	ts12item001.enabled = 0
	startB2S(43)
	chbkkries.enabled = True
	MoveKrishnaDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "krishnahit"
	addextratime()
	KrishnaChecker = (KrishnaChecker OR 1)
	EnableKrishnas()
end sub

sub MoveKrishnaDown()
	Dim X
	For Each X in Krishnas
		X.Visible = 0
	Next
end sub

sub ts12item002_hit()
	ts12item002.enabled = 0
	startB2S(43)
	chbkkries.enabled = True
	MoveKrishnaDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "krishnahit"
	addextratime()
	KrishnaChecker = (KrishnaChecker OR 2)
	EnableKrishnas()
end sub

sub ts12item003_hit()
	ts12item003.enabled = 0
	startB2S(43)
	chbkkries.enabled = True
	MoveKrishnaDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "krishnahit"
	addextratime()
	KrishnaChecker = (KrishnaChecker OR 4)
	EnableKrishnas()
end sub

sub ts12item004_hit()
	ts12item004.enabled = 0
	startB2S(43)
	chbkkries.enabled = True
	MoveKrishnaDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "krishnahit"
	addextratime()
	KrishnaChecker = (KrishnaChecker OR 8)
	EnableKrishnas()
end sub

sub ts12item005_hit()
	ts12item005.enabled = 0
	startB2S(43)
	chbkkries.enabled = True
	MoveKrishnaDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "krishnahit"
	addextratime()
	KrishnaChecker = (KrishnaChecker OR 16)
	EnableKrishnas()
end sub

sub CheckBonusKrishna()
	If KrishnaChecker = 31 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, "100k"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		StopmodeEndofBall()
		SMcompleted = SMcompleted + 1
		KrishnaChecker = 0
		SchoolSlot = SchoolSlot + 1
		p31 = 1
		li030.state = 1
		vpmTimer.AddTimer 3000, "changeb2sback2begin '"
		CheckSchool()
	end if
end sub

Sub chbkkries_timer
chbkkries.enabled = false
chhkj
end sub

Sub chhkj
startB2S(23)
end sub

'********schoolmission 13***************

Sub StartJoseph
StopSong
schoolmissionmodes = 2
     DMD CL(0, "FIND 5 TABLETS"), CL(1, "COMPLETE THE MISSION"), "", eNone, eNone, eNone, 3700, True, "josephstart"
	vpmTimer.AddTimer 3700, "startSmission13 '"
end Sub

Sub startSmission13
UpdateMusicNow
mode1TimerCount = 60
mode1timer.Enabled = 1
EnableJosephs()
JosephChecker = 0
kickeroutschool
end sub

Dim WhichJoseph, JosephChecker
WhichJoseph = 0
JosephChecker = 0
sub EnableJosephs()
	If JosephChecker = 31 Then
		CheckBonusJoseph()
		Exit Sub
	End If
	Randomize()
	WhichJoseph = INT(RND * 5) + 1
	Select Case WhichJoseph
		Case 3
			WhichJoseph = 4
		Case 4
			WhichJoseph = 8
		Case 5
			WhichJoseph = 16
	End Select
	Do While (WhichJoseph AND JosephChecker) > 0
		WhichJoseph = INT(RND * 5) + 1
		Select Case WhichJoseph
			Case 3
				WhichJoseph = 4
			Case 4
				WhichJoseph = 8
			Case 5
				WhichJoseph = 16
		End Select
	Loop
	Select Case WhichJoseph
		Case 1
			ts13item001.enabled = 1
			Joseph001.Visible = 1
			Joseph001.X = ts13item001.X
			Joseph001.Y = ts13item001.Y
		Case 2
			ts13item002.enabled = 1
			Joseph001.Visible = 1
			Joseph001.X = ts13item002.X
			Joseph001.Y = ts13item002.Y
		Case 4
			ts13item003.enabled = 1
			Joseph001.Visible = 1
			Joseph001.X = ts13item003.X
			Joseph001.Y = ts13item003.Y
		Case 8
			ts13item004.enabled = 1
			Joseph001.Visible = 1
			Joseph001.X = ts13item004.X
			Joseph001.Y = ts13item004.Y
		Case 16
			ts13item005.enabled = 1
			Joseph001.Visible = 1
			Joseph001.X = ts13item005.X
			Joseph001.Y = ts13item005.Y
	End Select
end sub


sub ts13item001_hit()
	ts13item001.enabled = 0
	startB2S(41)
	chbkjosp.enabled = True
	MoveJosephDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "josephhit"
	addextratime()
	JosephChecker = (JosephChecker OR 1)
	EnableJosephs()
end sub

sub MoveJosephDown()
	Dim X
	For Each X in Josephs
		X.Visible = 0
	Next
end sub

sub ts13item002_hit()
	ts13item002.enabled = 0
	startB2S(41)
	chbkjosp.enabled = True
	MoveJosephDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "josephhit"
	addextratime()
	JosephChecker = (JosephChecker OR 2)
	EnableJosephs()
end sub

sub ts13item003_hit()
	ts13item003.enabled = 0
	startB2S(41)
	chbkjosp.enabled = True
	MoveJosephDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "josephhit"
	addextratime()
	JosephChecker = (JosephChecker OR 4)
	EnableJosephs()
end sub

sub ts13item004_hit()
	ts13item004.enabled = 0
	startB2S(41)
	chbkjosp.enabled = True
	MoveJosephDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "josephhit"
	addextratime()
	JosephChecker = (JosephChecker OR 8)
	EnableJosephs()
end sub

sub ts13item005_hit()
	ts13item005.enabled = 0
	startB2S(41)
	chbkjosp.enabled = True
	MoveJosephDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "josephhit"
	addextratime()
	JosephChecker = (JosephChecker OR 16)
	EnableJosephs()
end sub

sub CheckBonusJoseph()
	If JosephChecker = 31 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, "100k"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		StopmodeEndofBall()
		SMcompleted = SMcompleted + 1
		JosephChecker = 0
		SchoolSlot = SchoolSlot + 1
		p32 = 1
		li031.state = 1
		vpmTimer.AddTimer 3000, "changeb2sback2begin '"
		CheckSchool()
	end if
end sub

Sub chbkjosp_timer
chbkjosp.enabled = false
chhjp
end sub

Sub chhjp
startB2S(17)
end sub

'********schoolmission 14***************

Sub StartLao
StopSong
schoolmissionmodes = 2
     DMD CL(0, "HIT 5 LATARNS"), CL(1, "COMPLETE THE MISSION"), "", eNone, eNone, eNone, 2450, True, "loasustart"
	vpmTimer.AddTimer 2450, "startSmission14 '"
end Sub

sub startSmission14
UpdateMusicNow
mode1TimerCount = 60
mode1timer.Enabled = 1
EnableLaos()
LaoChecker = 0
kickeroutschool
end sub

Dim WhichLao, LaoChecker
WhichLao = 0
LaoChecker = 0
sub EnableLaos()
	If LaoChecker = 31 Then
		CheckBonusLao()
		Exit Sub
	End If
	Randomize()
	WhichLao = INT(RND * 5) + 1
	Select Case WhichLao
		Case 3
			WhichLao = 4
		Case 4
			WhichLao = 8
		Case 5
			WhichLao = 16
	End Select
	Do While (WhichLao AND LaoChecker) > 0
		WhichLao = INT(RND * 5) + 1
		Select Case WhichLao
			Case 3
				WhichLao = 4
			Case 4
				WhichLao = 8
			Case 5
				WhichLao = 16
		End Select
	Loop
	Select Case WhichLao
		Case 1
			ts14item001.enabled = 1
			Lao001.Visible = 1
			Lao001.X = ts14item001.X
			Lao001.Y = ts14item001.Y
		Case 2
			ts14item002.enabled = 1
			Lao001.Visible = 1
			Lao001.X = ts14item002.X
			Lao001.Y = ts14item002.Y
		Case 4
			ts14item003.enabled = 1
			Lao001.Visible = 1
			Lao001.X = ts14item003.X
			Lao001.Y = ts14item003.Y
		Case 8
			ts14item004.enabled = 1
			Lao001.Visible = 1
			Lao001.X = ts14item004.X
			Lao001.Y = ts14item004.Y
		Case 16
			ts14item005.enabled = 1
			Lao001.Visible = 1
			Lao001.X = ts14item005.X
			Lao001.Y = ts14item005.Y
	End Select
end sub


sub ts14item001_hit()
	ts14item001.enabled = 0
	startB2S(44)
	chbkloa.enabled = True
	MoveLaoDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "LOAHIT"
	addextratime()
	LaoChecker = (LaoChecker OR 1)
	EnableLaos()
end sub

sub MoveLaoDown()
	Dim X
	For Each X in Laos
		X.Visible = 0
	Next
end sub

sub ts14item002_hit()
	ts14item002.enabled = 0
	startB2S(44)
	chbkloa.enabled = True
	MoveLaoDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "LOAHIT"
	addextratime()
	LaoChecker = (LaoChecker OR 2)
	EnableLaos()
end sub

sub ts14item003_hit()
	ts14item003.enabled = 0
	startB2S(44)
	chbkloa.enabled = True
	MoveLaoDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "LOAHIT"
	addextratime()
	LaoChecker = (LaoChecker OR 4)
	EnableLaos()
end sub

sub ts14item004_hit()
	ts14item004.enabled = 0
	startB2S(44)
	chbkloa.enabled = True
	MoveLaoDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "LOAHIT"
	addextratime()
	LaoChecker = (LaoChecker OR 8)
	EnableLaos()
end sub

sub ts14item005_hit()
	ts14item005.enabled = 0
	startB2S(44)
	chbkloa.enabled = True
	MoveLaoDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "LOAHIT"
	addextratime()
	LaoChecker = (LaoChecker OR 16)
	EnableLaos()
end sub

sub CheckBonusLao()
	If LaoChecker = 31 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, "100k"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		StopmodeEndofBall()
		SMcompleted = SMcompleted + 1
		LaoChecker = 0
		SchoolSlot = SchoolSlot + 1
		p33 = 1
		li032.state = 1
		vpmTimer.AddTimer 3000, "changeb2sback2begin '"
		CheckSchool()
	end if
end sub

Sub chbkloa_timer
chbkloa.enabled = false
chhloa
end sub

Sub chhloa
startB2S(18)
end sub

'********schoolmission 15***************

Sub StartSeaman
StopSong
schoolmissionmodes = 2
     DMD CL(0, "FIND 5 FLIPPERS"), CL(1, "COMPLETE THE MISSION"), "", eNone, eNone, eNone, 3000, True, "seamanstart"
	vpmTimer.AddTimer 3000, "startSmission15 '"
end Sub

Sub startSmission15
UpdateMusicNow
mode1TimerCount = 60
mode1timer.Enabled = 1
EnableSeamans()
SeamanChecker = 0
kickeroutschool
end sub

Dim WhichSeaman, SeamanChecker
WhichSeaman = 0
SeamanChecker = 0
sub EnableSeamans()
	If SeamanChecker = 31 Then
		CheckBonusSeaman()
		Exit Sub
	End If
	Randomize()
	WhichSeaman = INT(RND * 5) + 1
	Select Case WhichSeaman
		Case 3
			WhichSeaman = 4
		Case 4
			WhichSeaman = 8
		Case 5
			WhichSeaman = 16
	End Select
	Do While (WhichSeaman AND SeamanChecker) > 0
		WhichSeaman = INT(RND * 5) + 1
		Select Case WhichSeaman
			Case 3
				WhichSeaman = 4
			Case 4
				WhichSeaman = 8
			Case 5
				WhichSeaman = 16
		End Select
	Loop
	Select Case WhichSeaman
		Case 1
			ts15item001.enabled = 1
			Seaman001.Visible = 1
			Seaman001.X = ts15item001.X
			Seaman001.Y = ts15item001.Y
		Case 2
			ts15item002.enabled = 1
			Seaman001.Visible = 1
			Seaman001.X = ts15item002.X
			Seaman001.Y = ts15item002.Y
		Case 4
			ts15item003.enabled = 1
			Seaman001.Visible = 1
			Seaman001.X = ts15item003.X
			Seaman001.Y = ts15item003.Y
		Case 8
			ts15item004.enabled = 1
			Seaman001.Visible = 1
			Seaman001.X = ts15item004.X
			Seaman001.Y = ts15item004.Y
		Case 16
			ts15item005.enabled = 1
			Seaman001.Visible = 1
			Seaman001.X = ts15item005.X
			Seaman001.Y = ts15item005.Y
	End Select
end sub


sub ts15item001_hit()
	ts15item001.enabled = 0
	startB2S(50)
	chbkseaman.enabled = True
	MoveSeamanDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "seamanhit"
	addextratime()
	SeamanChecker = (SeamanChecker OR 1)
	EnableSeamans()
end sub

sub MoveSeamanDown()
	Dim X
	For Each X in Seamans
		X.Visible = 0
	Next
end sub

sub ts15item002_hit()
	ts15item002.enabled = 0
	startB2S(50)
	chbkseaman.enabled = True
	MoveSeamanDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "seamanhit"
	addextratime()
	SeamanChecker = (SeamanChecker OR 2)
	EnableSeamans()
end sub

sub ts15item003_hit()
	ts15item003.enabled = 0
	startB2S(50)
	chbkseaman.enabled = True
	MoveSeamanDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "seamanhit"
	addextratime()
	SeamanChecker = (SeamanChecker OR 4)
	EnableSeamans()
end sub

sub ts15item004_hit()
	ts15item004.enabled = 0
	startB2S(50)
	chbkseaman.enabled = True
	MoveSeamanDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "seamanhit"
	addextratime()
	SeamanChecker = (SeamanChecker OR 8)
	EnableSeamans()
end sub

sub ts15item005_hit()
	ts15item005.enabled = 0
	startB2S(50)
	chbkseaman.enabled = True
	MoveSeamanDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "seamanhit"
	addextratime()
	SeamanChecker = (SeamanChecker OR 16)
	EnableSeamans()
end sub

sub CheckBonusSeaman()
	If SeamanChecker = 31 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, "100k"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		StopmodeEndofBall()
		SMcompleted = SMcompleted + 1
		SeamanChecker = 0
		SchoolSlot = SchoolSlot + 1
		p34 = 1
		li033.state = 1
		vpmTimer.AddTimer 3000, "changeb2sback2begin '"
		CheckSchool()
	end if
end sub

Sub chbkseaman_timer
chbkseaman.enabled = false
chhsea
end sub

Sub chhsea
startB2S(24)
end sub

'********schoolmission 16***************

Sub StartMoses
StopSong
schoolmissionmodes = 2
     DMD CL(0, "FIND 5 TAPES"), CL(1, "COMPLETE THE MISSION"), "", eNone, eNone, eNone, 2800, True, "mosesstart"
	vpmTimer.AddTimer 2800, "startSmission16 '"
end Sub

sub startSmission16
UpdateMusicNow
mode1TimerCount = 60
mode1timer.Enabled = 1
EnableMosess()
MosesChecker = 0
kickeroutschool
end sub

Dim WhichMoses, MosesChecker
WhichMoses = 0
MosesChecker = 0
sub EnableMosess()
	If MosesChecker = 31 Then
		CheckBonusMoses()
		Exit Sub
	End If
	Randomize()
	WhichMoses = INT(RND * 5) + 1
	Select Case WhichMoses
		Case 3
			WhichMoses = 4
		Case 4
			WhichMoses = 8
		Case 5
			WhichMoses = 16
	End Select
	Do While (WhichMoses AND MosesChecker) > 0
		WhichMoses = INT(RND * 5) + 1
		Select Case WhichMoses
			Case 3
				WhichMoses = 4
			Case 4
				WhichMoses = 8
			Case 5
				WhichMoses = 16
		End Select
	Loop
	Select Case WhichMoses
		Case 1
			ts16item001.enabled = 1
			Moses001.Visible = 1
			Moses001.X = ts16item001.X
			Moses001.Y = ts16item001.Y
		Case 2
			ts16item002.enabled = 1
			Moses001.Visible = 1
			Moses001.X = ts16item002.X
			Moses001.Y = ts16item002.Y
		Case 4
			ts16item003.enabled = 1
			Moses001.Visible = 1
			Moses001.X = ts16item003.X
			Moses001.Y = ts16item003.Y
		Case 8
			ts16item004.enabled = 1
			Moses001.Visible = 1
			Moses001.X = ts16item004.X
			Moses001.Y = ts16item004.Y
		Case 16
			ts16item005.enabled = 1
			Moses001.Visible = 1
			Moses001.X = ts16item005.X
			Moses001.Y = ts16item005.Y
	End Select
end sub


sub ts16item001_hit()
	ts16item001.enabled = 0
	chbkmozes.enabled = true
	startB2S(45)
	MoveMosesDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "moseshit"
	addextratime()
	MosesChecker = (MosesChecker OR 1)
	EnableMosess()
end sub

sub MoveMosesDown()
	Dim X
	For Each X in Mosess
		X.Visible = 0
	Next
end sub

sub ts16item002_hit()
	ts16item002.enabled = 0
	chbkmozes.enabled = true
	startB2S(45)
	MoveMosesDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "moseshit"
	addextratime()
	MosesChecker = (MosesChecker OR 2)
	EnableMosess()
end sub

sub ts16item003_hit()
	ts16item003.enabled = 0
	chbkmozes.enabled = true
	startB2S(45)
	MoveMosesDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "moseshit"
	addextratime()
	MosesChecker = (MosesChecker OR 4)
	EnableMosess()
end sub

sub ts16item004_hit()
	ts16item004.enabled = 0
	chbkmozes.enabled = true
	startB2S(45)
	MoveMosesDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "moseshit"
	addextratime()
	MosesChecker = (MosesChecker OR 8)
	EnableMosess()
end sub

sub ts16item005_hit()
	ts16item005.enabled = 0
	chbkmozes.enabled = true
	startB2S(45)
	MoveMosesDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "moseshit"
	addextratime()
	MosesChecker = (MosesChecker OR 16)
	EnableMosess()
end sub

sub CheckBonusMoses()
	If MosesChecker = 31 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, "100k"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		StopmodeEndofBall()
		SMcompleted = SMcompleted + 1
		MosesChecker = 0
		SchoolSlot = SchoolSlot + 1
		p35 = 1
		li034.state = 1
		vpmTimer.AddTimer 3000, "changeb2sback2begin '"
		CheckSchool()
	end if
end sub

Sub chbkmozes_timer
chbkmozes.enabled = false
chhmoz
end sub

Sub chhmoz
startB2S(19)
end sub

'*********moving targets*********
MovingTarget_Init
Dim movingwall, movingwall2
Sub MovingTarget_Init
	movingwall = Array (W001,W002,W003,W004,W005,W006,W007,W008,W009,W010,W011,W012,W013,W014,W015,W016,W017,W018,W019)
	movingwall2 = Array (WU014,WU013,WU012,WU011,WU010,WU009,WU008,WU007,WU006,WU005,WU004,WU003,WU002,WU001)
End Sub

Sub disableWalls1
W001.collidable=0:W002.collidable=0:W003.collidable=0:W004.collidable=0:W005.collidable=0:W006.collidable=0:W007.collidable=0:W008.collidable=0:W009.collidable=0:W010.collidable=0:W011.collidable=0:W012.collidable=0:W013.collidable=0:W014.collidable=0:W015.collidable=0:W016.collidable=0:W017.collidable=0:W018.collidable=0:W019.collidable=0
end sub

Sub disableWalls2
WU001.collidable=0:WU002.collidable=0:WU003.collidable=0:WU004.collidable=0:WU005.collidable=0:WU006.collidable=0:WU007.collidable=0:WU008.collidable=0:WU009.collidable=0:WU010.collidable=0:WU011.collidable=0:WU012.collidable=0:WU013.collidable=0:WU014.collidable=0
end sub


'*********moving target up animation*********

Dim ShotUpDir
ShotUpDir = 5 'this is both the direction, if + goes up, if - goes down, and also the speed
dim lastwall2


Sub Wallupdate3
    movingwall2(Lastwall2).collidable=False
    Lastwall2=int(( 580- Terror1.x )/20)
	If lastwall2<0 Then Lastwall2=0
	If lastwall2>13 Then lastwall2=13
    debug.print "lastwall2" & lastwall2
    movingwall2(Lastwall2).collidable=True
End Sub


Sub shot001timer_Timer
    terror1.X = terror1.X + ShotUpDir
    movingshot001.x=movingshot001.X+ ShotUpDir
    wallupdate3
    If terror1.X < 310 Then ShotUpDir = 5 'goes down
    If terror1.X > 575 Then ShotUpDir = -5
End Sub


Dim ShotUpDir1
ShotUpDir1 = 5 'this is both the direction, if + goes up, if - goes down, and also the speed

Sub shot003timer_Timer
    terror1.X = terror1.X + ShotUpDir1
    movingshot001.x=movingshot001.X+ ShotUpDir1
    If terror1.X < 310 Then ShotUpDir1 = 5 'goes down
    If terror1.X > 575 Then ShotUpDir1 = -5
End Sub

Sub WU001_hit : terror3hit : End Sub
Sub WU002_hit : terror3hit : End Sub
Sub WU003_hit : terror3hit : End Sub
Sub WU004_hit : terror3hit : End Sub
Sub WU005_hit : terror3hit : End Sub
Sub WU006_hit : terror3hit : End Sub
Sub WU007_hit : terror3hit : End Sub
Sub WU008_hit : terror3hit : End Sub
Sub WU009_hit : terror3hit : End Sub
Sub WU010_hit : terror3hit : End Sub
Sub WU011_hit : terror3hit : End Sub
Sub WU012_hit : terror3hit : End Sub
Sub WU013_hit : terror3hit : End Sub
Sub WU014_hit : terror3hit : End Sub

sub Terror3Hit
	if ter1a = 0 then
		startB2S(57)
		Ttershoot.enabled = True
		Playsound "ter_hit"
		ter1a = ter1a + 1
		checkterrorhits1
		exit sub
	end if
	if ter2a = 0 then
		startB2S(57)
		Ttershoot.enabled = True
		Playsound "ter_hit"
		ter2a = ter2a + 1
		exit sub
	end if
	if ter2a = 1 then
		startB2S(57)
		Ttershoot.enabled = True
		Playsound "ter_hit"
		ter2a = ter2a + 1
		checkterrorhits1
		exit sub
	end if
	if ter3a = 0 then
		startB2S(57)
		Ttershoot.enabled = True
		Playsound "ter_hit"
		ter3a = ter3a + 1
		exit sub
	end if
	if ter3a = 1 then
		startB2S(57)
		Ttershoot.enabled = True
		Playsound "ter_hit"
		ter3a = ter3a + 1
		exit sub
	end if
	if ter3a = 2 then
		startB2S(57)
		Ttershoot.enabled = True
		Playsound "ter_hit"
		ter3a = ter3a + 1
		checkterrorhits1
		exit sub
	end if
end sub

Sub Ttershoot_timer
startB2S(10)
Ttershoot.enabled = False
end sub

sub checkterrorhits1
	if ter1a = 1 then
		startB2S(58)
		Tterbomb.enabled = True
		disableWalls1
		shot001timer.enabled = False
		Playsound "terboom"
		vpmTimer.AddTimer 1500, "resettargett1 '"
	end if
	if ter2a = 2 then
		startB2S(58)
		Tterbomb.enabled = True
		disableWalls1
		shot001timer.enabled = False
		Playsound "terboom"
		vpmTimer.AddTimer 1500, "resettargett1 '"
	end if
	if ter3a = 3 then
		startB2S(58)
		Tterbomb.enabled = True
		shot001timer.enabled = False
		Playsound "terboom"
		vpmTimer.AddTimer 1500, "resettargett1 '"
	end if
end sub 

Sub Tterbomb_timer
startB2S(10)
Tterbomb.enabled = False
end sub

sub resettargett1
Checkterrorklaar
movingwall2(Lastwall2).collidable=false
movingshot001.z = -35
terror1.visible = False
shot003timer.enabled = True
end sub

'*********moving target down animation*********

Dim ShotDownDir
ShotDownDir = 5 'this is both the direction, if + goes up, if - goes down, and also the speed
dim lastwall
Sub Wallupdate2
    movingwall(Lastwall).collidable=False
    Lastwall= int(( 639- Terror2.x )/20)
	If lastwall<0 Then Lastwall=0
	If lastwall>18 Then lastwall=18
    movingwall(Lastwall).collidable=True
End Sub
Sub shot002timer_Timer
    terror2.X = terror2.X - ShotDownDir
    movingshot002.x=movingshot002.X- ShotDownDir
    wallupdate2
    If terror2.X < 264 Then ShotDownDir = -5 'goes down
    If terror2.X > 634 Then ShotDownDir = 5
End Sub

Dim ShotDownDir1
ShotDownDir1 = 5 'this is both the direction, if + goes up, if - goes down, and also the speed

Sub shot004timer_Timer
    terror2.X = terror2.X - ShotDownDir1
    movingshot002.x=movingshot002.X- ShotDownDir1
    If terror2.X < 264 Then ShotDownDir1 = -5 'goes down
    If terror2.X > 634 Then ShotDownDir1 = 5
End Sub

Sub W001_hit : terror2hit : End Sub
Sub W002_hit : terror2hit : End Sub
Sub W003_hit : terror2hit : End Sub
Sub W004_hit : terror2hit : End Sub
Sub W005_hit : terror2hit : End Sub
Sub W006_hit : terror2hit : End Sub
Sub W007_hit : terror2hit : End Sub
Sub W008_hit : terror2hit : End Sub
Sub W009_hit : terror2hit : End Sub
Sub W010_hit : terror2hit : End Sub
Sub W011_hit : terror2hit : End Sub
Sub W012_hit : terror2hit : End Sub
Sub W013_hit : terror2hit : End Sub
Sub W014_hit : terror2hit : End Sub
Sub W015_hit : terror2hit : End Sub
Sub W016_hit : terror2hit : End Sub
Sub W017_hit : terror2hit : End Sub
Sub W018_hit : terror2hit : End Sub
Sub W019_hit : terror2hit : End Sub
'Sub W020_hit : terror2hit : End Sub

sub Terror2Hit
	if bosssaddam = 1 and bvul = 0 then
		DMD "", "", "dmdsinv", eNone, eNone, eNone, 1000, True, ""
	playsound "sadam_hit"
	end if
	if bosssaddam = 1 and bvul = 1 then
		playsound "sadam_hurt"
		bhurt = bhurt + 1
		checksaddamhurt
	end if
	if bosshitler = 1 and bvul = 0 then
		DMD "", "", "dmdhinv", eNone, eNone, eNone, 1000, True, ""
		playsound "HIT_hit"
	end if
	if bosshitler = 1 and bvul = 1 then
		playsound "HIT_hurt"
		bhurt = bhurt + 1
		checkhitlerhurt
	end if
	if bossosama = 1 and bvul = 0 then
		DMD "", "", "dmdoinv", eNone, eNone, eNone, 1000, True, ""
		playsound "os_hit"
	end if
	if bossosama = 1 and bvul = 1 then
		playsound "os_hurt"
		bhurt = bhurt + 1
		checkosamahurt
	end if
	if bossclitty = 1 and bvul = 0 then
		DMD "", "", "dmdclinv", eNone, eNone, eNone, 1000, True, ""
	playsound "clit_hit2"
	end if
	if bossclitty = 1 and bvul = 1 then
		playsound "clitorishit"
		bhurt = bhurt + 1
		checkclitorishurt
	end if
	if bosskim = 1 and bvul = 0 then
		DMD "", "", "dmdkiminv", eNone, eNone, eNone, 1000, True, ""
	playsound "rocketman"
	end if
	if bosskim = 1 and bvul = 1 then
		playsound "kimhurt"
		bhurt = bhurt + 1
		checkkimhurt
	end if
	if ter1b = 0 then
		startB2S(57)
		Ttershoot.enabled = True
		Playsound "ter_hit"
		ter1b = ter1b + 1
		checkterrorhits2
		exit sub
	end if
	if ter2b = 0 then
		startB2S(57)
		Ttershoot.enabled = True
		Playsound "ter_hit"
		ter2b = ter2b + 1
		exit sub
	end if
	if ter2b = 1 then
		startB2S(57)
		Ttershoot.enabled = True
		Playsound "ter_hit"
		ter2b = ter2b + 1
		checkterrorhits2
		exit sub
	end if
	if ter3b = 0 then
		startB2S(57)
		Ttershoot.enabled = True
		Playsound "ter_hit"
		ter3b = ter3b + 1
		exit sub
	end if
	if ter3b = 1 then
		startB2S(57)
		Ttershoot.enabled = True
		Playsound "ter_hit"
		ter3b = ter3b + 1
		exit sub
	end if
	if ter3b = 2 then
		startB2S(57)
		Ttershoot.enabled = True
		Playsound "ter_hit"
		ter3b = ter3b + 1
		checkterrorhits2
		exit sub
	end if
end sub

sub checkterrorhits2
	if ter1b = 1 then
		startB2S(58)
		Tterbomb.enabled = True
		shot002timer.enabled = False
		Playsound "terboom"
		vpmTimer.AddTimer 1500, "resettargett2 '"
	end if
	if ter2b = 2 then
		startB2S(58)
		Tterbomb.enabled = True
		shot002timer.enabled = False
		Playsound "terboom"
		vpmTimer.AddTimer 1500, "resettargett2 '"
	end if
	if ter3b = 3 then
		startB2S(58)
		Tterbomb.enabled = True
		shot002timer.enabled = False
		Playsound "terboom"
		vpmTimer.AddTimer 1500, "resettargett2 '"
	end if
end sub 

sub resettargett2
Checkterrorklaar
movingwall(Lastwall).collidable=False
disableWalls2
movingshot002.z = -35
terror2.visible = False
shot004timer.enabled = True
end sub

sub Checkterrorklaar
	if ter1a = 1 and ter1b = 1 Then
		vpmTimer.AddTimer 3000, "changeb2sback2begin '"
		DMD "", "", "300k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (300000*PFMultiplier)
		UpdateMusic = 0
		UpdateMusicNow
		kennymissionmodes = 1
		li101.state = 1
		StopBallon
		p2 = 1 
		p3 = 0
		ter1a = 5
		ter1b = 5
	end if
	if ter2a = 2 and ter2b = 2 Then
		vpmTimer.AddTimer 3000, "changeb2sback2begin '"
		DMD "", "", "300k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (300000*PFMultiplier)
		UpdateMusic = 0
		UpdateMusicNow
		kennymissionmodes = 1
		li102.state = 1
		StopBallon
		p4 = 1
		p5 = 0
		ter2a = 5
		ter2b = 5
	end if 
	if ter3a = 3 and ter3b = 3 Then
		vpmTimer.AddTimer 3000, "changeb2sback2begin '"
		DMD "", "", "300k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (300000*PFMultiplier)
		UpdateMusic = 0
		UpdateMusicNow
		kennymissionmodes = 1
		li103.state = 1
		StopBallon
		p6 = 1
		p7 = 0
		ter3a = 5
		ter3b = 5
	end if 
end sub

sub tsboss001_hit
	if bosssaddam = 1 then
		startB2S(49)
		Trightdildo001.enabled = True
		playsound "dildo_hit"
		bhit = bhit + 1
		tsboss001.enabled = false
		dildo001.Visible = false
		checkbosshit
	end if
	if bosshitler = 1 then
		startB2S(21)
		Trightflag001.enabled = True
		playsound "flag_hit"
		bhit = bhit + 1
		tsboss001.enabled = false
		fhit1.Visible = false
		checkbosshit
	end if
	if bossosama = 1 then
		startB2S(48)
		Trightcamel001.enabled = True
		playsound "camel_hit"
		bhit = bhit + 1
		tsboss001.enabled = false
		camel1.Visible = false
		checkbosshit
	end if 
	if bossclitty = 1 then
		startB2S(39)
		Trightclit001.enabled = True
		playsound "clitoris"
		bhit = bhit + 1
		tsboss001.enabled = false
		clita002.Visible = false
		checkbosshit
	end if 
	if bosskim = 1 then
		startB2S(42)
		Trightbomb001.enabled = True
		playsound "rockethit"
		bhit = bhit + 1
		tsboss001.enabled = false
		bombb.Visible = false
		checkbosshit
	end if 
end sub

sub tsboss002_hit
	if bosssaddam = 1 then
		startB2S(49)
		Tleftdildo001.enabled = True
		playsound "dildo_hit"
		bhit = bhit + 1
		tsboss002.enabled = false
		dildo002.Visible = false
		checkbosshit
	end if
	if bosshitler = 1 then
		startB2S(21)
		Tleftflag001.enabled = True
		playsound "flag_hit"
		bhit = bhit + 1
		tsboss002.enabled = false
		fhit2.Visible = false
		checkbosshit
	end if
	if bossosama = 1 then
		startB2S(48)
		Tleftcamel001.enabled = True
		playsound "camel_hit"
		bhit = bhit + 1
		tsboss002.enabled = false
		camel2.Visible = false
		checkbosshit
	end if
	if bossclitty = 1 then
		startB2S(39)
		Tleftclit001.enabled = True
		playsound "clitoris"
		bhit = bhit + 1
		tsboss002.enabled = false
		clita001.Visible = false
		checkbosshit
	end if 
	if bosskim = 1 then
		startB2S(42)
		Tleftbomb001.enabled = True
		playsound "rockethit"
		bhit = bhit + 1
		tsboss002.enabled = false
		bomba.Visible = false
		checkbosshit
	end if 
end sub

sub Tleftflag001_timer
startB2S(2)
Tleftflag001.enabled = false
end Sub

sub Trightflag001_timer
startB2S(2)
Trightflag001.enabled = false
end Sub

Sub Trightclit001_timer
startB2S(8)
Trightclit001.enabled = False
end sub

Sub Tleftclit001_timer
startB2S(8)
Tleftclit001.enabled = False
end sub

Sub Trightbomb001_timer
startB2S(20)
Trightbomb001.enabled = False
end sub

Sub Tleftbomb001_timer
startB2S(20)
Tleftbomb001.enabled = False
end sub

Sub Trightcamel001_timer
startB2S(11)
Trightcamel001.enabled = False
end sub

Sub Tleftcamel001_timer
startB2S(11)
Tleftcamel001.enabled = False
end sub

Sub Tleftdildo001_timer
startB2S(3)
Tleftdildo001.enabled = False
end sub

Sub Trightdildo001_timer
startB2S(3)
Trightdildo001.enabled = False
end sub

Sub checkbosshit
	if bosssaddam = 1 and bhit = 2 then
		bvul = 1
	end if
	if bosshitler = 1 and bhit = 2 then
		bvul = 1
	end if
	if bossosama = 1 and bhit = 2 then
		bvul = 1
	end if
	if bossclitty = 1 and bhit = 2 then
		bvul = 1
	end if
	if bosskim = 1 and bhit = 2 then
		bvul = 1
	end if
end sub

sub checksaddamhurt
	if bhurt = 1 then
		tsboss001.enabled = true
		tsboss002.enabled = true
		dildo001.Visible = True
		dildo002.Visible = True
		bhit = 0
		bvul = 0
		DMD "", "", "dmdsdh2", eNone, eNone, eNone, 1000, True, ""
	end if
	if bhurt = 2 then
		tsboss001.enabled = true
		tsboss002.enabled = true
		dildo001.Visible = True
		dildo002.Visible = True
		bhit = 0
		bvul = 0
		DMD "", "", "dmdsdh1", eNone, eNone, eNone, 1000, True, ""
	end if
	if bhurt = 3 then
		changeb2sback2begin
		UpdateMusic = 0
		UpdateMusicNow
		p3 = 1
		bhit = 0
		bvul = 0
		p4 = 0
		bosssaddam = 0
		li104.state = 1
		li010.state = 1
		StopBallon
		Bcompleted = Bcompleted + 1
		kennymissionmodes = 1
		resettargett2
		shot002timer.enabled = false
		DMD "", "", "dmdsdk", eNone, eNone, eNone, 1000, True, ""
		vpmtimer.addtimer 1500, "shoutpoints '"
	end if
end sub

sub checkhitlerhurt
	if bhurt = 1 then
		tsboss001.enabled = true
		tsboss002.enabled = true
		fhit1.Visible = True
		fhit2.Visible = True
		bhit = 0
		bvul = 0
		DMD "", "", "dmdhh2", eNone, eNone, eNone, 1000, True, ""
	end if
	if bhurt = 2 then
		tsboss001.enabled = true
		tsboss002.enabled = true
		fhit1.Visible = True
		fhit2.Visible = True
		bhit = 0
		bvul = 0
		DMD "", "", "dmdhh1", eNone, eNone, eNone, 1000, True, ""
	end if
	if bhurt = 3 then
		changeb2sback2begin
		UpdateMusic = 0
		UpdateMusicNow
		p5 = 1
		bhit = 0
		bvul = 0
		p6 = 0
		bosshitler = 0
		li105.state = 1
		li123.state = 1
		StopBallon
		Bcompleted = Bcompleted + 1
		kennymissionmodes = 1
		resettargett2
		shot002timer.enabled = false
		DMD "", "", "dmdhk", eNone, eNone, eNone, 1000, True, ""
		vpmtimer.addtimer 1500, "shoutpoints '"
	end if
end sub

sub checkosamahurt
	if bhurt = 1 then
		tsboss001.enabled = true
		tsboss002.enabled = true
		camel1.Visible = True
		camel2.Visible = True
		bhit = 0
		bvul = 0
		DMD "", "", "dmdoh2", eNone, eNone, eNone, 1000, True, ""
	end if
	if bhurt = 2 then
		tsboss001.enabled = true
		tsboss002.enabled = true
		camel1.Visible = True
		camel2.Visible = True
		bhit = 0
		bvul = 0
		DMD "", "", "dmdoh1", eNone, eNone, eNone, 1000, True, ""
	end if
	if bhurt = 3 then
		'playsound "os_death"
		changeb2sback2begin
		UpdateMusic = 0
		UpdateMusicNow
		p7 = 1
		bhit = 0
		bvul = 0
		bossosama = 0
		li125.state = 1
		li106.state = 1
		StopBallon
		Bcompleted = Bcompleted + 1
		kennymissionmodes = 1
		resettargett2
		shot002timer.enabled = false
		DMD "", "", "dmdok", eNone, eNone, eNone, 1000, True, ""
		vpmtimer.addtimer 1500, "shoutpoints '"
	end if
end sub

sub checkclitorishurt
	if bhurt = 1 then
		tsboss001.enabled = true
		tsboss002.enabled = true
		clita001.Visible = True
		clita002.Visible = True
		bhit = 0
		bvul = 0
		DMD "", "", "dmdclh2", eNone, eNone, eNone, 1000, True, ""
	end if
	if bhurt = 2 then
		tsboss001.enabled = true
		tsboss002.enabled = true
		clita001.Visible = True
		clita002.Visible = True
		bhit = 0
		bvul = 0
		DMD "", "", "dmdclh1", eNone, eNone, eNone, 1000, True, ""
	end if
	if bhurt = 3 then
		changeb2sback2begin
		UpdateMusic = 0
		UpdateMusicNow
		p36 = 1
		bhit = 0
		bvul = 0
		p37 = 0
		bossclitty = 0
		li009.state = 1
		StopBallon
		Bcompleted = Bcompleted + 1
		kennymissionmodes = 1
		resettargett2
		shot002timer.enabled = false
		DMD "", "", "dmdclk", eNone, eNone, eNone, 1000, True, ""
		vpmtimer.addtimer 1500, "shoutpoints '"
	end if
end sub

sub checkkimhurt
	if bhurt = 1 then
		tsboss001.enabled = true
		tsboss002.enabled = true
		bomba.Visible = True
		bombb.Visible = True
		bhit = 0
		bvul = 0
		DMD "", "", "dmdkimh2", eNone, eNone, eNone, 1000, True, ""
	end if
	if bhurt = 2 then
		tsboss001.enabled = true
		tsboss002.enabled = true
		bomba.Visible = True
		bombb.Visible = True
		bhit = 0
		bvul = 0
		DMD "", "", "dmdkimh1", eNone, eNone, eNone, 1000, True, ""
	end if
	if bhurt = 3 then
		changeb2sback2begin
		UpdateMusic = 0
		UpdateMusicNow
		p37 = 1
		bhit = 0
		bvul = 0
		bosskim = 0
		StopBallon
		li124.state = 1
		Bcompleted = Bcompleted + 1
		kennymissionmodes = 1
		resettargett2
		shot002timer.enabled = false
		DMD "", "", "dmdkimd", eNone, eNone, eNone, 1000, True, ""
		vpmtimer.addtimer 1500, "shoutpoints '"
	end if
end sub

sub shoutpoints
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
DMD "", "", "1mln", eNone, eNone, eNone, 1000, True, "1000000"
end sub

sub shoutpoints2
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
PlaySound "1000000"
end sub

sub resetbossys
	tsboss001.enabled = False
	tsboss002.enabled = False
	dildo001.Visible = False
	dildo002.Visible = False
	fhit1.Visible = False
	fhit2.Visible = False
	camel1.Visible = False
	camel2.Visible = False
	clita001.Visible = False
	clita002.Visible = False
	bomba.Visible = False
	bombb.Visible = False
end sub

'************************************************
'**************Bonusscore lights drainen case*****************
'************************************************

sub blicht1
if  li043.state = 1 then li043.state = 0: playsound "twang":Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*SPMultiplier) end if
end sub

sub blicht2
if  li042.state = 1 then li042.state = 0: playsound "twang":Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*SPMultiplier) end if
end sub

sub blicht3
if  li041.state = 1 then li041.state = 0: playsound "twang":Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*SPMultiplier) end if
end sub

sub blicht4
if  li040.state = 1 then li040.state = 0: playsound "twang":Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*SPMultiplier) end if
end sub

sub blicht5
if  li039.state = 1 then li039.state = 0: playsound "twang":Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*SPMultiplier) end if
end sub

sub blicht6
if  li038.state = 1 then li038.state = 0: playsound "twang":Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*SPMultiplier) end if
end sub

sub blicht7
if  li037.state = 1 then li037.state = 0: playsound "twang":Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*SPMultiplier) end if
end sub

sub blicht8
if  li036.state = 1 then li036.state = 0: playsound "twang":Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*SPMultiplier) end if
end sub

sub blicht9
if  li035.state = 1 then li035.state = 0: playsound "twang":Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*SPMultiplier) end if
end sub

sub resettempbonus
lichtTimer.enabled = False
tempbonusaf = 1
end sub

sub drainlights
    Select Case tempbonusaf
	Case 0:	resettempbonus
	Case 1: blicht1:Resettargets
	Case 2: blicht2
	Case 3: blicht3
	Case 4: blicht4
	Case 5: blicht5
	Case 6: blicht6
	Case 7: blicht7
	Case 8: blicht8
	Case 9: blicht9
    End Select
End Sub

Sub lichtTimer_Timer
If tempbonusaf > 1 then
tempbonusaf = tempbonusaf -1
drainlights
end if
end sub

'********************************
'		 Digital clock
'********************************

Dim ClockDigits(4), ClockChars(10)

ClockDigits(0) = Array(a00, a02, a05, a06, a04, a01, a03) 'clock left digit
ClockDigits(1) = Array(a10, a12, a15, a16, a14, a11, a13)
ClockChars(0) = Array(1, 1, 1, 1, 1, 1, 0)				  '0
ClockChars(1) = Array(0, 1, 1, 0, 0, 0, 0)				  '1
ClockChars(2) = Array(1, 1, 0, 1, 1, 0, 1)				  '2
ClockChars(3) = Array(1, 1, 1, 1, 0, 0, 1)				  '3
ClockChars(4) = Array(0, 1, 1, 0, 0, 1, 1)				  '4
ClockChars(5) = Array(1, 0, 1, 1, 0, 1, 1)				  '5
ClockChars(6) = Array(1, 0, 1, 1, 1, 1, 1)				  '6
ClockChars(7) = Array(1, 1, 1, 0, 0, 0, 0)				  '7
ClockChars(8) = Array(1, 1, 1, 1, 1, 1, 1)				  '8
ClockChars(9) = Array(1, 1, 1, 1, 0, 1, 1)				  '9

Sub UpdateClock(myTime)
	Dim a, b, i
	a = myTime \ 10
	b = myTime MOD 10
	For i = 0 to 6
		ClockDigits(0)(i).State = ClockChars(a)(i)
		ClockDigits(1)(i).State = ClockChars(b)(i)
	Next
End Sub

Sub TurnOffClock
	Dim i
	For i = 0 to 6
		ClockDigits(0)(i).State = 0
		ClockDigits(1)(i).State = 0
	Next
End Sub

'clocktimer
Sub mode1timer_Timer
	mode1TimerCount = mode1TimerCount - 1
	UpdateClock mode1TimerCount
	If mode1TimerCount = 30 Then PlaySound "hurry_1"
	If mode1TimerCount = 10 Then PlaySound "hurry_2"
	If mode1TimerCount = 0 Then
	playsound "Mfail"
		Stopmode1()
	End If
End Sub

'Sub mode2timer_Timer
'	mode2TimerCount = mode2TimerCount - 1
'	UpdateClock mode2TimerCount
'	If mode2TimerCount = 0 Then
'	DMD "", "", "tf", eNone, eNone, eNone, 1000, True, "treasurefailed"
'		Stopmode2()
'	End If
'End Sub

Sub Stopmode1()
	mode1timer.Enabled = 0
	stopquests()
'	StopSong()
'	UpdateMusicNow()
	TurnOffClock()
'	Wall016.TimerEnabled = 0
'	Wall018.TimerEnabled = 0
'	Dim X
'	For Each X in ShotsInserts
'		X.State = 0
'		X.BlinkInterval = 125
'		X.BlinkPattern = 10
'	Next
'	CheckBar()
	CheckSchool()
End Sub

'Sub Stopmode2()
'	RightFlipper001.RotatetoStart
'	LeftFlipper001.RotatetoStart
'	LowerFlippersActive = False
'	mode2timer.Enabled = 0
'	stopquests()
'	StopSong()
'	UpdateMusicNow()
'	TurnOffClock()
'	CheckBar()
'	CheckVoodoo()
'End Sub

Sub StopmodeEndofBall()
	mode1timer.Enabled = 0
'	mode2timer.Enabled = 0
'	StopSong()
'	itemrotytimer.Enabled = 0
'	itemrotztimer.Enabled = 0
	'UpdateMusicNow
	TurnOffClock()
	stopquests()
'	Status = "Normal"
'	CheckBar()
	CheckSchool()
End Sub


sub stopsongquests()
Dim X
musicmodes = 0
tsong1001.enabled = 0
tsong1002.enabled = 0
tsong1003.enabled = 0
tsong2001.enabled = 0
tsong2002.enabled = 0
tsong2003.enabled = 0
tsong3001.enabled = 0
tsong3002.enabled = 0
tsong3003.enabled = 0
tsong4001.enabled = 0
tsong4002.enabled = 0
tsong4003.enabled = 0
tsong5001.enabled = 0
tsong5002.enabled = 0
tsong5003.enabled = 0
tsong6001.enabled = 0
tsong6002.enabled = 0
tsong6003.enabled = 0
tsong7001.enabled = 0
tsong7002.enabled = 0
tsong7003.enabled = 0
tsong8001.enabled = 0
tsong8002.enabled = 0
tsong8003.enabled = 0
tsong9001.enabled = 0
tsong9002.enabled = 0
tsong9003.enabled = 0
tsong10001.enabled = 0
tsong10002.enabled = 0
tsong10003.enabled = 0
tsong11001.enabled = 0
tsong11002.enabled = 0
tsong11003.enabled = 0
tsong12001.enabled = 0
tsong12002.enabled = 0
tsong12003.enabled = 0
	For Each X in songss
		X.Visible = 0
	Next
end sub

sub stopquests()
Dim X
changeb2sback2begin
changeling001.image = "dsprt"
changeling003.image = "dspcp"
changeling001.Visible = true
changeling002.Visible  = false
changeling003.Visible  = true
changeling004.Visible  = false
changeling005.Visible  = false
turny1Timer.enabled = False
schoolmissionmodes = 1
'kennymissions = 1
tsitem001.enabled = 0
tsitem002.enabled = 0
tsitem003.enabled = 0
tsitem004.enabled = 0
tsitem005.enabled = 0
ts2item001.enabled = 0
ts2item002.enabled = 0
ts2item003.enabled = 0
ts2item004.enabled = 0
ts2item005.enabled = 0
ts3item001.enabled = 0
ts3item002.enabled = 0
ts3item003.enabled = 0
ts3item004.enabled = 0
ts3item005.enabled = 0
ts4item001.enabled = 0
ts4item002.enabled = 0
ts4item003.enabled = 0
ts4item004.enabled = 0
ts4item005.enabled = 0
ts5item001.enabled = 0
ts5item002.enabled = 0
ts5item003.enabled = 0
ts5item004.enabled = 0
ts5item005.enabled = 0
ts6item001.enabled = 0
ts6item002.enabled = 0
ts6item003.enabled = 0
ts6item004.enabled = 0
ts6item005.enabled = 0
ts7item001.enabled = 0
ts7item002.enabled = 0
ts7item003.enabled = 0
ts7item004.enabled = 0
ts7item005.enabled = 0
ts8item001.enabled = 0
ts8item002.enabled = 0
ts8item003.enabled = 0
ts8item004.enabled = 0
ts8item005.enabled = 0
ts9item001.enabled = 0
ts9item002.enabled = 0
ts9item003.enabled = 0
ts9item004.enabled = 0
ts9item005.enabled = 0
ts10item001.enabled = 0
ts10item002.enabled = 0
ts10item003.enabled = 0
ts10item004.enabled = 0
ts10item005.enabled = 0
ts11item001.enabled = 0
ts11item002.enabled = 0
ts11item003.enabled = 0
ts11item004.enabled = 0
ts11item005.enabled = 0
ts12item001.enabled = 0
ts12item002.enabled = 0
ts12item003.enabled = 0
ts12item004.enabled = 0
ts12item005.enabled = 0
ts13item001.enabled = 0
ts13item002.enabled = 0
ts13item003.enabled = 0
ts13item004.enabled = 0
ts13item005.enabled = 0
ts14item001.enabled = 0
ts14item002.enabled = 0
ts14item003.enabled = 0
ts14item004.enabled = 0
ts14item005.enabled = 0
ts15item001.enabled = 0
ts15item002.enabled = 0
ts15item003.enabled = 0
ts15item004.enabled = 0
ts15item005.enabled = 0
ts16item001.enabled = 0
ts16item002.enabled = 0
ts16item003.enabled = 0
ts16item004.enabled = 0
ts16item005.enabled = 0
	For Each X in Poopss
		X.Visible = 0
	Next
	For Each X in Clitoriss
		X.Visible = 0
	Next
	For Each X in Jobss
		X.Visible = 0
	Next
	For Each X in Flags
		X.Visible = 0
	Next
	For Each X in Starvings
		X.Visible = 0
	Next
	For Each X in Aids
		X.Visible = 0
	Next
	For Each X in Suppliess
		X.Visible = 0
	Next
	For Each X in Budhas
		X.Visible = 0
	Next
	For Each X in Muhhamads
		X.Visible = 0
	Next
	For Each X in Krishnas
		X.Visible = 0
	Next
	For Each X in Josephs
		X.Visible = 0
	Next
	For Each X in Laos
		X.Visible = 0
	Next
	For Each X in Seamans
		X.Visible = 0
	Next
	For Each X in Aliens
		X.Visible = 0
	Next
	For Each X in Jesuss
		X.Visible = 0
	Next
	For Each X in Mosess
		X.Visible = 0
	Next
end sub

sub addextratime()
	if mode1TimerCount > 90 then exit sub
	mode1TimerCount = mode1TimerCount + 5
end sub

'************************* rotate Items ***************************
sub turny1Timer_timer
   Jobs001.RotY= Jobs001.RotY+ 1
   if Jobs001.RotY> 360 then
	   Jobs001.RotY= 1
end if
   Flag001.RotY= Flag001.RotY+ 1
   if Flag001.RotY> 360 then
	   Flag001.RotY= 1
end if
   Starving001.RotY= Starving001.RotY+ 1
   if Starving001.RotY> 360 then
	   Starving001.RotY= 1
end if
   Alien001.RotY= Alien001.RotY+ 1
   if Alien001.RotY> 360 then
	   Alien001.RotY= 1
end if
   Budha001.RotY= Budha001.RotY+ 1
   if Budha001.RotY> 360 then
	   Budha001.RotY= 1
end if
   Muhhamad001.RotY= Muhhamad001.RotY+ 1
   if Muhhamad001.RotY> 360 then
	   Muhhamad001.RotY= 1
end if
   Krishna001.RotY= Krishna001.RotY+ 1
   if Krishna001.RotY> 360 then
	   Krishna001.RotY= 1
end if
   Poops001.RotY= Poops001.RotY+ 1
   if Poops001.RotY> 360 then
	   Poops001.RotY= 1
end if
   Supplies001.RotY= Supplies001.RotY+ 1
   if Supplies001.RotY> 360 then
	   Supplies001.RotY= 1
end if
   Aid001.RotZ= Aid001.RotZ+ 1
   if Aid001.RotZ> 360 then
	   Aid001.RotZ= 1
end if
   Seaman001.RotZ= Seaman001.RotZ+ 1
   if Seaman001.RotZ> 360 then
	   Seaman001.RotZ= 1
end if
   Jesus001.RotZ= Jesus001.RotZ+ 1
   if Jesus001.RotZ> 360 then
	   Jesus001.RotZ= 1
end if
   Moses001.RotY= Moses001.RotY+ 1
   if Moses001.RotY> 360 then
	   Moses001.RotY= 1
end if
   Joseph001.RotZ= Joseph001.RotZ+ 1
   if Joseph001.RotZ> 360 then
	   Joseph001.RotZ= 1
end if
   Lao001.RotY= Lao001.RotY+ 1
   if Lao001.RotY> 360 then
	   Lao001.RotY= 1
end if
   Clitoris001.RotY= Clitoris001.RotY+ 1
   if Clitoris001.RotY> 360 then
	   Clitoris001.RotY= 1
end if
   dildo001.RotY= dildo001.RotY+ 1
   if dildo001.RotY> 360 then
	   dildo001.RotY= 1
end if
   dildo002.RotY= dildo002.RotY+ 1
   if dildo002.RotY> 360 then
	   dildo002.RotY= 1
end if          
end sub


sub turny2Timer_timer
   Mnote001.RotY= Mnote001.RotY+ 1
   if Mnote001.RotY> 360 then
	   Mnote001.RotY= 1
end if
end sub

sub turny3Timer_timer
   weed010.RotY= weed010.RotY+ 1
   if weed010.RotY> 360 then
	   weed010.RotY= 1
end if
   fhit1.RotY= fhit1.RotY+ 1
   if fhit1.RotY> 360 then
	   fhit1.RotY= 1
end if
   fhit2.RotY= fhit2.RotY+ 1
   if fhit2.RotY> 360 then
	   fhit2.RotY= 1
end if
   camel1.RotY= camel1.RotY+ 1
   if camel1.RotY> 360 then
	   camel1.RotY= 1
end if
   camel2.RotY= camel2.RotY+ 1
   if camel2.RotY> 360 then
	   camel2.RotY= 1
end if
   bomba.RotZ= bomba.RotZ+ 1
   if bomba.RotZ> 360 then
	   bomba.RotZ= 1
end if
   bombb.RotZ= bombb.RotZ+ 1
   if bombb.RotZ> 360 then
	   bombb.RotZ= 1
end if
   clita001.RotY= clita001.RotY+ 1
   if clita001.RotY> 360 then
	   clita001.RotY= 1
end if
   clita002.RotY= clita002.RotY+ 1
   if clita002.RotY> 360 then
	   clita002.RotY= 1
end if
end sub


'***************southpark flag**********************
sub Flagdown_Timer
If Flag.z > 100 then 
Flag.z = Flag.z -5
End If
checkflagdown
end sub

sub checkflagdown
If Flag.z = 100 Then
Flagdown.Enabled = 0
end if
end sub

sub Flagup_Timer
If Flag.z < 200 Then
Flag.z = Flag.z + 5
end if
checkflagup
end sub

sub checkflagup
If Flag.z = 200 Then
Flagup.Enabled = 0
end if
end sub

'***************turkeys**********************
sub Tturkey1up_Timer
If Turkey001.z < 160 Then
Turkey001.z = Turkey001.z + 15
end if
checkTturkey1
end sub

sub checkTturkey1
If Turkey001.z = 160 Then
Tturkey1up.Enabled = 0
Tturkey1down.Enabled = 1
end if
end sub

sub Tturkey1down_Timer
If Turkey001.z > 130 then 
Turkey001.z = Turkey001.z -15
End If
checkTturkey1down
end sub

sub checkTturkey1down
If Turkey001.z = 130 Then
Tturkey1down.Enabled = 0
end if
end sub
'****************

sub Tturkey2up_Timer
If Turkey002.z < 160 Then
Turkey002.z = Turkey002.z + 15
end if
checkTturkey2
end sub

sub checkTturkey2
If Turkey002.z = 160 Then
Tturkey2up.Enabled = 0
Tturkey2down.Enabled = 1
end if
end sub

sub Tturkey2down_Timer
If Turkey002.z > 130 then 
Turkey002.z = Turkey002.z -15
End If
checkTturkey2down
end sub

sub checkTturkey2down
If Turkey002.z = 130 Then
Tturkey2down.Enabled = 0
end if
end sub
'****************

sub Tturkey3up_Timer
If Turkey003.z < 160 Then
Turkey003.z = Turkey003.z + 15
end if
checkTturkey3
end sub

sub checkTturkey3
If Turkey003.z = 160 Then
Tturkey3up.Enabled = 0
Tturkey3down.Enabled = 1
end if
end sub

sub Tturkey3down_Timer
If Turkey003.z > 130 then 
Turkey003.z = Turkey003.z -15
End If
checkTturkey3down
end sub

sub checkTturkey3down
If Turkey003.z = 130 Then
Tturkey3down.Enabled = 0
end if
end sub

'*****************
'* Maths
'*****************
Dim Pi
Pi = Round(4 * Atn(1), 6)
Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
	if ABS(dSin) < 0.000001 Then dSin = 0
	if ABS(dSin) > 0.999999 Then dSin = 1 * sgn(dSin)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
	if ABS(dCos) < 0.000001 Then dCos = 0
	if ABS(dCos) > 0.999999 Then dCos = 1 * sgn(dCos)
End Function

'***************mr hankey dancing**********************

Dim ChickenAngle, ChickenHeight, ChickenSpeed, ChickenLean
ChickenAngle = 0
ChickenHeight = 150
ChickenSpeed = 3
ChickenLean = 30
Sub TiChickenDance_Timer()
	Mrhankey.Z = ChickenHeight'ABS(dSin(ChickenAngle) * ChickenHeight) + 10 'ChickenHeight
	Mrhankey.RotZ = dCos(ChickenAngle) * ChickenLean
	ChickenAngle = ChickenAngle + ChickenSpeed
	If ChickenAngle >=360 Then ChickenAngle = ChickenAngle - 360
End Sub

Sub TiChickenturn_Timer
   Mrhankey.ObjRotz = Mrhankey.ObjRotz + 10
   if Mrhankey.ObjRotz > 360 then
       Mrhankey.ObjRotz = 1
   end if
end sub

sub TiChickenDown_Timer
If Mrhankey.z > -100 then 
Mrhankey.z = Mrhankey.z -5
End If
checkTiChickenDown
end sub

sub checkTiChickenDown
If Mrhankey.z = -100 Then
TiChickenDown.Enabled = 0
TiChickenturn.Enabled = 0
resetmrhankey
end if
end sub

sub resetmrhankey
Mrhankey.Visible = False
Mrhankey.x = 285
Mrhankey.y = 194
Mrhankey.z = 150
Mrhankey.Rotz = 0
end sub

'***************kick the baby**********************
Sub KickbackPulse_Hit()
If li011.state = 0 Then
	KickbackPulse.enabled = 0
	kickbacktimer.enabled = False
	end If
   	KickbackPulse.enabled = 0
    'DMDUpdate.interval = 2000
    'DMDUpdate.Enabled = 1
    KickbackPulse.kick 0, 30
    LaserKickP1.TransY = 90
    vpmtimer.addtimer 800, "LaserKickP1.TransY = 0 '"
    Playsound "kickbaby3"
  DOF 115, DOFPulse
	ikeup1.enabled = true
	li011.state = 0
	kickbabysonplay = 0
	p1 = 0
'	vpmtimer.addtimer 4000, "save.state = 0 '"
End sub

Sub KickbackPulse2_Hit()
If li012.state = 0 Then
	KickbackPulse.enabled = 0
	kickbacktimer.enabled = False
end If
   	KickbackPulse2.enabled = 0
    'DMDUpdate.interval = 2000
    'DMDUpdate.Enabled = 1
    KickbackPulse2.kick 0, 30
    LaserKickP2.TransY = 90
    vpmtimer.addtimer 800, "LaserKickP2.TransY = 0 '"
    Playsound "kickbaby3"
  DOF 114, DOFPulse
	ikeup2.enabled = true
	li012.state = 0
	kickbabysonplay = 0
	p1 = 0
'	vpmtimer.addtimer 4000, "save2.state = 0 '"
End sub


Sub Tbabyturn_Timer
   ike1.Rotz = ike1.Rotz + 10
   if ike1.Rotz > 360 then
       ike1.Rotz = 1
   end if
end sub

Sub ikeup1_Timer
If ike1.Transz > -1100 Then
ike1.Transz = ike1.Transz - 20
end if
checkikeup1
end sub

sub checkikeup1
If ike1.Transz = 95 Then 
Tbabyturn.enabled = true
slingy002.visible = False
end if
If ike1.Transz = -805 Then 
changeling001.visible = False
end if
If ike1.Transz = -985 Then
playsound "kickbaby2"  
churchy.Image = "churchy2"
end if
If ike1.Transz = -1105 Then
resetikes
'Tbabyturn.enabled = False
'changeling001.visible = true
'ike1.visible = False
'ike1.Rotx = 90
'ike1.Roty = -150
'ike1.Rotz = 0
'ike1.Transz = 235
'ikeup1.Enabled = 0
end if
end sub

Sub Tbabyturn2_Timer
   ike2.Rotz = ike2.Rotz + 10
   if ike2.Rotz > 360 then
       ike2.Rotz = 1
   end if
end sub

Sub ikeup2_Timer
If ike2.Transz > -1100 Then
ike2.Transz = ike2.Transz - 20
end if
checkikeup2
end sub

sub checkikeup2
If ike2.Transz = 95 Then 
Tbabyturn2.enabled = true
slingy003.visible = False
end if
If ike2.Transz = -545 Then 
changeling001.visible = False
end if
If ike2.Transz = -725 Then
playsound "kickbaby2"  
churchy.Image = "churchy2"
end if
If ike2.Transz = -805 Then
resetikes
'Tbabyturn2.enabled = False
'changeling001.visible = true
'ike2.visible = False
'ike2.Rotx = 90
'ike2.Roty = -183
'ike2.Rotz = 0
'ike2.Transz = 235
'ikeup2.Enabled = 0
end if
end sub

sub resetikes
Tbabyturn2.enabled = False
changeling001.visible = true
ike2.visible = False
ike2.Rotx = 90
ike2.Roty = -183
ike2.Rotz = 0
ike2.Transz = 235
ikeup2.Enabled = 0
Tbabyturn.enabled = False
ike1.visible = False
ike1.Rotx = 90
ike1.Roty = -150
ike1.Rotz = 0
ike1.Transz = 235
ikeup1.Enabled = 0
slingy003.visible = False
slingy002.visible = False
end sub

Sub checkkickthebaby
If Countr34 > 2 then Countr34 = 1 : end If 
select case countr34
				case 1 : kickleftvissible
				case 2 : kickrightvissible
			end Select
End Sub

sub kickleftvissible
ike1.visible = True
ike2.visible = False
li011.state=1
li012.state=0
slingy002.visible = True
slingy003.visible = False
end sub

sub kickrightvissible
ike1.visible = False
ike2.visible = True
li011.state=0
li012.state=1
slingy002.visible = False
slingy003.visible = True
end sub


sub restthebaby
	churchy.Image = "churchy"
	li011.state = 0
	li012.state = 0
	kickbabysonplay = 0
	p1 = 0
end sub

'****************************************
' schoolbuss skillshot
'****************************************

Dim SkillshotActive
Dim SkillshotCounter
Dim craptreeY
Sub Tskillshot_hit
	Debug.print "skillshotvalue" & Skillshotactive
	If skillshotactive=1 Then
		skillshotactive=3
		' too early
		DMD "", "", "dmdsklm", eNone, eNone, eNone, 1000, True, ""
		Tskillshot.enabled = false
	Elseif skillshotactive=2 Then
		skillshotactive=3
		' award Skillshot
		DMD "", "", "dmdsklh", eNone, eNone, eNone, 1000, True, ""
		AddScore 250000
		Tskillshot.enabled = false
	Else
		' too late
		DMD "", "", "dmdsklm", eNone, eNone, eNone, 1000, True, ""
		Tskillshot.enabled = false
	End If
End Sub


Sub Tskillshot_Timer
	SkillshotCounter=SkillshotCounter+1
	If SkillshotCounter<13 Then craptree.roty=craptree.roty+10
	If SkillshotCounter<373 Then 
		schoolbus.transZ=schoolbus.transZ-2 
		craptree.y=craptree.y-2
	If Skillshotactive=2 and ( SkillshotCounter mod 5) = 1 Then schoolbus.blenddisablelighting=2 else schoolbus.blenddisablelighting=0
	Else 
		skillshotactive=3 'off
	End If
	If Skillshotactive=1 And SkillshotCounter>170 Then Skillshotactive=2 'On
	If SkillshotCounter=413 then playsound "busback"
	If SkillshotCounter>413 And SkillshotCounter<786 Then schoolbus.transZ=schoolbus.transZ+2 : craptree.y=craptree.y+2
	If SkillshotCounter>785 And SkillshotCounter<797 Then craptree.roty=craptree.roty-10
	If SkillshotCounter>796 Then SkillshotCounter=0 : Skillshotactive=0 : Tskillshot.TimerEnabled=False
End Sub


Sub StartBallControl_hit
	StartBallControl.enabled = false
	If Skillshotactive=0 Then
		Skillshotactive=1
		Tskillshot.TimerEnabled=True
		craptreeY=1829

		'startskillshot
	End If
End Sub

'****************************************
' moving imatination balloon
'****************************************
Dim ballon1Pos, ballonMove
ballonMove = Array("ballon00", "ballon01", "ballon02", "ballon03", "ballon04", "ballon05", "ballon06", "ballon07", "ballon08", "ballon09", "ballon10", "ballon11", "ballon12", "ballon13", "ballon14", "ballon15", "ballon16", "ballon17", "ballon18", "ballon19"_
, "ballon20", "ballon21", "ballon22", "ballon23", "ballon24", "ballon25", "ballon26", "ballon27", "ballon28", "ballon29", "ballon30", "ballon31", "ballon32", "ballon33", "ballon34", "ballon35", "ballon36", "ballon37", "ballon38", "ballon39"_
, "ballon40", "ballon41", "ballon42", "ballon43", "ballon44", "ballon45", "ballon46", "ballon47", "ballon48", "ballon49")

Sub Startballon
    ballon1Pos = 0
	Flasher005.Visible = true
    ballonTimer.Enabled = 1
End Sub

Sub StopBallon
    ballon1Pos = 0
	Flasher005.Visible = false
    ballonTimer.Enabled = 0
	Flasher005.ImageA = "ballon00"
end sub

Sub ballonTimer_Timer
    'debug.print ballon1pos
    Flasher005.ImageA = ballonMove(ballon1Pos)
    ballon1Pos = (ballon1Pos + 1) MOD 50
End Sub
