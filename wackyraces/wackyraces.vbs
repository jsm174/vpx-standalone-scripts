' ****************************************************************
'                       VISUAL PINBALL X
'                		Wacky races by joe picasso and remdwaas1986
'                       Version 1.0.0
'						started 26/01/2022
' ****************************************************************


'DOF Config by Outhere
'101 Left Flipper
'102 Right Flipper
'103 Left Slingshot
'104 
'105 Right Slingshot
'106 
'107 Bumper Left
'108 Bumper Center
'109 Bumper Right
'110 Ball Release
'111 AutoPlunger
'112 VUKGH Kicker
'113 VUKGH006 Kicker
'114 VUKGH007 Kicker
'115 VUKGH005 Kicker
'116 Target011_hit = Shaker
'117 VUKGH001_Hit = Shaker
'118 VUKGH004 Kicker


Option Explicit
Randomize

Const BallSize = 50    ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1
Const SongVolume = 0.1 ' 1 is full volume. Value is from 0 to 1

'----- General Sound Options -----
Const VolumeDial = 0.8				'Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Const BallRollVolume = 0.5 			'Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 1 			'Level of ramp rolling volume. Value between 0 and 1



' Load the core.vbs for supporting Subs and functions

On Error Resume Next
ExecuteGlobal GetTextFile("core.vbs")
If Err Then MsgBox "Can't open core.vbs"
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0


'----- Shadow Options -----
Const DynamicBallShadowsOn = 0		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									'2 = flasher image shadow, but it moves like ninuzzu's



' Define any Constants
Const cGameName = "wackyraces"
Const TableName = "wackyraces"
Const myVersion = "1.0.0"
Const MaxPlayers = 4     ' from 1 to 4
Const BallSaverTime = 20 ' in seconds
Const MaxMultiplier = 3  ' limit to 3x in this game, both bonus multiplier and playfield multiplier
Const BallsPerGame = 5   ' usually 3 or 5
Const MaxMultiballs = 11  ' max number of balls during multiballs

Const Special1 = 5000000  ' High score to obtain an extra ball/game
Const Special2 = 10000000
Const Special3 = 15000000

Const tnob = 11 ' total number of balls
Const lob = 0   'number of locked balls

'*****************
'ROM ID's b2s
'*****************
'p1 - ID 1
'p2 - ID 2
'p3 - ID 3
'p4 - ID 4
'p5 - ID 5
'p6 - ID 6
'p7 - ID 7
'p8 - ID 8
'p9 - ID 9
'p10 - ID 10
'p11 - ID 11
'wacky challange - ID 12
'trophee mania - ID 13
'smoke challange - ID 14
'road closure challange - ID 15
'oil challange - ID 16
'mud challange - ID 17
'gas challange - ID 18
'pidgeon challange - ID 19
'cone challange - ID 20
'burnout multiball - ID 21
'bomb multiball - ID 22
'game over - ID 23
'balllost - ID 24
'start1 - ID 25
'start2 - ID 26 
'startscreen - ID 27
'passyby - ID 28
'1e - ID 29
'2e - ID 30
'3e - ID 31
'4e - ID 32
'5e - ID 33
'6e - ID 34
'7e - ID 35
'8e - ID 36
'9e - ID 37
'10e - ID 38
'11e - ID 39

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
	Controller.B2SSetData aB2S,1
	End If
End Sub


' Use FlexDMD if in FS mode
Dim UseFlexDMD
If Table1.ShowDT = True then
'	Tbackground.enabled = 1
    UseFlexDMD = False
Else
    UseFlexDMD = True
'	Tbackground.enabled = 1
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
Dim PlayerSelectActive
Dim Ssoundz
Dim Ssoundz2
dim PFMultiplier
dim WKlights
dim WKMultiplier
dim P0slot
dim P1slot
dim P2slot
dim P3slot
dim P4slot
dim P5slot
dim P6slot
dim P7slot
dim P8slot
dim P9slot
dim P10slot
dim b00
dim b01
dim b02
dim b03
dim b04
dim b05
dim b06
dim b07
dim b08
dim b09
dim b10
dim ChallangeSlot
dim CHcompleted
dim PlayerSelectActive2
dim wackychallanges
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
dim mode1TimerCount
dim countr30
dim jackpotmulti
dim DRATMaultiplier
dim LowerFlippersActive
dim bhit
dim bvul
dim bhurt
dim Sboss01
dim Sboss02
dim Sboss03
dim Sboss04
dim Sboss05
dim Sboss06
dim Sboss07
dim Sboss08
dim Sboss09
dim Sboss10
dim Sboss11
dim Bcomplete
dim BSmode
dim Bdone
dim glock
dim mlock
dim rloop
dim lloop
dim wloop
dim wsteal
Dim raceplace

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

rbwall001.IsDropped = True
rbwall002.IsDropped = True
roablock1a.visible = false
roablock1b.visible = false
roablock2a.visible = false
roablock2b.visible = false
rbwall004.IsDropped = True
rbwall003.IsDropped = True
rbwall005.IsDropped = True
rbwall006.IsDropped = True
roablock3a.visible = false
roablock3b.visible = false
Ramp050.Image = "track"




	GiOff
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

Sub GameTimer_Timer
    'RollingUpdate 'Doing this in RDampen with 10ms timer
    ' add any other real time update subs, like gates or diverters
    FlipperLSh.Rotz = LeftFlipper.CurrentAngle
    FlipperRSh.Rotz = RightFlipper.CurrentAngle
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)
	    If Keycode = AddCreditKey Then
		'Select Case Int(rnd*3)
		'	Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
		'	Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
		'	Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
        'End Select

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
        'PlaySoundAt "fx_plungerpull", plunger
        'PlaySoundAt "fx_reload", plunger
		SoundPlungerPull
    End If
	
	If tilted then exit sub 

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

        'If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
        'If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
        'If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt
        If keycode = LeftTiltKey Then Nudge 90, 8:SoundNudgeLeft:CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 8:SoundNudgeRight:CheckTilt
        If keycode = CenterTiltKey Then Nudge 0, 9:SoundNudgeCenter:CheckTilt
		If keycode = MechanicalTilt Then SoundNudgeCenter() : CheckTilt

        If keycode = StartGameKey Then

			soundStartButton

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
				soundStartButton
                If(bFreePlay = True) Then
                    If(BallsOnPlayfield = 0) Then
                        ResetForNewGame()
						UpperFlippersActive=True
						DisableTable False
						UpdateMusicNow
                    End If
                Else
                    If(Credits> 0) Then
                        If(BallsOnPlayfield = 0) Then
                            Credits = Credits - 1
                            If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
                            ResetForNewGame()
						UpperFlippersActive=True
						DisableTable False
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

    If LowerFlippersActive Then
		If keycode = RightFlipperkey then
		    'PlaySound "fx_flipperup", 0, 1, 0, 0.25
            'RightFlipper001.RotateToEnd
			SolRFlipper1 True
		end If
			If keycode = LeftFlipperkey then
		    'PlaySound "fx_flipperup", 0, 1, 0, 0.25
            'LeftFlipper001.RotateToEnd
			SolLFlipper1 True
		end If
	elseif UpperFlippersActive Then
        If keycode = LeftFlipperKey Then
			FlipperActivate LeftFlipper, LFPress
			SolLFlipper True			
		End If
		If keycode = RightFlipperKey Then
			FlipperActivate RightFlipper, RFPress
			SolRFlipper True			
		End If
    End If

'table keys
'If keycode = RightMagnaSave or keycode = LeftMagnasave Then ShowPost 
End Sub



Sub Table1_KeyUp(ByVal keycode)

    If keycode = PlungerKey Then
        Plunger.Fire
		'PlaySound "fx_fire"
'		PlaySoundAt "fx_fire", Trigger1
		If bBallInPlungerLane = 1 Then
			SoundPlungerReleaseBall()   'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()  'Plunger release sound when there is no ball in shooter lane
		End If

        'PlaySoundAt "fx_plunger", plunger
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
			SolRFlipper1 False
    If LowerFlippersActive And Not Tilted Then
		If keycode = RightFlipperkey then
		    'PlaySound "fx_flipperup", 0, 1, 0, 0.25
            'RightFlipper001.RotatetoStart
			SolRFlipper1 False
		end If
		If keycode = LeftFlipperkey then
		    'PlaySound "fx_flipperup", 0, 1, 0, 0.25
            'LeftFlipper001.RotatetoStart
			SolLFlipper1 False
		Else
		If keycode = LeftFlipperKey Then
			FlipperDeActivate LeftFlipper, LFPress
			SolLFlipper False						'This would be called by the solenoid callbacks if using a ROM
		End If

		If keycode = RightFlipperKey Then
			FlipperDeActivate RightFlipper, RFPress
			SolRFlipper False						'This would be called by the solenoid callbacks if using a ROM
		End If
		end If
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

Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
    If Enabled Then
        'PlaySoundAt SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
        DOF 101, DOFOn
        LF.Fire' LeftFlipper.RotateToEnd
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If		

		RotateLaneLightsLeft
		RotateLaneLightsLeft2

    Else
        'PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        DOF 101, DOFOff
        LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        'PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        DOF 102, DOFOn
        RF.Fire 'rightflipper.rotatetoend

		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If

		RotateLaneLightsRight
		RotateLaneLightsRight2
    Else
        'PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        DOF 102, DOFOff
        RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
    End If
End Sub



Sub SolLFlipper1(Enabled)
    If Enabled Then
        'PlaySoundAt SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper001
        DOF 101, DOFOn
        LeftFlipper001.RotateToEnd
		If LeftFlipper001.currentangle > LeftFlipper001.endangle - ReflipAngle Then
			RandomSoundReflipUpLeft LeftFlipper001
		Else 
			SoundFlipperUpAttackLeft LeftFlipper001
			RandomSoundFlipperUpLeft LeftFlipper001
		End If
    Else
        'PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper001
        DOF 101, DOFOff
        LeftFlipper001.RotateToStart
		If LeftFlipper001.currentangle < LeftFlipper001.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper001
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
    End If
End Sub

Sub SolRFlipper1(Enabled)
If Enabled Then
        'PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper001
        DOF 102, DOFOn
        RightFlipper001.RotateToEnd
		If RightFlipper001.currentangle > RightFlipper001.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper001
		Else 
			SoundFlipperUpAttackRight RightFlipper001
			RandomSoundFlipperUpRight RightFlipper001
		End If
	Else
        'PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper001
        DOF 102, DOFOff
        RightFlipper001.RotateToStart
		If RightFlipper001.currentangle > RightFlipper001.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper001
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
    End If
End Sub


' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    'PlaySound "fx_rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
    'PlaySound "fx_rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
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
    TempState = li056.State
    li056.State = li057.State
    li057.state = TempState
End Sub

Sub RotateLaneLightsRight2
    Dim TempState
    TempState = li057.State
    li057.State = li056.State
    li056.state = TempState
End Sub

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                    'Called when table is nudged
    Tilt = Tilt + TiltSensitivity                'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt> TiltSensitivity) AND(Tilt <15) Then 'show a warning
        DMD "", "", "carefuldmd", eNone, eNone, eNone, 500, False, "v_watchout"
    End if
    If Tilt> 15 Then 'If more that 15 then TILT the table
        Tilted = True
	       'display Tilt
        DMDFlush
		playsound "youareinmypower"
        DMD "", "", "TILT", eNone, eNone,  eNone, 200, False, ""
        DisableTable True
		LowerFlippersActive=False
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


Dim UpperFlippersActive
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
		UpperFlippersActive=False
    Else
		UpperFlippersActive=True

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
        Case 1:PlaySong "2"
        Case 2:PlaySong "3"
        Case 3:PlaySong "4"
        Case 4:PlaySong "5"
        Case 5:PlaySong "M_end"
        Case 6:PlaySong "62"
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

'********************
' Play random quotes
'********************

Sub PlayQuoteB
    Dim tmp
    tmp = INT(RND * 10) + 1
    PlaySound "burn_" &tmp
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

'Sub ChangeGi(col) 'changes the gi color
'    Dim bulb
'    For each bulb in aGILights
'        SetLightColor bulb, col, -1
'    Next
'End Sub

'Sub GIUpdateTimer_Timer
'    Dim tmp, obj
'    tmp = Getballs
'    If UBound(tmp) <> OldGiState Then
'        OldGiState = Ubound(tmp)
'        If UBound(tmp) = 1 Then 'we have 2 captive balls on the table (-1 means no balls, 0 is the first ball, 1 is the second..)
'            GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
'        Else
'            Gion
'        End If
'    End If
'End Sub
Sub BlinkGI(nr,Time)
	dim x
	for x = 1 to nr
		vpmtimer.addtimer x*2*time-time, "Gioff '"
		vpmtimer.addtimer x*2*time, "Gion '"
	Next
End Sub


Sub GiOn
	ShadowGI.visible=1

    DOF 127, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
    For each bulb in aBumperLights
        bulb.State = 1
    Next
	Table1.ColorGradeImage = "ColorGradeLUT256x16_1to1"
	gilut3
	vpmtimer.addtimer 20,"gilut2 '"
	vpmtimer.addtimer 40,"gilut1 '"


End Sub


Sub gilut1 : Table1.ColorGradeImage = "ColorGradeLUT256x16_1to1" : End Sub
Sub gilut2 : Table1.ColorGradeImage = "ColorGradeLUT256x162" : End Sub
Sub gilut3 : Table1.ColorGradeImage = "ColorGradeLUT256x163" : End Sub
Sub gilut4 : Table1.ColorGradeImage = "-30" : End Sub


Sub GiOff
	ShadowGI.visible=0
    DOF 127, DOFOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
    For each bulb in aBumperLights
        bulb.State = 0
    Next
	gilut2
	vpmtimer.addtimer 20,"gilut3 '"
	vpmtimer.addtimer 40,"gilut4 '"

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


'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

'****** INSTRUCTIONS please read ******

'****** Part A:  Table Elements ******
'
' Import the "bsrtx7" and "ballshadow" images
' Import the shadow materials file (3 sets included) (you can also export the 3 sets from this table to create the same file)
' Copy in the BallShadowA flasher set and the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#
'	* with at least as many objects each as there can be balls, including locked balls
' Ensure you have a timer with a -1 interval that is always running

' Create a collection called DynamicSources that includes all light sources you want to cast ball shadows
'***These must be organized in order, so that lights that intersect on the table are adjacent in the collection***
'***If there are more than 3 lights that overlap in a playable area, exclude the less important lights***
' This is because the code will only project two shadows if they are coming from lights that are consecutive in the collection, and more than 3 will cause "jumping" between which shadows are drawn
' The easiest way to keep track of this is to start with the group on the right slingshot and move anticlockwise around the table
'	For example, if you use 6 lights: A & B on the left slingshot and C & D on the right, with E near A&B and F next to C&D, your collection would look like EBACDF
'
'G				H											^	E
'															^	B
'	A		 C												^	A
'	 B		D			your collection should look like	^	G		because E&B, B&A, etc. intersect; but B&D or E&F do not
'  E		  F												^	H
'															^	C
'															^	D
'															^	F
'		When selecting them, you'd shift+click in this order^^^^^

'****** End Part A:  Table Elements ******


'****** Part B:  Code and Functions ******

' *** Timer sub
Sub FrameTimer_Timer()
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then
		DynamicBSUpdate 'update ball shadows
	Else
		me.Enabled=False
	End If
End Sub


' *** These are usually defined elsewhere (ballrolling), but activate here if necessary
'Const tnob = 10 ' total number of balls
'Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
'Dim tablewidth: tablewidth = Table1.width
'Dim tableheight: tableheight = Table1.height

' *** User Options - Uncomment here or move to top
'----- Shadow Options -----
'Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
'Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
'									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
'									'2 = flasher image shadow, but it moves like ninuzzu's

Const fovY					= -2	'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 0.90	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source


' *** This segment goes within the RollingUpdate sub, so that if Ambient...=0 and Dynamic...=0 the entire DynamicBSUpdate sub can be skipped for max performance
'	' stop the sound of deleted balls
'	For b = UBound(BOT) + 1 to tnob
'		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
'		...rolling(b) = False
'		...StopSound("BallRoll_" & b)
'	Next
'
'...rolling and drop sounds...

'		If DropCount(b) < 5 Then
'			DropCount(b) = DropCount(b) + 1
'		End If
'
'		' "Static" Ball Shadows
'		If AmbientBallShadowOn = 0 Then
'			If BOT(b).Z > 30 Then
'				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
'			Else
'				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
'			End If
'			BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + fovY
'			BallShadowA(b).X = BOT(b).X
'			BallShadowA(b).visible = 1
'		End If

' *** Required Functions, enable these if they are not already present elswhere in your table
Function DistanceFast(x, y)
	dim ratio, ax, ay
	ax = abs(x)					'Get absolute value of each vector
	ay = abs(y)
	ratio = 1 / max(ax, ay)		'Create a ratio
	ratio = ratio * (1.29289 - (ax + ay) * ratio * 0.29289)
	if ratio > 0 then			'Quickly determine if it's worth using
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

'Dim PI: PI = 4*Atn(1)

'Function Atn2(dy, dx)
'	If dx > 0 Then
'		Atn2 = Atn(dy / dx)
'	ElseIf dx < 0 Then
'		If dy = 0 Then 
'			Atn2 = pi
'		Else
'			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
'		end if
'	ElseIf dx = 0 Then
'		if dy = 0 Then
'			Atn2 = 0
'		else
'			Atn2 = Sgn(dy) * pi / 2
'		end if
'	End If
'End Function
'

'****** End Part B:  Code and Functions ******


'****** Part C:  The Magic ******
Dim sourcenames, currentShadowCount, DSSources(30), numberofsources, numberofsources_hold
sourcenames = Array ("","","","","","","","","","","","")
currentShadowCount = Array (0,0,0,0,0,0,0,0,0,0,0,0)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!
dim objrtx1(12), objrtx2(12)
dim objBallShadow(12)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadowA5,BallShadowA6,BallShadowA7,BallShadowA8,BallShadowA9,BallShadowA10,BallShadowA11)

DynamicBSInit

sub DynamicBSInit()
	Dim iii, source

	for iii = 0 to tnob									'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = iii/1000 + 0.01
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = (iii)/1000 + 0.02
		objrtx2(iii).visible = 0

		currentShadowCount(iii) = 0

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = iii/1000 + 0.04
		objBallShadow(iii).visible = 0

		BallShadowA(iii).Opacity = 100*AmbientBSFactor
		BallShadowA(iii).visible = 0
	Next

	iii = 0

	For Each Source in DynamicSources
		DSSources(iii) = Array(Source.x, Source.y)
		iii = iii + 1
	Next
	numberofsources = iii
	numberofsources_hold = iii
end sub


Sub DynamicBSUpdate
	Dim falloff:	falloff = 150			'Max distance to light sources, can be changed if you have a reason
	Dim ShadowOpacity, ShadowOpacity2 
	Dim s, Source, LSd, currentMat, AnotherSource, BOT, iii
	BOT = GetBalls

	'Hide shadow of deleted balls
	For s = UBound(BOT) + 1 to tnob
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next

	If UBound(BOT) < lob Then Exit Sub		'No balls in play, exit

'The Magic happens now
	For s = lob to UBound(BOT)

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

		If AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
			If BOT(s).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If BOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = BOT(s).Y + BallSize/10 + fovY
				objBallShadow(s).visible = 1

				BallShadowA(s).X = BOT(s).X
				BallShadowA(s).Y = BOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=BOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif BOT(s).Z <= 30 And BOT(s).Z > 20 Then	'On pf, primitive only
				objBallShadow(s).visible = 1
				If BOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = BOT(s).Y + fovY
				BallShadowA(s).visible = 0
			Else											'Under pf, no shadows
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 0
			end if

		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
			If BOT(s).Z > 30 Then							'In a ramp
				BallShadowA(s).X = BOT(s).X
				BallShadowA(s).Y = BOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=BOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif BOT(s).Z <= 30 And BOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				If BOT(s).X < tablewidth/2 Then
					BallShadowA(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					BallShadowA(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				BallShadowA(s).Y = BOT(s).Y + Ballsize/10 + fovY
				BallShadowA(s).height=BOT(s).z - BallSize/2 + 5
			Else											'Under pf
				BallShadowA(s).visible = 0
			End If
		End If

' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If BOT(s).Z < 30 Then 'And BOT(s).Y < (TableHeight - 200) Then 'Or BOT(s).Z > 105 Then		'Defining when and where (on the table) you can have dynamic shadows
				For iii = 0 to numberofsources - 1 
					LSd=DistanceFast((BOT(s).x-DSSources(iii)(0)),(BOT(s).y-DSSources(iii)(1)))	'Calculating the Linear distance to the Source
					If LSd < falloff Then' And gilvl > 0 Then						    'If the ball is within the falloff range of a light and light is on (we will set numberofsources to 0 when GI is off)
						currentShadowCount(s) = currentShadowCount(s) + 1		'Within range of 1 or 2
						if currentShadowCount(s) = 1 Then						'1 dynamic shadow source
							sourcenames(s) = iii
							currentMat = objrtx1(s).material
							objrtx2(s).visible = 0 : objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
	'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01						'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity = (falloff-LSd)/falloff									'Sets opacity/darkness of shadow by distance to light
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness						'Scales shape of shadow with distance/opacity
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^2,RGB(0,0,0),0,0,False,True,0,0,0,0
							If AmbientBallShadowOn = 1 Then
								currentMat = objBallShadow(s).material									'Brightens the ambient primitive when it's close to a light
								UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-ShadowOpacity),RGB(0,0,0),0,0,False,True,0,0,0,0
							Else
								BallShadowA(s).Opacity = 100*AmbientBSFactor*(1-ShadowOpacity)
							End If

						Elseif currentShadowCount(s) = 2 Then
																	'Same logic as 1 shadow, but twice
							currentMat = objrtx1(s).material
							AnotherSource = sourcenames(s)
							objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
	'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01							'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(DSSources(AnotherSource)(0),DSSources(AnotherSource)(1), BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity = (falloff-DistanceFast((BOT(s).x-DSSources(AnotherSource)(0)),(BOT(s).y-DSSources(AnotherSource)(1))))/falloff
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0

							currentMat = objrtx2(s).material
							objrtx2(s).visible = 1 : objrtx2(s).X = BOT(s).X : objrtx2(s).Y = BOT(s).Y + fovY
	'						objrtx2(s).Z = BOT(s).Z - 25 + s/1000 + 0.02							'Uncomment if you want to add shadows to an upper/lower pf
							objrtx2(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity2 = (falloff-LSd)/falloff
							objrtx2(s).size_y = Wideness*ShadowOpacity2+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
							If AmbientBallShadowOn = 1 Then
								currentMat = objBallShadow(s).material									'Brightens the ambient primitive when it's close to a light
								UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
							Else
								BallShadowA(s).Opacity = 100*AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2))
							End If
						end if
					Else
						currentShadowCount(s) = 0
						BallShadowA(s).Opacity = 100*AmbientBSFactor
					End If
				Next
			Else									'Hide dynamic shadows everywhere else
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
			End If
		End If
	Next
End Sub
'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************


' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'

Dim SlotAward1, SelectCounter1, SlotValue1, chooseplayert

Sub ResetForNewGame()
bossesaway
resetjackpotNewGame
challangenewgame
raceplace = 11
'	StopAttractMode
	StopSong
'	PlaySong "6"
'	playsong "6"
	Tcarturny.enabled = 1
    UpdateMusic = 0
    UpdateMusic = UpdateMusic + 6
	UpdateMusicNow
    SlotValue1 = 0
    SelectCounter1 = 0
	LowerFlippersActive = False
    SlotAward1 = Array("00dmd", "01dmd", "02dmd", "03dmd", "04dmd", "05dmd", "06dmd", "07dmd", "08dmd", "09dmd", "10dmd")
	StopAttractMode
	vpmtimer.addtimer 100, "delaychoosepicture '"
	startB2S(40)
    PlayerSelectActive = True
	TurnOffPlayfieldLights()
end Sub

sub delaychoosepicture
    DMDFlush
    DMD "", "", "choosedmd", eNone, eNone, eNone, 10000, False, "v_whichcarareyou"
end sub

Sub SelectPlayerStart(Keycode)
    DMD "", "", SlotAward1(SelectCounter1), eNone, eNone, eNone, 500, False, ""
    If keycode = LeftFlipperKey then
		Playsound "select"
        SelectCounter1 = SelectCounter1 - 1
        If SelectCounter1 = -1 Then SelectCounter1 = 10
    end If
    If keycode = RightFlipperKey then
		Playsound "select"
        SelectCounter1 = SelectCounter1 + 1
        If SelectCounter1 = 11 Then SelectCounter1 = 0
    end If
    Select Case SelectCounter1
        Case 0
            DMDFlush
            DMD "", "", "00dmd", eNone, eNone, eNone, 200000, False, ""
			optionc00
			startB2S(11)
            SlotValue1 = 0			
        Case 1
            DMDFlush
            DMD "", "", "01dmd", eNone, eNone, eNone, 200000, False, ""
			optionc01
			startB2S(1)
            SlotValue1 = 1
        Case 2
            DMDFlush
            DMD "", "", "02dmd", eNone, eNone, eNone, 200000, False, ""
			optionc02
			startB2S(2)
            SlotValue1 = 2
        Case 3
            DMDFlush
            DMD "", "", "03dmd", eNone, eNone, eNone, 200000, False, ""
			optionc03
			startB2S(3)
            SlotValue1 = 3
		Case 4
		    DMDFlush
            DMD "", "", "04dmd", eNone, eNone, eNone, 200000, False, ""
			optionc04
			startB2S(4)
           SlotValue1 = 4
		Case 5
		    DMDFlush
            DMD "", "", "05dmd", eNone, eNone, eNone, 200000, False, ""
			optionc05
			startB2S(5)
            SlotValue1 = 5
		Case 6
		    DMDFlush
            DMD "", "", "06dmd", eNone, eNone, eNone, 200000, False, ""
			optionc06
			startB2S(6)
            SlotValue1 = 6
		Case 7
		    DMDFlush
            DMD "", "", "07dmd", eNone, eNone, eNone, 200000, False, ""
			optionc07
			startB2S(7)
            SlotValue1 = 7
		Case 8
		    DMDFlush
            DMD "", "", "08dmd", eNone, eNone, eNone, 200000, False, ""
			optionc08
			startB2S(8)
            SlotValue1 = 8
		Case 9
		    DMDFlush
            DMD "", "", "09dmd", eNone, eNone, eNone, 200000, False, ""
			optionc09
			startB2S(9)
            SlotValue1 = 9
		Case 10
		    DMDFlush
            DMD "", "", "10dmd", eNone, eNone, eNone, 200000, False, ""
			optionc10
			startB2S(10)
            SlotValue1 = 10
    End Select

    If keycode = PlungerKey Then
        Select Case SlotValue1
            Case 0
            'DMDFlush
				Playsound "v_chr_00"
				chooseplayert = 0
				c000.visible = true
				Bcomplete = 1
				li011.state = 1
				PlayerSelectActive = False
                vpmtimer.addtimer 1000, "ResetForNewGame2() '"
            Case 1
            'DMDFlush
				Playsound "v_chr_1"
				chooseplayert = 1
				c001.visible = true
				Bcomplete = 1
				li001.state = 1
				PlayerSelectActive = False
                vpmtimer.addtimer 1000, "ResetForNewGame2() '"
            Case 2
            'DMDFlush
				Playsound "v_chr_2"
				chooseplayert = 2
				c002.visible = true
				Bcomplete = 1
				li002.state = 1
				PlayerSelectActive = False 
                vpmtimer.addtimer 1000, "ResetForNewGame2() '"
            Case 3
            'DMDFlush
				Playsound "v_chr_3"
				chooseplayert = 3
				c003.visible = true
				Bcomplete = 1
				li003.state = 1
				PlayerSelectActive = False
                vpmtimer.addtimer 1000, "ResetForNewGame2() '"
			Case 4
            'DMDFlush
				Playsound "v_chr_4"
				chooseplayert = 4
				c004.visible = true
				Bcomplete = 1
				li004.state = 1
				PlayerSelectActive = False
                vpmtimer.addtimer 1000, "ResetForNewGame2() '"
            Case 5
            'DMDFlush
				Playsound "v_chr_5"
				chooseplayert = 5
				c005.visible = true
				Bcomplete = 1
				li005.state = 1
				PlayerSelectActive = False
                vpmtimer.addtimer 1000, "ResetForNewGame2() '"
            Case 6
            'DMDFlush
				Playsound "v_chr_6"
				chooseplayert = 6
				c006.visible = true
				Bcomplete = 1
				li006.state = 1
				PlayerSelectActive = False
                vpmtimer.addtimer 1000, "ResetForNewGame2() '"
            Case 7
            'DMDFlush
				Playsound "v_chr_7"
				chooseplayert = 7
				c007.visible = true
				Bcomplete = 1
				li007.state = 1
				PlayerSelectActive = False
                vpmtimer.addtimer 1000, "ResetForNewGame2() '"
            Case 8
            'DMDFlush
				Playsound "v_chr_8"
				chooseplayert = 8
				c008.visible = true
				Bcomplete = 1
				li008.state = 1
				PlayerSelectActive = False
                vpmtimer.addtimer 1000, "ResetForNewGame2() '"
            Case 9
            'DMDFlush
				Playsound "v_chr_9"
				chooseplayert = 9
				c009.visible = true
				Bcomplete = 1
				li009.state = 1
				PlayerSelectActive = False
                vpmtimer.addtimer 1000, "ResetForNewGame2() '"
            Case 10
            'DMDFlush
				Playsound "v_chr_10"
				chooseplayert = 10
				c010.visible = true
				Bcomplete = 1
				li010.state = 1
				PlayerSelectActive = False
                vpmtimer.addtimer 1000, "ResetForNewGame2() '"
        End Select
    end if
end sub

sub optionc00
c00.visible = True
c01.visible = False
c02.visible = False
c03.visible = False
c04.visible = False
c05.visible = False
c06.visible = False
c07.visible = False
c08.visible = False
c09.visible = False
c10.visible = False
end sub

sub optionc01
c00.visible = False
c01.visible = true
c02.visible = False
c03.visible = False
c04.visible = False
c05.visible = False
c06.visible = False
c07.visible = False
c08.visible = False
c09.visible = False
c10.visible = False
end sub

sub optionc02
c00.visible = False
c01.visible = False
c02.visible = True
c03.visible = False
c04.visible = False
c05.visible = False
c06.visible = False
c07.visible = False
c08.visible = False
c09.visible = False
c10.visible = False
end sub

sub optionc03
c00.visible = False
c01.visible = False
c02.visible = False
c03.visible = True
c04.visible = False
c05.visible = False
c06.visible = False
c07.visible = False
c08.visible = False
c09.visible = False
c10.visible = False
end sub

sub optionc04
c00.visible = False
c01.visible = False
c02.visible = False
c03.visible = False
c04.visible = True
c05.visible = False
c06.visible = False
c07.visible = False
c08.visible = False
c09.visible = False
c10.visible = False
end sub

sub optionc05
c00.visible = False
c01.visible = False
c02.visible = False
c03.visible = False
c04.visible = False
c05.visible = True
c06.visible = False
c07.visible = False
c08.visible = False
c09.visible = False
c10.visible = False
end sub

sub optionc06
c00.visible = False
c01.visible = False
c02.visible = False
c03.visible = False
c04.visible = False
c05.visible = False
c06.visible = True
c07.visible = False
c08.visible = False
c09.visible = False
c10.visible = False
end sub

sub optionc07
c00.visible = False
c01.visible = False
c02.visible = False
c03.visible = False
c04.visible = False
c05.visible = False
c06.visible = False
c07.visible = True
c08.visible = False
c09.visible = False
c10.visible = False
end sub

sub optionc08
c00.visible = False
c01.visible = False
c02.visible = False
c03.visible = False
c04.visible = False
c05.visible = False
c06.visible = False
c07.visible = False
c08.visible = True
c09.visible = False
c10.visible = False
end sub

sub optionc09
c00.visible = False
c01.visible = False
c02.visible = False
c03.visible = False
c04.visible = False
c05.visible = False
c06.visible = False
c07.visible = False
c08.visible = False
c09.visible = True
c10.visible = False
end sub

sub optionc10
c00.visible = False
c01.visible = False
c02.visible = False
c03.visible = False
c04.visible = False
c05.visible = False
c06.visible = False
c07.visible = False
c08.visible = False
c09.visible = False
c10.visible = true

end sub

sub optionc11

c00.visible = False
c01.visible = False
c02.visible = False
c03.visible = False
c04.visible = False
c05.visible = False
c06.visible = False
c07.visible = False
c08.visible = False
c09.visible = False
c10.visible = False
end sub

sub optionc12
c010.visible = False
c009.visible = False
c008.visible = False
c007.visible = False
c006.visible = False
c005.visible = False
c004.visible = False
c003.visible = False
c002.visible = False
c001.visible = False
c000.visible = False
end sub

Sub ResetForNewGame2()
    Dim i

    bGameInPLay = True

	Tcarturny.enabled = 0
	optionc11

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
	bumperHits = 100
	Ssoundz = 0
	Ssoundz2 = 0
	PFMultiplier = 1
	WKMultiplier = 0
	WKlights = 0
	P0slot = 0
	P1slot = 0
	P2slot = 0
	P3slot = 0
	P4slot = 0
	P5slot = 0
	P6slot = 0
	P7slot = 0
	P8slot = 0
	P9slot = 0
	P10slot = 0
	b00 = 0
	b01 = 0
	b02 = 0
	b03 = 0
	b04 = 0
	b05 = 0
	b06 = 0
	b07 = 0
	b08 = 0
	b09 = 0
	b10 = 0
	ChallangeSlot = 0
	CHcompleted  = 0
	wackychallanges = 1
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
	countr30 = 0
	jackpotmulti = 1
	DRATMaultiplier = 1
	bhit = 0
	bvul = 0
	bhurt = 0
	Sboss01 = 0
	Sboss02 = 0
	Sboss03 = 0
	Sboss04 = 0
	Sboss05 = 0
	Sboss06 = 0
	Sboss07 = 0
	Sboss08 = 0
	Sboss09 = 0
	Sboss10 = 0
	Sboss11 = 0
	'Bcomplete = 0
	BSmode = 0
	Bdone = 0
	rloop = 0
	lloop = 0
	wloop = 0
	glock = 0
	mlock = 0
	wsteal = 0
	burny=0
	minecart.visible = True
	doory.visible = True
	Wall040.Isdropped = False
	Wall039.Isdropped = False
    UpdateMusic = 0
stopsong
    ' initialise Game variables
    Game_Init()
	
    ' you may wish to start some music, play a sound, do whatever at this point
'StopSong
PlaySound ""


    vpmtimer.addtimer 100, "FirstBall '"
End Sub

' This is used to delay the start of a game to allow any attract sequence to

' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
    ' reset the table for a new ball
	'UpdateMusic = 0
flashplaat2.ImageA="fsb"
DMD "", "", "on_your_marks", eNone, eNone, eNone, 5500, True, ""
countr38 = 0
PlaySound "fuelintro2"
vpmtimer.addtimer 1100, "introfuelT.enabled=true '"
vpmtimer.addtimer 5500, "ResetForNewPlayerBall() '"
	

    ' create a new ball in the shooters lane
    vpmtimer.addtimer 5500, "UpdateMusicNow '"
    vpmtimer.addtimer 9500, "CreateNewBall() '"

End Sub


' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()
	GiOff

    ' make sure the correct display is upto date
    AddScore 0
	startB2S(25)
	if UpdateMusic = 0 then
	Flasher002.ImageA="n18"
	end if
	if UpdateMusic = 1 then
	Flasher002.ImageA="n15"
	end if
	if UpdateMusic = 2 then
	Flasher002.ImageA="n12"
	end if
	if UpdateMusic = 3 then
	Flasher002.ImageA="n9"
	end if
	if UpdateMusic = 4 then
	Flasher002.ImageA="n7"
	end if

	flashplaat2.ImageA="gr"
	'enable traficlight
	countr37=0
	countr40=0
	trafficT.enabled=true
	speedT002.enabled = 1
    ' set the current players bonus multiplier back down to 1X
    BonusMultiplier(CurrentPlayer) = 1
    'UpdateBonusXLights
	
' reset any drop targets, lights, game Mode etc..
    
   'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'Reset any table specific
	BumperBonus = 0
	rloop = 0
	lloop = 0
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
    'PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease

    RandomSoundBallRelease BallRelease
	BallRelease.Kick 90, 4
 DOF 110, DOFPulse
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


	wsteal = 1
	startB2S(24)
	LightSeqTrophy.Play SeqRandom,30,,2000
	mutleylaugh
	flashplaat2.ImageA="Nlost"
	Dim BonusDelayTime
	' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)

	'LightSeqAttract.Play SeqBlinking, , 5, 150

Stopmode2
StopSong
'bonuscheckie


	if UpdateMusic = 0 then
	playsound "balllost_1"
	end if
	if UpdateMusic = 1 then
	playsound "balllost_2"
	end if
	if UpdateMusic = 2 then
	playsound "balllost_3"
	end if
	if UpdateMusic = 3 then
	playsound "balllost_4"
	end if
	if UpdateMusic = 4 then
	playsound "balllost_5"
	end if

	vpmtimer.addtimer 1300, "endofballcontinue '"	
end sub

sub endofballcontinue


    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 0

    'If NOT Tilted Then
	If(Tilted = False) Then
		
        'Number of Target hits
'       AwardPoints = TargetBonus * 2000
'       TotalBonus = TotalBonus + AwardPoints
'       DMD CL(0, FormatScore(AwardPoints)), CL(1, "TARGET BONUS " & TargetBonus), "", eBlink, eNone, eNone, 300, False, "whip" <- with dmd scores otherwise only total bonus

        AwardPoints = TargetBonus * 10000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = rloop * 25000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = lloop * 25000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = BumperBonus * 100000
        TotalBonus = TotalBonus + AwardPoints
        
		DMD CL(0, FormatScore(TotalBonus) ), CL(1, "TOTAL BONUS" & BonusMultiplier(CurrentPlayer) ), "", eBlinkFast, eNone, eNone, 1000, True, ""
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)
        
		AddScore TotalBonus

		' add a bit of a delay to allow for the bonus points to be shown & added up
		vpmtimer.addtimer 1000, "EndOfBall2 '"
    Else 'Si hay falta simplemente espera un momento y va directo a la segunta parte después de perder la bola
'		BonusDelayTime = 100  took this one out as it errors
		EndOfBall2
    End If
	'vpmtimer.addtimer BonusDelayTime, "EndOfBall2 '"
End Sub

sub mutleylaugh
DMD "", "", "mutlie005", eNone, eNone, eNone, 100, True, ""
DMD "", "", "mutlie006", eNone, eNone, eNone, 100, True, ""
DMD "", "", "mutlie007", eNone, eNone, eNone, 100, True, ""
DMD "", "", "mutlie009", eNone, eNone, eNone, 100, True, ""
DMD "", "", "mutlie011", eNone, eNone, eNone, 100, True, ""
DMD "", "", "mutlie013", eNone, eNone, eNone, 100, True, ""
DMD "", "", "mutlie014", eNone, eNone, eNone, 100, True, ""
DMD "", "", "mutlie011", eNone, eNone, eNone, 100, True, ""
DMD "", "", "mutlie013", eNone, eNone, eNone, 100, True, ""
DMD "", "", "mutlie014", eNone, eNone, eNone, 100, True, ""
DMD "", "", "mutlie011", eNone, eNone, eNone, 100, True, ""
DMD "", "", "mutlie013", eNone, eNone, eNone, 100, True, ""
DMD "", "", "mutlie014", eNone, eNone, eNone, 100, True, ""
end sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the CurrentPlayer)
'
Sub EndOfBall2()
    ' if were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful if we are changing player LOL
    UpdateMusic = UpdateMusic + 1
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
		DMD "", "", "EBDMD", eNone, eNone, eNone, 1000, True, ""
        'DMD CL(0, "EXTRA BALL"), CL(1, "SHOOT AGAIN"), "", eNone, eNone, eBlink, 1000, True, "vo_extraball"

		UpdateMusic = UpdateMusic - 1
		UpdateMusicNow

        ' reset the playfield for the new ball
        ResetForNewPlayerBall()
		
		' set the dropped wall for bonus

		
        ' Create a new ball in the shooters lane
		vpmtimer.addtimer 4000, "CreateNewBall() '"
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
		if sboss10 = 1 then
		sawmovedown
		end if
		tchecksteal.enabled = false
		Tstealmultie.enabled = false
		startB2S(23)
		StopSong
		'DMD CL(0, "GAME OVER") "", eNone, 13000, True, ""
Playsound "gameover"
DMD CL(0, "WACKY"), CL(1, "LOOPS " &wloop), "", eNone, eNone, eNone, 900, True, ""
DMD CL(0, "WACKY"), CL(1, "CHALANGES " &CHcompleted), "", eNone, eNone, eNone, 900, True, ""
DMD CL(0, "WACKY"), CL(1, "BOSSES " &Bdone), "", eNone, eNone, eNone, 900, True, ""
DMD CL(0, "FINISHED"), CL(1, "PLACE " &raceplace), "", eNone, eNone, eNone, 1500, True, ""
DMD "", "", "gm00", eNone, eNone, eNone, 100, True, ""
DMD "", "", "gm01", eNone, eNone, eNone, 100, True, ""
DMD "", "", "gm02", eNone, eNone, eNone, 100, True, ""
DMD "", "", "gm03", eNone, eNone, eNone, 100, True, ""
DMD "", "", "gm04", eNone, eNone, eNone, 100, True, ""
DMD "", "", "gm05", eNone, eNone, eNone, 100, True, ""
DMD "", "", "gm06", eNone, eNone, eNone, 100, True, ""
DMD "", "", "gm07", eNone, eNone, eNone, 100, True, ""
DMD "", "", "gm08", eNone, eNone, eNone, 100, True, ""
DMD "", "", "gm00", eNone, eNone, eNone, 100, True, ""
DMD "", "", "gm01", eNone, eNone, eNone, 100, True, ""
DMD "", "", "gm02", eNone, eNone, eNone, 100, True, ""
DMD "", "", "gm03", eNone, eNone, eNone, 100, True, ""
DMD "", "", "gm04", eNone, eNone, eNone, 100, True, ""
DMD "", "", "gm05", eNone, eNone, eNone, 100, True, ""
DMD "", "", "gm06", eNone, eNone, eNone, 100, True, ""
DMD "", "", "gm07", eNone, eNone, eNone, 100, True, ""
DMD "", "", "gm08", eNone, eNone, eNone, 100, True, ""
'DMD "", "", "gameoverdmd", eNone, eNone, eNone, 2710, True, ""


        ' set the machine into game over mode
        vpmtimer.addtimer 6050, "EndOfGame() '"

    ' you may wish to put a Game Over message on the desktop/backglass

    Else
        ' set the next player
        CurrentPlayer = NextPlayer

        ' make sure the correct display is up to date
        DMDScoreNow

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
		vpmtimer.addtimer 4000, "CreateNewBall() '"

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
GiOff
    LightSeqAttract.StopPlay
	'debug.print "End Of Game"
    bGameInPLay = False
	UpperFlippersActive=False
	LowerFlippersActive=False
    ' just ended your game then play the end of game tune
    If NOT bJustStarted Then
        ChangeSong
    End If

    bJustStarted = False
    ' ensure that the flippers are down
	DisableTable True
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
    ' Destroy the ball
    Drain.DestroyBall
    BallsOnPlayfield = BallsOnPlayfield - 1 

	debug.print BallsOnPlayfield
	If BallsOnPlayfield<2 Then
		bMultiBallMode = False
	end if
	
    ' pretend to knock the ball into the ball storage mech
    'PlaySoundAt "fx_drain", Drain
	RandomSoundDrain Drain
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
flashplaat2.ImageA="Nstart"
startB2S(26)
If bAutoPlunger Then
        'debug.print "autofire the ball"
        PlungerIM.AutoFire
        DOF 111, DOFPulse
        PlaySoundAt "fx_fire", Trigger1
        bAutoPlunger = False
    End If	
'StopSong
    'DMDScoreNow
    bBallInPlungerLane = True
    'DMD "_", CL(1, "SHOOT THE BALL"), "", eNone, eBlink, eNone, 1000, True, ""
    If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
        EnableBallSaver BallSaverTime
        Else
        ' show the message to shoot the ball in case the player has fallen sleep
        Trigger1.TimerEnabled = 1
    End If
End Sub

' The ball is released from the plunger

Sub Trigger1_UnHit()
	startB2S(27)
	DMDScoreNow
    bBallInPlungerLane = False
    'LightEffect 4
	'ChangeSong
End Sub


Sub Trigger1_Timer
'    DMD "_", CL(1, "SHOOT THE BALL"), "", eNone, eNone, eNone, 800, True, ""
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
	'PlaySound "tone"&points

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
	Playsound "Sextraball"
	DMD "", "", "EBDMD", eNone, eNone, eNone, 1000, True, ""
    'DMD "_", CL(1, ("EXTRA BALL WON") ), "", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
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
        'PlaySound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
		KnockerSolenoid ' knocker sound
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
    ChangeSong
	PlaySound "WIN"
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
        playsound "select"
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0) then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = RightFlipperKey Then
        playsound "select"
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

	DisableTable True

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



Sub FlasherSol1(level)
	If Enabled Then
		Objlevel(1) = 1 : FlasherFlash1_Timer
	End If
End Sub

Sub FlasherSol2(level)
	If Enabled Then
		Objlevel(1) = 1 : FlasherFlash2_Timer
	End If
End Sub

Sub FlasherSol3(level)
	If Enabled Then
		Objlevel(1) = 1 : FlasherFlash3_Timer
	End If
End Sub

Sub FlasherSol4(level)
	If Enabled Then
		Objlevel(1) = 1 : FlasherFlash4_Timer
	End If
End Sub

Sub FlasherSol5(level)
	If Enabled Then
		Objlevel(1) = 1 : FlasherFlash5_Timer
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
InitFlasher 1, "purple"
InitFlasher 2, "purple"
InitFlasher 3, "red"
InitFlasher 4, "yellow"
InitFlasher 5, "blue"

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
'Sub FlasherFlash6_Timer() : FlashFlasher(6) : End Sub 
'Sub FlasherFlash7_Timer() : FlashFlasher(7) : End Sub
'Sub FlasherFlash8_Timer() : FlashFlasher(8) : End Sub
'Sub FlasherFlash9_Timer() : FlashFlasher(9) : End Sub
'Sub FlasherFlash10_Timer() : FlashFlasher(10) : End Sub
'Sub FlasherFlash11_Timer() : FlashFlasher(11) : End Sub

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


 Sub Flash1(Enabled)
	If Enabled Then
		Objlevel(1) = 1 : FlasherFlash1_Timer
	End If
 End Sub


 Sub Flash2(Enabled)
	If Enabled Then
		Objlevel(2) = 1 : FlasherFlash2_Timer
	End If
 End Sub


 Sub Flash3(Enabled)
	If Enabled Then
		Objlevel(3) = 1 : FlasherFlash3_Timer
	End If
 End Sub


 Sub Flash4(Enabled)
	If Enabled Then
		Objlevel(4) = 1 : FlasherFlash4_Timer
	End If
 End Sub


 Sub Flash5(Enabled)
	If Enabled Then
		Objlevel(5) = 1 : FlasherFlash5_Timer
	End If
 End Sub

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
	DMD "", "", "creditsdmd", eNone, eNone, eNone, 3000, True, ""
	DMD "", "", "presentsdmd", eNone, eNone, eNone, 1000, True, ""
DMD "", "", "intro01", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro02", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro03", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro04", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro05", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro06", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro07", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro08", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro13", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro21", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro25", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro29", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro30", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro31", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro32", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro33", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro35", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro37", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro39", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro41", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro43", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro45", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro47", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro49", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro51", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro53", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro55", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro57", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro58", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro59", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro60", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro61", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro62", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro63", eNone, eNone, eNone, 100, True, ""
DMD "", "", "intro64", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "titledmd", eNone, eNone, eNone, 2000, True, "v_wackyraces"
    DMD CL(0, "HIGHSCORES"), Space(dCharsPerLine(1) ), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL(0, "HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL(0, "HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(dCharsPerLine(0) ), Space(dCharsPerLine(1) ), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

Sub StartAttractMode
	
	startB2S(27)
	tonny.Roty = 110
	Tturnton.enabled = True
	sawT002.enabled = True
	Tnavattract.enabled = 1
	Flasher003.ImageA="n0"
	Flasher002.ImageA="n7"
	optionc12
    ChangeSong
    StartLightSeq
    DMDFlush
    ShowTableInfo
End Sub

Sub StopAttractMode
    LightSeqAttract.StopPlay
    DMDScoreNow
	countr35=0
	countr36=0
	speedT.enabled=true
	Tnavattract.enabled = 0
	flashplaat2.ImageA="carselect"
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
Dim TargetBonus, BumperBonus  

Sub Game_Init() 'called at the start of a new game
    Dim i, j
    'ChangeSong
	TargetBonus = 0
	'bumperHits = 100
	BallInHole = 0
	BallInHole1 = 0
	BallInHole8 = 0
    'TurnOffPlayfieldLights()
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
'	gi1.state = 1
'	gi2.state = 1
'	gi3.state = 1
'	gi4.state = 1
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
	PlaySound SoundFXDOF("sling2", 105, DOFPulse,DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    
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
        Case 1:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10:gi1.State = 0:Gi2.State = 0
		Case 1:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 5:gi1.State = 0:Gi2.State = 0
        Case 2:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:gi1.State = 1:Gi2.State = 1:RightSlingShot.TimerEnabled = False
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    PlaySound SoundFXDOF("sling1", 103, DOFPulse,DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
	
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
        Case 1:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10:gi3.State = 0:Gi4.State = 0
        Case 2:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 5:gi3.State = 0:Gi4.State = 0
        Case 3:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:gi3.State = 1:Gi4.State = 1:LeftSlingShot.TimerEnabled = False
    End Select
    LStep = LStep + 1
End Sub

'*****************
'triggers
'*****************

'**************** mine/gas locked ************************

sub Wall039_hit()
DMD "", "", "gaslock", eNone, eNone, eNone, 1000, True, "doorhit"
end sub

sub Gaslocked_hit()
WireRampOff
Debug.print "Gaslocked_hit OFF"

lloop = lloop + 1
wloop = wloop + 1
if mmbb=1 Then
DMD "", "", "1Mdmd", eNone, eNone, eNone, 1000, True, ""
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
end if
if glock = 3 Then
exit Sub
end if
if glock = 3 then exit sub
glock = glock + 1
checkgasdoor
end sub

sub checkgasdoor
if glock = 1 Then
DMD "", "", "gashit2", eNone, eNone, eNone, 1000, True, ""
exit sub
end if
if glock = 2 Then
DMD "", "", "gashit1", eNone, eNone, eNone, 1000, True, ""
exit sub
end if
if glock = 3 Then
Wall039.Isdropped=true
doory.visible=false
end if
end sub

sub Wall040_hit()
DMD "", "", "minelocked", eNone, eNone, eNone, 1000, True, "minehit"
end sub

sub minelocked_hit()

WireRampOff
Debug.print "minelocked_hit OFF"

rloop = rloop + 1
wloop = wloop + 1
if mmbb=1 Then
DMD "", "", "1Mdmd", eNone, eNone, eNone, 1000, True, ""
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
end if
if mlock = 3 Then
exit Sub
end if
mlock = mlock + 1
checkminecart
end sub

sub checkminecart
if mlock = 1 Then
DMD "", "", "lock1hit", eNone, eNone, eNone, 1000, True, ""
exit sub
end if
if mlock = 2 Then
DMD "", "", "lock2hit", eNone, eNone, eNone, 1000, True, ""
exit sub
end if
if mlock = 3 Then
Wall040.Isdropped=true
minecart.visible=false
end if
end sub

'**************** uplanes ************************
sub Trigger002_hit
DMD "", "", "1kdmd", eNone, eNone, eNone, 750, True, "triggerup"
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
li056.state=1
checkuplights
end sub

sub Trigger003_hit
FlashForMs Flasher005, 1000, 50, 0
dickieshaker
DMD "", "", "1kdmd", eNone, eNone, eNone, 750, True, "triggerup"
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
li057.state=1
checkuplights
end sub

sub checkuplights
if li056.state=1 and li057.state=1 Then
DMD "", "", "godmd2", eNone, eNone, eNone, 1000, True, "v_go"
Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
li056.state=0
li057.state=0
end if
end sub

'**********************inner/outerlane*********************

Sub TLeftInlane_Hit
	Flash1 True
	Score(CurrentPlayer) = Score(CurrentPlayer) + (25000*PFMultiplier)
		If activeball.vely < 0 then
			PlaySound "blaneup"
			else
			PlaySound "blanedw"
		end if
if LeftInlane.State = 1 Then
DMD "", "", "25kdmd", eNone, eNone, eNone, 500, True, ""
CheckDrat
exit Sub
end if
LeftInlane.State = 1
DMD "", "", "rdmd", eNone, eNone, eNone, 500, True, ""
CheckDrat
End Sub

Sub TLeftOutlane_Hit
	Flash1 True
	Score(CurrentPlayer) = Score(CurrentPlayer) + (50000*PFMultiplier)
		If activeball.vely < 0 then
			PlaySound "blaneup"
			else
			PlaySound "blanedw"
		end if
if LeftOutlane.State = 1 Then
DMD "", "", "50kdmd", eNone, eNone, eNone, 500, True, ""
CheckDrat
exit Sub
end if
LeftOutlane.State = 1
DMD "", "", "ddmd", eNone, eNone, eNone, 500, True, ""
CheckDrat
End Sub

Sub TRightInlane_Hit
	Flash2 True
	Score(CurrentPlayer) = Score(CurrentPlayer) + (25000*PFMultiplier)
		If activeball.vely < 0 then
			PlaySound "blaneup"
			else
			PlaySound "blanedw"
		end if
if RightInlane.State = 1 Then
DMD "", "", "25kdmd", eNone, eNone, eNone, 500, True, ""
CheckDrat
exit Sub
end if
RightInlane.State = 1
DMD "", "", "Admd", eNone, eNone, eNone, 500, True, ""
CheckDrat
End Sub

Sub TRightOutlane_Hit
	Flash2 True
	Score(CurrentPlayer) = Score(CurrentPlayer) + (50000*PFMultiplier)
		If activeball.vely < 0 then
			PlaySound "blaneup"
			else
			PlaySound "blanedw"
		end if
if RightOutlane.State = 1 Then
DMD "", "", "50kdmd", eNone, eNone, eNone, 500, True, ""
CheckDrat
exit Sub
end if
RightOutlane.State = 1
DMD "", "", "tdmd", eNone, eNone, eNone, 500, True, ""
CheckDrat
End Sub

Sub CheckDrat
	If(LeftInlane.State = 1) And(LeftOutlane.State = 1) And(RightInlane.State = 1) And(RightOutlane.State = 1) Then
	DMD "", "", "dratdmd", eNone, eNone, eNone, 1000, True, ""
	Score(CurrentPlayer) = Score(CurrentPlayer) + (300000*PFMultiplier*DRATMaultiplier)
	if DRATMaultiplier = 4 then
	playsound "drat"
	LeftInlane.State=0
	LeftOutlane.State=0
	RightInlane.State=0
	RightOutlane.State=0
	exit sub
	End If
	if DRATMaultiplier = 1 then
	playsound "drat"
	LeftInlane.State=0
	LeftOutlane.State=0
	RightInlane.State=0
	RightOutlane.State=0	
	DRATMaultiplier = DRATMaultiplier + 1
	exit sub
	End If
	if DRATMaultiplier = 2 then
	playsound "drat_6"
	LeftInlane.State=0
	LeftOutlane.State=0
	RightInlane.State=0
	RightOutlane.State=0	
	DRATMaultiplier = DRATMaultiplier + 1
	exit sub
	End If
	if DRATMaultiplier = 3 then
	playsound "drat_5"
	LeftInlane.State=0
	LeftOutlane.State=0
	RightInlane.State=0
	RightOutlane.State=0	
	DRATMaultiplier = DRATMaultiplier + 1
	exit sub
	End If
	End if
End Sub


'**************** burnouts ************************

dim burny:burny=0
sUB fLASHBURNOUT
	ObjLevel(3) = 1 : FlasherFlash3_Timer
End Sub	

sub BurnOut_hit()
	vpmtimer.addtimer 200,"fLASHBURNOUT '"
	vpmtimer.addtimer 330,"fLASHBURNOUT '"
	vpmtimer.addtimer 400,"fLASHBURNOUT '"
	vpmtimer.addtimer 450,"fLASHBURNOUT '"

    if burny = 20 then exit sub
	If Burnout.timerenabled=False Then
        Burnout.timerenabled=True
	if burny=19 and bMultiBallMode = true Then
	exit Sub
	end if
	dothewave1
	burny=burny+1
	checkburnoutstatus
    End If
end sub

Sub Burnout_Timer
    Burnout.timerenabled=False
End Sub

sub checkburnoutstatus
    If burny > 20 Then burny = 20 
    debug.print "checkburnout: burny = " & burny
    Select Case burny
        Case 1:DMD "", "", "ut1", eNone, eNone, eNone, 1000, True, "burn_01"
        Case 2:DMD "", "", "ut2", eNone, eNone, eNone, 1000, True, "burn_02"
        Case 3:DMD "", "", "ut3", eNone, eNone, eNone, 1000, True, "burn_03"
        Case 4:DMD "", "", "ut4", eNone, eNone, eNone, 1000, True, "burn_04"
        Case 5:DMD "", "", "ut5", eNone, eNone, eNone, 1000, True, "burn_05"
        Case 6:DMD "", "", "ut6", eNone, eNone, eNone, 1000, True, "burn_06"
        Case 7:DMD "", "", "ut7", eNone, eNone, eNone, 1000, True, "burn_07"
        Case 8:DMD "", "", "ut8", eNone, eNone, eNone, 1000, True, "burn_08"
        Case 9:DMD "", "", "ut9", eNone, eNone, eNone, 1000, True, "burn_09"
        Case 10:DMD "", "", "ut10", eNone, eNone, eNone, 1000, True, "burn_10"
        Case 11:DMD "", "", "ut11", eNone, eNone, eNone, 1000, True, "burn_01"
        Case 12:DMD "", "", "ut12", eNone, eNone, eNone, 1000, True, "burn_02"
        Case 13:DMD "", "", "ut13", eNone, eNone, eNone, 1000, True, "burn_03"
        Case 14:DMD "", "", "ut14", eNone, eNone, eNone, 1000, True, "burn_04"
        Case 15:DMD "", "", "ut15", eNone, eNone, eNone, 1000, True, "burn_05"
        Case 16:DMD "", "", "ut16", eNone, eNone, eNone, 1000, True, "burn_06"
        Case 17:DMD "", "", "ut17", eNone, eNone, eNone, 1000, True, "burn_07"
        Case 18:DMD "", "", "ut18", eNone, eNone, eNone, 1000, True, "burn_08"
        Case 19:DMD "", "", "ut19", eNone, eNone, eNone, 1000, True, "burn_09"
        Case 20:DMD "", "", "ut20", eNone, eNone, eNone, 1000, True, "burn_10":startburningmultiball
    End Select
end sub

sub startburningmultiball
createballsburnout
flashplaat2.ImageA="Nbm"
playsound "multiballstart"
vpmtimer.addtimer 3000, "shootoutballsburnout ' "
end sub

sub createballsburnout
li047.state = 1
BallsOnPlayfield = BallsOnPlayfield + 5
bMultiBallMode = True
VUKGH003.CreateSizedball BallSize / 2
VUKGH004.CreateSizedball BallSize / 2
VUKGH005.CreateSizedball BallSize / 2
VUKGH002.CreateSizedball BallSize / 2
VUKGH006.CreateSizedball BallSize / 2
end sub

sub shootoutballsburnout
burny=0
Playsound "fx_popper"

VUKGH002.Kick 0, 50, 1.25
VUKGH003.Kick 180, 2
VUKGH004.Kick 0, 250
VUKGH005.Kick 0, 34, 1.25
VUKGH006.Kick 190, 2
end sub

Sub Bonuschecker_Hit
FlashForMs Flasher004, 1000, 50, 0
FlashForMs Flasher005, 1000, 50, 0
Flash1 True
Flash2 True
Flash3 True
Flash4 True
Flash5 True
End Sub

'************************** 
'Bumpers 
'************************** 
Dim bumperHits

Sub Bumper001_hit()
Flash4 True
bumper1.blenddisablelighting = 1
Tbumpy001.enabled = true
playsound "pop2"
DOF 107,  DOFPulse
RandomSoundBumperMiddle Bumper001
End sub
' Bumper Bonus
' 100000 i bonus after each 100 hits

Sub Bumper002_hit()
Flash5 True
mutleyshaker
bumper2.blenddisablelighting = 1
Tbumpy002.enabled = true
playsound "pop1" 
DOF 108,  DOFPulse
RandomSoundBumperMiddle Bumper002
End sub

Sub Bumper003_hit()
Flash3 True
bumper3.blenddisablelighting = 1
Tbumpy003.enabled = true
DOF 109,  DOFPulse
playsound "pop3"
RandomSoundBumperBottom Bumper003
End sub

Sub CheckBumpers()
    If bumperHits <= 0 Then
        BumperBonus = BumperBonus + 1
        DMD "_", CL(1, "BUMPERS BONUS " & BumperBonus), "_", eNone, eBlink, eNone, 500, True, ""
        bumperHits = 100
    ' do something more
    End If
End Sub

Sub ResetBumpers()
    bumperHits = 100
End Sub

sub Tbumpy001_timer()
bumper1.blenddisablelighting = 0
Tbumpy001.enabled = false
end sub

sub Tbumpy002_timer()
bumper2.blenddisablelighting = 0
Tbumpy002.enabled = false
end sub

sub Tbumpy003_timer()
bumper3.blenddisablelighting = 0
Tbumpy003.enabled = false
end sub

'*****************
'Targets
'*****************

'********************wackytargets********************

'W
sub Target006_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)

If li029.state = 1 Then
DMD "", "", "1kdmd", eNone, eNone, eNone, 750, True, ""
playsound "th2"
checkAllDropped
Exit Sub
end if
playsound "th1"
WKlights = WKlights + 1
DMD "", "", "Wdmd", eNone, eNone, eNone, 750, True, ""
li029.state = 1
li033.state = 1
checkAllDropped
end sub

'A
sub Target007_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)

If li043.state = 1 Then
DMD "", "", "1kdmd", eNone, eNone, eNone, 750, True, ""
playsound "th2"
checkAllDropped
Exit Sub
end if
playsound "th1"
DMD "", "", "Admd", eNone, eNone, eNone, 750, True, ""
WKlights = WKlights + 1
li043.state = 1
li034.state = 1
checkAllDropped
end sub

'C
sub Target008_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)

If li030.state = 1 Then
DMD "", "", "1kdmd", eNone, eNone, eNone, 750, True, ""
playsound "th2"
checkAllDropped
Exit Sub
end if
playsound "th1"
DMD "", "", "cdmd", eNone, eNone, eNone, 750, True, ""
WKlights = WKlights + 1
li030.state = 1
li035.state = 1
checkAllDropped
end sub

'K
sub Target009_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)

If li031.state = 1 Then
DMD "", "", "1kdmd", eNone, eNone, eNone, 750, True, ""
playsound "th2"
checkAllDropped
Exit Sub
end if
playsound "th1"
DMD "", "", "kdmd", eNone, eNone, eNone, 750, True, ""
WKlights = WKlights + 1
li031.state = 1
li036.state = 1
checkAllDropped
end sub

'Y
sub Target010_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)

If li032.state = 1 Then
DMD "", "", "1kdmd", eNone, eNone, eNone, 750, True, ""
playsound "th2"
checkAllDropped
Exit Sub
end if
playsound "th1"
DMD "", "", "ydmd", eNone, eNone, eNone, 750, True, ""
WKlights = WKlights + 1
li032.state = 1
li037.state = 1
checkAllDropped
end sub

'R
sub Target001_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)

If li020.state = 1 Then
DMD "", "", "1kdmd", eNone, eNone, eNone, 750, True, ""
playsound "th2"
checkAllDropped
Exit Sub
end if
playsound "th1"
DMD "", "", "rdmd", eNone, eNone, eNone, 750, True, ""
WKlights = WKlights + 1
li020.state = 1
li038.state = 1
checkAllDropped
end sub

'A
sub Target002_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)

If li019.state = 1 Then
DMD "", "", "1kdmd", eNone, eNone, eNone, 750, True, ""
playsound "th2"
checkAllDropped
Exit Sub
end if
playsound "th1"
DMD "", "", "a2dmd", eNone, eNone, eNone, 750, True, ""
WKlights = WKlights + 1
li019.state = 1
li039.state = 1
checkAllDropped
end sub

'C
sub Target003_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)

If li018.state = 1 Then
DMD "", "", "1kdmd", eNone, eNone, eNone, 750, True, ""
playsound "th2"
checkAllDropped
Exit Sub
end if
playsound "th1"
DMD "", "", "cdmd", eNone, eNone, eNone, 750, True, ""
WKlights = WKlights + 1
li018.state = 1
li040.state = 1
checkAllDropped
end sub

'E
sub Target004_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)

If li017.state = 1 Then
DMD "", "", "1kdmd", eNone, eNone, eNone, 750, True, ""
playsound "th2"
checkAllDropped
Exit Sub
end if
playsound "th1"
DMD "", "", "edmd", eNone, eNone, eNone, 750, True, ""
WKlights = WKlights + 1
li017.state = 1
li041.state = 1
checkAllDropped
end sub

'S
sub Target005_hit()
If Tilted Then Exit Sub
TargetBonus = TargetBonus + 1
Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)

If li016.state = 1 Then
DMD "", "", "1kdmd", eNone, eNone, eNone, 750, True, ""
playsound "th2"
checkAllDropped
Exit Sub
end if
playsound "th1"
DMD "", "", "sdmd", eNone, eNone, eNone, 750, True, ""
WKlights = WKlights + 1
li016.state = 1
li042.state = 1
checkAllDropped
end sub


sub checkAllDropped
If WKlights = 10 then
DMD "", "", "wackyracesdmd", eNone, eNone, eNone, 1000, True, "v_wackyraces2"
Score(CurrentPlayer) = Score(CurrentPlayer) + (500000*PFMultiplier*WKMultiplier)
Resettargets
'vpmtimer.addtimer 2005, "Resettargets ' "
WKMultiplier = WKMultiplier + 1
end if
end sub

Sub Resettargets
WKlights = 0
li016.state = 0
li017.state = 0
li018.state = 0
li019.state = 0
li020.state = 0
li029.state = 0
li030.state = 0
li031.state = 0
li032.state = 0
li033.state = 0
li034.state = 0
li035.state = 0
li036.state = 0
li037.state = 0
li038.state = 0
li039.state = 0
li040.state = 0
li041.state = 0
li042.state = 0
li043.state = 0
end sub

'*****************
'Gates
'*****************
sub Gate_Hit()
'ObjLevel(1) = 1 : FlasherFlash9_Timer
End Sub

'*****************
'Kickers
'*****************
Dim Pbossy:Pbossy=0

Sub VUKGH_Hit()
Flash3 True
tvukg001.enabled = True
countr42=0
Tnavlaugh.enabled = 1
'	ObjLevel(2) = 1 : FlasherFlash2_Timer
	DOF 125, DOFPulse
	playsound "routeadv1"
	kijkerDir = 5
	kijkerTimer.Enabled = 1
	vpmtimer.AddTimer 2000, "rightbelowkickout'"
End Sub

sub rightbelowkickout
tvukg001.enabled = False
VUKGH.Kick 0, 52, 1.25
	'BlinkGI 10,50
Playsound "fx_popper"
 DOF 112, DOFPulse
Tnavlaugh.enabled = 0
flashplaat2.ImageA="Ndesti"
end sub

sub tvukg001_timer()
Flash3 True
end sub

Sub VUKGH002_Hit()
	playsound "gasstop"
	DOF 125, DOFPulse
	Pbossy = Pbossy + 1
	if bMultiBallMode=True and Pbossy = 3 then
	leftbelowkickout
	exit sub
	end if
	if Pbossy > 3 then Pbossy = 3 
	checkplaysomeboss
	if BSmode = 1 or wackychallanges = 2 then
	leftbelowkickout
	exit sub
	end if
	flashplaat2.ImageA="gassg"
	leftbelowkickout
End Sub

sub checkplaysomeboss
	If BSmode=1 then
		vpmTimer.AddTimer 950, "leftbelowkickout'"
	exit sub
	end if
	if Pbossy = 3 Then
	if bMultiBallMode=false and BSmode=0 then
		playsound "passbybossintro"
		StartPlayer0Slotmachine
		vpmTimer.AddTimer 2150, "leftbelowkickout'"
	exit sub
	end if
	else
		vpmTimer.AddTimer 950, "leftbelowkickout'"
	end If
end sub
 
sub leftbelowkickout
VUKGH002.Kick 0, 50, 1.25
if Ssoundz2 = 0 then 
Playsound "fx_popper"
end if
Tlgas.enabled = true
Ssoundz2 = Ssoundz2 + 1
Tsound001.enabled = 1
end sub

sub Tsound001_timer()
Ssoundz2 = 0
Tsound001.enabled = 0
end sub

sub VUKGH003_Hit()
kickoutup
end sub

sub kickoutup
VUKGH003.Kick 180, 2
end sub

'************************* Mine  kicker ************************* 
Dim SlotAward3, SlotValue3
SlotAward3 = Array("100kdmd", "200kdmd", "300kdmd", "500kdmd", "1Mdmd")


sub VUKGH004_Hit()
'WireRampOff
if PFMultiplier = 8 Then
Startminechoisemachine
exit Sub
end if
if PFMultiplier = 1 Then
PFMultiplier = 2
li012.state = 1
DMD "", "", "2xdmd", eNone, eNone, eNone, 2000, true, "multipliert"
vpmTimer.AddTimer 2000, "kickoutleftup'"
vpmTimer.AddTimer 2000, "enablestealtimert'"
exit Sub
end if
if PFMultiplier = 2 Then
PFMultiplier = 4
li013.state = 1
DMD "", "", "4xdmd", eNone, eNone, eNone, 2000, true, "multipliert"
vpmTimer.AddTimer 2000, "kickoutleftup'"
vpmTimer.AddTimer 2000, "enablestealtimert'"
exit Sub
end if
if PFMultiplier = 4 Then
PFMultiplier = 6
li014.state = 1
DMD "", "", "6xdmd", eNone, eNone, eNone, 2000, true, "multipliert"
vpmTimer.AddTimer 2000, "kickoutleftup'"
vpmTimer.AddTimer 2000, "enablestealtimert'"
exit Sub
end if
if PFMultiplier = 6 Then
PFMultiplier = 8
li015.state = 1
DMD "", "", "8xdmd", eNone, eNone, eNone, 2000, true, "multipliert"
vpmTimer.AddTimer 2000, "kickoutleftup'"
vpmTimer.AddTimer 2000, "enablestealtimert'"
exit Sub
end if
end sub

sub enablestealtimert
Tstealmultie.enabled = true
end sub

sub Tstealmultie_timer()
if wsteal = 1 Then
tchecksteal.enabled = true
exit sub
end if
stealthemultiplier
end sub

sub tchecksteal_timer
checkdiesteail
end sub

sub checkdiesteail
if wsteal = 0 Then
stealthemultiplier
end if
end sub

sub stealthemultiplier
tchecksteal.enabled = false
if PFMultiplier = 2 Then
DMD "", "", "muhaha", eNone, eNone, eNone, 1000, true, "mop1"
PFMultiplier = 1
li012.state = 0
Tstealmultie.enabled = False
exit sub
end if
if PFMultiplier = 4 Then
DMD "", "", "muhaha", eNone, eNone, eNone, 1000, true, "mop1"
PFMultiplier = 2
li012.state = 1
li013.state = 0
exit sub
end if
if PFMultiplier = 6 Then
DMD "", "", "muhaha", eNone, eNone, eNone, 1000, true, "mop1"
PFMultiplier = 4
li013.state = 1
li014.state = 0
exit sub
end if
if PFMultiplier = 8 Then
DMD "", "", "muhaha", eNone, eNone, eNone, 1000, true, "mop1"
PFMultiplier = 6
li014.state = 1
li015.state = 0
exit sub
end if
end sub

sub kickoutleftup
VUKGH004.Kick 0, 250
Playsound "above2"
 DOF 118, DOFPulse
if BSmode = 1 or wackychallanges = 2 then
exit sub
end if
flashplaat2.ImageA="Nmaway"
end sub

Sub Startminechoisemachine()
	Dim i
	DMDFlush
	For i = 0 to 1
		DMD "", "", SlotAward3(i), eNone, eNone, eNone, 100, False, ""
	Next
	vpmtimer.AddTimer 1000, "minawardy '"
End Sub

sub minawardy
	DMDFlush()
	Randomize()
	SlotValue3 = INT(RND * 5)
	DMD "", "", SlotAward3(SlotValue3), eNone, eNone, eNone, 500, True, ""
	Select Case SlotValue3
		Case 0:minereward1
		Case 1:minereward2
		Case 2:minereward3
		Case 3:minereward4
		Case 4:minereward5
	End Select
end sub

sub minereward1
Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
DMD "", "", "100kdmd", eNone, eNone, eNone, 1000, true, ""
vpmTimer.AddTimer 1500, "kickoutleftup'"
end sub

sub minereward2
Score(CurrentPlayer) = Score(CurrentPlayer) + (200000*PFMultiplier)
DMD "", "", "200kdmd", eNone, eNone, eNone, 1000, true, ""
vpmTimer.AddTimer 1500, "kickoutleftup'"
end sub

sub minereward3
Score(CurrentPlayer) = Score(CurrentPlayer) + (300000*PFMultiplier)
DMD "", "", "300kdmd", eNone, eNone, eNone, 1000, true, ""
vpmTimer.AddTimer 1500, "kickoutleftup'"
end sub

sub minereward4
Score(CurrentPlayer) = Score(CurrentPlayer) + (500000*PFMultiplier)
DMD "", "", "500kdmd", eNone, eNone, eNone, 1000, true, ""
vpmTimer.AddTimer 1500, "kickoutleftup'"
end sub

sub minereward5
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
DMD "", "", "1Mdmd", eNone, eNone, eNone, 1000, true, ""
vpmTimer.AddTimer 1500, "kickoutleftup'"
end sub

'************************* Middle go kicker ************************* 

sub VUKGH005_hit()
Playsound "KickerIn"
vpmTimer.AddTimer 1000, "kickoutunderup'"
end Sub

sub kickoutunderup
VUKGH005.Kick 0, 34, 1.25
if Ssoundz = 0 then 
Playsound "boing"
 DOF 115, DOFPulse
end if
Ssoundz = Ssoundz + 1
Tsound.enabled = 1
if BSmode = 1 or wackychallanges = 2 then
exit sub
end if
flashplaat2.ImageA="Nup"
end sub

'************************* Ton kicker ************************* 
dim weightf:weightf=0

sub VUKGH006_hit()
Playsound "KickerIn"
if weightf = 3 Then
Checkweightfall
exit Sub
end if
weightf = weightf + 1
Checkweightfall
end Sub

sub Checkweightfall
if weightf = 1 Then
DMD "", "", "1t2t", eNone, eNone, eNone, 1000, True, "weightsignmove"
vpmTimer.AddTimer 1000, "kickout5ton'"
exit Sub
end if
if weightf = 2 Then
DMD "", "", "1t1t", eNone, eNone, eNone, 1000, True, "weightsignmove"
vpmTimer.AddTimer 1000, "kickout5ton'"
exit Sub
end if
if weightf = 3 and wackychallanges = 1 and bMultiBallMode = false and BSmode = 0 then
flashplaat2.ImageA="Ntmm"
DMD "", "", "1tmulti", eNone, eNone, eNone, 4000, True, ""
'vpmTimer.AddTimer 1000, "kickout5ton'"
Start1tonmb
exit Sub
end if
if weightf = 3 Then
playsound "weightsignmove"
vpmTimer.AddTimer 1000, "kickout5ton'"
end if
end sub

sub kickout5ton
VUKGH006.Kick 190, 2
Playsound "holeout"
 DOF 113, DOFPulse
end sub

sub Start1tonmb
	weightf=0
	DisableTable True
	playsound "1tonfall"
	Tturnton.enabled=false
	tonny.RotY=110
	Ttonnydown.enabled = 1
	'VUKGH001.CreateSizedball BallSize / 2 ''' just a temp create
	vpmtimer.addtimer 300, "tonballbroken '"
	vpmtimer.addtimer 4000, "DisableTable False : multiballoutkicker '"
	vpmtimer.addtimer 4500, "multiballoutkicker '"	
	'vpmtimer.addtimer 4650, "kickout5ton '"
	vpmtimer.addtimer 4650, "changetonnyback '"
end sub

sub tonballbroken
	VUKGH006.DestroyBall
	'Cor.Update
end sub

sub changetonnyback
	'VUKGH001.DestroyBall
	'Cor.Update
	tonny.Z=70
	Tturnton.enabled=true
	BallsOnPlayfield = 2
	bMultiBallMode = True 
end sub 

sub Tturnton_timer() '110
   tonny.RotY= tonny.RotY+ 1
   if tonny.RotY> 360 then
	   tonny.RotY= 1
end if
end sub

sub Ttonnydown_timer()
if tonny.Z > 15 then
tonny.Z = tonny.Z - 5
End If
checktonnymovedown
end sub

sub checktonnymovedown
If tonny.Z = 15 Then
Ttonnydown.Enabled = 0
end if
end sub

'************************* worldcup  kicker ************************* 

Dim BallInHole1
Dim dBall
Dim SlotAward, SlotValue
SlotAward = Array("gasdmd", "minesdmd")

Sub VUKGH001_Hit()
	li046.state = 1
	Flash4 True
	if sboss11 = 1 Then
	flashplaat2.ImageA="Ncar00"
	end if
	if sboss10 = 1 Then
	flashplaat2.ImageA="Ncar10"
	end if
	if sboss09 = 1 Then
	flashplaat2.ImageA="Ncar09"
	end if
	if sboss08 = 1 Then
	flashplaat2.ImageA="Ncar08"
	end if
	if sboss07 = 1 Then
	flashplaat2.ImageA="Ncar07"
	end if
	if sboss06 = 1 Then
	flashplaat2.ImageA="Ncar06"
	end if
	if sboss05 = 1 Then
	flashplaat2.ImageA="Ncar05"
	end if
	if sboss04 = 1 Then
	flashplaat2.ImageA="Ncar04"
	end if
	if sboss03 = 1 Then
	flashplaat2.ImageA="Ncar03"
	end if
	if sboss02 = 1 Then
	flashplaat2.ImageA="Ncar02"
	end if
	if sboss01 = 1 Then
	flashplaat2.ImageA="Ncar01"
	end if
	Playsound "ballincup"
 DOF 117, DOFon
	BallInHole1 = BallInHole1 + 1
    Set dBall = ActiveBall:Me.TimerEnabled = 1
	Startquakerchoisemachine
	Me.Enabled = 0
	Tcupturny.enabled = true
gion
End Sub

Sub VUKGH001_Timer
    Do While dBall.Z > 0
        dBall.Z = dBall.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
	Me.Enabled = 1
End Sub

sub Tcupturny_timer()
   trophy.RotY= trophy.RotY+ 1
   if trophy.RotY> 720 then
	   trophy.RotY= 0
	   Tcupturny.enabled = false
 DOF 117, DOFoff	
end if 
end sub

Sub Startquakerchoisemachine()
	Dim i
	DMDFlush
	For i = 0 to 1
		DMD "", "", SlotAward(i), eNone, eNone, eNone, 100, False, ""
	Next
	vpmtimer.AddTimer 1700, "createrandomtoyball '"
End Sub

sub createrandomtoyball
	DMDFlush()
	Randomize()
	SlotValue = INT(RND * 2)
	DMD "", "", SlotAward(SlotValue), eNone, eNone, eNone, 500, True, ""
	Select Case SlotValue
		Case 0:SuperVukAddBall1
		Case 1:SuperVukAddBall2
	End Select
end sub

Sub SuperVukAddBall1()
'	ObjLevel(3) = 1 : FlasherFlash3_Timer
	If BallInHole1> 0 Then
        BallInHole1 = BallInHole1 - 1
	VUKGH002.CreateSizedball BallSize / 2
	'ChangeBallImage
	Playsound "fx_popper"
	VUKGH002.Kick 0, 50, 1.25
 vpmtimer.addtimer 1000, "SuperVukAddBall1 '" 
end If
End Sub

Sub SuperVukAddBall2()
'	ObjLevel(4) = 1 : FlasherFlash4_Timer
	If BallInHole1> 0 Then
        BallInHole1 = BallInHole1 - 1
	VUKGH003.CreateSizedball BallSize / 2
	'ChangeBallImage
	Playsound "fx_popper"
	VUKGH003.Kick 180, 2
 vpmtimer.addtimer 1000, "SuperVukAddBall2 '" 
end If
End Sub

'*****************
'choose driver 
'*****************

sub Tcarturny_timer()
   c00.RotY= c00.RotY+ 1
   if c00.RotY> 360 then
	   c00.RotY= 1	
end if
   c01.RotY= c01.RotY+ 1
   if c01.RotY> 360 then
	   c01.RotY= 1	
end if
   c02.RotY= c02.RotY+ 1
   if c02.RotY> 360 then
	   c02.RotY= 1	
end if
   c03.RotY= c03.RotY+ 1
   if c03.RotY> 360 then
	   c03.RotY= 1	
end if
   c04.RotY= c04.RotY+ 1
   if c04.RotY> 360 then
	   c04.RotY= 1	
end if
   c05.RotY= c05.RotY+ 1
   if c05.RotY> 360 then
	   c05.RotY= 1	
end if
   c06.RotY= c06.RotY+ 1
   if c06.RotY> 360 then
	   c06.RotY= 1	
end if
   c07.RotY= c07.RotY+ 1
   if c07.RotY> 360 then
	   c07.RotY= 1	
end if
   c08.RotY= c08.RotY+ 1
   if c08.RotY> 360 then
	   c08.RotY= 1	
end if
   c09.RotY= c09.RotY+ 1
   if c09.RotY> 360 then
	   c09.RotY= 1	
end if
   c10.RotY= c10.RotY+ 1
   if c10.RotY> 360 then
	   c10.RotY= 1	
end if
'   kijker.ObjRotz= kijker.ObjRotz+ 1
'   if kijker.ObjRotz> 360 then
'	   kijker.ObjRotz= 1	
'end if
end sub

'*****************
'rotate items 
'*****************

sub TrotateI001_timer()
   pidgeon01.RotY= pidgeon01.RotY+ 1
   if pidgeon01.RotY> 360 then
	   pidgeon01.RotY= 1	
end if
   cone01.RotY= cone01.RotY+ 1
   if cone01.RotY> 360 then
	   cone01.RotY= 1	
end if
   Mud001.RotY= Mud001.RotY+ 1
   if Mud001.RotY> 360 then
	   Mud001.RotY= 1	
end if
   gase01.RotY= gase01.RotY+ 1
   if gase01.RotY> 360 then
	   gase01.RotY= 1	
end if
'   kijker.ObjRotz= kijker.ObjRotz+ 1
'   if kijker.ObjRotz> 360 then
'	   kijker.ObjRotz= 1	
'end if
end sub

sub TrotateI002_timer()
   rock001.RotY= rock001.RotY+ 1
   if rock001.RotY> 360 then
	   rock001.RotY= 1
end if
   rock002.RotY= rock002.RotY+ 1
   if rock002.RotY> 360 then
	   rock002.RotY= 1
end if
   rock003.RotY= rock003.RotY+ 1
   if rock003.RotY> 360 then
	   rock003.RotY= 1
end if
   bat001.RotY= bat001.RotY+ 1
   if bat001.RotY> 360 then
	   bat001.RotY= 1
end if
   hammer01.RotY= hammer01.RotY+ 1
   if hammer01.RotY> 360 then
	   hammer01.RotY= 1
end if
   bar01.RotY= bar01.RotY+ 1
   if bar01.RotY> 360 then
	   bar01.RotY= 1
end if
   paperplany.RotY= paperplany.RotY+ 1
   if paperplany.RotY> 360 then
	   paperplany.RotY= 1
end if
   lipstick01.RotY= lipstick01.RotY+ 1
   if lipstick01.RotY> 360 then
	   lipstick01.RotY= 1
end if
   bombmode01.RotY= bombmode01.RotY+ 1
   if bombmode01.RotY> 360 then
	   bombmode01.RotY= 1
end if
   wbox.RotY= wbox.RotY+ 1
   if wbox.RotY> 360 then
	   wbox.RotY= 1
end if
   beer01.RotY= beer01.RotY+ 1
   if beer01.RotY> 360 then
	   beer01.RotY= 1
end if
   honey01.RotY= honey01.RotY+ 1
   if honey01.RotY> 360 then
	   honey01.RotY= 1
end if
   beaver01.RotZ= beaver01.RotZ+ 1
   if beaver01.RotZ> 360 then
	   beaver01.RotZ= 1
end if
end sub

'*****************
'workaround double sounds 
'*****************

sub Tsound_timer()
Ssoundz = 0
Tsound.enabled = 0
end sub

'*****************
'position takeover
'*****************

'player00

Sub StartPlayer0Slotmachine()
	if chooseplayert = 0 then 
	b00 = 1
	end if
	if chooseplayert = 1 then 
	b01 = 1
	end if
	if chooseplayert = 2 then 
	b02 = 1
	end if
	if chooseplayert = 3 then 
	b03 = 1
	end if
	if chooseplayert = 4 then 
	b04 = 1
	end if
	if chooseplayert = 5 then 
	b05 = 1
	end if
	if chooseplayert = 6 then 
	b06 = 1
	end if
	if chooseplayert = 7 then 
	b07 = 1
	end if
	if chooseplayert = 8 then 
	b08 = 1
	end if
	if chooseplayert = 9 then 
	b09 = 1
	end if
	if chooseplayert = 10 then 
	b10 = 1
	end if
	Tplayer000.Enabled = true
	flashplaat1.visible = true
    vpmtimer.AddTimer 3000, "SlotAwardPlayer00 '"
End Sub

Dim Holostep:Holostep = INT(RND(1)*78)
Sub Tplayer000_Timer()
Randomize()
Playsound "fx_spinner"
Holostep = (Holostep + 1) MOD 11
'Holostep=int(rnd(1)*11)   = 0-10
Select Case Holostep
case 0: 	if b00= 0 then 
			flashplaat1.ImageA = "p" & chooseplayert  &"_0" & Holostep
			P0slot = 0
			Holostep = 0
			end if
case 1: 	if b01= 0 then 
			flashplaat1.ImageA = "p" & chooseplayert  &"_0" & Holostep
			P0slot = 1
			Holostep = 1
			end if
case 2: 	if b02= 0 then 
			flashplaat1.ImageA = "p" & chooseplayert  &"_0" & Holostep 
			P0slot = 2
			Holostep = 2
			end if
case 3: 	if b03= 0 then 
			flashplaat1.ImageA = "p" & chooseplayert  &"_0" & Holostep
			P0slot = 3
			Holostep = 3
			end if
case 4: 	if b04= 0 then 
			flashplaat1.ImageA = "p" & chooseplayert  &"_0" & Holostep
			P0slot = 4 
			Holostep = 4
			end if
case 5: 	if b05= 0 then 
			flashplaat1.ImageA = "p" & chooseplayert  &"_0" & Holostep 
			P0slot = 5
			Holostep = 5
			end if
case 6: 	if b06= 0 then 
			flashplaat1.ImageA = "p" & chooseplayert  &"_0" & Holostep 
			P0slot = 6
			Holostep = 6
			end if
case 7: 	if b07= 0 then 
			flashplaat1.ImageA = "p" & chooseplayert  &"_0" & Holostep
			P0slot = 7
			Holostep = 7
			end if
case 8: 	if b08= 0 then 
			flashplaat1.ImageA = "p" & chooseplayert  &"_0" & Holostep 
			P0slot = 8
			Holostep = 8
			end if
case 9: 	if b09= 0 then 
			flashplaat1.ImageA = "p" & chooseplayert  &"_0" & Holostep 
			P0slot = 9
			Holostep = 9
			end if
case 10: 	if b10= 0 then 
			flashplaat1.ImageA = "p" & chooseplayert  &"_0" & Holostep 
			P0slot = 10
			Holostep = 10
			end if
End Select
End Sub

sub SlotAwardPlayer00
Tplayer000.Enabled = false
Select Case P0slot
case 0:
boss00
vpmTimer.AddTimer 2500, "disableflasplaat1'"
case 1:
boss01
vpmTimer.AddTimer 2500, "disableflasplaat1'"
case 2:
boss02
vpmTimer.AddTimer 2500, "disableflasplaat1'"
case 3:
boss03
vpmTimer.AddTimer 2500, "disableflasplaat1'"
case 4:
boss04
vpmTimer.AddTimer 2500, "disableflasplaat1'"
case 5:
boss05
vpmTimer.AddTimer 2500, "disableflasplaat1'"
case 6:
boss06
vpmTimer.AddTimer 2500, "disableflasplaat1'"
case 7:
boss07
vpmTimer.AddTimer 2500, "disableflasplaat1'"
case 8:
boss08
vpmTimer.AddTimer 2500, "disableflasplaat1'"
case 9:
boss09
vpmTimer.AddTimer 2500, "disableflasplaat1'"
case 10:
boss10
vpmTimer.AddTimer 2500, "disableflasplaat1'"
End Select
End Sub

sub disableflasplaat1
flashplaat1.visible = false
end sub

sub Theadlight_timer()
LightSeqHead.Play SeqClockLeftOn, 180
end sub

'*******boss00***************

sub boss00
flashplaat2.ImageA="Ncar00"
Theadlight.enabled=True
BSmode = 1
Sboss11 = 1
Wcar011.Isdropped = False
c021.visible=true
TrotateI002.enabled=true
enablebomba
end sub

sub enablebomba
bombmode01.visible=true
TBA009.enabled=true
end sub

sub TBA009_hit()
TBA009.enabled= False
bombmode01.visible=False
bhit = bhit + 1
playsound "bombpickup"
checkbosshit11
end sub

Sub checkbosshit11
	if Sboss11 = 1 and bhit = 1 then
		bvul = 1
	end if
end sub


sub Wcar011_hit()
	if Sboss11 = 1 and bvul = 0 then
	bosshit500
	playsound "DIK01"
	end if
	if Sboss11 = 1 and bvul = 1 then
		playsound "DIK02"
		bhurt = bhurt + 1
		checksboss11hurt
	end if
end sub


sub checksboss11hurt
	if bhurt = 1 then
		bhit = 0
		bvul = 0
		bosshit400
		enablebomba
	end if
	if bhurt = 2 then
		bhit = 0
		bvul = 0
		bosshit300
		enablebomba
	end if
	if bhurt = 3 then
		bhit = 0
		bvul = 0
		bosshit200
		enablebomba
	end if
	if bhurt = 4 then
		bhit = 0
		bvul = 0
		bosshit100
		enablebomba
	end if
	if bhurt = 5 then
		b00= 1
		Bcomplete = Bcomplete + 1
		Bdone = Bdone + 1
		li011.state = 1
		bossesaway
		checkbossescomplete
		startroady
		exit sub
	end if
end sub


sub bosshit100
DMD "", "", "car00hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit001b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit200
DMD "", "", "car00hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit002b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit300
DMD "", "", "car00hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit003b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit400
DMD "", "", "car00hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit004b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit500
DMD "", "", "car00hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car00hit005b", eNone, eNone, eNone, 100, True, ""
end sub

'*******boss01***************

sub boss01
flashplaat2.ImageA="Ncar01"
Theadlight.enabled=True
BSmode = 1
Sboss01 = 1
Wcar001.Isdropped = False
c020.visible=true
TrotateI002.enabled=true
enablerockys
end sub

sub enablerockys
rock001.visible=true
rock002.visible=true
rock003.visible=true
TBA001.enabled=true
TBA002.enabled=true
TBA003.enabled=true
end sub

sub TBA001_hit()
TBA001.enabled= False
rock001.visible=False
bhit = bhit + 1
playsound "rock2"
checkbosshit01
end Sub

sub TBA002_hit()
TBA002.enabled= False
rock002.visible=False
bhit = bhit + 1
playsound "rock2"
checkbosshit01
end Sub

sub TBA003_hit()
TBA003.enabled= False
rock003.visible=False
bhit = bhit + 1
playsound "rock2"
checkbosshit01
end Sub


Sub checkbosshit01
	if Sboss01 = 1 and bhit = 3 then
		bvul = 1
	end if
end sub


sub Wcar001_hit()
	if Sboss01 = 1 and bvul = 0 then
	bosshit501
	playsound "SLU01"
	end if
	if Sboss01 = 1 and bvul = 1 then
		playsound "SLU02"
		bhurt = bhurt + 1
		checksboss01hurt
	end if
end sub


sub checksboss01hurt
	if bhurt = 1 then
		bhit = 0
		bvul = 0
		bosshit401
		enablerockys
	end if
	if bhurt = 2 then
		bhit = 0
		bvul = 0
		bosshit301
		enablerockys
	end if
	if bhurt = 3 then
		bhit = 0
		bvul = 0
		bosshit201
		enablerockys
	end if
	if bhurt = 4 then
		bhit = 0
		bvul = 0
		bosshit101
		enablerockys
	end if
	if bhurt = 5 then
		b01= 1
		Bcomplete = Bcomplete + 1
		Bdone = Bdone + 1
		li001.state = 1
		bossesaway
		checkbossescomplete
		startroady
		exit sub
	end if
end sub

sub bosshit101
DMD "", "", "car01hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit001b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit201
DMD "", "", "car01hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit002b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit301
DMD "", "", "car01hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit003b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit401
DMD "", "", "car01hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit004b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit501
DMD "", "", "car01hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car01hit005b", eNone, eNone, eNone, 100, True, ""
end sub

'*******boss02***************

sub boss02
flashplaat2.ImageA="Ncar02"
Theadlight.enabled=True
BSmode = 1
Sboss02 = 1
Wcar002.Isdropped = False
c019.visible=true
TrotateI002.enabled=true
enablefang
end sub

sub enablefang
bat001.visible=true
TBA004.enabled=true
end sub

sub TBA004_hit()
TBA004.enabled= False
bat001.visible=False
bhit = bhit + 1
playsound "BATS"
checkbosshit02
end sub

Sub checkbosshit02
	if Sboss02 = 1 and bhit = 1 then
		bvul = 1
	end if
end sub


sub Wcar002_hit()
	if Sboss02 = 1 and bvul = 0 then
	bosshit502
	playsound "CREEP01"
	end if
	if Sboss02 = 1 and bvul = 1 then
		playsound "CREEP02"
		bhurt = bhurt + 1
		checksboss02hurt
	end if
end sub


sub checksboss02hurt
	if bhurt = 1 then
		bhit = 0
		bvul = 0
		bosshit402
		enablefang
	end if
	if bhurt = 2 then
		bhit = 0
		bvul = 0
		bosshit302
		enablefang
	end if
	if bhurt = 3 then
		bhit = 0
		bvul = 0
		bosshit202
		enablefang
	end if
	if bhurt = 4 then
		bhit = 0
		bvul = 0
		bosshit102
		enablefang
	end if
	if bhurt = 5 then
		b02= 1
		Bcomplete = Bcomplete + 1
		Bdone = Bdone + 1
		li002.state = 1
		bossesaway
		checkbossescomplete
		startroady
		exit sub
	end if
end sub

sub bosshit102
DMD "", "", "car02hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit001b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit202
DMD "", "", "car02hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit002b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit302
DMD "", "", "car02hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit003b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit402
DMD "", "", "car02hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit004b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit502
DMD "", "", "car02hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car02hit005b", eNone, eNone, eNone, 100, True, ""
end sub

'*******boss03***************

sub boss03
flashplaat2.ImageA="Ncar03"
Theadlight.enabled=True
BSmode = 1
Sboss03 = 1
Wcar003.Isdropped = False
c018.visible=true
TrotateI002.enabled=true
enabletools
end sub

sub enabletools
hammer01.visible=true
bar01.visible=true
TBA005.enabled=true
TBA006.enabled=true
end sub

sub TBA005_hit()
TBA005.enabled= False
bar01.visible=False
bhit = bhit + 1
playsound "sleutelhit"
checkbosshit03
end sub

sub TBA006_hit()
TBA006.enabled= False
hammer01.visible=False
bhit = bhit + 1
playsound "hammerhit"
checkbosshit03
end sub

Sub checkbosshit03
	if Sboss03 = 1 and bhit = 2 then
		bvul = 1
	end if
end sub


sub Wcar003_hit()
	if Sboss03 = 1 and bvul = 0 then
	bosshit503
	playsound "PROF01"
	end if
	if Sboss03 = 1 and bvul = 1 then
		playsound "PROF02"
		bhurt = bhurt + 1
		checksboss03hurt
	end if
end sub

sub checksboss03hurt
	if bhurt = 1 then
		bhit = 0
		bvul = 0
		bosshit403
		enabletools
	end if
	if bhurt = 2 then
		bhit = 0
		bvul = 0
		bosshit303
		enabletools
	end if
	if bhurt = 3 then
		bhit = 0
		bvul = 0
		bosshit203
		enabletools
	end if
	if bhurt = 4 then
		bhit = 0
		bvul = 0
		bosshit103
		enabletools
	end if
	if bhurt = 5 then
		b03= 1
		Bcomplete = Bcomplete + 1
		Bdone = Bdone + 1
		li003.state = 1
		bossesaway
		checkbossescomplete
		startroady
		exit sub
	end if
end sub

sub bosshit103
DMD "", "", "car03hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit001b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit203
DMD "", "", "car03hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit002b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit303
DMD "", "", "car03hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit003b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit403
DMD "", "", "car03hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit004b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit503
DMD "", "", "car03hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car03hit005b", eNone, eNone, eNone, 100, True, ""
end sub

'*******boss04***************

sub boss04
flashplaat2.ImageA="Ncar04"
Theadlight.enabled=True
BSmode = 1
Sboss04 = 1
Wcar004.Isdropped = False
c017.visible=true
TrotateI002.enabled=true
enablepaperplane
end sub

sub enablepaperplane
paperplany.visible=true
TBA007.enabled=true
end sub

sub TBA007_hit()
TBA007.enabled= False
paperplany.visible=False
bhit = bhit + 1
playsound "paperhit"
checkbosshit04
end sub

Sub checkbosshit04
	if Sboss04 = 1 and bhit = 1 then
		bvul = 1
	end if
end sub


sub Wcar004_hit()
	if Sboss04 = 1 and bvul = 0 then
	bosshit504
	playsound "RED01"
	end if
	if Sboss04 = 1 and bvul = 1 then
		playsound "RED02"
		bhurt = bhurt + 1
		checksboss04hurt
	end if
end sub


sub checksboss04hurt
	if bhurt = 1 then
		bhit = 0
		bvul = 0
		bosshit404
		enablepaperplane
	end if
	if bhurt = 2 then
		bhit = 0
		bvul = 0
		bosshit304
		enablepaperplane
	end if
	if bhurt = 3 then
		bhit = 0
		bvul = 0
		bosshit204
		enablepaperplane
	end if
	if bhurt = 4 then
		bhit = 0
		bvul = 0
		bosshit104
		enablepaperplane
	end if
	if bhurt = 5 then
		b04= 1
		Bcomplete = Bcomplete + 1
		Bdone = Bdone + 1
		li004.state = 1
		bossesaway
		checkbossescomplete
		startroady
		exit sub
	end if
end sub

sub bosshit104
DMD "", "", "car04hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit001b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit204
DMD "", "", "car04hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit002b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit304
DMD "", "", "car04hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit003b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit404
DMD "", "", "car04hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit004b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit504
DMD "", "", "car04hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car04hit005b", eNone, eNone, eNone, 100, True, ""
end sub

'*******boss05***************

sub boss05
flashplaat2.ImageA="Ncar05"
Theadlight.enabled=True
BSmode = 1
Sboss05 = 1
Wcar005.Isdropped = False
c016.visible=true
TrotateI002.enabled=true
enablelipstick
end sub

sub enablelipstick
lipstick01.visible=true
TBA008.enabled=true
end sub

sub TBA008_hit()
TBA008.enabled= False
lipstick01.visible=False
bhit = bhit + 1
playsound "lipstick"
checkbosshit05
end sub

Sub checkbosshit05
	if Sboss05 = 1 and bhit = 1 then
		bvul = 1
	end if
end sub


sub Wcar005_hit()
	if Sboss05 = 1 and bvul = 0 then
	bosshit505
	playsound "PEN01"
	end if
	if Sboss05 = 1 and bvul = 1 then
		playsound "PEN02"
		bhurt = bhurt + 1
		checksboss05hurt
	end if
end sub


sub checksboss05hurt
	if bhurt = 1 then
		bhit = 0
		bvul = 0
		bosshit405
		enablelipstick
	end if
	if bhurt = 2 then
		bhit = 0
		bvul = 0
		bosshit305
		enablelipstick
	end if
	if bhurt = 3 then
		bhit = 0
		bvul = 0
		bosshit205
		enablelipstick
	end if
	if bhurt = 4 then
		bhit = 0
		bvul = 0
		bosshit105
		enablelipstick
	end if
	if bhurt = 5 then
		b05= 1
		Bcomplete = Bcomplete + 1
		Bdone = Bdone + 1
		li005.state = 1
		bossesaway
		checkbossescomplete
		startroady
		exit sub
	end if
end sub


sub bosshit105
DMD "", "", "car05hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit001b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit205
DMD "", "", "car05hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit002b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit305
DMD "", "", "car05hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit003b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit405
DMD "", "", "car05hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit004b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit505
DMD "", "", "car05hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car05hit005b", eNone, eNone, eNone, 100, True, ""
end sub

'*******boss06***************

sub boss06
flashplaat2.ImageA="Ncar06"
Theadlight.enabled=True
BSmode = 1
Sboss06 = 1
Wcar006.Isdropped = False
c015.visible=true
TrotateI002.enabled=true
enableweaponbox
end sub

sub enableweaponbox
wbox.visible=true
TBA010.enabled=true
end sub

sub TBA010_hit()
TBA010.enabled= False
wbox.visible=False
bhit = bhit + 1
playsound "boxhit"
checkbosshit06
end sub

Sub checkbosshit06
	if Sboss06 = 1 and bhit = 1 then
		bvul = 1
	end if
end sub


sub Wcar006_hit()
	if Sboss06 = 1 and bvul = 0 then
	bosshit506
	playsound "getgoingthatsanorder"
	end if
	if Sboss06 = 1 and bvul = 1 then
		playsound "fire"
		bhurt = bhurt + 1
		checksboss06hurt
	end if
end sub


sub checksboss06hurt
	if bhurt = 1 then
		bhit = 0
		bvul = 0
		bosshit406
		enableweaponbox
	end if
	if bhurt = 2 then
		bhit = 0
		bvul = 0
		bosshit305
		enableweaponbox
	end if
	if bhurt = 3 then
		bhit = 0
		bvul = 0
		bosshit206
		enableweaponbox
	end if
	if bhurt = 4 then
		bhit = 0
		bvul = 0
		bosshit106
		enableweaponbox
	end if
	if bhurt = 5 then
		b06= 1
		Bcomplete = Bcomplete + 1
		Bdone = Bdone + 1
		li006.state = 1
		bossesaway
		checkbossescomplete
		startroady
		exit sub
	end if
end sub


sub bosshit106
DMD "", "", "car06hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit001b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit206
DMD "", "", "car06hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit002b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit306
DMD "", "", "car06hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit003b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit406
DMD "", "", "car06hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit004b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit506
DMD "", "", "car06hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car06hit005b", eNone, eNone, eNone, 100, True, ""
end sub

'*******boss07***************

sub boss07
Theadlight.enabled=True
flashplaat2.ImageA="Ncar07"
BSmode = 1
Sboss07 = 1
rbwall007.IsDropped = false
roablock2a001.visible = True
mob001.Visible=True
mob002.Visible=True
mob003.Visible=True
mob004.Visible=True
mob005.Visible=True
mob006.Visible=True
mob007.Visible=True
wally001.Visible=True
wally002.Visible=True
wally003.Visible=True
wally004.Visible=True
wally005.Visible=True
wally006.Visible=True
wally007.Visible=True
wally001.Collidable=True
wally002.Collidable=True
wally003.Collidable=True
wally004.Collidable=True
wally005.Collidable=True
wally006.Collidable=True
wally007.Collidable=True
end sub

sub wally001_hit()
bosshit507
Playsound "MOB01"
mob001.Visible=False
wally001.Visible=False
wally001.Collidable=False
end sub

sub wally002_hit()
bosshit507
Playsound "MOB01"
mob002.Visible=False
wally002.Visible=False
wally002.Collidable=False
end sub

sub wally003_hit()
bosshit507
Playsound "MOB01"
mob003.Visible=False
wally003.Visible=False
wally003.Collidable=False
end sub

sub wally004_hit()
bosshit507
Playsound "MOB01"
mob004.Visible=False
wally004.Visible=False
wally004.Collidable=False
end sub

sub wally005_hit()
bosshit507
Playsound "MOB01"
mob005.Visible=False
wally005.Visible=False
wally005.Collidable=False
end sub

sub wally006_hit()
bosshit507
Playsound "MOB01"
mob006.Visible=False
wally006.Visible=False
wally006.Collidable=False
end sub

sub wally007_hit()
bosshit507
Playsound "MOB02"
mob007.Visible=False
wally007.Visible=False
wally007.Collidable=False
rbwall007.IsDropped = True
roablock2a001.visible = false
mob001.Visible=False
wally001.Visible=False
wally001.Collidable=False
c014.visible=true
Wcar007.Isdropped = False
end sub

sub Wcar007_hit()
Playsound "MOB03"
bhit = bhit + 1
checkbosshit07
exit sub
end sub

sub checkbosshit07
if bhit = 1 Then
bosshit407
exit sub
end if
if bhit = 2 Then
bosshit307
exit sub
end if
if bhit = 3 Then
bosshit207
exit sub
end if
if bhit = 4 Then
bosshit107
exit sub
end if
if bhit = 5 Then
b07= 1
Bcomplete = Bcomplete + 1
Bdone = Bdone + 1
li007.state = 1
bossesaway
checkbossescomplete
startroady
exit sub
end if
end sub

sub bosshit107
DMD "", "", "car07hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit001b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit207
DMD "", "", "car07hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit002b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit307
DMD "", "", "car07hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit003b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit407
DMD "", "", "car07hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit004b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit507
DMD "", "", "car07hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car07hit005b", eNone, eNone, eNone, 100, True, ""
end sub

'*******boss08***************

sub boss08
flashplaat2.ImageA="Ncar08"
Theadlight.enabled=True
BSmode = 1
Sboss08 = 1
Wcar008.Isdropped = False
c013.visible=true
TrotateI002.enabled=true
enablebh
end sub

sub enablebh
honey01.visible=true
beer01.visible=true
TBA011.enabled=true
TBA012.enabled=true
end sub

sub TBA011_hit()
TBA011.enabled= False
beer01.visible=False
bhit = bhit + 1
playsound "fx_POP"
checkbosshit08
end sub

sub TBA012_hit()
TBA012.enabled= False
honey01.visible=False
bhit = bhit + 1
playsound "Honey"
checkbosshit08
end sub

Sub checkbosshit08
	if Sboss08 = 1 and bhit = 2 then
		bvul = 1
	end if
end sub


sub Wcar008_hit()
	if Sboss08 = 1 and bvul = 0 then
	bosshit508
	playsound "HIL01"
	end if
	if Sboss08 = 1 and bvul = 1 then
		playsound "HIL02"
		bhurt = bhurt + 1
		checksboss08hurt
	end if
end sub


sub checksboss08hurt
	if bhurt = 1 then
		bhit = 0
		bvul = 0
		bosshit408
		enablebh
	end if
	if bhurt = 2 then
		bhit = 0
		bvul = 0
		bosshit308
		enablebh
	end if
	if bhurt = 3 then
		bhit = 0
		bvul = 0
		bosshit208
		enablebh
	end if
	if bhurt = 4 then
		bhit = 0
		bvul = 0
		bosshit108
		enablebh
	end if
	if bhurt = 5 then
		b08= 1
		Bcomplete = Bcomplete + 1
		Bdone = Bdone + 1
		li008.state = 1
		bossesaway
		checkbossescomplete
		startroady
		exit sub
	end if
end sub

sub bosshit108
DMD "", "", "car08hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit001b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit208
DMD "", "", "car08hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit002b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit308
DMD "", "", "car08hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit003b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit408
DMD "", "", "car08hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit004b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit508
DMD "", "", "car08hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car08hit005b", eNone, eNone, eNone, 100, True, ""
end sub

'*******boss09***************

sub boss09
flashplaat2.ImageA="Ncar09"
Theadlight.enabled=True
flashplaat2.ImageA="Ncar09"
li027.state=2
li023.state=2
BSmode = 1
Sboss09 = 1
end sub

Sub checkbosshit09
	if Sboss09 = 1 and bhit = 1 then
		bvul = 1
		bosshit509
		Wcar009.Isdropped = False
		c012.visible=true
	end if
end sub

sub Wcar009_hit()
		Wcar009.Isdropped = True
		c012.visible=False
		playsound "PET01"
		bhurt = bhurt + 1
		checksboss09hurt
end sub

sub checksboss09hurt
	if bhurt = 1 then
		bhit = 0
		bvul = 0
		bosshit409
	end if
	if bhurt = 2 then
		bhit = 0
		bvul = 0
		bosshit309
	end if
	if bhurt = 3 then
		bhit = 0
		bvul = 0
		bosshit209
	end if
	if bhurt = 4 then
		bhit = 0
		bvul = 0
		bosshit109
	end if
	if bhurt = 5 then
		b09= 1
		Bcomplete = Bcomplete + 1
		Bdone = Bdone + 1
		li009.state = 1
		bossesaway
		checkbossescomplete
		startroady		
	exit sub
	end if
end sub


sub bosshit109
DMD "", "", "car09hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit001b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit209
DMD "", "", "car09hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit002b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit309
DMD "", "", "car09hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit003b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit409
DMD "", "", "car09hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit004b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit509
DMD "", "", "car09hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car09hit005b", eNone, eNone, eNone, 100, True, ""
end sub

'*******boss10***************

sub boss10
flashplaat2.ImageA="Ncar10"
Theadlight.enabled=True
BSmode = 1
Sboss10 = 1
sawT002.enabled = false
Saw001.ObjRotZ = 1
Tzaagmoveup.enabled=true
TrotateI002.enabled=true
enablebeaver1
end sub

sub enablebeaver1
beaver01.X = TBA013.X
beaver01.Y = TBA013.Y
beaver01.visible = 1
TBA013.enabled=true
end Sub

sub enablebeaver2
beaver01.X = TBA014.X
beaver01.Y = TBA014.Y
beaver01.visible = 1
TBA014.enabled=true
end Sub

sub enablebeaver3
beaver01.X = TBA015.X
beaver01.Y = TBA015.Y
beaver01.visible = 1
TBA015.enabled=true
end Sub

sub TBA013_hit()
playsound "bever"
bhit = bhit + 1
TBA013.enabled=false
beaver01.visible = 0
checkbosshit10
end sub

sub TBA014_hit()
playsound "bever"
bhit = bhit + 1
TBA014.enabled=false
beaver01.visible = 0
checkbosshit10
end sub

sub TBA015_hit()
playsound "bever"
bhit = bhit + 1
TBA015.enabled=false
beaver01.visible = 0
checkbosshit10
end sub


Sub checkbosshit10
	if Sboss10 = 1 and bhit = 1 then
		bvul = 1
		bosshit510
		c011.visible=true
		Wcar010.Isdropped = False
	end if
end sub


sub Wcar010_hit()
		Playsound "BUZ02"
		bhurt = bhurt + 1
		c011.visible=false
		Wcar010.Isdropped = true
		checksboss10hurt
end sub

sub checksboss10hurt
	if bhurt = 1 then
		bhit = 0
		bvul = 0
		bosshit410
		enablebeaver3
	end if
	if bhurt = 2 then
		bhit = 0
		bvul = 0
		bosshit310
		enablebeaver2
	end if
	if bhurt = 3 then
		bhit = 0
		bvul = 0
		bosshit210
		enablebeaver1
	end if
	if bhurt = 4 then
		bhit = 0
		bvul = 0
		bosshit110
		enablebeaver3
	end if
	if bhurt = 5 then
		sawT001.Enabled = 0
		Saw.ObjRotZ = 1
		sawmovedown
		b10= 1
		Bcomplete = Bcomplete + 1
		Bdone = Bdone + 1
		li010.state = 1
		bossesaway
		checkbossescomplete
		startroady
		exit sub
	end if
end sub

sub bosshit110
DMD "", "", "car10hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit001b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit001a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit001b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit210
DMD "", "", "car10hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit002b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit002a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit002b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit310
DMD "", "", "car10hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit003b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit003a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit003b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit410
DMD "", "", "car10hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit004b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit004a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit004b", eNone, eNone, eNone, 100, True, ""
end sub

sub bosshit510
DMD "", "", "car10hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit005b", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit005a", eNone, eNone, eNone, 100, True, ""
DMD "", "", "car10hit005b", eNone, eNone, eNone, 100, True, ""
end sub


sub checkbossescomplete
if Bcomplete = 2 Then
loadpassby
vpmTimer.AddTimer 1700, "place10'"
exit sub
end if
if Bcomplete = 3 Then
loadpassby
vpmTimer.AddTimer 1700, "place09'"
exit sub
end if
if Bcomplete = 4 Then
loadpassby
vpmTimer.AddTimer 1700, "place08'"
exit sub
end if
if Bcomplete = 5 Then
loadpassby
vpmTimer.AddTimer 1700, "place07'"
exit sub
end if
if Bcomplete = 6 Then
loadpassby
vpmTimer.AddTimer 1700, "place06'"
exit sub
end if
if Bcomplete = 7 Then
loadpassby
vpmTimer.AddTimer 1700, "place05'"
exit sub
end if
if Bcomplete = 8 Then
loadpassby
vpmTimer.AddTimer 1700, "place04'"
exit sub
end if
if Bcomplete = 9 Then
loadpassby
vpmTimer.AddTimer 1700, "place03'"
exit sub
end if
if Bcomplete = 10 Then
loadpassby
vpmTimer.AddTimer 1700, "place02'"
exit sub
end if
if Bcomplete = 11 Then
loadpassby
vpmTimer.AddTimer 1700, "place01'"
vpmTimer.AddTimer 1800, "Resetbosses'"
exit sub
end if
end sub

sub loadpassby
playsound "passby"
startB2S(28)
end sub

sub place10
startB2S(38)
raceplace = 10
DMD "", "", "10dmd10", eNone, eNone, eNone, 1500, True, ""
vpmTimer.AddTimer 1500, "changeb2sback2begin'"
end sub

sub place09
startB2S(37)
raceplace = 9
DMD "", "", "9dmd", eNone, eNone, eNone, 1500, True, ""
vpmTimer.AddTimer 1500, "changeb2sback2begin'"
end sub

sub place08
startB2S(36)
raceplace = 8
DMD "", "", "8dmd", eNone, eNone, eNone, 1500, True, ""
vpmTimer.AddTimer 1500, "changeb2sback2begin'"
end sub

sub place07
startB2S(35)
raceplace = 7
DMD "", "", "7dmd", eNone, eNone, eNone, 1500, True, ""
vpmTimer.AddTimer 1500, "changeb2sback2begin'"
end sub

sub place06
startB2S(34)
raceplace = 6
DMD "", "", "6dmd", eNone, eNone, eNone, 1500, True, ""
vpmTimer.AddTimer 1500, "changeb2sback2begin'"
end sub

sub place05
startB2S(33)
raceplace = 5
DMD "", "", "5dmd", eNone, eNone, eNone, 1500, True, ""
vpmTimer.AddTimer 1500, "changeb2sback2begin'"
end sub

sub place04
startB2S(32)
raceplace = 4
DMD "", "", "4dmd", eNone, eNone, eNone, 1500, True, ""
vpmTimer.AddTimer 1500, "changeb2sback2begin'"
end sub

sub place03
startB2S(31)
raceplace = 3
DMD "", "", "3dmd3", eNone, eNone, eNone, 1500, True, ""
vpmTimer.AddTimer 1500, "changeb2sback2begin'"
end sub

sub place02
startB2S(30)
raceplace = 2
DMD "", "", "2dmd2", eNone, eNone, eNone, 1500, True, ""
vpmTimer.AddTimer 1500, "changeb2sback2begin'"
end sub

sub place01
startB2S(29)
raceplace = 1
DMD "", "", "1dmd2", eNone, eNone, eNone, 1500, True, ""
vpmTimer.AddTimer 1500, "changeb2sback2begin'"
end sub


sub bossesaway
bhit = 0
bvul = 0
bhurt = 0
rbwall007.IsDropped = True
roablock2a001.visible = false
mob001.Visible=false
mob002.Visible=false
mob003.Visible=false
mob004.Visible=false
mob005.Visible=false
mob006.Visible=false
mob007.Visible=false
wally001.Visible=false
wally002.Visible=false
wally003.Visible=false
wally004.Visible=false
wally005.Visible=false
wally006.Visible=false
wally007.Visible=false
wally001.Collidable=false
wally002.Collidable=false
wally003.Collidable=false
wally004.Collidable=false
wally005.Collidable=false
wally006.Collidable=false
wally007.Collidable=false
li027.state=0
li023.state=0
bombmode01.visible=false
rock001.visible=false
rock002.visible=false
rock003.visible=false
bat001.visible=false
hammer01.visible=false
bar01.visible=false
paperplany.visible=false
lipstick01.visible=false
wbox.visible=false
beaver01.visible=false
honey01.visible=false
beer01.visible=false
Sboss01 = 0
Sboss02 = 0
Sboss03 = 0
Sboss04 = 0
Sboss05 = 0
Sboss06 = 0
Sboss07 = 0
Sboss08 = 0
Sboss09 = 0
Sboss10 = 0
Sboss11 = 0
Theadlight.enabled=false
BSmode = 0
TrotateI002.enabled=false
c021.visible=false
c020.visible=false
c019.visible=false
c018.visible=false
c017.visible=false
c016.visible=false
c015.visible=false
c014.visible=false
c013.visible=false
c012.visible=false
c011.visible=false
Wcar001.Isdropped = True
Wcar002.Isdropped = True
Wcar003.Isdropped = True
Wcar004.Isdropped = True
Wcar005.Isdropped = True
Wcar006.Isdropped = True
Wcar007.Isdropped = True
Wcar008.Isdropped = True
Wcar009.Isdropped = True
Wcar010.Isdropped = True
Wcar011.Isdropped = True
TBA001.enabled=false
TBA002.enabled=false
TBA003.enabled=false
TBA004.enabled=false
TBA005.enabled=false
TBA006.enabled=false
TBA007.enabled=false
TBA008.enabled=false
TBA009.enabled=false
TBA010.enabled=false
TBA011.enabled=false
TBA012.enabled=false
TBA013.enabled=false
TBA014.enabled=false
TBA015.enabled=false
Pbossy=0
end sub

sub Resetbosses
Bcomplete = 1
b00= 0
b01= 0
b02= 0
b03= 0
b04= 0
b05= 0
b06= 0
b07= 0
b08= 0
b09= 0
b10= 0
end sub

'*********************************************************
'**************Wacky challange kicker**********************
'*********************************************************
dim SlotValue2,SelectCounter2:SelectCounter2=0
Dim BallInHole
dim cmbll:cmbll=0
dim mmbb:mmbb=0

sub VUKGH007_hit()
	'BlinkGI 4,150
	BallInHole = BallInHole + 1
	VUKGH007.DestroyBall
	cmbll = cmbll + 1
	if cmbll => 11 and wackychallanges = 1 and bMultiBallMode = false and BSmode = 0 then 
		BallInHole = 0
		startB2S(22)
		flashplaat2.ImageA="Nwm"
		playsound "bombmb"
		vpmTimer.AddTimer 3700, "checkcmbll'"
		exit sub
	end if
	playsound "gochg"
	if wackychallanges = 2 or bMultiBallMode = true or BSmode = 1 then 
		vpmTimer.AddTimer 1000, "kickeroutwackychallange'"
		exit Sub
	end if
	TrotateI001.enabled = true
	StopSong
	flashplaat2.ImageA="Npause"
	startB2S(12)
	Playsong "challangem"
	DMDFlush
	PlayerSelectActive2 = True
	autockickout007.enabled = True
	DMD "", "", "cchoose", eNone, eNone, eNone, 10000, False, ""
end sub

sub checkcmbll
	startbomby
	mmbb=1
	BallInHole = 11
	BallsOnPlayfield = 11
	bMultiBallMode = True
	kickeroutwackychallange
	cmbll = 0
end sub

sub autockickout007_timer
	UpdateMusicNow
	changeb2sback2begin
	startroady
	autockickout007.enabled = False
	PlayerSelectActive2 = False
	kickeroutwackychallange
	DMDFlush
	DMDScore
End Sub

sub multiballoutkicker
FlashForMs Flasher004, 1000, 50, 0
VUKGH007.CreateSizedball BallSize / 2
PlaySoundAt SoundFXDOF("fx_popper", 120, DOFPulse, DOFContactors), VUKGH007
VUKGH007.Kick 220, 2, 1
end sub

sub kickeroutwackychallange
    If BallInHole> 0 Then
		FlashForMs Flasher004, 1000, 50, 0
        BallInHole = BallInHole - 1
        VUKGH007.CreateSizedball BallSize / 2
        'UpdateBallImage
        PlaySoundAt SoundFXDOF("fx_popper", 114, DOFPulse, DOFContactors), VUKGH007
        DOF 121, DOFPulse
        VUKGH007.Kick 210, 2, 1
        vpmtimer.addtimer 1000, "kickeroutwackychallange '" 'repeat until all the balls are kicked out
    End If
End Sub


Sub SelectPlayerStart2(Keycode)
   If keycode = LeftFlipperKey then
		Playsound "select"
        SelectCounter2 = SelectCounter2 - 1
        If SelectCounter2 = -1 Then SelectCounter2 = 13
    end If
    If keycode = RightFlipperKey then
		Playsound "select"
        SelectCounter2 = SelectCounter2 + 1
        If SelectCounter2 = 14 Then SelectCounter2 = 0
    end If
    Select Case SelectCounter2
        Case 0
			if p20 = 1 then
            DMDFlush
            DMD "", "", "c1l", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 0
			end if
			if p20 = 0 then 
            DMDFlush
            DMD "", "", "c1", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 0
			end if
        Case 1
			if p31 = 1 then
            DMDFlush
            DMD "", "", "tm1ldmd", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 1
			end if
			if p31 = 0 then 
            DMDFlush
            DMD "", "", "tm1dmd", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 1
			end if
        Case 2
			if p22 = 1 then
            DMDFlush
            DMD "", "", "c3l", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 2
			end if
			if p22 = 0 then 
            DMDFlush
            DMD "", "", "c3", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 2
			end if
        Case 3
			if p23 = 1 then
            DMDFlush
            DMD "", "", "c4l", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 3
			end if
			if p23 = 0 then 
            DMDFlush
            DMD "", "", "c4", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 3
			end if
        Case 4
			if p24 = 1 then
            DMDFlush
            DMD "", "", "c5l", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 4
			end if
			if p24 = 0 then 
            DMDFlush
            DMD "", "", "c5", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 4
			end if
        Case 5
			if p25 = 1 then
            DMDFlush
            DMD "", "", "c6l", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 5
			end if
			if p25 = 0 then 
            DMDFlush
            DMD "", "", "c6", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 5
			end if
        Case 6
			if p26 = 1 then
            DMDFlush
            DMD "", "", "c7l", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 6
			end if
			if p26 = 0 then 
            DMDFlush
            DMD "", "", "c7", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 6
			end if
        Case 7
			if p32 = 1 then
            DMDFlush
            DMD "", "", "tml2dmd", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 7
			end if
			if p32 = 0 then 
            DMDFlush
            DMD "", "", "tm2dmd", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 7
			end if
        Case 8
			if p28 = 1 then
            DMDFlush
            DMD "", "", "c9l", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 8
			end if
			if p28 = 0 then 
            DMDFlush
            DMD "", "", "c9", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 8
			end if
        Case 9
			if p29 = 1 then
            DMDFlush
            DMD "", "", "c10l", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 9
			end if
			if p29 = 0 then 
            DMDFlush
            DMD "", "", "c10", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 9
			end if
        Case 10
			if p33 = 1 then
            DMDFlush
            DMD "", "", "tml3dmd", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 10
			end if
			if p33 = 0 then 
            DMDFlush
            DMD "", "", "tm3dmd", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 10
			end if
        Case 11
			if p21 = 1 then
            DMDFlush
            DMD "", "", "c2l", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 11
			end if
			if p21 = 0 then 
            DMDFlush
            DMD "", "", "c2", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 11
			end if
        Case 12
			if p27 = 1 then
            DMDFlush
            DMD "", "", "c8l", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 12
			end if
			if p27 = 0 then 
            DMDFlush
            DMD "", "", "c8", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 12
			end if
        Case 13
			if p30 = 1 then
            DMDFlush
            DMD "", "", "c11l", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 13
			end if
			if p30 = 0 then 
            DMDFlush
            DMD "", "", "c11", eNone, eNone, eNone, 10000, False, ""
            SlotValue2 = 13
			end if
    End Select

    If keycode = PlungerKey Then
        Select Case SlotValue2
            Case 0
				if p20 = 1 then
				playsound "locked"
				exit sub
				end if
				startB2S(5)
				PlayerSelectActive2 = False
				DMDFlush
				StartMud
            Case 1
				if p31 = 1 then
				playsound "locked"
				exit sub
				end if
				PlayerSelectActive2 = False
				DMDFlush
				StartTrophy1
            Case 2
				if p22 = 1 then
				playsound "locked"
				exit sub
				end if
				startB2S(7)
				PlayerSelectActive2 = False
				DMDFlush
				StartRoadclosure1
            Case 3
				if p23 = 1 then
				playsound "locked"
				exit sub
				end if
				startB2S(9)
				PlayerSelectActive2 = False
				DMDFlush
				StartSmokescreen1
            Case 4
				if p24 = 1 then
				playsound "locked"
				exit sub
				end if
				startB2S(12)
				PlayerSelectActive2 = False
				DMDFlush
				StartCon
            Case 5
				if p25 = 1 then
				playsound "locked"
				exit sub
				end if
				startB2S(13)
				PlayerSelectActive2 = False
				DMDFlush
				StartRoadclosure2
            Case 6
				if p26 = 1 then
				playsound "locked"
				exit sub
				end if
				startB2S(14)
				PlayerSelectActive2 = False
				DMDFlush
				StartSmokescreen2
            Case 7
				if p32 = 1 then
				playsound "locked"
				exit sub
				end if
				PlayerSelectActive2 = False
				DMDFlush
				StartTrophy2
            Case 8
				if p28 = 1 then
				playsound "locked"
				exit sub
				end if
				startB2S(16)
				PlayerSelectActive2 = False
				DMDFlush
				StartSmokescreen3
            Case 9
				if p29 = 1 then
				playsound "locked"
				exit sub
				end if
				startB2S(22)
				PlayerSelectActive2 = False
				DMDFlush
				StartPidgeon
            Case 10
				if p33 = 1 then
				playsound "locked"
				exit sub
				end if
				PlayerSelectActive2 = False
				DMDFlush
				StartTrophy3
			case 11
				if p21 = 1 then
				playsound "locked"
				exit sub
				end if
				startB2S(6)
				PlayerSelectActive2 = False
				DMDFlush
				StartOil
			case 12
				if p27 = 1 then
				playsound "locked"
				exit sub
				end if
				startB2S(15)
				PlayerSelectActive2 = False
				DMDFlush
				StartRoadclosure3
			case 13
				if p30 = 1 then
				playsound "locked"
				exit sub
				end if
				startB2S(4)
				PlayerSelectActive2 = False
				DMDFlush
				StartGas
        End Select
		autockickout007.enabled = False
    end if
end sub


Sub Checkchallange()
If ChallangeSlot = 14 Then
DMD "", "", "wccs", eNone, eNone, eNone, 1000, True, ""
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
ChallangeSlot = 0
end if
End Sub

sub challangenewgame
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
ChallangeSlot = 0
End Sub

'********challange 1***************
Sub StartMud
startB2S(17)
flashplaat2.ImageA="Nmud"
StopSong
Mud001.image="muds"
StartyMud
end sub

Sub StartyMud
wackychallanges = 2
     DMD CL(0, "HIT 5 MUD SPOTS"), CL(1, "COMPLETE CHALLANGE"), "", eNone, eNone, eNone, 2000, True, "mud4"
	vpmTimer.AddTimer 2000, "startchallange1 '"
end sub

sub startchallange1
UpdateMusicNow
mode1TimerCount = 60
mode1timer.Enabled = 1
EnableMuds()
MudChecker = 0
kickeroutwackychallange
end sub

Dim WhichMud, MudChecker
WhichMud = 0
MudChecker = 0
sub EnableMuds()
	If MudChecker = 31 Then
		CheckBonusMud()
		Exit Sub
	End If
	Randomize()
	WhichMud = INT(RND * 5) + 1
	Select Case WhichMud
		Case 3
			WhichMud = 4
		Case 4
			WhichMud = 8
		Case 5
			WhichMud = 16
	End Select
	Do While (WhichMud AND MudChecker) > 0
		WhichMud = INT(RND * 5) + 1
		Select Case WhichMud
			Case 3
				WhichMud = 4
			Case 4
				WhichMud = 8
			Case 5
				WhichMud = 16
		End Select
	Loop
	Select Case WhichMud
		Case 1
			ts2item001.enabled = 1
			Mud001.Visible = 1
			Mud001.X = ts2item001.X
			Mud001.Y = ts2item001.Y
		Case 2
			ts2item002.enabled = 1
			Mud001.Visible = 1
			Mud001.X = ts2item002.X
			Mud001.Y = ts2item002.Y
		Case 4
			ts2item003.enabled = 1
			Mud001.Visible = 1
			Mud001.X = ts2item003.X
			Mud001.Y = ts2item003.Y
		Case 8
			ts2item004.enabled = 1
			Mud001.Visible = 1
			Mud001.X = ts2item004.X
			Mud001.Y = ts2item004.Y
		Case 16
			ts2item005.enabled = 1
			Mud001.Visible = 1
			Mud001.X = ts2item005.X
			Mud001.Y = ts2item005.Y
	End Select
end sub

sub ts2item001_hit()
	ts2item001.enabled = 0
	MoveMudDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "mudhit"
	addextratime()
	MudChecker = (MudChecker OR 1)
	EnableMuds()
end sub

sub MoveMudDown()
	Dim X
	For Each X in Muds
		X.Visible = 0
	Next
end sub

sub ts2item002_hit()
	ts2item002.enabled = 0
	MoveMudDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "mudhit"
	addextratime()
	MudChecker = (MudChecker OR 2)
	EnableMuds()
end sub

sub ts2item003_hit()
	ts2item003.enabled = 0
	MoveMudDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "mudhit"
	addextratime()
	MudChecker = (MudChecker OR 4)
	EnableMuds()
end sub

sub ts2item004_hit()
	ts2item004.enabled = 0
	MoveMudDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "mudhit"
	addextratime()
	MudChecker = (MudChecker OR 8)
	EnableMuds()
end sub

sub ts2item005_hit()
	ts2item005.enabled = 0
	MoveMudDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "mudhit"
	addextratime()
	MudChecker = (MudChecker OR 16)
	EnableMuds()
end sub

sub CheckBonusMud()
	If MudChecker = 31 then
		DMD "", "", "300kdmd", eNone, eNone, eNone, 1000, True, "v_winner"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		Stopmode1
		CHcompleted = CHcompleted + 1
		MudChecker = 0
		ChallangeSlot = ChallangeSlot + 1
		wackychallanges = 1
		p20 = 1
		li050.state = 1
		startroady
		changeb2sback2begin
		Checkchallange()
	end if
end sub

sub changeb2sback2begin
startB2S(27)
end sub

'********challange 2***************

Dim BallInHole8
Dim bonustime:bonustime=0

sub StartTrophy1
startB2S(13)
flashplaat2.ImageA="Nht"
StopSong
StartyTrophy1
Theadlight2.enabled = True
Ttropheemania.enabled = True
end sub

sub StartyTrophy1
wackychallanges = 2
     DMD CL(0, "HIT THE TROPHYS"), CL(1, "COMPLETE CHALLANGE"), "", eNone, eNone, eNone, 1000, True, "backtotheplottingboard"
	vpmTimer.AddTimer 1100, "startchallange12 '"
end sub

sub startchallange12
ttrophee.enabled = True

Playsong "zombie"
headDir = 1
headdownTimer.enabled = 1
vpmtimer.addtimer 2000, "kickbonus '"
vpmtimer.addtimer 2200, "enabletropy1 '"
end sub


sub enabletropy1
trophy001.Visible = True
trophy001.X = tph001.X
trophy001.Y = tph001.Y
trophy001.Z = -40
tph001.enabled = 1
end sub

sub tph001_hit()
playsound "tropheehit"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
tph001.enabled = 0
trophy001.X = tph002.X
trophy001.Y = tph002.Y
trophy001.Z = -40
tph002.enabled = 1
end sub

sub tph002_hit()
playsound "tropheehit"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
tph002.enabled = 0
trophy001.X = tph003.X
trophy001.Y = tph003.Y
trophy001.Z = -40
tph003.enabled = 1
end sub

sub tph003_hit()
playsound "tropheehit"
ttrophee.enabled = false
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
tph003.enabled = 0
trophy001.Visible = False
bonustime = 0
		DMD "", "", "300kdmd", eNone, eNone, eNone, 1000, True, "v_winner"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (300000*PFMultiplier)
		Stopmode1
		CHcompleted = CHcompleted + 1
		ChallangeSlot = ChallangeSlot + 1
		wackychallanges = 1
		p31 = 1
Checkchallange()
startroady
RightFlipper001.RotatetoStart
LeftFlipper001.RotatetoStart
LowerFlippersActive = False
Theadlight2.enabled = false
Ttropheemania.enabled = false
end sub


Sub kickbonus
	If BallInHole> 0 Then
        BallInHole = BallInHole - 1
	VUKGH008.CreateSizedball BallSize / 2
	LowerFlippersActive = True
	Playsound "fx_popper"
	bonustime = 1
	VUKGH008.Kick 8, 28, 1
 vpmtimer.addtimer 1000, "kickbonus '"
end if
end sub

sub VUKGH008_hit()
	if bonustime = 1 then
	continuebonus
	exit Sub
	end if
	VUKGH008.DestroyBall
	UpdateMusicNow
	changeb2sback2begin
	BallInHole8 = BallInHole8 + 1
	Playsound "fx_ballrampdrop"
	headDir = 1
	headupTimer.enabled = 1
	vpmtimer.addtimer 2100, "SuperVukAddBall8 '" 
end sub

Sub SuperVukAddBall8()
	'UpdateMusicNow
	If BallInHole8> 0 Then
        BallInHole8 = BallInHole8 - 1
	VUKGH007.CreateSizedball BallSize / 2
	disablebonusgames
	flashplaat2.ImageA = "Nnowhere"
	Playsound "fx_popper"
	VUKGH007.Kick 190, 2, 1
 vpmtimer.addtimer 1000, "SuperVukAddBall8 '" 
end If
End Sub

sub continuebonus
Playsound "fx_popper"
VUKGH008.Kick 8, 28, 1
end sub

Sub disablebonusgames
bonustime = 0
end sub

sub ttrophee_timer()
roundgo
end sub

Sub roundgo
roundy001
vpmtimer.addtimer 200, "roundy002 '"
vpmtimer.addtimer 400, "roundy003 '"
vpmtimer.addtimer 600, "roundy004 '"
vpmtimer.addtimer 800, "roundy005 '"
end sub   

sub roundy001
Flash1 True
end sub

sub roundy002
Flash2 True
end sub

sub roundy003
Flash3 True
end sub

sub roundy004
Flash4 True
end sub

sub roundy005
Flash5 True
end sub

sub Theadlight2_timer()
LightSeqHead.Play SeqClockRightOn, 180
end sub

sub Ttropheemania_timer()
Randommind
end sub

Sub Randommind
    Dim tmp
    tmp = INT(RND * 24) + 1
    Flasher001.ImageA="text_" &tmp
End Sub

'********challange 3***************
dim road1cls:road1cls=0
sub StartRoadclosure1
startB2S(15)
flashplaat2.ImageA="Nrb"
StopSong
StartyRoadclosure1
end sub

sub StartyRoadclosure1
wackychallanges = 2
     DMD CL(0, "DONT HIT THE"), CL(1, "ROAD BLOCKS"), "", eNone, eNone, eNone, 2000, True, "roadblock3"
	vpmTimer.AddTimer 2000, "startchallange3 '"
end sub

sub startchallange3
road1cls=1
li028.state = 2
rbwall001.IsDropped = false
rbwall002.IsDropped = false
roablock1a.visible = True
roablock1b.visible = True
UpdateMusicNow
mode1TimerCount = 30
mode1timer.Enabled = 1
kickeroutwackychallange
end sub

sub rbwall001_hit()
playsound "v_timeextended2"
addextratime
end sub

sub rbwall002_hit()
playsound "v_timeextended2"
addextratime
end sub

sub StopRoadblock1
		DMD "", "", "300kdmd", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (300000*PFMultiplier)
		road1cls=0
		p22 = 1	
		CHcompleted = CHcompleted + 1
		ChallangeSlot = ChallangeSlot + 1
		wackychallanges = 1
		changeb2sback2begin
		Checkchallange()
		Stopmode1
		li052.state = 1
		startroady
end sub

'********challange 4***************
Sub StartSmokescreen1
startB2S(14)
flashplaat2.ImageA="Nsmoke"
StopSong
StartySmokescreen1
end sub

Sub StartySmokescreen1
wackychallanges = 2
     DMD CL(0, "SURVIVE THE"), CL(1, "SMOKESCREEN"), "", eNone, eNone, eNone, 2000, True, "fogbank"
	vpmTimer.AddTimer 2000, "startchallange4 '"
end sub

sub startchallange4
UpdateMusicNow
mode1TimerCount = 20
mode1timer.Enabled = 1
Smokescreen1Checker = 0
smokeyT001.enabled = True
kickeroutwackychallange
end sub

dim Smokescreen1Checker
sub smokeyT001_Timer
Smokescreen1Checker = Smokescreen1Checker + 1 : If Smokescreen1Checker > 20 then stopSmokescreen1: end If 
select case Smokescreen1Checker
				case 1 :pfflashert.ImageA="sb1":pfflashert.visible=True
				case 2 :pfflashert.ImageA="sb1"
				case 3 :pfflashert.ImageA="sb1"
				case 4 :pfflashert.ImageA="sb2"
				case 5 :pfflashert.ImageA="sb2"
				case 6 :pfflashert.ImageA="sb2"
				case 7 :pfflashert.ImageA="sb3"
				case 8 :pfflashert.ImageA="sb3"
				case 9 :pfflashert.ImageA="sb4"
				case 10 :pfflashert.ImageA="sb4"
				case 11 :pfflashert.ImageA="sb4"
				case 12 :pfflashert.ImageA="sb4"
				case 13 :pfflashert.ImageA="sb3"
				case 14 :pfflashert.ImageA="sb3"
				case 15 :pfflashert.ImageA="sb2"
				case 16 :pfflashert.ImageA="sb2"
				case 17 :pfflashert.ImageA="sb2"
				case 18 :pfflashert.ImageA="sb1"
				case 19 :pfflashert.ImageA="sb1"
				case 20 :pfflashert.ImageA="sb1"
			end Select
End Sub

sub stopSmokescreen1
		DMD "", "", "300kdmd", eNone, eNone, eNone, 1000, True, "v_winner"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (300000*PFMultiplier)
		p23 = 1
		smokeyT001.enabled = False	
		CHcompleted = CHcompleted + 1
		ChallangeSlot = ChallangeSlot + 1
		wackychallanges = 1
		Smokescreen1Checker = 0
		changeb2sback2begin
		Checkchallange()
		Stopmode1
		li055.state = 1
		startroady
end sub

'********challange 5***************

Sub StartCon
startB2S(20)
flashplaat2.ImageA="Ncone"
StopSong
Startycone
end sub

Sub Startycone
wackychallanges = 2
     DMD CL(0, "HIT 5 CONES"), CL(1, "COMPLETE CHALLANGE"), "", eNone, eNone, eNone, 2000, True, "CONESbegin2"
	vpmTimer.AddTimer 2000, "startchallange5 '"
end sub

sub startchallange5
UpdateMusicNow
mode1TimerCount = 60
mode1timer.Enabled = 1
Enablecones()
coneChecker = 0
kickeroutwackychallange
end sub

Dim Whichcone, coneChecker
Whichcone = 0
coneChecker = 0
sub Enablecones()
	If coneChecker = 31 Then
		CheckBonuscone()
		Exit Sub
	End If
	Randomize()
	Whichcone = INT(RND * 5) + 1
	Select Case Whichcone
		Case 3
			Whichcone = 4
		Case 4
			Whichcone = 8
		Case 5
			Whichcone = 16
	End Select
	Do While (Whichcone AND coneChecker) > 0
		Whichcone = INT(RND * 5) + 1
		Select Case Whichcone
			Case 3
				Whichcone = 4
			Case 4
				Whichcone = 8
			Case 5
				Whichcone = 16
		End Select
	Loop
	Select Case Whichcone
		Case 1
			ts5item001.enabled = 1
			cone01.Visible = 1
			cone01.X = ts5item001.X
			cone01.Y = ts5item001.Y
		Case 2
			ts5item002.enabled = 1
			cone01.Visible = 1
			cone01.X = ts5item002.X
			cone01.Y = ts5item002.Y
		Case 4
			ts5item003.enabled = 1
			cone01.Visible = 1
			cone01.X = ts5item003.X
			cone01.Y = ts5item003.Y
		Case 8
			ts5item004.enabled = 1
			cone01.Visible = 1
			cone01.X = ts5item004.X
			cone01.Y = ts5item004.Y
		Case 16
			ts5item005.enabled = 1
			cone01.Visible = 1
			cone01.X = ts5item005.X
			cone01.Y = ts5item005.Y
	End Select
end sub

sub ts5item001_hit()
	ts5item001.enabled = 0
	MoveconeDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "conehit"
	addextratime()
	coneChecker = (coneChecker OR 1)
	Enablecones()
end sub

sub MoveconeDown()
	Dim X
	For Each X in cones
		X.Visible = 0
	Next
end sub

sub ts5item002_hit()
	ts5item002.enabled = 0
	MoveconeDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "conehit"
	addextratime()
	coneChecker = (coneChecker OR 2)
	Enablecones()
end sub

sub ts5item003_hit()
	ts5item003.enabled = 0
	MoveconeDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "conehit"
	addextratime()
	coneChecker = (coneChecker OR 4)
	Enablecones()
end sub

sub ts5item004_hit()
	ts5item004.enabled = 0
	MoveconeDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "conehit"
	addextratime()
	coneChecker = (coneChecker OR 8)
	Enablecones()
end sub

sub ts5item005_hit()
	ts5item005.enabled = 0
	MoveconeDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "conehit"
	addextratime()
	coneChecker = (coneChecker OR 16)
	Enablecones()
end sub

sub CheckBonuscone()
	If coneChecker = 31 then
		DMD "", "", "300kdmd", eNone, eNone, eNone, 1000, True, "v_winner"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (300000*PFMultiplier)
		Stopmode1
		CHcompleted = CHcompleted + 1
		coneChecker = 0
		ChallangeSlot = ChallangeSlot + 1
		wackychallanges = 1
		p24 = 1
		li048.state = 1
		changeb2sback2begin
		Checkchallange()
		startroady
	end if
end sub

'********challange 6***************

dim road2cls:road2cls=0
sub StartRoadclosure2
startB2S(15)
flashplaat2.ImageA="Nrb"
StopSong
StartyRoadclosure2
end sub

sub StartyRoadclosure2
wackychallanges = 2
     DMD CL(0, "DONT HIT THE"), CL(1, "ROAD BLOCKS"), "", eNone, eNone, eNone, 2000, True, "roadblock2"
	vpmTimer.AddTimer 2000, "startchallange6 '"
end sub

sub startchallange6
road2cls=1
li022.state = 2
li024.state = 2
rbwall003.IsDropped = false
rbwall004.IsDropped = false
roablock2a.visible = True
roablock2b.visible = True
UpdateMusicNow
mode1TimerCount = 30
mode1timer.Enabled = 1
kickeroutwackychallange
end sub

sub rbwall003_hit()
if sboss07 = 1 then
playsound ""
exit sub
end if 
playsound "v_timeextended2"
addextratime
end sub

sub rbwall004_hit()
playsound "v_timeextended2"
addextratime
end sub

sub StopRoadblock2
		DMD "", "", "300kdmd", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (300000*PFMultiplier)
		p25 = 1
		road2cls=0
		'playsound "knocker"
		KnockerSolenoid ' knocker sound
		CHcompleted = CHcompleted + 1
		ChallangeSlot = ChallangeSlot + 1
		wackychallanges = 1
		changeb2sback2begin
		Checkchallange()
		Stopmode1
		li053.state = 1
		startroady
end sub

'********challange 7***************
Sub StartSmokescreen2
startB2S(14)
flashplaat2.ImageA="Nsmoke"
StopSong
StartySmokescreen2
end sub

Sub StartySmokescreen2
wackychallanges = 2
     DMD CL(0, "SURVIVE THE"), CL(1, "SMOKESCREEN"), "", eNone, eNone, eNone, 2000, True, "fogbank"
	vpmTimer.AddTimer 2000, "startchallange7 '"
end sub

sub startchallange7
UpdateMusicNow
mode1TimerCount = 20
mode1timer.Enabled = 1
Smokescreen2Checker = 0
smokeyT002.enabled = True
kickeroutwackychallange
end sub

dim Smokescreen2Checker
sub smokeyT002_Timer
Smokescreen2Checker = Smokescreen2Checker + 1 : If Smokescreen2Checker > 20 then stopSmokescreen2: end If 
select case Smokescreen2Checker
				case 1 :pfflashert.ImageA="sm1":pfflashert.visible=True
				case 2 :pfflashert.ImageA="sm1"
				case 3 :pfflashert.ImageA="sm1"
				case 4 :pfflashert.ImageA="sm2"
				case 5 :pfflashert.ImageA="sm2"
				case 6 :pfflashert.ImageA="sm2"
				case 7 :pfflashert.ImageA="sm3"
				case 8 :pfflashert.ImageA="sm3"
				case 9 :pfflashert.ImageA="sm4"
				case 10 :pfflashert.ImageA="sm4"
				case 11 :pfflashert.ImageA="sm4"
				case 12 :pfflashert.ImageA="sm4"
				case 13 :pfflashert.ImageA="sm3"
				case 14 :pfflashert.ImageA="sm3"
				case 15 :pfflashert.ImageA="sm2"
				case 16 :pfflashert.ImageA="sm2"
				case 17 :pfflashert.ImageA="sm2"
				case 18 :pfflashert.ImageA="sm1"
				case 19 :pfflashert.ImageA="sm1"
				case 20 :pfflashert.ImageA="sm1"
			end Select
End Sub


sub stopSmokescreen2
		DMD "", "", "300kdmd", eNone, eNone, eNone, 1000, True, "v_winner"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (300000*PFMultiplier)
		smokeyT002.enabled = False
		Stopmode1
		CHcompleted = CHcompleted + 1
		ChallangeSlot = ChallangeSlot + 1
		wackychallanges = 1
		Smokescreen2Checker = 0
		p26 = 1
		changeb2sback2begin
		Checkchallange()
		li058.state = 1
		startroady
end sub

'********challange 8***************
sub StartTrophy2
startB2S(13)
flashplaat2.ImageA="Nht"
StopSong
StartyTrophy2
Theadlight2.enabled = True
Ttropheemania.enabled = true
end sub

sub StartyTrophy2
wackychallanges = 2
     DMD CL(0, "HIT THE TROPHYS"), CL(1, "COMPLETE CHALLANGE"), "", eNone, eNone, eNone, 1000, True, "idea"
	vpmTimer.AddTimer 1100, "startchallange11 '"
end sub

sub startchallange11
ttrophee.enabled = True
Playsong "zombie"
headDir = 1
headdownTimer.enabled = 1
vpmtimer.addtimer 2000, "kickbonus '"
vpmtimer.addtimer 2200, "enabletropy2 '"
end sub


sub enabletropy2
trophy001.Visible = True
trophy001.X = tph004.X
trophy001.Y = tph004.Y
trophy001.Z = -40
tph004.enabled = 1
end sub

sub tph004_hit()
playsound "tropheehit"
tph004.enabled = 0
trophy001.X = tph005.X
trophy001.Y = tph005.Y
trophy001.Z = -40
tph005.enabled = 1
end sub

sub tph005_hit()
playsound "tropheehit"
tph005.enabled = 0
trophy001.X = tph006.X
trophy001.Y = tph006.Y
trophy001.Z = -40
tph006.enabled = 1
end sub

sub tph006_hit()
Theadlight2.enabled = false
RightFlipper001.RotatetoStart
LeftFlipper001.RotatetoStart
LowerFlippersActive = False
playsound "tropheehit"
tph006.enabled = 0
trophy001.Visible = False
bonustime = 0
		DMD "", "", "300kdmd", eNone, eNone, eNone, 1000, True, "v_winner"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (300000*PFMultiplier)
		Stopmode1
		CHcompleted = CHcompleted + 1
		ChallangeSlot = ChallangeSlot + 1
		wackychallanges = 1
		p32 = 1
		Checkchallange()
		startroady
ttrophee.enabled = false
Ttropheemania.enabled = false
end sub

'********challange 9***************
Sub StartSmokescreen3
startB2S(14)
flashplaat2.ImageA="Nsmoke"
StopSong
StartySmokescreen3
end sub

Sub StartySmokescreen3
wackychallanges = 2
     DMD CL(0, "SURVIVE THE"), CL(1, "SMOKESCREEN"), "", eNone, eNone, eNone, 2000, True, "fogbank"
	vpmTimer.AddTimer 2000, "startchallange9 '"
end sub

sub startchallange9
UpdateMusicNow
mode1TimerCount = 20
mode1timer.Enabled = 1
Smokescreen3Checker = 0
smokeyT003.enabled = True
kickeroutwackychallange
end sub

dim Smokescreen3Checker
sub smokeyT003_Timer
Smokescreen3Checker = Smokescreen3Checker + 1 : If Smokescreen3Checker > 20 then stopSmokescreen3: end If 
select case Smokescreen3Checker
				case 1 :pfflashert.ImageA="su1":pfflashert.visible=True
				case 2 :pfflashert.ImageA="su1"
				case 3 :pfflashert.ImageA="su2"
				case 4 :pfflashert.ImageA="su2"
				case 5 :pfflashert.ImageA="su3"
				case 6 :pfflashert.ImageA="su3"
				case 7 :pfflashert.ImageA="su4"
				case 8 :pfflashert.ImageA="su4"
				case 9 :pfflashert.ImageA="su5"
				case 10 :pfflashert.ImageA="su5"
				case 11 :pfflashert.ImageA="su5"
				case 12 :pfflashert.ImageA="su5"
				case 13 :pfflashert.ImageA="su4"
				case 14 :pfflashert.ImageA="su4"
				case 15 :pfflashert.ImageA="su3"
				case 16 :pfflashert.ImageA="su3"
				case 17 :pfflashert.ImageA="su2"
				case 18 :pfflashert.ImageA="su2"
				case 19 :pfflashert.ImageA="su1"
				case 20 :pfflashert.ImageA="su1"
			end Select
End Sub

sub stopSmokescreen3
		DMD "", "", "300kdmd", eNone, eNone, eNone, 1000, True, "v_winner"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (300000*PFMultiplier)
		smokeyT003.enabled = False
		Stopmode1
		CHcompleted = CHcompleted + 1
		ChallangeSlot = ChallangeSlot + 1
		wackychallanges = 1
		Smokescreen3Checker = 0
		p28 = 1
		changeb2sback2begin
		Checkchallange()
		li059.state = 1
		startroady
end sub
'********challange 10***************
Sub StartPidgeon
startB2S(19)
flashplaat2.ImageA="Npidgeon"
StopSong
StartyPidgeone
end sub

Sub StartyPidgeone
wackychallanges = 2
     DMD CL(0, "HIT 5 PIDGEONS"), CL(1, "COMPLETE CHALLANGE"), "", eNone, eNone, eNone, 1100, True, "stopthatpidgeon"
	vpmTimer.AddTimer 1200, "startchallange10 '"
end sub

sub startchallange10
'UpdateMusicNow
Playsong "pidgeon"
mode1TimerCount = 60
mode1timer.Enabled = 1
EnablePidgeones()
PidgeoneChecker = 0
kickeroutwackychallange
end sub

Dim WhichPidgeone, PidgeoneChecker
WhichPidgeone = 0
PidgeoneChecker = 0
sub EnablePidgeones()
	If PidgeoneChecker = 31 Then
		CheckBonusPidgeone()
		Exit Sub
	End If
	Randomize()
	WhichPidgeone = INT(RND * 5) + 1
	Select Case WhichPidgeone
		Case 3
			WhichPidgeone = 4
		Case 4
			WhichPidgeone = 8
		Case 5
			WhichPidgeone = 16
	End Select
	Do While (WhichPidgeone AND PidgeoneChecker) > 0
		WhichPidgeone = INT(RND * 5) + 1
		Select Case WhichPidgeone
			Case 3
				WhichPidgeone = 4
			Case 4
				WhichPidgeone = 8
			Case 5
				WhichPidgeone = 16
		End Select
	Loop
	Select Case WhichPidgeone
		Case 1
			ts6item001.enabled = 1
			Pidgeon01.Visible = 1
			Pidgeon01.X = ts6item001.X
			Pidgeon01.Y = ts6item001.Y
		Case 2
			ts6item002.enabled = 1
			Pidgeon01.Visible = 1
			Pidgeon01.X = ts6item002.X
			Pidgeon01.Y = ts6item002.Y
		Case 4
			ts6item003.enabled = 1
			Pidgeon01.Visible = 1
			Pidgeon01.X = ts6item003.X
			Pidgeon01.Y = ts6item003.Y
		Case 8
			ts6item004.enabled = 1
			Pidgeon01.Visible = 1
			Pidgeon01.X = ts6item004.X
			Pidgeon01.Y = ts6item004.Y
		Case 16
			ts6item005.enabled = 1
			Pidgeon01.Visible = 1
			Pidgeon01.X = ts6item005.X
			Pidgeon01.Y = ts6item005.Y
	End Select
end sub

sub ts6item001_hit()
	ts6item001.enabled = 0
	MovePidgeoneDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "Pidgeonsound"
	addextratime()
	PidgeoneChecker = (PidgeoneChecker OR 1)
	EnablePidgeones()
end sub

sub MovePidgeoneDown()
	Dim X
	For Each X in Pidgeones
		X.Visible = 0
	Next
end sub

sub ts6item002_hit()
	ts6item002.enabled = 0
	MovePidgeoneDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "Pidgeonsound"
	addextratime()
	PidgeoneChecker = (PidgeoneChecker OR 2)
	EnablePidgeones()
end sub

sub ts6item003_hit()
	ts6item003.enabled = 0
	MovePidgeoneDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "Pidgeonsound"
	addextratime()
	PidgeoneChecker = (PidgeoneChecker OR 4)
	EnablePidgeones()
end sub

sub ts6item004_hit()
	ts6item004.enabled = 0
	MovePidgeoneDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "Pidgeonsound"
	addextratime()
	PidgeoneChecker = (PidgeoneChecker OR 8)
	EnablePidgeones()
end sub

sub ts6item005_hit()
	ts6item005.enabled = 0
	MovePidgeoneDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "Pidgeonsound"
	addextratime()
	PidgeoneChecker = (PidgeoneChecker OR 16)
	EnablePidgeones()
end sub

sub CheckBonusPidgeone()
	If PidgeoneChecker = 31 then
		DMD "", "", "300kdmd", eNone, eNone, eNone, 1000, True, "v_winner"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (300000*PFMultiplier)
		Stopmode1
		CHcompleted = CHcompleted + 1
		PidgeoneChecker = 0
		ChallangeSlot = ChallangeSlot + 1
		wackychallanges = 1
		UpdateMusicNow
		p29 = 1
		li051.state = 1
		changeb2sback2begin
		Checkchallange()
		startroady
	end if
end sub

'********challange 11***************
sub StartTrophy3
startB2S(13)
flashplaat2.ImageA="Nht"
StopSong
StartyTrophy3
Theadlight2.enabled = True
Ttropheemania.enabled = True
end sub

sub StartyTrophy3
wackychallanges = 2
     DMD CL(0, "HIT THE TROPHYS"), CL(1, "COMPLETE CHALLANGE"), "", eNone, eNone, eNone, 1000, True, "mind"
	vpmTimer.AddTimer 1100, "startchallange13 '"
end sub

sub startchallange13
ttrophee.enabled = True
Playsong "zombie"
headDir = 1
headdownTimer.enabled = 1
vpmtimer.addtimer 2000, "kickbonus '"
vpmtimer.addtimer 2200, "enabletropy3 '"
end sub


sub enabletropy3
trophy001.Visible = True
trophy001.X = tph007.X
trophy001.Y = tph007.Y
trophy001.Z = -40
tph007.enabled = 1
end sub

sub tph007_hit()
playsound "tropheehit"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
tph007.enabled = 0
trophy001.X = tph008.X
trophy001.Y = tph008.Y
trophy001.Z = -40
tph008.enabled = 1
end sub

sub tph008_hit()
playsound "tropheehit"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
tph008.enabled = 0
trophy001.X = tph009.X
trophy001.Y = tph009.Y
trophy001.Z = -40
tph009.enabled = 1
end sub

sub tph009_hit()
Theadlight2.enabled = false
RightFlipper001.RotatetoStart
LeftFlipper001.RotatetoStart
LowerFlippersActive = False
playsound "tropheehit"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
tph009.enabled = 0
trophy001.Visible = False
bonustime = 0
		DMD "", "", "300kdmd", eNone, eNone, eNone, 1000, True, "v_winner"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (300000*PFMultiplier)
		Stopmode1
		CHcompleted = CHcompleted + 1
		ChallangeSlot = ChallangeSlot + 1
		wackychallanges = 1
		p33 = 1
		startroady
Checkchallange()
ttrophee.enabled = false
Ttropheemania.enabled = false
end sub

'********challange 12***************
Sub StartOil
startB2S(16)
flashplaat2.ImageA="Noil"
StopSong
Mud001.image="oils"
StartyOil
end sub

Sub StartyOil
wackychallanges = 2
     DMD CL(0, "HIT 5 OIL SPOTS"), CL(1, "COMPLETE CHALLANGE"), "", eNone, eNone, eNone, 3000, True, "oil"
	vpmTimer.AddTimer 3000, "startchallange2 '"
end sub

sub startchallange2
UpdateMusicNow
mode1TimerCount = 60
mode1timer.Enabled = 1
EnableOils()
OilChecker = 0
kickeroutwackychallange
end sub

Dim WhichOil, OilChecker
WhichOil = 0
OilChecker = 0
sub EnableOils()
	If OilChecker = 31 Then
		CheckBonusOil()
		Exit Sub
	End If
	Randomize()
	WhichOil = INT(RND * 5) + 1
	Select Case WhichOil
		Case 3
			WhichOil = 4
		Case 4
			WhichOil = 8
		Case 5
			WhichOil = 16
	End Select
	Do While (WhichOil AND OilChecker) > 0
		WhichOil = INT(RND * 5) + 1
		Select Case WhichOil
			Case 3
				WhichOil = 4
			Case 4
				WhichOil = 8
			Case 5
				WhichOil = 16
		End Select
	Loop
	Select Case WhichOil
		Case 1
			ts3item001.enabled = 1
			Mud001.Visible = 1
			Mud001.X = ts3item001.X
			Mud001.Y = ts3item001.Y
		Case 2
			ts3item002.enabled = 1
			Mud001.Visible = 1
			Mud001.X = ts3item002.X
			Mud001.Y = ts3item002.Y
		Case 4
			ts3item003.enabled = 1
			Mud001.Visible = 1
			Mud001.X = ts3item003.X
			Mud001.Y = ts3item003.Y
		Case 8
			ts3item004.enabled = 1
			Mud001.Visible = 1
			Mud001.X = ts3item004.X
			Mud001.Y = ts3item004.Y
		Case 16
			ts3item005.enabled = 1
			Mud001.Visible = 1
			Mud001.X = ts3item005.X
			Mud001.Y = ts3item005.Y
	End Select
end sub

sub ts3item001_hit()
	ts3item001.enabled = 0
	MoveOilDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "oilhit"
	addextratime()
	OilChecker = (OilChecker OR 1)
	EnableOils()
end sub

sub MoveOilDown()
	Dim X
	For Each X in Muds
		X.Visible = 0
	Next
end sub

sub ts3item002_hit()
	ts3item002.enabled = 0
	MoveOilDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "oilhit"
	addextratime()
	OilChecker = (OilChecker OR 2)
	EnableOils()
end sub

sub ts3item003_hit()
	ts3item003.enabled = 0
	MoveOilDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "oilhit"
	addextratime()
	OilChecker = (OilChecker OR 4)
	EnableOils()
end sub

sub ts3item004_hit()
	ts3item004.enabled = 0
	MoveOilDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "oilhit"
	addextratime()
	OilChecker = (OilChecker OR 8)
	EnableOils()
end sub

sub ts3item005_hit()
	ts3item005.enabled = 0
	MoveOilDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "oilhit"
	addextratime()
	OilChecker = (OilChecker OR 16)
	EnableOils()
end sub

sub CheckBonusOil()
	If OilChecker = 31 then
		DMD "", "", "300kdmd", eNone, eNone, eNone, 1000, True, "v_winner"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (300000*PFMultiplier)
		Stopmode1
		CHcompleted = CHcompleted + 1
		OilChecker = 0
		ChallangeSlot = ChallangeSlot + 1
		wackychallanges = 1
		p21 = 1
		li049.state = 1
		changeb2sback2begin
		Checkchallange()
		startroady
	end if
end sub

'********challange 13***************
dim road3cls:road3cls=0
sub StartRoadclosure3
startB2S(15)
flashplaat2.ImageA="Nrb"
StopSong
StartyRoadclosure3
end sub

sub StartyRoadclosure3
wackychallanges = 2
     DMD CL(0, "DONT HIT THE"), CL(1, "ROAD BLOCKS"), "", eNone, eNone, eNone, 2000, True, "roadblock"
	vpmTimer.AddTimer 2000, "startchallange8 '"
end sub

sub startchallange8
road3cls=1
li021.state = 2
rbwall005.IsDropped = false
rbwall006.IsDropped = false
roablock3a.visible = True
roablock3b.visible = True
UpdateMusicNow
mode1TimerCount = 30
mode1timer.Enabled = 1
kickeroutwackychallange
end sub

sub rbwall005_hit()
playsound "v_timeextended2"
addextratime
end sub

sub rbwall006_hit()
playsound "v_timeextended2"
addextratime
end sub

sub StopRoadblock3
		DMD "", "", "300kdmd", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (300000*PFMultiplier)
		p27 = 1
		road3cls=0
		'playsound "knocker"
		KnockerSolenoid ' knocker sound
		CHcompleted = CHcompleted + 1
		ChallangeSlot = ChallangeSlot + 1
		wackychallanges = 1
		Checkchallange()
		changeb2sback2begin
		Stopmode1
		li054.state = 1
		startroady
end sub

'********challange 14***************
Sub StartGas
startB2S(18)
flashplaat2.ImageA="Ngas"
StopSong
StartyGase
end sub

Sub StartyGase
wackychallanges = 2
     DMD CL(0, "HIT 5 FUELTANKS"), CL(1, "COMPLETE CHALLANGE"), "", eNone, eNone, eNone, 1200, True, "getsomegas"
	vpmTimer.AddTimer 2000, "startchallange14 '"
end sub

sub startchallange14
UpdateMusicNow
mode1TimerCount = 60
mode1timer.Enabled = 1
EnableGases()
GaseChecker = 0
kickeroutwackychallange
end sub

Dim WhichGase, GaseChecker
WhichGase = 0
GaseChecker = 0
sub EnableGases()
	If GaseChecker = 31 Then
		CheckBonusGase()
		Exit Sub
	End If
	Randomize()
	WhichGase = INT(RND * 5) + 1
	Select Case WhichGase
		Case 3
			WhichGase = 4
		Case 4
			WhichGase = 8
		Case 5
			WhichGase = 16
	End Select
	Do While (WhichGase AND GaseChecker) > 0
		WhichGase = INT(RND * 5) + 1
		Select Case WhichGase
			Case 3
				WhichGase = 4
			Case 4
				WhichGase = 8
			Case 5
				WhichGase = 16
		End Select
	Loop
	Select Case WhichGase
		Case 1
			ts4item001.enabled = 1
			Gase01.Visible = 1
			Gase01.X = ts4item001.X
			Gase01.Y = ts4item001.Y
		Case 2
			ts4item002.enabled = 1
			Gase01.Visible = 1
			Gase01.X = ts4item002.X
			Gase01.Y = ts4item002.Y
		Case 4
			ts4item003.enabled = 1
			Gase01.Visible = 1
			Gase01.X = ts4item003.X
			Gase01.Y = ts4item003.Y
		Case 8
			ts4item004.enabled = 1
			Gase01.Visible = 1
			Gase01.X = ts4item004.X
			Gase01.Y = ts4item004.Y
		Case 16
			ts4item005.enabled = 1
			Gase01.Visible = 1
			Gase01.X = ts4item005.X
			Gase01.Y = ts4item005.Y
	End Select
end sub

sub ts4item001_hit()
	ts4item001.enabled = 0
	MoveGaseDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "fuelcan"
	addextratime()
	GaseChecker = (GaseChecker OR 1)
	EnableGases()
end sub

sub MoveGaseDown()
	Dim X
	For Each X in Gases
		X.Visible = 0
	Next
end sub

sub ts4item002_hit()
	ts4item002.enabled = 0
	MoveGaseDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "fuelcan"
	addextratime()
	GaseChecker = (GaseChecker OR 2)
	EnableGases()
end sub

sub ts4item003_hit()
	ts4item003.enabled = 0
	MoveGaseDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "fuelcan"
	addextratime()
	GaseChecker = (GaseChecker OR 4)
	EnableGases()
end sub

sub ts4item004_hit()
	ts4item004.enabled = 0
	MoveGaseDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "fuelcan"
	addextratime()
	GaseChecker = (GaseChecker OR 8)
	EnableGases()
end sub

sub ts4item005_hit()
	ts4item005.enabled = 0
	MoveGaseDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "fuelcan"
	addextratime()
	GaseChecker = (GaseChecker OR 16)
	EnableGases()
end sub

sub CheckBonusGase()
	If GaseChecker = 31 then
		DMD "", "", "300kdmd", eNone, eNone, eNone, 1000, True, "v_winner"
		Score(CurrentPlayer) = Score(CurrentPlayer) + (300000*PFMultiplier)
		Stopmode1
		CHcompleted = CHcompleted + 1
		GaseChecker = 0
		ChallangeSlot = ChallangeSlot + 1
		wackychallanges = 1
		p30 = 1
		li060.state = 1
		changeb2sback2begin
		Checkchallange()
		startroady
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
'	If mode1TimerCount = 30 Then PlaySound "hurry_1"
	If mode1TimerCount = 10 Then PlaySound "hurrydumdum"
	If mode1TimerCount = 0 and road1cls=1 Then
	StopRoadblock1
	Playsound "v_winner"
	exit sub
	end if
	If mode1TimerCount = 0 and road2cls=1 Then
	StopRoadblock2
	Playsound "v_winner"
	exit sub
	end if
	If mode1TimerCount = 0 and road3cls=1 Then
	StopRoadblock3
	Playsound "v_winner"
	exit sub
	end if
	If mode1TimerCount = 0 Then
	Playsound "v_timeup"
 	DMD "", "", "wcf", eNone, eNone, eNone, 1000, True, ""
		Stopmode1()
	End If
End Sub

Sub Stopmode1()
	wackychallanges = 1
	mode1timer.Enabled = 0
	stopquests	
	TurnOffClock()
End Sub

Sub Stopmode2()
	wackychallanges = 1
	mode1timer.Enabled = 0
	stopquests2
	TurnOffClock()
End Sub

sub stopquests()
Dim X
TrotateI001.enabled = false
li021.state = 0
li022.state = 0
li024.state = 0
li028.state = 0
rbwall001.IsDropped = True
rbwall002.IsDropped = True
roablock1a.visible = false
roablock1b.visible = false
roablock2a.visible = false
roablock2b.visible = false
rbwall004.IsDropped = True
rbwall003.IsDropped = True
rbwall005.IsDropped = True
rbwall006.IsDropped = True
roablock3a.visible = false
roablock3b.visible = false
pfflashert.visible=False
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
	For Each X in Muds
		X.Visible = 0
	Next
	For Each X in Pidgeones
		X.Visible = 0
	Next
	For Each X in cones
		X.Visible = 0
	Next
	For Each X in Gases
		X.Visible = 0
	Next
end sub

sub stopquests2
Dim X
TrotateI001.enabled = false
road1cls=0
road2cls=0
road3cls=0
li021.state = 0
li022.state = 0
li024.state = 0
li028.state = 0
rbwall001.IsDropped = True
rbwall002.IsDropped = True
roablock1a.visible = false
roablock1b.visible = false
roablock2a.visible = false
roablock2b.visible = false
rbwall004.IsDropped = True
rbwall003.IsDropped = True
rbwall005.IsDropped = True
rbwall006.IsDropped = True
roablock3a.visible = false
roablock3b.visible = false
pfflashert.visible=False
smokeyT001.enabled = False
smokeyT002.enabled = False
smokeyT003.enabled = False
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
	For Each X in Muds
		X.Visible = 0
	Next
	For Each X in cones
		X.Visible = 0
	Next
	For Each X in Pidgeones
		X.Visible = 0
	Next
	For Each X in Gases
		X.Visible = 0
	Next
end sub


sub addextratime()
	if mode1TimerCount > 90 then exit sub
	mode1TimerCount = mode1TimerCount + 5
end sub

sub addextratime2()
	if mode1TimerCount > 90 then exit sub
	Road2Checker = Road2Checker - 5
	mode1TimerCount = mode1TimerCount + 5
end sub

'********************************
'		 lights gasstation
'********************************

Sub Tlgas_Timer
countr30 = countr30 + 1 : If Countr30 > 10 then stoplightsgas: end If 
select case countr30
				case 1 : gl001.image = "1_red":gl002.image = "1_red":gl003.image = "1_red":gl004.image = "1_red":gl005.image = "1_red":gl006.image = "1_red":gl007.image = "1_red":gl008.image = "1_red":gl009.image = "1_red":gl010.image = "1_red":gl011.image = "1_red":_
gl012.image = "1_red":gl013.image = "1_red":gl014.image = "1_red":gl015.image = "1_red":gl016.image = "1_red":gl017.image = "1_red":gl018.image = "1_red":gl019.image = "1_red":gl020.image = "1_red":gl021.image = "1_red":gl022.image = "1_red"
				case 2 : gl001.image = "1_green":gl002.image = "1_red":gl003.image = "1_green":gl004.image = "1_red":gl005.image = "1_green":gl006.image = "1_red":gl007.image = "1_green":gl008.image = "1_red":gl009.image = "1_green":gl010.image = "1_red":gl011.image = "1_green":_
gl012.image = "1_red":gl013.image = "1_green":gl014.image = "1_red":gl015.image = "1_green":gl016.image = "1_red":gl017.image = "1_green":gl018.image = "1_red":gl019.image = "1_green":gl020.image = "1_red":gl021.image = "1_green":gl022.image = "1_red"
				case 3 : gl001.image = "1_red":gl002.image = "1_green":gl003.image = "1_red":gl004.image = "1_green":gl005.image = "1_red":gl006.image = "1_green":gl007.image = "1_red":gl008.image = "1_green":gl009.image = "1_red":gl010.image = "1_green":gl011.image = "1_red":_
gl012.image = "1_green":gl013.image = "1_red":gl014.image = "1_green":gl015.image = "1_red":gl016.image = "1_green":gl017.image = "1_red":gl018.image = "1_green":gl019.image = "1_red":gl020.image = "1_green":gl021.image = "1_red":gl022.image = "1_green"
				case 4 : gl001.image = "1_green":gl002.image = "1_red":gl003.image = "1_green":gl004.image = "1_red":gl005.image = "1_green":gl006.image = "1_red":gl007.image = "1_green":gl008.image = "1_red":gl009.image = "1_green":gl010.image = "1_red":gl011.image = "1_green":_
gl012.image = "1_red":gl013.image = "1_green":gl014.image = "1_red":gl015.image = "1_green":gl016.image = "1_red":gl017.image = "1_green":gl018.image = "1_red":gl019.image = "1_green":gl020.image = "1_red":gl021.image = "1_green":gl022.image = "1_red"
				case 5 : gl001.image = "1_red":gl002.image = "1_green":gl003.image = "1_red":gl004.image = "1_green":gl005.image = "1_red":gl006.image = "1_green":gl007.image = "1_red":gl008.image = "1_green":gl009.image = "1_red":gl010.image = "1_green":gl011.image = "1_red":_
gl012.image = "1_green":gl013.image = "1_red":gl014.image = "1_green":gl015.image = "1_red":gl016.image = "1_green":gl017.image = "1_red":gl018.image = "1_green":gl019.image = "1_red":gl020.image = "1_green":gl021.image = "1_red":gl022.image = "1_green"
				case 6 : gl001.image = "1_green":gl002.image = "1_red":gl003.image = "1_green":gl004.image = "1_red":gl005.image = "1_green":gl006.image = "1_red":gl007.image = "1_green":gl008.image = "1_red":gl009.image = "1_green":gl010.image = "1_red":gl011.image = "1_green":_
gl012.image = "1_red":gl013.image = "1_green":gl014.image = "1_red":gl015.image = "1_green":gl016.image = "1_red":gl017.image = "1_green":gl018.image = "1_red":gl019.image = "1_green":gl020.image = "1_red":gl021.image = "1_green":gl022.image = "1_red"
				case 7 : gl001.image = "1_red":gl002.image = "1_green":gl003.image = "1_red":gl004.image = "1_green":gl005.image = "1_red":gl006.image = "1_green":gl007.image = "1_red":gl008.image = "1_green":gl009.image = "1_red":gl010.image = "1_green":gl011.image = "1_red":_
gl012.image = "1_green":gl013.image = "1_red":gl014.image = "1_green":gl015.image = "1_red":gl016.image = "1_green":gl017.image = "1_red":gl018.image = "1_green":gl019.image = "1_red":gl020.image = "1_green":gl021.image = "1_red":gl022.image = "1_green"
				case 8 :gl001.image = "1_green":gl002.image = "1_red":gl003.image = "1_green":gl004.image = "1_red":gl005.image = "1_green":gl006.image = "1_red":gl007.image = "1_green":gl008.image = "1_red":gl009.image = "1_green":gl010.image = "1_red":gl011.image = "1_green":_
gl012.image = "1_red":gl013.image = "1_green":gl014.image = "1_red":gl015.image = "1_green":gl016.image = "1_red":gl017.image = "1_green":gl018.image = "1_red":gl019.image = "1_green":gl020.image = "1_red":gl021.image = "1_green":gl022.image = "1_red"
				case 9 :gl001.image = "1_red":gl002.image = "1_green":gl003.image = "1_red":gl004.image = "1_green":gl005.image = "1_red":gl006.image = "1_green":gl007.image = "1_red":gl008.image = "1_green":gl009.image = "1_red":gl010.image = "1_green":gl011.image = "1_red":_
gl012.image = "1_green":gl013.image = "1_red":gl014.image = "1_green":gl015.image = "1_red":gl016.image = "1_green":gl017.image = "1_red":gl018.image = "1_green":gl019.image = "1_red":gl020.image = "1_green":gl021.image = "1_red":gl022.image = "1_green"
				case 10 :gl001.image = "1_red":gl002.image = "1_red":gl003.image = "1_red":gl004.image = "1_red":gl005.image = "1_red":gl006.image = "1_red":gl007.image = "1_red":gl008.image = "1_red":gl009.image = "1_red":gl010.image = "1_red":gl011.image = "1_red":_
gl012.image = "1_red":gl013.image = "1_red":gl014.image = "1_red":gl015.image = "1_red":gl016.image = "1_red":gl017.image = "1_red":gl018.image = "1_red":gl019.image = "1_red":gl020.image = "1_red":gl021.image = "1_red":gl022.image = "1_red"
			end Select
End Sub

sub stoplightsgas
Tlgas.enabled = false
Countr30=0
end sub

'**************************************
'Ramp sounds
'**************************************

Sub RampBase1_Hit()
		If activeball.vely < 0 then
			PlaySound "RampUp"
			if BSmode = 1 or wackychallanges = 2 then
			exit sub
			end if
			flashplaat2.ImageA="CMgo"
			else
			PlaySound "RampDown"
			'WireRampOff
			if BSmode = 1 or wackychallanges = 2 then
			exit sub
			end if
			startroady
		end if
End Sub

Sub RampBase2_Hit()
		If activeball.vely < 0 then
			PlaySound "RampUp"
			if BSmode = 1 or wackychallanges = 2 then
			exit sub
			end if
			flashplaat2.ImageA="takedetour"
			else
			PlaySound "RampDown"
			'WireRampOff
			if BSmode = 1 or wackychallanges = 2 then
			exit sub
			end if
			startroady
		end if
End Sub

Sub RampBase3_Hit()
		If activeball.vely < 0 then
			PlaySound "RampUp"
			if BSmode = 1 or wackychallanges = 2 then
			exit sub
			end if
			flashplaat2.ImageA="thw"
			else
			PlaySound "RampDown"
			'WireRampOff
			if BSmode = 1 or wackychallanges = 2 then
			exit sub
			end if
			startroady
		end if
End Sub

sub RampBase001_hit()
if wsteal = 1 Then
wsteal = 0
end if
PlaySound "rampshot"
countr35=0
speedT.enabled = 1
if bhit = 2 then exit sub
if Sboss09 = 1 then
pointsboss9
exit sub
end if
if BSmode = 1 or wackychallanges = 2 then
exit sub
end if
startroady
end sub

sub RampBase002_hit()
if wsteal = 1 Then
wsteal = 0
end if
PlaySound "rampshot"
countr35=0
speedT.enabled = 1
if bhit = 2 then exit sub
if Sboss09 = 1 then
pointsboss9
exit sub
end if
if BSmode = 1 or wackychallanges = 2 then
exit sub
end if
startroady
end sub

sub pointsboss9
bhit = bhit + 1
checkbosshit09
end sub

sub startroady
flashplaat2.ImageA="Nnowhere"
end sub

'**************************************
'jackpot pomp
'**************************************

dim jckpt:jckpt=0

sub Target011_hit
playsound "gasbell"
 DOF 116, DOFPulse
pomp1Shaker
jckpt = jckpt + 1
jackpotchecker
end sub


Sub jackpotchecker
Select Case jckpt
case 1: jp1
case 2: jp2
case 3: jp3
case 4: jp4
case 5: jp5
case 6: jp6
End Select
End Sub


sub jp1
li061.state=1
Score(CurrentPlayer) = Score(CurrentPlayer) + (50000*jackpotmulti)
DMD "", "", "50kdmd", eNone, eNone, eNone, 750, True, ""
end sub

sub jp2
li062.state=1
Score(CurrentPlayer) = Score(CurrentPlayer) + (50000*jackpotmulti)
DMD "", "", "50kdmd", eNone, eNone, eNone, 750, True, ""
end sub

sub jp3
li063.state=1
li045.state=1
playsound "jackpot2"
Score(CurrentPlayer) = Score(CurrentPlayer) + (500000*jackpotmulti)
DMD "", "", "500kdmd", eNone, eNone, eNone, 750, True, ""
end sub

sub jp4
li064.state=1
Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*jackpotmulti)
DMD "", "", "100kdmd", eNone, eNone, eNone, 750, True, ""
end sub

sub jp5
li065.state=1
Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*jackpotmulti)
DMD "", "", "100kdmd", eNone, eNone, eNone, 750, True, ""
end sub

sub jp6
li066.state=1
li044.state=1
playsound "jackpot"
Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*jackpotmulti)
DMD "", "", "1Mdmd", eNone, eNone, eNone, 750, True, ""
vpmTimer.AddTimer 4000, "resetjackpot'"
end sub

sub resetjackpot
li061.state=0
li062.state=0
li063.state=0
li045.state=0
li064.state=0
li065.state=0
li066.state=0
li044.state=0
jackpotmulti = jackpotmulti + 1
jckpt = 0
end sub

sub resetjackpotNewGame
li061.state=0
li062.state=0
li063.state=0
li045.state=0
li064.state=0
li065.state=0
li066.state=0
li044.state=0
jackpotmulti = 1
jckpt = 0
end sub

'***********pomp shaker*************
Dim pomp1Shake

Sub pomp1Shaker()
    pomp1Shake = 6
    pomp1Timer.Enabled = True
End Sub

Sub pomp1Timer_Timer()
    pomp.Transz = pomp1Shake / 2
    If pomp1Shake = 0 Then Me.Enabled = False:Exit Sub
    If pomp1Shake <0 Then
        pomp1Shake = ABS(pomp1Shake)- 0.1
    Else
        pomp1Shake = - pomp1Shake + 0.1
    End If
End Sub


'**************************************
'explosion
'**************************************
dim countr34:countr34=0

Sub explosiontimer_Timer
countr34 = countr34 + 1 : If Countr34 > 12 then switchbomb 'Countr34 = 1 : end If 'stopbonusaniL
select case countr34
				case 1 : expl001.visible=True:expl002.visible=False:expl003.visible=False:expl004.visible=False:expl005.visible=False:expl006.visible=False:expl007.visible=False:expl008.visible=False:expl009.visible=False:expl010.visible=False:expl011.visible=False:expl012.visible=False
				case 2 : expl001.visible=False:expl002.visible=True:expl003.visible=False:expl004.visible=False:expl005.visible=False:expl006.visible=False:expl007.visible=False:expl008.visible=False:expl009.visible=False:expl010.visible=False:expl011.visible=False:expl012.visible=False
				case 3 : expl001.visible=False:expl002.visible=False:expl003.visible=True:expl004.visible=False:expl005.visible=False:expl006.visible=False:expl007.visible=False:expl008.visible=False:expl009.visible=False:expl010.visible=False:expl011.visible=False:expl012.visible=False
				case 4 : expl001.visible=False:expl002.visible=False:expl003.visible=False:expl004.visible=True:expl005.visible=False:expl006.visible=False:expl007.visible=False:expl008.visible=False:expl009.visible=False:expl010.visible=False:expl011.visible=False:expl012.visible=False
				case 5 : expl001.visible=False:expl002.visible=False:expl003.visible=False:expl004.visible=False:expl005.visible=True:expl006.visible=False:expl007.visible=False:expl008.visible=False:expl009.visible=False:expl010.visible=False:expl011.visible=False:expl012.visible=False
				case 6 : expl001.visible=False:expl002.visible=False:expl003.visible=False:expl004.visible=False:expl005.visible=False:expl006.visible=True:expl007.visible=False:expl008.visible=False:expl009.visible=False:expl010.visible=False:expl011.visible=False:expl012.visible=False
				case 7 : expl001.visible=False:expl002.visible=False:expl003.visible=False:expl004.visible=False:expl005.visible=False:expl006.visible=False:expl007.visible=True:expl008.visible=False:expl009.visible=False:expl010.visible=False:expl011.visible=False:expl012.visible=False
				case 8 : expl001.visible=False:expl002.visible=False:expl003.visible=False:expl004.visible=False:expl005.visible=False:expl006.visible=False:expl007.visible=False:expl008.visible=True:expl009.visible=False:expl010.visible=False:expl011.visible=False:expl012.visible=False
				case 9 : expl001.visible=False:expl002.visible=False:expl003.visible=False:expl004.visible=False:expl005.visible=False:expl006.visible=False:expl007.visible=False:expl008.visible=False:expl009.visible=True:expl010.visible=False:expl011.visible=False:expl012.visible=False
				case 10 : expl001.visible=False:expl002.visible=False:expl003.visible=False:expl004.visible=False:expl005.visible=False:expl006.visible=False:expl007.visible=False:expl008.visible=False:expl009.visible=False:expl010.visible=True:expl011.visible=False:expl012.visible=False
				case 11 : expl001.visible=False:expl002.visible=False:expl003.visible=False:expl004.visible=False:expl005.visible=False:expl006.visible=False:expl007.visible=False:expl008.visible=False:expl009.visible=False:expl010.visible=False:expl011.visible=True:expl012.visible=False
				case 12 : expl001.visible=False:expl002.visible=False:expl003.visible=False:expl004.visible=False:expl005.visible=False:expl006.visible=False:expl007.visible=False:expl008.visible=False:expl009.visible=False:expl010.visible=False:expl011.visible=False:expl012.visible=True
			end Select
End Sub

sub switchbomb
explosiontimer.enabled = 0
expl012.visible=False
countr34 = 0
bombaction = 0
end sub


sub bomby1
expl001.X = tbomb001.X
expl001.Y = tbomb001.Y
expl001.z = 50
expl002.X = tbomb001.X
expl002.Y = tbomb001.Y
expl002.z = 50
expl003.X = tbomb001.X
expl003.Y = tbomb001.Y
expl003.z = 50
expl004.X = tbomb001.X
expl004.Y = tbomb001.Y
expl004.z = 50
expl005.X = tbomb001.X
expl005.Y = tbomb001.Y
expl005.z = 50
expl006.X = tbomb001.X
expl006.Y = tbomb001.Y
expl006.z = 50
expl007.X = tbomb001.X
expl007.Y = tbomb001.Y
expl007.z = 50
expl008.X = tbomb001.X
expl008.Y = tbomb001.Y
expl008.z = 50
expl009.X = tbomb001.X
expl009.Y = tbomb001.Y
expl009.z = 50
expl010.X = tbomb001.X
expl010.Y = tbomb001.Y
expl010.z = 50
expl011.X = tbomb001.X
expl011.Y = tbomb001.Y
expl011.z = 50
expl012.X = tbomb001.X
expl012.Y = tbomb001.Y
expl012.z = 50
explosiontimer.enabled = 1
end Sub

sub bomby2
expl001.X = tbomb002.X
expl001.Y = tbomb002.Y
expl001.z = 50
expl002.X = tbomb002.X
expl002.Y = tbomb002.Y
expl002.z = 50
expl003.X = tbomb002.X
expl003.Y = tbomb002.Y
expl003.z = 50
expl004.X = tbomb002.X
expl004.Y = tbomb002.Y
expl004.z = 50
expl005.X = tbomb002.X
expl005.Y = tbomb002.Y
expl005.z = 50
expl006.X = tbomb002.X
expl006.Y = tbomb002.Y
expl006.z = 50
expl007.X = tbomb002.X
expl007.Y = tbomb002.Y
expl007.z = 50
expl008.X = tbomb002.X
expl008.Y = tbomb002.Y
expl008.z = 50
expl009.X = tbomb002.X
expl009.Y = tbomb002.Y
expl009.z = 50
expl010.X = tbomb002.X
expl010.Y = tbomb002.Y
expl010.z = 50
expl011.X = tbomb002.X
expl011.Y = tbomb002.Y
expl011.z = 50
expl012.X = tbomb002.X
expl012.Y = tbomb002.Y
expl012.z = 50
explosiontimer.enabled = 1
end Sub

sub bomby3
expl001.X = tbomb003.X
expl001.Y = tbomb003.Y
expl001.z = 50
expl002.X = tbomb003.X
expl002.Y = tbomb003.Y
expl002.z = 50
expl003.X = tbomb003.X
expl003.Y = tbomb003.Y
expl003.z = 50
expl004.X = tbomb003.X
expl004.Y = tbomb003.Y
expl004.z = 50
expl005.X = tbomb003.X
expl005.Y = tbomb003.Y
expl005.z = 50
expl006.X = tbomb003.X
expl006.Y = tbomb003.Y
expl006.z = 50
expl007.X = tbomb003.X
expl007.Y = tbomb003.Y
expl007.z = 50
expl008.X = tbomb003.X
expl008.Y = tbomb003.Y
expl008.z = 50
expl009.X = tbomb003.X
expl009.Y = tbomb003.Y
expl009.z = 50
expl010.X = tbomb003.X
expl010.Y = tbomb003.Y
expl010.z = 50
expl011.X = tbomb003.X
expl011.Y = tbomb003.Y
expl011.z = 50
expl012.X = tbomb003.X
expl012.Y = tbomb003.Y
expl012.z = 50
explosiontimer.enabled = 1
end Sub

sub bomby4
expl001.X = tbomb004.X
expl001.Y = tbomb004.Y
expl001.z = 50
expl002.X = tbomb004.X
expl002.Y = tbomb004.Y
expl002.z = 50
expl003.X = tbomb004.X
expl003.Y = tbomb004.Y
expl003.z = 50
expl004.X = tbomb004.X
expl004.Y = tbomb004.Y
expl004.z = 50
expl005.X = tbomb004.X
expl005.Y = tbomb004.Y
expl005.z = 50
expl006.X = tbomb004.X
expl006.Y = tbomb004.Y
expl006.z = 50
expl007.X = tbomb004.X
expl007.Y = tbomb004.Y
expl007.z = 50
expl008.X = tbomb004.X
expl008.Y = tbomb004.Y
expl008.z = 50
expl009.X = tbomb004.X
expl009.Y = tbomb004.Y
expl009.z = 50
expl010.X = tbomb004.X
expl010.Y = tbomb004.Y
expl010.z = 50
expl011.X = tbomb004.X
expl011.Y = tbomb004.Y
expl011.z = 50
expl012.X = tbomb004.X
expl012.Y = tbomb004.Y
expl012.z = 50
explosiontimer.enabled = 1
end sub

sub bomby5
expl001.X = tbomb005.X
expl001.Y = tbomb005.Y
expl001.z = 50
expl002.X = tbomb005.X
expl002.Y = tbomb005.Y
expl002.z = 50
expl003.X = tbomb005.X
expl003.Y = tbomb005.Y
expl003.z = 50
expl004.X = tbomb005.X
expl004.Y = tbomb005.Y
expl004.z = 50
expl005.X = tbomb005.X
expl005.Y = tbomb005.Y
expl005.z = 50
expl006.X = tbomb005.X
expl006.Y = tbomb005.Y
expl006.z = 50
expl007.X = tbomb005.X
expl007.Y = tbomb005.Y
expl007.z = 50
expl008.X = tbomb005.X
expl008.Y = tbomb005.Y
expl008.z = 50
expl009.X = tbomb005.X
expl009.Y = tbomb005.Y
expl009.z = 50
expl010.X = tbomb005.X
expl010.Y = tbomb005.Y
expl010.z = 50
expl011.X = tbomb005.X
expl011.Y = tbomb005.Y
expl011.z = 50
expl012.X = tbomb005.X
expl012.Y = tbomb005.Y
expl012.z = 50
explosiontimer.enabled = 1
end sub

sub bomby6
expl001.X = tbomb006.X
expl001.Y = tbomb006.Y
expl001.z = 50
expl002.X = tbomb006.X
expl002.Y = tbomb006.Y
expl002.z = 50
expl003.X = tbomb006.X
expl003.Y = tbomb006.Y
expl003.z = 50
expl004.X = tbomb006.X
expl004.Y = tbomb006.Y
expl004.z = 50
expl005.X = tbomb006.X
expl005.Y = tbomb006.Y
expl005.z = 50
expl006.X = tbomb006.X
expl006.Y = tbomb006.Y
expl006.z = 50
expl007.X = tbomb006.X
expl007.Y = tbomb006.Y
expl007.z = 50
expl008.X = tbomb006.X
expl008.Y = tbomb006.Y
expl008.z = 50
expl009.X = tbomb006.X
expl009.Y = tbomb006.Y
expl009.z = 50
expl010.X = tbomb006.X
expl010.Y = tbomb006.Y
expl010.z = 50
expl011.X = tbomb006.X
expl011.Y = tbomb006.Y
expl011.z = 50
expl012.X = tbomb006.X
expl012.Y = tbomb006.Y
expl012.z = 50
explosiontimer.enabled = 1
end sub

sub bomby7
expl001.X = tbomb007.X
expl001.Y = tbomb007.Y
expl001.z = 50
expl002.X = tbomb007.X
expl002.Y = tbomb007.Y
expl002.z = 50
expl003.X = tbomb007.X
expl003.Y = tbomb007.Y
expl003.z = 50
expl004.X = tbomb007.X
expl004.Y = tbomb007.Y
expl004.z = 50
expl005.X = tbomb007.X
expl005.Y = tbomb007.Y
expl005.z = 50
expl006.X = tbomb007.X
expl006.Y = tbomb007.Y
expl006.z = 50
expl007.X = tbomb007.X
expl007.Y = tbomb007.Y
expl007.z = 50
expl008.X = tbomb007.X
expl008.Y = tbomb007.Y
expl008.z = 50
expl009.X = tbomb007.X
expl009.Y = tbomb007.Y
expl009.z = 50
expl010.X = tbomb007.X
expl010.Y = tbomb007.Y
expl010.z = 50
expl011.X = tbomb007.X
expl011.Y = tbomb007.Y
expl011.z = 50
expl012.X = tbomb007.X
expl012.Y = tbomb007.Y
expl012.z = 50
explosiontimer.enabled = 1
end sub

sub bomby8
expl001.X = tbomb008.X
expl001.Y = tbomb008.Y
expl001.z = 50
expl002.X = tbomb008.X
expl002.Y = tbomb008.Y
expl002.z = 50
expl003.X = tbomb008.X
expl003.Y = tbomb008.Y
expl003.z = 50
expl004.X = tbomb008.X
expl004.Y = tbomb008.Y
expl004.z = 50
expl005.X = tbomb008.X
expl005.Y = tbomb008.Y
expl005.z = 50
expl006.X = tbomb008.X
expl006.Y = tbomb008.Y
expl006.z = 50
expl007.X = tbomb008.X
expl007.Y = tbomb008.Y
expl007.z = 50
expl008.X = tbomb008.X
expl008.Y = tbomb008.Y
expl008.z = 50
expl009.X = tbomb008.X
expl009.Y = tbomb008.Y
expl009.z = 50
expl010.X = tbomb008.X
expl010.Y = tbomb008.Y
expl010.z = 50
expl011.X = tbomb008.X
expl011.Y = tbomb008.Y
expl011.z = 50
expl012.X = tbomb008.X
expl012.Y = tbomb008.Y
expl012.z = 50
explosiontimer.enabled = 1
end sub

sub bomby9
expl001.X = tbomb009.X
expl001.Y = tbomb009.Y
expl001.z = 50
expl002.X = tbomb009.X
expl002.Y = tbomb009.Y
expl002.z = 50
expl003.X = tbomb009.X
expl003.Y = tbomb009.Y
expl003.z = 50
expl004.X = tbomb009.X
expl004.Y = tbomb009.Y
expl004.z = 50
expl005.X = tbomb009.X
expl005.Y = tbomb009.Y
expl005.z = 50
expl006.X = tbomb009.X
expl006.Y = tbomb009.Y
expl006.z = 50
expl007.X = tbomb009.X
expl007.Y = tbomb009.Y
expl007.z = 50
expl008.X = tbomb009.X
expl008.Y = tbomb009.Y
expl008.z = 50
expl009.X = tbomb009.X
expl009.Y = tbomb009.Y
expl009.z = 50
expl010.X = tbomb009.X
expl010.Y = tbomb009.Y
expl010.z = 50
expl011.X = tbomb009.X
expl011.Y = tbomb009.Y
expl011.z = 50
expl012.X = tbomb009.X
expl012.Y = tbomb009.Y
expl012.z = 50
explosiontimer.enabled = 1
end sub

sub bomby10
expl001.X = tbomb010.X
expl001.Y = tbomb010.Y
expl001.z = 50
expl002.X = tbomb010.X
expl002.Y = tbomb010.Y
expl002.z = 50
expl003.X = tbomb010.X
expl003.Y = tbomb010.Y
expl003.z = 50
expl004.X = tbomb010.X
expl004.Y = tbomb010.Y
expl004.z = 50
expl005.X = tbomb010.X
expl005.Y = tbomb010.Y
expl005.z = 50
expl006.X = tbomb010.X
expl006.Y = tbomb010.Y
expl006.z = 50
expl007.X = tbomb010.X
expl007.Y = tbomb010.Y
expl007.z = 50
expl008.X = tbomb010.X
expl008.Y = tbomb010.Y
expl008.z = 50
expl009.X = tbomb010.X
expl009.Y = tbomb010.Y
expl009.z = 50
expl010.X = tbomb010.X
expl010.Y = tbomb010.Y
expl010.z = 50
expl011.X = tbomb010.X
expl011.Y = tbomb010.Y
expl011.z = 50
expl012.X = tbomb010.X
expl012.Y = tbomb010.Y
expl012.z = 50
explosiontimer.enabled = 1
end sub


sub stopbomby
explosiontimer.enabled = 0
expl001.visible =false
expl002.visible =false
expl003.visible =false
expl004.visible =false
expl005.visible =false
expl006.visible =false
expl007.visible =false
expl008.visible =false
expl009.visible =false
expl010.visible =false
expl011.visible =false
expl012.visible =false
countr34 = 0
end sub

sub startbomby
bombaction = 0
checkbombmulti.enabled = 1
bomb001.Image = "bomb1"
bomb002.Image = "bomb1"
bomb003.Image = "bomb1"
bomb004.Image = "bomb1"
bomb005.Image = "bomb1"
bomb006.Image = "bomb1"
bomb007.Image = "bomb1"
bomb008.Image = "bomb1"
bomb009.Image = "bomb1"
bomb010.Image = "bomb1"
bomb001.Visible = True
bomb002.Visible = True
bomb003.Visible = True
bomb004.Visible = True
bomb005.Visible = True
bomb006.Visible = True
bomb007.Visible = True
bomb008.Visible = True
bomb009.Visible = True
bomb010.Visible = True
tbomb001.enabled = true
tbomb002.enabled = true
tbomb003.enabled = true
tbomb004.enabled = true
tbomb005.enabled = true
tbomb006.enabled = true
tbomb007.enabled = true
tbomb008.enabled = true
tbomb009.enabled = true
tbomb010.enabled = true
end sub 

sub checkbombmulti_timer()
checkballieonpf
end sub

sub checkballieonpf
if BallsOnPlayfield < 3 Then
stopbomby2
end if
end sub

sub stopbomby2
checkbombmulti.enabled = False
mmbb=0
bomb001.Visible = False
bomb002.Visible = False
bomb003.Visible = False
bomb004.Visible = False
bomb005.Visible = False
bomb006.Visible = False
bomb007.Visible = False
bomb008.Visible = False
bomb009.Visible = False
bomb010.Visible = False
tbomb001.enabled = False
tbomb002.enabled = False
tbomb003.enabled = False
tbomb004.enabled = False
tbomb005.enabled = False
tbomb006.enabled = False
tbomb007.enabled = False
tbomb008.enabled = False
tbomb009.enabled = False
tbomb010.enabled = False
countr34 = 0
If BallsOnPlayfield = 1 Then
bMultiBallMode = False
end if
end sub

sub tbomb001_hit()
Playsound "bombclick"
checkTbomby001.enabled = true
end sub

sub tbomb002_hit()
Playsound "bombclick"
checkTbomby002.enabled = true
end sub

sub tbomb003_hit()
Playsound "bombclick"
checkTbomby003.enabled = true
end sub

sub tbomb004_hit()
Playsound "bombclick"
checkTbomby004.enabled = true
end sub

sub tbomb005_hit()
Playsound "bombclick"
checkTbomby005.enabled = true
end sub

sub tbomb006_hit()
Playsound "bombclick"
checkTbomby006.enabled = true
end sub

sub tbomb007_hit()
Playsound "bombclick"
checkTbomby007.enabled = true
end sub

sub tbomb008_hit()
Playsound "bombclick"
checkTbomby008.enabled = true
end sub

sub tbomb009_hit()
Playsound "bombclick"
checkTbomby009.enabled = true
end sub

sub tbomb010_hit()
Playsound "bombclick"
checkTbomby010.enabled = true
end sub

sub checkTbomby001_timer
checkdiebomb01
end sub

sub checkTbomby002_timer
checkdiebomb02
end sub

sub checkTbomby003_timer
checkdiebomb03
end sub

sub checkTbomby004_timer
checkdiebomb04
end sub

sub checkTbomby005_timer
checkdiebomb05
end sub

sub checkTbomby006_timer
checkdiebomb06
end sub

sub checkTbomby007_timer
checkdiebomb07
end sub

sub checkTbomby008_timer
checkdiebomb08
end sub

sub checkTbomby009_timer
checkdiebomb09
end sub

sub checkTbomby010_timer
checkdiebomb10
end sub


sub checkdiebomb01
if bombaction = 0 Then
startbomb01
end if
end sub

sub checkdiebomb02
if bombaction = 0 Then
startbomb02
end if
end sub

sub checkdiebomb03
if bombaction = 0 Then
startbomb03
end if
end sub

sub checkdiebomb04
if bombaction = 0 Then
startbomb04
end if
end sub

sub checkdiebomb05
if bombaction = 0 Then
startbomb05
end if
end sub

sub checkdiebomb06
if bombaction = 0 Then
startbomb06
end if
end sub

sub checkdiebomb07
if bombaction = 0 Then
startbomb07
end if
end sub

sub checkdiebomb08
if bombaction = 0 Then
startbomb08
end if
end sub

sub checkdiebomb09
if bombaction = 0 Then
startbomb09
end if
end sub

sub checkdiebomb10
if bombaction = 0 Then
startbomb010
end if
end sub

'**************************************
' bomb explosion animation
'**************************************
dim bombaction:bombaction=0

'bomb01

sub startbomb01
bombaction = 1
tbomb001.enabled = false
tbomb001.DestroyBall
BallsOnPlayfield = BallsOnPlayfield - 1
checkTbomby001.enabled = False
if ballsonplayfield <2 then  
bMultiBallMode = False 
end if
if BallsOnPlayfield = 0 then 
vpmtimer.addtimer 250, "BallsOnPlayfield = 1 : bMultiBallMode = False : multiballoutkicker '"
end if
bomb001.visible = True
changebombtored
vpmTimer.AddTimer 200, "changebombtopurple'"
vpmTimer.AddTimer 400, "changebombtored'"
vpmTimer.AddTimer 600, "changebombtopurple'"
vpmTimer.AddTimer 700, "changebombtored'"
vpmTimer.AddTimer 800, "changebombtopurple'"
vpmTimer.AddTimer 900, "changebombtored'"
vpmTimer.AddTimer 1000, "hidebomb001'"
end sub

sub changebombtored
bomb001.image = "bomb2"
end sub

sub changebombtopurple
bomb001.image = "bomb1"
end sub

sub hidebomb001
bomb001.visible = False
bomby1
Playsound "explo1"
end sub

'bomb02

sub startbomb02
bombaction = 1
tbomb002.enabled = false
tbomb002.DestroyBall
BallsOnPlayfield = BallsOnPlayfield - 1
checkTbomby002.enabled = False
if ballsonplayfield <2 then  
bMultiBallMode = False 
end if
if BallsOnPlayfield = 0 then 
vpmtimer.addtimer 250, "BallsOnPlayfield = 1 : bMultiBallMode = False : multiballoutkicker '"
end if
bomb002.visible = True
changebombtored2
vpmTimer.AddTimer 200, "changebombtopurple2'"
vpmTimer.AddTimer 400, "changebombtored2'"
vpmTimer.AddTimer 600, "changebombtopurple2'"
vpmTimer.AddTimer 700, "changebombtored2'"
vpmTimer.AddTimer 800, "changebombtopurple2'"
vpmTimer.AddTimer 900, "changebombtored2'"
vpmTimer.AddTimer 1000, "hidebomb002'"
end sub

sub changebombtored2
bomb002.image = "bomb2"
end sub

sub changebombtopurple2
bomb002.image = "bomb1"
end sub

sub hidebomb002
bomb002.visible = False
bomby2
Playsound "explo1"
end sub

'bomb03

sub startbomb03
bombaction = 1
tbomb003.enabled = false
tbomb003.DestroyBall
BallsOnPlayfield = BallsOnPlayfield - 1
checkTbomby003.enabled = False
if ballsonplayfield <2 then  
bMultiBallMode = False 
end if
if BallsOnPlayfield = 0 then 
vpmtimer.addtimer 250, "BallsOnPlayfield = 1 : bMultiBallMode = False : multiballoutkicker '"
end if
bomb003.visible = True
changebombtored3
vpmTimer.AddTimer 200, "changebombtopurple3'"
vpmTimer.AddTimer 400, "changebombtored3'"
vpmTimer.AddTimer 600, "changebombtopurple3'"
vpmTimer.AddTimer 700, "changebombtored3'"
vpmTimer.AddTimer 800, "changebombtopurple3'"
vpmTimer.AddTimer 900, "changebombtored3'"
vpmTimer.AddTimer 1000, "hidebomb003'"
end sub

sub changebombtored3
bomb003.image = "bomb2"
end sub

sub changebombtopurple3
bomb003.image = "bomb1"
end sub

sub hidebomb003
bomb003.visible = False
bomby3
Playsound "explo1"
end sub

'bomb04

sub startbomb04
bombaction = 1
tbomb004.enabled = false
tbomb004.DestroyBall
BallsOnPlayfield = BallsOnPlayfield - 1
checkTbomby004.enabled = False
if ballsonplayfield <2 then  
bMultiBallMode = False 
end if
if BallsOnPlayfield = 0 then 
vpmtimer.addtimer 250, "BallsOnPlayfield = 1 : bMultiBallMode = False : multiballoutkicker '"
end if
bomb004.visible = True
changebombtored4
vpmTimer.AddTimer 200, "changebombtopurple4'"
vpmTimer.AddTimer 400, "changebombtored4'"
vpmTimer.AddTimer 600, "changebombtopurple4'"
vpmTimer.AddTimer 700, "changebombtored4'"
vpmTimer.AddTimer 800, "changebombtopurple4'"
vpmTimer.AddTimer 900, "changebombtored4'"
vpmTimer.AddTimer 1000, "hidebomb004'"
end sub

sub changebombtored4
bomb004.image = "bomb2"
end sub

sub changebombtopurple4
bomb004.image = "bomb1"
end sub

sub hidebomb004
bomb004.visible = False
bomby4
Playsound "explo1"
end sub

'bomb05

sub startbomb05
bombaction = 1
tbomb005.enabled = false
tbomb005.DestroyBall
BallsOnPlayfield = BallsOnPlayfield - 1
checkTbomby005.enabled = False
if ballsonplayfield <2 then  
bMultiBallMode = False 
end if
if BallsOnPlayfield = 0 then 
vpmtimer.addtimer 250, "BallsOnPlayfield = 1 : bMultiBallMode = False : multiballoutkicker '"
end if
bomb005.visible = True
changebombtored5
vpmTimer.AddTimer 200, "changebombtopurple5'"
vpmTimer.AddTimer 400, "changebombtored5'"
vpmTimer.AddTimer 600, "changebombtopurple5'"
vpmTimer.AddTimer 700, "changebombtored5'"
vpmTimer.AddTimer 800, "changebombtopurple5'"
vpmTimer.AddTimer 900, "changebombtored5'"
vpmTimer.AddTimer 1000, "hidebomb005'"
end sub

sub changebombtored5
bomb005.image = "bomb2"
end sub

sub changebombtopurple5
bomb005.image = "bomb1"
end sub

sub hidebomb005
bomb005.visible = False
bomby5
Playsound "explo1"
end sub

'bomb06

sub startbomb06
bombaction = 1
tbomb006.enabled = false
tbomb006.DestroyBall
BallsOnPlayfield = BallsOnPlayfield - 1
checkTbomby006.enabled = False
if ballsonplayfield <2 then  
bMultiBallMode = False 
end if
if BallsOnPlayfield = 0 then 
vpmtimer.addtimer 250, "BallsOnPlayfield = 1 : bMultiBallMode = False : multiballoutkicker '"
end if
bomb006.visible = True
changebombtored6
vpmTimer.AddTimer 200, "changebombtopurple6'"
vpmTimer.AddTimer 400, "changebombtored6'"
vpmTimer.AddTimer 600, "changebombtopurple6'"
vpmTimer.AddTimer 700, "changebombtored6'"
vpmTimer.AddTimer 800, "changebombtopurple6'"
vpmTimer.AddTimer 900, "changebombtored6'"
vpmTimer.AddTimer 1000, "hidebomb006'"
end sub

sub changebombtored6
bomb006.image = "bomb2"
end sub

sub changebombtopurple6
bomb006.image = "bomb1"
end sub

sub hidebomb006
bomb006.visible = False
bomby6
Playsound "explo1"
end sub

'bomb07

sub startbomb07
bombaction = 1
tbomb007.enabled = false
tbomb007.DestroyBall
BallsOnPlayfield = BallsOnPlayfield - 1
checkTbomby007.enabled = False
if ballsonplayfield <2 then  
bMultiBallMode = False 
end if
if BallsOnPlayfield = 0 then 
vpmtimer.addtimer 250, "BallsOnPlayfield = 1 : bMultiBallMode = False : multiballoutkicker '"
end if
bomb007.visible = True
changebombtored7
vpmTimer.AddTimer 200, "changebombtopurple7'"
vpmTimer.AddTimer 400, "changebombtored7'"
vpmTimer.AddTimer 600, "changebombtopurple7'"
vpmTimer.AddTimer 700, "changebombtored7'"
vpmTimer.AddTimer 800, "changebombtopurple7'"
vpmTimer.AddTimer 900, "changebombtored7'"
vpmTimer.AddTimer 1000, "hidebomb007'"
end sub

sub changebombtored7
bomb007.image = "bomb2"
end sub

sub changebombtopurple7
bomb007.image = "bomb1"
end sub

sub hidebomb007
bomb007.visible = False
bomby7
Playsound "explo1"
end sub

'bomb08

sub startbomb08
bombaction = 1
tbomb008.enabled = false
tbomb008.DestroyBall
BallsOnPlayfield = BallsOnPlayfield - 1
checkTbomby008.enabled = False
if ballsonplayfield <2 then  
bMultiBallMode = False 
end if
if BallsOnPlayfield = 0 then 
vpmtimer.addtimer 250, "BallsOnPlayfield = 1 : bMultiBallMode = False : multiballoutkicker '"
end if
bomb008.visible = True
changebombtored8
vpmTimer.AddTimer 200, "changebombtopurple8'"
vpmTimer.AddTimer 400, "changebombtored8'"
vpmTimer.AddTimer 600, "changebombtopurple8'"
vpmTimer.AddTimer 700, "changebombtored8'"
vpmTimer.AddTimer 800, "changebombtopurple8'"
vpmTimer.AddTimer 900, "changebombtored8'"
vpmTimer.AddTimer 1000, "hidebomb008'"
end sub

sub changebombtored8
bomb008.image = "bomb2"
end sub

sub changebombtopurple8
bomb008.image = "bomb1"
end sub

sub hidebomb008
bomb008.visible = False
bomby8
Playsound "explo1"
end sub

'bomb09

sub startbomb09
bombaction = 1
tbomb009.enabled = false
tbomb009.DestroyBall
BallsOnPlayfield = BallsOnPlayfield - 1
checkTbomby009.enabled = False
if ballsonplayfield <2 then  
bMultiBallMode = False 
end if
if BallsOnPlayfield = 0 then 
vpmtimer.addtimer 250, "BallsOnPlayfield = 1 : bMultiBallMode = False : multiballoutkicker '"
end if
bomb009.visible = True
changebombtored9
vpmTimer.AddTimer 200, "changebombtopurple9'"
vpmTimer.AddTimer 400, "changebombtored9'"
vpmTimer.AddTimer 600, "changebombtopurple9'"
vpmTimer.AddTimer 700, "changebombtored9'"
vpmTimer.AddTimer 800, "changebombtopurple9'"
vpmTimer.AddTimer 900, "changebombtored9'"
vpmTimer.AddTimer 1000, "hidebomb009'"
end sub

sub changebombtored9
bomb009.image = "bomb2"
end sub

sub changebombtopurple9
bomb009.image = "bomb1"
end sub

sub hidebomb009
bomb009.visible = False
bomby9
Playsound "explo1"
end sub

'bomb010

sub startbomb010
bombaction = 1
tbomb010.enabled = false
tbomb010.DestroyBall
BallsOnPlayfield = BallsOnPlayfield - 1
checkTbomby010.enabled = False
if ballsonplayfield <2 then  
bMultiBallMode = False 
end if
if BallsOnPlayfield = 0 then 
vpmtimer.addtimer 250, "BallsOnPlayfield = 1 : bMultiBallMode = False : multiballoutkicker '"
end if
bomb010.visible = True
changebombtored10
vpmTimer.AddTimer 200, "changebombtopurple10'"
vpmTimer.AddTimer 400, "changebombtored10'"
vpmTimer.AddTimer 600, "changebombtopurple10'"
vpmTimer.AddTimer 700, "changebombtored10'"
vpmTimer.AddTimer 800, "changebombtopurple10'"
vpmTimer.AddTimer 900, "changebombtored10'"
vpmTimer.AddTimer 1000, "hidebomb010'"
end sub

sub changebombtored10
bomb010.image = "bomb2"
end sub

sub changebombtopurple10
bomb010.image = "bomb1"
end sub

sub hidebomb010
bomb010.visible = False
bomby10
Playsound "explo1"
end sub

'**************************************
' Speed apron animation
'**************************************

dim countr35:countr35=0

Sub speedT_Timer
countr35 = countr35 + 1 : If Countr35 > 24 then switchspeed : end If 'stopbonusaniL
select case countr35
				case 1 : Flasher003.ImageA="n0"
				case 2 : Flasher003.ImageA="n1"
				case 3 : Flasher003.ImageA="n2"
				case 4 : Flasher003.ImageA="n3"
				case 5 : Flasher003.ImageA="n4"
				case 6 : Flasher003.ImageA="n5"
				case 7 : Flasher003.ImageA="n6"
				case 8 : Flasher003.ImageA="n7"
				case 9 : Flasher003.ImageA="n8"
				case 10 :Flasher003.ImageA="n9"
				case 11 :Flasher003.ImageA="n10"
				case 12 :Flasher003.ImageA="n11"
				case 13 :Flasher003.ImageA="n12"
				case 14 :Flasher003.ImageA="n13"
				case 15 :Flasher003.ImageA="n14"
				case 16 :Flasher003.ImageA="n15"
				case 17 :Flasher003.ImageA="n16"
				case 18 :Flasher003.ImageA="n17"
				case 19 :Flasher003.ImageA="n18"
				case 20 :Flasher003.ImageA="n19"
				case 21 :Flasher003.ImageA="n20"
				case 22 :Flasher003.ImageA="n21"
				case 23 :Flasher003.ImageA="n22"
				case 24 :Flasher003.ImageA="n23"							
			end Select
End Sub


sub switchspeed
speedT.enabled=false
countr36=0
speedT001.enabled=true
end sub

dim countr36:countr36=0
Sub speedT001_Timer
countr36 = countr36 + 1 : If Countr36 > 24 then speedT001.enabled=False : end If
select case countr36
				case 1 : Flasher003.ImageA="n23"
				case 2 : Flasher003.ImageA="n22"
				case 3 : Flasher003.ImageA="n21"
				case 4 : Flasher003.ImageA="n20"
				case 5 : Flasher003.ImageA="n19"
				case 6 : Flasher003.ImageA="n18"
				case 7 : Flasher003.ImageA="n17"
				case 8 : Flasher003.ImageA="n16"
				case 9 : Flasher003.ImageA="n15"
				case 10 :Flasher003.ImageA="n14"
				case 11 :Flasher003.ImageA="n13"
				case 12 :Flasher003.ImageA="n12"
				case 13 :Flasher003.ImageA="n11"
				case 14 :Flasher003.ImageA="n10"
				case 15 :Flasher003.ImageA="n9"
				case 16 :Flasher003.ImageA="n8"
				case 17 :Flasher003.ImageA="n7"
				case 18 :Flasher003.ImageA="n6"
				case 19 :Flasher003.ImageA="n5"
				case 20 :Flasher003.ImageA="n4"
				case 21 :Flasher003.ImageA="n3"
				case 22 :Flasher003.ImageA="n2"
				case 23 :Flasher003.ImageA="n1"
				case 24 :Flasher003.ImageA="n0"							
			end Select
End Sub


dim countr39:countr39=0
sub speedT002_Timer
countr39 = countr39 + 1 : If Countr39 > 3 then Countr39 = 1 : end If
select case countr39
				case 1 :Flasher003.ImageA="n10"
				case 2 :Flasher003.ImageA="n11"
				case 3 :Flasher003.ImageA="n12"
			end Select
End Sub

dim countr40:countr40=0
sub speedT003_Timer
countr40 = countr40 + 1 : If Countr40 > 13 then speedT003.enabled=0 : end If
select case countr40
				case 1 :Flasher003.ImageA="n11"
				case 2 :Flasher003.ImageA="n12"
				case 3 :Flasher003.ImageA="n13"
				case 4 :Flasher003.ImageA="n14"
				case 5 :Flasher003.ImageA="n15"
				case 6 :Flasher003.ImageA="n16"
				case 7 :Flasher003.ImageA="n17"
				case 8 :Flasher003.ImageA="n18"
				case 9 :Flasher003.ImageA="n19"
				case 10 :Flasher003.ImageA="n20"
				case 11 :Flasher003.ImageA="n21"
				case 12 :Flasher003.ImageA="n22"
				case 13 :Flasher003.ImageA="n23":speedT001.enabled=1
			end Select
End Sub

'**************************************
' Trafficlight
'**************************************
dim countr37:countr37=0

sub trafficT_timer
countr37 = countr37 + 1 : If Countr37 > 4 then trafficT.enabled=False : end If
select case countr37
				case 1 : stpl001.visible=false:stpl002.visible=true:dmd3
				case 2 : stpl002.visible=false:stpl003.visible=true:dmd2
				case 3 : stpl003.visible=false:stpl004.visible=true:dmd1
				case 4 : stpl004.visible=false:stpl001.visible=true:dmdgo:speedT002.enabled=0:speedT003.enabled=1
			end Select
End Sub


sub dmd3
DMD "", "", "3dmd", eNone, eNone, eNone, 1000, True, "v_three"
end sub

sub dmd2
DMD "", "", "2dmd", eNone, eNone, eNone, 1000, True, "v_two"
end sub

sub dmd1
DMD "", "", "1dmd", eNone, eNone, eNone, 1000, True, "v_one"
end sub

sub dmdgo
DMD "", "", "godmd", eNone, eNone, eNone, 1000, True, "v_awaytheygo"
countr39=0
countr36=0
end sub

'**************************************
' Trafficlight
'**************************************
dim countr38:countr38=0

sub introfuelT_timer
countr38 = countr38 + 1 : If Countr38 > 4 then introfuelT.enabled=False : end If
select case countr38
				case 1 : Flasher002.ImageA="n9"
				case 2 : Flasher002.ImageA="n12"
				case 3 : Flasher002.ImageA="n15"
				case 4 : Flasher002.ImageA="n18"
			end Select
End Sub

'*****************
' kijker
'*****************

Dim kijkerDir
kijkerDir = 5 'this is both the direction, if + goes up, if - goes down, and also the speed

Sub kijkerTimer_Timer
    kijker.z = kijker.z + kijkerDir
    If kijker.z > 180 Then kijkerDir = -5 'goes down
    If kijker.z < 105 Then kijkerTimer.Enabled = 0
End Sub

'************************************************
'**************Head lower pf Animation***********
'************************************************
Dim headDir, headPos

sub headdownTimer_Timer
If head.TransZ > -70 then 
head.TransZ = head.TransZ -2
end if
checkdiheaddown
end sub

Sub checkdiheaddown
If head.TransZ = -70 Then 
headdownTimer.enabled = 0
End If
end sub 

sub headupTimer_Timer
If head.TransZ < 0 then 
head.TransZ = head.TransZ +2
End If
checkdiheadup
end sub

Sub checkdiheadup
If head.TransZ = 0 Then 
	headupTimer.enabled = 0
End If
end sub 

'***************
'***************people waves**********************
'***************

sub dothewave1
Tpubu001.Enabled = 1
vpmtimer.addtimer 250, "dothewave2 ' "
vpmtimer.addtimer 500, "dothewave3 ' "
end sub

sub dothewave2
Tpubu002.Enabled = 1
end Sub

sub dothewave3
Tpubu003.Enabled = 1
end Sub


'***************row 1**********************
'pers001,pers002,pers003,pers004,pers005,pers006

sub Tpubu001_Timer
If pers001.z < 130 Then
pers001.z = pers001.z + 15
end if
If pers002.z < 130 Then
pers002.z = pers002.z + 15
end if
If pers003.z < 130 Then
pers003.z = pers003.z + 15
end if
If pers004.z < 130 Then
pers004.z = pers004.z + 15
end if
If pers005.z < 130 Then
pers005.z = pers005.z + 15
end if
If pers006.z < 130 Then
pers006.z = pers006.z + 15
end if
checkTpers001
end sub

sub checkTpers001
If pers001.z = 130 Then
Tpubu001.Enabled = 0
Tpubd001.Enabled = 1
end if
end sub

sub Tpubd001_Timer
If pers001.z > 100 then 
pers001.z = pers001.z -15
End If
If pers002.z > 100 then 
pers002.z = pers002.z -15
End If
If pers003.z > 100 then 
pers003.z = pers003.z -15
End If
If pers004.z > 100 then 
pers004.z = pers004.z -15
End If
If pers005.z > 100 then 
pers005.z = pers005.z -15
End If
If pers006.z > 100 then 
pers006.z = pers006.z -15
End If
checkTpers001down
end sub

sub checkTpers001down
If pers001.z = 100 Then
Tpubd001.Enabled = 0
end if
end sub

'***************row2**********************
'pers015, pers008pers009,pers007pers018,pers013

sub Tpubu002_Timer
If pers015.z < 145 Then
pers015.z = pers015.z + 15
end if
If pers008.z < 145 Then
pers008.z = pers008.z + 15
end if
If pers009.z < 145 Then
pers009.z = pers009.z + 15
end if
If pers007.z < 145 Then
pers007.z = pers007.z + 15
end if
If pers018.z < 145 Then
pers018.z = pers018.z + 15
end if
If pers013.z < 145 Then
pers013.z = pers013.z + 15
end if
checkTpers002
end sub

sub checkTpers002
If pers015.z = 145 Then
Tpubu002.Enabled = 0
Tpubd002.Enabled = 1
end if
end sub

sub Tpubd002_Timer
If pers015.z > 115 then 
pers015.z = pers015.z -15
End If
If pers008.z > 115 then 
pers008.z = pers008.z -15
End If
If pers009.z > 115 then 
pers009.z = pers009.z -15
End If
If pers007.z > 115 then 
pers007.z = pers007.z -15
End If
If pers018.z > 115 then 
pers018.z = pers018.z -15
End If
If pers013.z > 115 then 
pers013.z = pers013.z -15
End If
checkTpers002down
end sub

sub checkTpers002down
If pers015.z = 115 Then
Tpubd002.Enabled = 0
end if
end sub

'***************row3**********************
'pers014,pers017,pers016,pers011,pers012,pers010

sub Tpubu003_Timer
If pers014.z < 160 Then
pers014.z = pers014.z + 15
end if
If pers017.z < 160 Then
pers017.z = pers017.z + 15
end if
If pers016.z < 160 Then
pers016.z = pers016.z + 15
end if
If pers011.z < 160 Then
pers011.z = pers011.z + 15
end if
If pers012.z < 160 Then
pers012.z = pers012.z + 15
end if
If pers010.z < 160 Then
pers010.z = pers010.z + 15
end if
checkTpers003
end sub

sub checkTpers003
If pers014.z = 160 Then
Tpubu003.Enabled = 0
Tpubd003.Enabled = 1
end if
end sub

sub Tpubd003_Timer
If pers014.z > 130 then 
pers014.z = pers014.z -15
End If
If pers017.z > 130 then 
pers017.z = pers017.z -15
End If
If pers016.z > 130 then 
pers016.z = pers016.z -15
End If
If pers011.z > 130 then 
pers011.z = pers011.z -15
End If
If pers012.z > 130 then 
pers012.z = pers012.z -15
End If
If pers010.z > 130 then 
pers010.z = pers010.z -15
End If
checkTpers003down
end sub

sub checkTpers003down
If pers014.z = 130 Then
Tpubd003.Enabled = 0
end if
end sub

'***************
'********************************** SAW *****************
'***************

sub Tzaagmoveup_Timer
If saw001.TransY > -280 then 
saw001.TransY = saw001.TransY - 10
End If
checkTsaw
end sub

sub checkTsaw
If saw001.TransY = -280 Then
Tzaagmoveup.Enabled = 0
saw001.Visible = False
saw001.TransY = -280
Saw.Visible = True
sawT001.Enabled = True
end if
end sub

Sub sawT001_Timer()
	Saw.ObjRotZ = Saw.ObjRotZ + 1
	if Saw.ObjRotZ > 360 then
		Saw.ObjRotZ = 1
		'sawmovedown
	end if
end sub

sub sawmovedown
sawT001.Enabled = 0
Saw.visible = False
Saw001.visible = true
Tzaagmovedown.enabled = 1
end sub

sub Tzaagmovedown_timer
If Saw001.TransY < 0 then 
Saw001.TransY = Saw001.TransY + 10
End If
checkzaagmovedown
end sub

sub checkzaagmovedown
If Saw001.TransY = 0 Then
Tzaagmovedown.Enabled = 0
sawT002.enabled = 1
end if
end sub

Sub sawT002_Timer()
	Saw001.ObjRotZ = Saw001.ObjRotZ + 1
	if Saw001.ObjRotZ > 360 then
		Saw001.ObjRotZ = 1
		'sawmovedown
	end if
	'Saw.TransY = Saw.TransY - 1
end sub


'***************
'********************************** navigation images *****************
'***************

dim countr41:countr41=0

sub Tnavattract_timer
countr41 = countr41 + 1 : If Countr41 > 184 then Countr41 = 1 : end If
select case countr41
				case 1 : flashplaat2.ImageA="nav1"
				case 2 : flashplaat2.ImageA="nav1"
				case 3 : flashplaat2.ImageA="nav1"
				case 4 : flashplaat2.ImageA="nav1"
				case 5 : flashplaat2.ImageA="nav1"
				case 6 : flashplaat2.ImageA="nav1"
				case 7 : flashplaat2.ImageA="nav1"
				case 8 : flashplaat2.ImageA="nav1"
				case 9 : flashplaat2.ImageA="nav1"
				case 10 : flashplaat2.ImageA="nav1"
				case 11 : flashplaat2.ImageA="nav1"
				case 12 : flashplaat2.ImageA="nav1"
				case 13 : flashplaat2.ImageA="nav1"
				case 14 : flashplaat2.ImageA="nav1"
				case 15 : flashplaat2.ImageA="nav1"
				case 16 : flashplaat2.ImageA="nav1"
				case 17 : flashplaat2.ImageA="nav1"
				case 18 : flashplaat2.ImageA="nav1"
				case 19 : flashplaat2.ImageA="nav1"
				case 20 : flashplaat2.ImageA="nav1"
				case 21 : flashplaat2.ImageA="nav1"
				case 22 : flashplaat2.ImageA="nav1"
				case 23 : flashplaat2.ImageA="nav1"
				case 24 : flashplaat2.ImageA="nav1"
				case 25 : flashplaat2.ImageA="nav1"
				case 26 : flashplaat2.ImageA="nav1"
				case 27 : flashplaat2.ImageA="nav1"
				case 28 : flashplaat2.ImageA="nav1"
				case 29 : flashplaat2.ImageA="nav1"
				case 30 : flashplaat2.ImageA="nav1"
				case 31 : flashplaat2.ImageA="nav2"
				case 32 : flashplaat2.ImageA="nav2"
				case 33 : flashplaat2.ImageA="nav2"
				case 34 : flashplaat2.ImageA="nav2"
				case 35 : flashplaat2.ImageA="nav2"
				case 36 : flashplaat2.ImageA="nav2"
				case 37 : flashplaat2.ImageA="nav2"
				case 38 : flashplaat2.ImageA="nav2"
				case 39 : flashplaat2.ImageA="nav2"
				case 40 : flashplaat2.ImageA="nav2"
				case 41 : flashplaat2.ImageA="nav2"
				case 42 : flashplaat2.ImageA="nav2"
				case 43 : flashplaat2.ImageA="nav2"
				case 44 : flashplaat2.ImageA="nav2"
				case 45 : flashplaat2.ImageA="nav2"
				case 46 : flashplaat2.ImageA="nav2"
				case 47 : flashplaat2.ImageA="nav2"
				case 48 : flashplaat2.ImageA="nav2"
				case 49 : flashplaat2.ImageA="nav2"
				case 50 : flashplaat2.ImageA="nav2"
				case 51 : flashplaat2.ImageA="wtrac03"
				case 52 : flashplaat2.ImageA="wtrac04"
				case 53 : flashplaat2.ImageA="wtrac05"
				case 54 : flashplaat2.ImageA="wtrac06"
				case 55 : flashplaat2.ImageA="wtrac07"
				case 56 : flashplaat2.ImageA="wtrac08"
				case 57 : flashplaat2.ImageA="wtrac09"
				case 58 : flashplaat2.ImageA="wtrac10"
				case 59 : flashplaat2.ImageA="wtrac11"
				case 60 : flashplaat2.ImageA="wtrac12"
				case 61 : flashplaat2.ImageA="wtrac13"
				case 62 : flashplaat2.ImageA="wtrac14"
				case 63 : flashplaat2.ImageA="wtrac15"
				case 64 : flashplaat2.ImageA="wtrac16"
				case 65 : flashplaat2.ImageA="wtrac17"
				case 66 : flashplaat2.ImageA="wtrac18"
				case 67 : flashplaat2.ImageA="nav3"
				case 68 : flashplaat2.ImageA="nav3"
				case 69 : flashplaat2.ImageA="nav3"
				case 70 : flashplaat2.ImageA="nav3"
				case 71 : flashplaat2.ImageA="nav3"
				case 72 : flashplaat2.ImageA="nav3"
				case 73 : flashplaat2.ImageA="nav3"
				case 74 : flashplaat2.ImageA="nav3"
				case 75 : flashplaat2.ImageA="nav3"
				case 76 : flashplaat2.ImageA="nav3"
				case 77 : flashplaat2.ImageA="nav3"
				case 78 : flashplaat2.ImageA="nav3"
				case 79 : flashplaat2.ImageA="nav3"
				case 80 : flashplaat2.ImageA="nav3"
				case 81 : flashplaat2.ImageA="nav3"
				case 82 : flashplaat2.ImageA="nav3"
				case 83 : flashplaat2.ImageA="nav3"
				case 84 : flashplaat2.ImageA="nav3"
				case 85 : flashplaat2.ImageA="nav3"
				case 86 : flashplaat2.ImageA="nav3"
				case 87 : flashplaat2.ImageA="nav4"
				case 88 : flashplaat2.ImageA="nav4"
				case 89 : flashplaat2.ImageA="nav4"
				case 90 : flashplaat2.ImageA="nav4"
				case 91 : flashplaat2.ImageA="nav4"
				case 92 : flashplaat2.ImageA="nav4"
				case 93 : flashplaat2.ImageA="nav4"
				case 94 : flashplaat2.ImageA="nav4"
				case 95 : flashplaat2.ImageA="nav4"
				case 96 : flashplaat2.ImageA="nav4"
				case 97 : flashplaat2.ImageA="nav4"
				case 98 : flashplaat2.ImageA="nav4"
				case 99 : flashplaat2.ImageA="nav4"
				case 100 : flashplaat2.ImageA="nav4"
				case 101 : flashplaat2.ImageA="nav4"
				case 102 : flashplaat2.ImageA="nav4"
				case 103 : flashplaat2.ImageA="nav4"
				case 104 : flashplaat2.ImageA="nav4"
				case 105 : flashplaat2.ImageA="nav4"
				case 106 : flashplaat2.ImageA="nav4"
				case 107 : flashplaat2.ImageA="nav5"
				case 108 : flashplaat2.ImageA="nav5"
				case 109 : flashplaat2.ImageA="nav5"
				case 110 : flashplaat2.ImageA="nav5"
				case 111 : flashplaat2.ImageA="nav5"
				case 112 : flashplaat2.ImageA="nav5"
				case 113 : flashplaat2.ImageA="nav5"
				case 114 : flashplaat2.ImageA="nav5"
				case 115 : flashplaat2.ImageA="nav5"
				case 116 : flashplaat2.ImageA="nav5"
				case 117 : flashplaat2.ImageA="nav5"
				case 118 : flashplaat2.ImageA="nav5"
				case 119 : flashplaat2.ImageA="nav5"
				case 120 : flashplaat2.ImageA="nav5"
				case 121 : flashplaat2.ImageA="nav5"
				case 122 : flashplaat2.ImageA="nav5"
				case 123 : flashplaat2.ImageA="nav5"
				case 124 : flashplaat2.ImageA="nav5"
				case 125 : flashplaat2.ImageA="nav5"
				case 126 : flashplaat2.ImageA="nav5"
				case 127 : flashplaat2.ImageA="nav6"
				case 128 : flashplaat2.ImageA="nav6"
				case 129 : flashplaat2.ImageA="nav6"
				case 130 : flashplaat2.ImageA="nav6"
				case 131 : flashplaat2.ImageA="nav6"
				case 132 : flashplaat2.ImageA="nav6"
				case 133 : flashplaat2.ImageA="nav6"
				case 134 : flashplaat2.ImageA="nav6"
				case 135 : flashplaat2.ImageA="nav6"
				case 136 : flashplaat2.ImageA="nav6"
				case 137 : flashplaat2.ImageA="nav6"
				case 138 : flashplaat2.ImageA="nav6"
				case 139 : flashplaat2.ImageA="nav6"
				case 140 : flashplaat2.ImageA="nav6"
				case 141 : flashplaat2.ImageA="nav6"
				case 142 : flashplaat2.ImageA="nav6"
				case 143 : flashplaat2.ImageA="nav6"
				case 144 : flashplaat2.ImageA="nav6"
				case 145 : flashplaat2.ImageA="nav6"
				case 146 : flashplaat2.ImageA="nav6"
				case 146 : flashplaat2.ImageA="nav7"
				case 146 : flashplaat2.ImageA="nav7"
				case 147 : flashplaat2.ImageA="nav7"
				case 148 : flashplaat2.ImageA="nav7"
				case 149 : flashplaat2.ImageA="nav7"
				case 150 : flashplaat2.ImageA="nav7"
				case 151 : flashplaat2.ImageA="nav7"
				case 152 : flashplaat2.ImageA="nav7"
				case 153 : flashplaat2.ImageA="nav7"
				case 154 : flashplaat2.ImageA="nav7"
				case 155 : flashplaat2.ImageA="nav7"
				case 156 : flashplaat2.ImageA="nav7"
				case 157 : flashplaat2.ImageA="nav7"
				case 158 : flashplaat2.ImageA="nav7"
				case 159 : flashplaat2.ImageA="nav7"
				case 160 : flashplaat2.ImageA="nav7"
				case 161 : flashplaat2.ImageA="nav7"
				case 162 : flashplaat2.ImageA="nav7"
				case 163 : flashplaat2.ImageA="nav7"
				case 164 : flashplaat2.ImageA="nav7"
				case 165 : flashplaat2.ImageA="nav8"
				case 166 : flashplaat2.ImageA="nav8"
				case 167 : flashplaat2.ImageA="nav8"
				case 168 : flashplaat2.ImageA="nav8"
				case 169 : flashplaat2.ImageA="nav8"
				case 170 : flashplaat2.ImageA="nav8"
				case 171 : flashplaat2.ImageA="nav8"
				case 172 : flashplaat2.ImageA="nav8"
				case 173 : flashplaat2.ImageA="nav8"
				case 174 : flashplaat2.ImageA="nav8"
				case 175 : flashplaat2.ImageA="nav8"
				case 176 : flashplaat2.ImageA="nav8"
				case 177 : flashplaat2.ImageA="nav8"
				case 178 : flashplaat2.ImageA="nav8"
				case 179 : flashplaat2.ImageA="nav8"
				case 180 : flashplaat2.ImageA="nav8"
				case 181 : flashplaat2.ImageA="nav8"
				case 182 : flashplaat2.ImageA="nav8"
				case 183 : flashplaat2.ImageA="nav8"
				case 184 : flashplaat2.ImageA="nav8"
			end Select
End Sub


dim countr42:countr42=0
sub Tnavlaugh_timer
countr42 = countr42 + 1 : If Countr42 > 20 then Tnavlaugh.enabled = 0 : end If
select case countr42
				case 1 : flashplaat2.ImageA="dick00"
				case 2 : flashplaat2.ImageA="dick01"
				case 3 : flashplaat2.ImageA="dick02"
				case 4 : flashplaat2.ImageA="dick03"
				case 5 : flashplaat2.ImageA="dick04"
				case 6 : flashplaat2.ImageA="dick05"
				case 7 : flashplaat2.ImageA="dick06"
				case 8 : flashplaat2.ImageA="dick07"
				case 9 : flashplaat2.ImageA="dick08"
				case 10 : flashplaat2.ImageA="dick09"
				case 11 : flashplaat2.ImageA="dick10"
				case 12 : flashplaat2.ImageA="dick11"
				case 13 : flashplaat2.ImageA="dick12"
				case 14 : flashplaat2.ImageA="dick13"
				case 15 : flashplaat2.ImageA="dick14"
				case 16 : flashplaat2.ImageA="dick15"
				case 17 : flashplaat2.ImageA="dick16"
				case 18 : flashplaat2.ImageA="dick01"
				case 19 : flashplaat2.ImageA="dick02"
				case 20 : flashplaat2.ImageA="dick03"
			end Select
End Sub

'***********dick & muttley shakers*************
Dim p00shake1, p00shake2

Sub mutleyshaker()
    p00shake1 = 6
    t00shake001.Enabled = True
End Sub

Sub t00shake001_Timer()
   mutley.Transz = p00shake1 / 2
    If  p00shake1 = 0 Then Me.Enabled = False:Exit Sub
    If  p00shake1 <0 Then
         p00shake1 = ABS( p00shake1)- 0.1
    Else
         p00shake1 = -  p00shake1 + 0.1
    End If
End Sub


Sub dickieshaker()
    p00shake2 = 6
    t00shake002.Enabled = True
End Sub

Sub t00shake002_Timer()
   dickie.Transz = p00shake2 / 2
    If  p00shake2 = 0 Then Me.Enabled = False:Exit Sub
    If  p00shake2 <0 Then
         p00shake2 = ABS( p00shake2)- 0.1
    Else
         p00shake2 = -  p00shake2 + 0.1
    End If
End Sub




'******************************************************
' Flippers Polarity (Select appropriate sub based on era) 
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity


'*******************************************
' Early 90's and after

Sub InitPolarity()
        dim x, a : a = Array(LF, RF)
        for each x in a
                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
                x.enabled = True
                x.TimeDelay = 60
        Next

        AddPt "Polarity", 0, 0, 0
        AddPt "Polarity", 1, 0.05, -5.5
        AddPt "Polarity", 2, 0.4, -5.5
        AddPt "Polarity", 3, 0.6, -5.0
        AddPt "Polarity", 4, 0.65, -4.5
        AddPt "Polarity", 5, 0.7, -4.0
        AddPt "Polarity", 6, 0.75, -3.5
        AddPt "Polarity", 7, 0.8, -3.0
        AddPt "Polarity", 8, 0.85, -2.5
        AddPt "Polarity", 9, 0.9,-2.0
        AddPt "Polarity", 10, 0.95, -1.5
        AddPt "Polarity", 11, 1, -1.0
        AddPt "Polarity", 12, 1.05, -0.5
        AddPt "Polarity", 13, 1.1, 0
        AddPt "Polarity", 14, 1.3, 0

        addpt "Velocity", 0, 0,         1
        addpt "Velocity", 1, 0.16, 1.06
        addpt "Velocity", 2, 0.41,         1.05
        addpt "Velocity", 3, 0.53,         1'0.982
        addpt "Velocity", 4, 0.702, 0.968
        addpt "Velocity", 5, 0.95,  0.968
        addpt "Velocity", 6, 1.03,         0.945

        LF.Object = LeftFlipper        
        LF.EndPoint = EndPointLp
        RF.Object = RightFlipper
        RF.EndPoint = EndPointRp
End Sub


' Flipper trigger hit subs
Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub



'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt        'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay        'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart,FlipperEnd, FlipperEndY, LR, PartialFlipCoef
	Private Balls(20), balldata(20)

	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut
	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
	End Sub

	Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
	Public Property Let StartPoint(aInput) : if IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end if : End Property
	Public Property Get StartPoint : StartPoint = FlipperStart : End Property
	Public Property Let EndPoint(aInput) : FlipperEnd = aInput.x: FlipperEndY = aInput.y: End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property        
	Public Property Get EndPointY: EndPointY = FlipperEndY : End Property

	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
		if gametime > 100 then Report aChooseArray
	End Sub 

	Public Sub Report(aChooseArray)         'debug, reports all coords in tbPL.text
		if not DebugOn then exit sub
		dim a1, a2 : Select Case aChooseArray
			case "Polarity" : a1 = PolarityIn : a2 = PolarityOut
			Case "Velocity" : a1 = VelocityIn : a2 = VelocityOut
			Case "Ycoef" : a1 = YcoefIn : a2 = YcoefOut 
				case else :tbpl.text = "wrong string" : exit sub
		End Select
		dim str, x : for x = 0 to uBound(a1) : str = str & aChooseArray & " x: " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		tbpl.text = str
	End Sub

	Public Sub AddBall(aBall) : dim x : for x = 0 to uBound(balls) : if IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end if : Next  : End Sub

	Private Sub RemoveBall(aBall)
		dim x : for x = 0 to uBound(balls)
			if TypeName(balls(x) ) = "IBall" then 
				if aBall.ID = Balls(x).ID Then
					balls(x) = Empty
					Balldata(x).Reset
				End If
			End If
		Next
	End Sub

	Public Sub Fire() 
		Flipper.RotateToEnd
		processballs
	End Sub

	Public Property Get Pos 'returns % position a ball. For debug stuff.
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next                
	End Property

	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				balldata(x).Data = balls(x)
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function        'Timer shutoff for polaritycorrect

	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then 
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1

			'y safety Exit
			if aBall.VelY > -8 then 'ball going down
				RemoveBall aBall
				exit Sub
			end if

			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)                                'find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)                                                'find safety coefficient 'ycoef' data
			End If

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR

				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)        'Shuffle objects in a temp array
		if not IsEmpty(aArray(x) ) Then
			if IsObject(aArray(x)) then 
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	if offset < 0 then offset = 0
	redim aArray(aCount-1+offset)        'Resize original array
	for x = 0 to aCount-1                'set objects back into original array
		if IsObject(a(x)) then 
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

' Used for flipper correction and rubber dampeners
Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

' Used for flipper correction, rubber dampeners, and drop targets
Function BallSpeed(ball) 'Calculates the ball speed
	BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)        'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball 
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius 
	Public Property Let Data(aBall)
		With aBall
			x = .x : y = .y : z = .z : velx = .velx : vely = .vely : velz = .velz
			id = .ID : mass = .mass : radius = .radius
		end with
	End Property
	Public Sub Reset()
		x = Empty : y = Empty : z = Empty  : velx = Empty : vely = Empty : velz = Empty 
		id = Empty : mass = Empty : radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)        'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)        'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) )         'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )        'Clamp upper

	LinearEnvelope = Y
End Function

'******************************************************
'  FLIPPER TRICKS 
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
end sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b, BOT
	BOT = GetBalls

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					exit Sub
				end If
			Next
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				end If
			Next
		End If
	Else 
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 then EOSNudge1 = 0
	End If
End Sub

'*****************
' Maths
'*****************
Dim PI: PI = 4*Atn(1)

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

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

'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI /180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle+90))+Flipper.x, Sin(Radians(Flipper.currentangle+90))+Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle  = ABS(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360

	If DistanceFromFlipper(ballx,bally,Flipper) < 48 and DiffAngle <= 90 and Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If        
End Function


'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1 'EM's to late 80's
Const EOSTnew = 0.8 '90's and later
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode 
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 6
	Case 2:
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'Const EOSReturn = 0.055  'EM's
'Const EOSReturn = 0.045  'late 70's to mid 80's
'Const EOSReturn = 0.035  'mid 80's to early 90's
Const EOSReturn = 0.025  'mid 90's and later

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle

Sub FlipperActivate(Flipper, FlipperPress)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity

	Flipper.eostorque = EOST         
	Flipper.eostorqueangle = EOSA         
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
	FlipperPress = 0
	Flipper.eostorqueangle = EOSA
	Flipper.eostorque = EOST*EOSReturn/FReturn


	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b, BOT
		BOT = GetBalls

		For b = 0 to UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= -0.4 Then BOT(b).vely = -0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)        '-1 for Right Flipper

	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup 
			Flipper.endangle = FEndAngle - 3*Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0 
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) and FlipperPress = 1 then
		if FCount = 0 Then FCount = GameTime

		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup                        
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 
		If FState <> 3 Then
			Flipper.eostorque = EOST        
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If

	End If
End Sub

Const LiveDistanceMin = 30  'minimum distance in vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)    '-1 for Right Flipper
	Dim LiveCatchBounce                                                                                                                        'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount

	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.5 Then                                                'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)        'Partial catch when catch happens a bit late
		end If

		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm
	End If
End Sub


'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************



'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7

sub TargetBouncer(aBall,defvalue)
    dim zMultiplier, vel, vratio
    if TargetBouncerEnabled = 1 and aball.z < 30 then
        'debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        vel = BallSpeed(aBall)
        if aBall.velx = 0 then vratio = 1 else vratio = aBall.vely/aBall.velx
        Select Case Int(Rnd * 6) + 1
            Case 1: zMultiplier = 0.2*defvalue
			Case 2: zMultiplier = 0.25*defvalue
            Case 3: zMultiplier = 0.3*defvalue
			Case 4: zMultiplier = 0.4*defvalue
            Case 5: zMultiplier = 0.45*defvalue
            Case 6: zMultiplier = 0.5*defvalue
        End Select
        aBall.velz = abs(vel * zMultiplier * TargetBouncerFactor)
        aBall.velx = sgn(aBall.velx) * sqr(abs((vel^2 - aBall.velz^2)/(1+vratio^2)))
        aBall.vely = aBall.velx * vratio
        'debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        'debug.print "conservation check: " & BallSpeed(aBall)/vel
	end if
end sub

' Add targets or posts to the TargetBounce collection if you want to activate the targetbouncer code from them
Sub TargetBounce_Hit
	TargetBouncer activeball, 1
End Sub


'******************************************************
'****  PHYSICS DAMPENERS
'******************************************************
'
' These are data mined bounce curves, 
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR



Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	TargetBouncer Activeball, 1
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
	TargetBouncer Activeball, 0.7
End Sub


dim RubbersD : Set RubbersD = new Dampener        'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False        'shows info in textbox "TBPout"
RubbersD.Print = False        'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967        'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64        'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener        'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False        'shows info in textbox "TBPout"
SleevesD.Print = False        'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'#########################    Adjust these values to increase or lessen the elasticity

dim FlippersD : Set FlippersD = new Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False	
FlippersD.addpoint 0, 0, 1.1	
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold         'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): End Sub 

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		if gametime > 100 then Report
	End Sub

	public sub Dampen(aBall)
		if threshold then if BallSpeed(aBall) < threshold then exit sub end if end if
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)

		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	public sub Dampenf(aBall, parm) 'Rubberizer is handle here
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
		coef = desiredcor / realcor 
		If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
			aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		End If
	End Sub

	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub


	Public Sub Report()         'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub

End Class



'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
	public ballvel, ballvelx, ballvely

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs

		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds
		if uBound(ballvelx) < highestID then redim ballvelx(highestID)	'set bounds
		if uBound(ballvely) < highestID then redim ballvely(highestID)	'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

' Note, cor.update must be called in a 10 ms timer. The example table uses the GameTimer for this purpose, but sometimes a dedicated timer call RDampen is used.
'
Sub RDampen_Timer
	Cor.Update
	RollingUpdate
End Sub



'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************


'******************************************************
'****  BALL ROLLING AND DROP SOUNDS
'******************************************************
'
' Be sure to call RollingUpdate in a timer with a 10ms interval see the GameTimer_Timer() sub

ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
	Next
End Sub

Sub RollingUpdate()
	Dim BOT, b
	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
		' Comment the next line if you are not implementing Dyanmic Ball Shadows
		'If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next

	' exit the sub if no balls on the table
	If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(BOT)
		If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If

		' Ball Drop Sounds
		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If BOT(b).velz > -7 Then
					RandomSoundBallBouncePlayfieldSoft BOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard BOT(b)
				End If				
			End If
		End If
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If

		If BOT(b).z < -50 Then 
			BOT(b).z = 50 : BOT(b).x = 400 : BOT(b).y= 1983
			VUKGH.CreateSizedball BallSize / 2
			VUKGH.Kick 0, 52, 1.25
			BallsOnPlayfield=BallsOnPlayfield+1
			debug.print "LOST BALL"
			debug.print "LOST BALL"
			debug.print "LOST BALL"
		End If

		' "Static" Ball Shadows
		' Comment the next If block, if you are not implementing the Dyanmic Ball Shadows
'		If AmbientBallShadowOn = 0 Then
'			If BOT(b).Z > 30 Then
'				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
'			Else
'				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
'			End If
'			BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + fovY
'			BallShadowA(b).X = BOT(b).X
'			BallShadowA(b).visible = 1
'		End If
	Next
End Sub


'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************








'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.  
' Create the following new collections:
' 	Metals (all metal objects, metal walls, metal posts, metal wire guides)
' 	Apron (the apron walls and plunger wall)
' 	Walls (all wood or plastic walls)
' 	Rollovers (wire rollover triggers, star triggers, or button triggers)
' 	Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
' 	Gates (plate gates)
' 	GatesWire (wire gates)
' 	Rubbers (all rubbers including posts, sleeves, pegs, and bands)
' When creating the collections, make sure "Fire events for this collection" is checked.  
' You'll also need to make sure "Has Hit Event" is checked for each object placed in these collections (not necessary for gates and triggers).  
' Once the collections and objects are added, the save, close, and restart VPX.
'
' Many places in the script need to be modified to include the correct sound effect subroutine calls. The tutorial videos linked below demonstrate 
' how to make these updates. But in summary the following needs to be updated:	
'	- Nudging, plunger, coin-in, start button sounds will be added to the keydown and keyup subs.
'	- Flipper sounds in the flipper solenoid subs. Flipper collision sounds in the flipper collide subs.
'	- Bumpers, slingshots, drain, ball release, knocker, spinner, and saucers in their respective subs
'	- Ball rolling sounds sub
'
' Tutorial vides by Apophis
' Part 1: 	https://youtu.be/PbE2kNiam3g
' Part 2: 	https://youtu.be/B5cm1Y8wQsk
' Part 3: 	https://youtu.be/eLhWyuYOyGg


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1														'volume level; range [0, 1]
NudgeLeftSoundLevel = 1													'volume level; range [0, 1]
NudgeRightSoundLevel = 1												'volume level; range [0, 1]
NudgeCenterSoundLevel = 1												'volume level; range [0, 1]
StartButtonSoundLevel = 0.1												'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr											'volume level; range [0, 1]
PlungerPullSoundLevel = 1												'volume level; range [0, 1]
RollingSoundFactor = 1.1/5		

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010           						'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635								'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                        						'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                      						'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel								'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel								'sound helper; not configurable
SlingshotSoundLevel = 0.95												'volume level; range [0, 1]
BumperSoundFactor = 4.25												'volume multiplier; must not be zero
KnockerSoundLevel = 1 													'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2									'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5											'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5											'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5										'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025									'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025									'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8									'volume level; range [0, 1]
WallImpactSoundFactor = 0.075											'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5													'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10											'volume multiplier; must not be zero
DTSoundLevel = 0.25														'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                              					'volume level; range [0, 1]
SpinnerSoundLevel = 0.5                              					'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8														'volume level; range [0, 1]
BallReleaseSoundLevel = 1												'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2									'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015										'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5													'volume multiplier; must not be zero


'/////////////////////////////  SOUND PLAYBACK FUNCTIONS  ////////////////////////////
'/////////////////////////////  POSITIONAL SOUND PLAYBACK METHODS  ////////////////////////////
' Positional sound playback methods will play a sound, depending on the X,Y position of the table element or depending on ActiveBall object position
' These are similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
' For surround setup - positional sound playback functions will fade between front and rear surround channels and pan between left and right channels
' For stereo setup - positional sound playback functions will only pan between left and right channels
' For mono setup - positional sound playback functions will not pan between left and right channels and will not fade between front and rear channels

' PlaySound full syntax - PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)
' Note - These functions will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
	PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
	PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
	PlaySound soundname, 1, aVol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
	PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
    tmp = tableobj.y * 2 / tableheight-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / tablewidth-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = Csng(BallVel(ball) ^2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = Csng((ball.velz) ^2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^2 * 15
End Function

Function RndInt(min, max)
	RndInt = Int(Rnd() * (max-min + 1) + min)' Sets a random number integer between min and max
End Function

Function RndNum(min, max)
	RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////
Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
End Sub


Sub SoundPlungerPull()
	PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseBall()
	PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Plunger	
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub


'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////
Sub KnockerSolenoid()
	PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, KnockerPosition
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////
Sub RandomSoundDrain(drainswitch)
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd*11)+1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd*7)+1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd*10)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd*8)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////
Sub SoundSpinner(spinnerswitch)
	PlaySoundAtLevelStatic ("Spinner"), SpinnerSoundLevel, spinnerswitch
End Sub


'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////
Sub SoundFlipperUpAttackLeft(flipper)
	FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-L01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-R01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////
Sub RandomSoundFlipperUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd*9)+1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd*9)+1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd*7)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd*8)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm/10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm/10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd*7)+1), parm  * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd*4)+1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////
Sub Rubbers_Hit(idx)
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 5 then		
		RandomSoundRubberStrong 1
	End if
	If finalspeed <= 5 then
		RandomSoundRubberWeak()
	End If	
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////
Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 2 : PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 3 : PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 4 : PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 5 : PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 6 : PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 7 : PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 8 : PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 9 : PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 10 : PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6*voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////
Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd*9)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
	RandomSoundWall()      
End Sub

Sub RandomSoundWall()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		Select Case Int(Rnd*5)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*4)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////
Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd*13)+1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

Sub Metals_Hit (idx)
	RandomSoundMetal
End Sub

Sub ShooterDiverter_collide(idx)
	RandomSoundMetal
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////
Sub RandomSoundBottomArchBallGuide()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		PlaySoundAtLevelActiveBall ("Apron_Bounce_"& Int(Rnd*2)+1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////
Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd*3)+1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(activeball.id) < 4) and cor.ballvely(activeball.id) > 7 then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////
Sub RandomSoundFlipperBallGuide()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd*3)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd*7)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()		
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 10 then
		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft Activeball
	Else 
		RandomSoundTargetHitWeak()
	End If	
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound	
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////
Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd*7)+1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////
Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()			
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd*2)+1), GateSoundLevel, Activeball
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, Activeball
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)	
	SoundPlayfieldGate	
End Sub	

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub


Sub Arch1_hit()
	If Activeball.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If activeball.velx < -8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If Activeball.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If activeball.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd*2)+1), SaucerLockSoundLevel, Activeball
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0: PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1: PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////
Sub OnBallBallCollision(ball1, ball2, velocity)
	Dim snd
	Select Case Int(Rnd*7)+1
		Case 1 : snd = "Ball_Collide_1"
		Case 2 : snd = "Ball_Collide_2"
		Case 3 : snd = "Ball_Collide_3"
		Case 4 : snd = "Ball_Collide_4"
		Case 5 : snd = "Ball_Collide_5"
		Case 6 : snd = "Ball_Collide_6"
		Case 7 : snd = "Ball_Collide_7"
	End Select

	PlaySound (snd), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd*6)+1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd*6)+1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER RELAYS  ////////////////////////////

Const RelayFlashSoundLevel = 0.315									'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05									'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025*RelayGISoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025*RelayGISoundLevel, obj
	End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025*RelayFlashSoundLevel, obj			
		Case 0
			PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025*RelayFlashSoundLevel, obj		
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************



'******************************************************
'**** RAMP ROLLING SFX
'******************************************************

'Ball tracking ramp SFX 1.0
'   Reqirements:
'          * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'          * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'          * Create a Timer called RampRoll, that is enabled, with a interval of 100
'          * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'          * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'          * To stop tracking ball
'                 * call WireRampOff
'                 * Otherwise, the ball will auto remove if it's below 30 vp units
'

dim RampMinLoops : RampMinLoops = 4

' RampBalls
'      Setup:        Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RammBalls(6,2)
'      Description:  
dim RampBalls(12,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(12)	

Sub WireRampOn(input)  : Waddball ActiveBall, input : RampRollUpdate: End Sub
Sub WireRampOff() : WRemoveBall ActiveBall.ID	: End Sub


' WaddBall (Active Ball, Boolean)
'     Description: This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
Sub Waddball(input, RampInput)	'Add ball
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	dim x : for x = 1 to uBound(RampBalls)	'Check, don't add balls twice
		if RampBalls(x, 1) = input.id then 
			if Not IsEmpty(RampBalls(x,1) ) then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next

	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
	For x = 1 to uBound(RampBalls)
		if IsEmpty(RampBalls(x, 1)) then 
			Set RampBalls(x, 0) = input
			RampBalls(x, 1)	= input.ID
			RampType(x) = RampInput
			RampBalls(x, 2)	= 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			exit Sub
		End If
		if x = uBound(RampBalls) then 	'debug
			Debug.print "WireRampOn error, ball queue is full: " & vbnewline & _
			RampBalls(0, 0) & vbnewline & _
			Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbnewline & _
			Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbnewline & _
			Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbnewline & _
			Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbnewline & _
			Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbnewline & _
			" "
		End If
	next
End Sub

' WRemoveBall (BallId)
'    Description: This subroutine is called from the RampRollUpdate subroutine 
'                 and is used to remove and stop the ball rolling sounds
Sub WRemoveBall(ID)		'Remove ball
	'Debug.Print "In WRemoveBall() + Remove ball from loop array"
	dim ballcount : ballcount = 0
	dim x : for x = 1 to Ubound(RampBalls)
		if ID = RampBalls(x, 1) then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		if not IsEmpty(Rampballs(x,1)) then ballcount = ballcount + 1
	next
	if BallCount = 0 then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer():RampRollUpdate:End Sub

Sub RampRollUpdate()		'Timer update
	dim x : for x = 1 to uBound(RampBalls)
		if Not IsEmpty(RampBalls(x,1) ) then 
			if BallVel(RampBalls(x,0) ) > 1 then ' if ball is moving, play rolling sound
				If RampType(x) then 
					PlaySound("RampLoop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial*2, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))				
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial*2, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2)	= RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
			end if
			if RampBalls(x,0).Z < 30 and RampBalls(x, 2) > RampMinLoops then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end if
	next
	if not RampBalls(0,0) then RampRoll.enabled = 0

End Sub

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
Sub tbWR_Timer()	'debug textbox
	me.text =	"on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbnewline & _
	"1 " & Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbnewline & _
	"2 " & Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbnewline & _
	"3 " & Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbnewline & _
	"4 " & Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbnewline & _
	"5 " & Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbnewline & _
	"6 " & Typename(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbnewline & _
	" "
End Sub


Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
    BallPitch = pSlope(BallVel(ball), 1, -1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, -4000, 60, 7000)
End Function



'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************


'*******************************************
'  Ramp Triggers
'*******************************************

Sub ramptrigger01_hit()
	debug.print "trigger01 hit-on"
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

sub ramptrigger001_hit()
	debug.print "trigger001 hit-off"
	WireRampOff 
End Sub

Sub ramptrigger003_hit()
	debug.print "trigger003 hit-on"
	playsound "fx_metalrolling"
	WireRampOn True ' On Wire Ramp Play Wire Ramp Sound
End Sub

Sub ramptrigger004_hit() 
	debug.print "trigger004 hit-off"
	WireRampOff ' turn off ramp sound
End Sub

Sub ramptrigger005_hit() 
	debug.print "trigger005 hit-on"
	WireRampOn true ' On Wire Ramp Play Wire Ramp Sound
End Sub

Sub ramptrigger006_hit()
	debug.print "trigger006 hit-off"
	WireRampOff ' turn off ramp sound
End Sub

Sub ramptrigger007_hit() 
	debug.print "trigger003 hit-on"
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub ramptrigger009_hit() 
	debug.print "trigger009 hit-on"
	WireRampOn True' On Wire Ramp Play Wire Ramp Sound
End Sub

Sub ramptrigger011_hit()
	debug.print "trigger011 hit-on"
	WireRampOn true ' On Wire Ramp Play Wire Ramp Sound
End Sub

Sub ramptrigger012_hit()
	debug.print "trigger012 hit-on"
	WireRampOn true ' On Wire Ramp Play Wire Ramp Sound
End Sub


'sub ramptrigger014_hit()
'	debug.print "trigger014 hit-on"
'	WireRampOn False 
'End Sub


'*******************************************
'  End Ramp Triggers
'*******************************************
