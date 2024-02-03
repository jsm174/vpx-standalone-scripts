' ******************************************************************
'       VPX - version by Klodo81 2023, Cannes version 1.3
'
'                 VISUAL PINBALL X EM Script Based on
'               JPSalas Basic EM script up to 4 players		          
'                     JPSalas Physics 4.3.0
'    	
'
'          Cannes / IPD No.428 / June ,1976 / 4 Players  
'
'V1.1 Mod table and Backglass
'- B2S Mod (download the backglass)
'- Mod Song more realistic with 3 chimes
'- Mod some lights and script for Backglass proposed by Mustang1961
'
'V1.2 mod table
'- Mod Rev3 Physics from JPSalas
'- Added 3D Playfield Mesh + new kickers
'- Some others things
'
'V1.3 mod table
'- New LUT from JPSalas
'- Mod Playfield Mesh
'- Added "Same player shoots again" on Backglass DT
'- Some others things
' ******************************************************************

Option Explicit
Randomize

' DOF config
'
' Flippers L/R - 101/102
' Slingshots L/R - 103/104
' Bumpers L to R - 105/106
' Kickers - 131/132/133/250/104
' Targets Round - 201 to 203
' Triggers - 204 to 210
' Knocker - 110
' Chimes - 301/302/303
' Drain - 250

' core.vbs constants
Const BallSize = 50 ' 50 is the normal size
Const BallMass = 1  ' 1 is the normal ball mass.

' load extra vbs files
LoadCoreFiles

Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    On Error Resume Next
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "Can't open controller.vbs"
End Sub

'****************** Options you can change *******************

Const BallsPerGame = 3     ' to play with 3 or 5 balls (Segasa said : 3 Ball is a must on this Game)
Const AttractMode = 1      ' 0 = no attract mode
Const FreePlay = False     ' Free play or coins
 
'*************************************************************

' Constants
Const TableName = "Cannes_76VPX"   ' file name to save highscores and other variables
Const cGameName = "Cannes1976"   ' B2S name
Const MaxPlayers = 4        ' 1 to 4 can play
Const Special1 = 370000      ' 3 Balls award credit
Const Special2 = 460000      ' 3 Balls award credit
Const Special3 = 550000      ' 3 Balls award credit
Const Special4 = 530000      ' 5 Balls award credit
Const Special5 = 710000      ' 5 Balls award credit

' Global variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim Bonus
Dim BallsRemaining(4)
Dim BonusMultiplier
Dim ExtraBallsAwards(4)
Dim Special1Awarded(4)
Dim Special2Awarded(4)
Dim Special3Awarded(4)
Dim Special4Awarded(4)
Dim Special5Awarded(4)
Dim Score(4)
Dim HighScore
Dim Match
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim Add10
Dim Add100
Dim Add1000
Dim Add10000
Dim CBonus
Dim x
Dim SpinCounter
Dim KickerTBonus
Dim CenterBonusCounter
Dim SpecialCenter
Dim MoveTarget
Dim MoveDirection

' Control variables
Dim BallsOnPlayfield

' Boolean variables
Dim bAttractMode
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bExtraBallWonThisBall
Dim bJustStarted
Dim bBallInPlungerLane
Dim bBallSaverActive

' core.vbs variables

' *********************************************************************
'                Common rutines to all the tables
' *********************************************************************

Sub Table1_Init()
    Dim x

    ' Init som objects, like walls, targets
    VPObjects_Init
    LoadEM

    ' load highscores and credits
    Loadhs
    ScoreReel1.SetValue HSScore(1)
    If B2SOn then
        Controller.B2SSetScorePlayer 1, HSScore(1)
    End If
    UpdateCredits

    ' init all the global variables
    bFreePlay = Freeplay
    bAttractMode = False
    bOnTheFirstBall = False
    bGameInPlay = False
    bBallInPlungerLane = False
    BallsOnPlayfield = 0
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    Match = 0
    bJustStarted = True
    Add10 = 0
    Add100 = 0
    Add1000 = 0
	Add10000 = 0

	' select Card instruction on Apron
	If BallsPerGame = 3 Then
		Flasher3balls.visible = 1
		Flasher5balls.visible = 0
	Else 'BallsPerGame = 5
		Flasher3balls.visible = 0
		Flasher5balls.visible = 1
	End If

	'Test position Match on desktop screen
	'Match = 50 '(00 to 90)
	'Display_Match

    ' setup table in game over mode
    EndOfGame

    'turn on GI lights
    vpmtimer.addtimer 1000, "GiOn '"

    ' Remove desktop items in FS mode
    If Table1.ShowDT then
        For each x in aReels
            x.Visible = 1
        Next
    Else
        For each x in aReels
            x.Visible = 0
        Next
    End If

	' Moving Target Init
	for MoveTarget = 0 to 49
		Collection2(MoveTarget).Visible = 0
		Collection2(MoveTarget).collidable = False
		Collection2(MoveTarget).hashitevent = False
	Next
	CenterT50.Collidable = True
	CenterT50.HasHitEvent = True
	MoveTarget=49
	MoveDirection=0
	MovingTargetTimer.enabled = true
	PtargetC.objroty=((MoveTarget*.8)-20.5)

    ' LUT - Darkness control
    LoadLUT
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

    If EnteringInitials then
        CollectInitials(keycode)
        Exit Sub
    End If

    If keycode = LeftMagnaSave Then bLutActive = True:SetLUTLine "Color LUT image " & table1.ColorGradeImage
    If keycode = RightMagnaSave AND bLutActive Then NextLUT:End If

    ' add coins
    If Keycode = AddCreditKey Then
        If(Tilted = False) Then
            AddCredits 1
            PlaySound "fx_coin"
        End If
    End If

    ' plunger
    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
    End If

    ' tilt keys
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt

    ' keys during game

    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1

        If keycode = StartGameKey Then
            If((PlayersPlayingGame <MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1    
                    UpdateBallInPlay
					UpdatePlayers
                Else
                    If(Credits> 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        Credits = Credits - 1
                        UpdateCredits
                        UpdateBallInPlay
						UpdatePlayers
						Playsound"BallyStartButtonPlayers2-4 plus 10dB"
                    End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = True) Then
                    If(BallsOnPlayfield = 0) Then
                        ResetScores
                        ResetForNewGame()
                    End If
                Else
                    If(Credits> 0) Then
                        If(BallsOnPlayfield = 0) Then
                            Credits = Credits - 1
                            UpdateCredits
                            ResetScores
                            ResetForNewGame()
						Playsound"BallyStartButtonPlayer1+3dB"
                        End If
                    End If
                End If
            End If
    End If ' If (GameInPlay)
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If EnteringInitials then
        Exit Sub
    End If

    If keycode = LeftMagnaSave Then bLutActive = False:HideLUT

    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftFlipperKey Then SolLFlipper 0
        If keycode = RightFlipperKey Then SolRFlipper 0
    End If

    If keycode = PlungerKey Then
        Plunger.Fire
        If bBallInPlungerLane Then
            PlaySoundAt "fx_plunger", plunger
        Else
            PlaySoundAt "fx_plunger_empty", plunger
        End If
    End If
End Sub

'******************
' Table stop/pause
'******************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub table1_Exit
    Savehs
	'Controller.Stop
End Sub

'************************************
'       LUT - Darkness control
' 10 normal level & 10 warmer levels
'************************************

Dim bLutActive, LUTImage

Sub LoadLUT
    Dim x
    bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "") Then LUTImage = x Else LUTImage = 0
    UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT:LUTImage = (LUTImage + 1) MOD 22:UpdateLUT:SaveLUT:SetLUTLine "Color LUT image " & table1.ColorGradeImage:End Sub

Sub UpdateLUT
    Select Case LutImage
        Case 0:table1.ColorGradeImage = "LUT0"
        Case 1:table1.ColorGradeImage = "LUT1"
        Case 2:table1.ColorGradeImage = "LUT2"
        Case 3:table1.ColorGradeImage = "LUT3"
        Case 4:table1.ColorGradeImage = "LUT4"
        Case 5:table1.ColorGradeImage = "LUT5"
        Case 6:table1.ColorGradeImage = "LUT6"
        Case 7:table1.ColorGradeImage = "LUT7"
        Case 8:table1.ColorGradeImage = "LUT8"
        Case 9:table1.ColorGradeImage = "LUT9"
        Case 10:table1.ColorGradeImage = "LUT10"
        Case 11:table1.ColorGradeImage = "LUT Warm 0"
        Case 12:table1.ColorGradeImage = "LUT Warm 1"
        Case 13:table1.ColorGradeImage = "LUT Warm 2"
        Case 14:table1.ColorGradeImage = "LUT Warm 3"
        Case 15:table1.ColorGradeImage = "LUT Warm 4"
        Case 16:table1.ColorGradeImage = "LUT Warm 5"
        Case 17:table1.ColorGradeImage = "LUT Warm 6"
        Case 18:table1.ColorGradeImage = "LUT Warm 7"
        Case 19:table1.ColorGradeImage = "LUT Warm 8"
        Case 20:table1.ColorGradeImage = "LUT Warm 9"
        Case 21:table1.ColorGradeImage = "LUT Warm 10"
    End Select
End Sub

' New LUT postit
Sub SetLUTLine(String)
    Dim Index
    Dim xFor
    Index = 1
    LUBack.imagea = "PostItNote"
    String = CL2(String)
    For xFor = 1 to 40
        Eval("LU" &xFor).imageA = GetHSChar(String, Index)
        Index = Index + 1
    Next
End Sub

Sub HideLUT
    SetLUTLine ""
    LUBack.imagea = "PostitBL"
End Sub

Function CL2(NumString) 'center line
    Dim Temp, TempStr
    If Len(NumString)> 40 Then NumString = Left(NumString, 40)
    Temp = (40 - Len(NumString) ) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL2 = TempStr
End Function

'**********************************************
'    Flipper adjustments - enable tricks
'             by JLouLouLou
'**********************************************

Dim FlipperPower
Dim FlipperElasticity
Dim SOSTorque, SOSAngle
Dim FullStrokeEOS_Torque, LiveStrokeEOS_Torque
Dim LeftFlipperOn
Dim RightFlipperOn

Dim LLiveCatchTimer
Dim RLiveCatchTimer
Dim LiveCatchSensivity

FlipperPower = 5000
FlipperElasticity = 0.85
FullStrokeEOS_Torque = 0.3 	' EOS Torque when flipper hold up ( EOS Coil is fully charged. Ampere increase due to flipper can't move or when it pushed back when "On". EOS Coil have more power )
LiveStrokeEOS_Torque = 0.2	' EOS Torque when flipper rotate to end ( When flipper move, EOS coil have less Ampere due to flipper can freely move. EOS Coil have less power )

LeftFlipper.EOSTorqueAngle = 10
RightFlipper.EOSTorqueAngle = 10

SOSTorque = 0.1
SOSAngle = 6

LiveCatchSensivity = 10

LLiveCatchTimer = 0
RLiveCatchTimer = 0

LeftFlipper.TimerInterval = 1
LeftFlipper.TimerEnabled = 1

Sub LeftFlipper_Timer 'flipper's tricks timer
'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
    If LeftFlipper.CurrentAngle >= LeftFlipper.StartAngle - SOSAngle Then LeftFlipper.Strength = FlipperPower * SOSTorque else LeftFlipper.Strength = FlipperPower : End If
 
'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
	If LeftFlipperOn = 1 Then
		If LeftFlipper.CurrentAngle = LeftFlipper.EndAngle then
			LeftFlipper.EOSTorque = FullStrokeEOS_Torque
			LLiveCatchTimer = LLiveCatchTimer + 1
			If LLiveCatchTimer < LiveCatchSensivity Then
				LeftFlipper.Elasticity = 0
			Else
				LeftFlipper.Elasticity = FlipperElasticity
				LLiveCatchTimer = LiveCatchSensivity
			End If
		End If
	Else
		LeftFlipper.Elasticity = FlipperElasticity
		LeftFlipper.EOSTorque = LiveStrokeEOS_Torque
		LLiveCatchTimer = 0
	End If
	

'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
    If RightFlipper.CurrentAngle <= RightFlipper.StartAngle + SOSAngle Then RightFlipper.Strength = FlipperPower * SOSTorque else RightFlipper.Strength = FlipperPower : End If
 
'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
 	If RightFlipperOn = 1 Then
		If RightFlipper.CurrentAngle = RightFlipper.EndAngle Then
			RightFlipper.EOSTorque = FullStrokeEOS_Torque
			RLiveCatchTimer = RLiveCatchTimer + 1
			If RLiveCatchTimer < LiveCatchSensivity Then
				RightFlipper.Elasticity = 0
			Else
				RightFlipper.Elasticity = FlipperElasticity
				RLiveCatchTimer = LiveCatchSensivity
			End If
		End If
	Else
		RightFlipper.Elasticity = FlipperElasticity
		RightFlipper.EOSTorque = LiveStrokeEOS_Torque
		RLiveCatchTimer = 0
	End If

End Sub


'*******************
'  Flipper Subs
'*******************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF ("LeftflipperupH-2dB", 101, DOFOn, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToEnd
		LeftFlipperOn = 1
    Else
        PlaySoundAt SoundFXDOF ("LeftflipperdownH", 101, DOFOff, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
		LeftFlipperOn = 0
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF ("RightflipperupH-2dB", 102, DOFOn, DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
        RightFlipperOn = 1
    Else
        PlaySoundAt SoundFXDOF ("RightflipperdownH", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
        RightFlipperOn = 0
    End If
End Sub

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub


'*******************
' GI lights
'*******************

Sub GiOn 'GI lights on
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff 'GI lights off
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
End Sub

'**************
'     TILT
'**************

Sub CheckTilt
    Tilt = Tilt + TiltSensitivity
    TiltDecreaseTimer.Enabled = True
    If Tilt> 15 Then
        Tilted = True
        TiltReel.SetValue 1
        If B2SOn then
            Controller.B2SSetTilt 1
        end if
        DisableTable True
        'BallsRemaining(CurrentPlayer) = 0 'player looses the game
        TiltRecoveryTimer.Enabled = True 'wait for all the balls to drain
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
        GiOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        Bumper001.Threshold = 100
        Bumper002.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
        DOF 101, DOFOff
        DOF 102, DOFOff
    Else
        GiOn
        Bumper001.Threshold = 1
        Bumper002.Threshold = 1
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' all the balls have drained ..
    If(BallsOnPlayfield = 0) Then
        EndOfBall
        TiltRecoveryTimer.Enabled = False
    End If
' otherwise repeat
End Sub

'***************************************************************
'             Supporting Ball & Sound Functions v4.0
'  includes random pitch in PlaySoundAt and PlaySoundAtBall
'***************************************************************

Dim TableWidth, TableHeight

TableWidth = Table1.width
TableHeight = Table1.height

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / TableWidth-1
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
    tmp = ball.y * 2 / TableHeight-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.2, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, Pitch(ActiveBall) * 10, 0, 0, AudioFade(ActiveBall)
End Sub

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function

'***********************************
'   JP's VP10 Rolling Sounds v4.0
'   JP's Ball Shadows
'   JP's Ball Speed Control
'   Rothbauer's dropping sounds
'***********************************

Const tnob = 19   'total number of balls
Const lob = 0     'number of locked balls
Const maxvel = 28 'max ball velocity
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate() 'call this routine from any realtime timer you may have, running at an interval of 10 is good.

    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls and hide the shadow
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
        aBallShadow(b).Y = 2100 'under the apron 'may differ from table to table
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' draw the ball shadow
    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y
        aBallShadow(b).Height = BOT(b).Z - Ballsize / 2 + 1

    'play the rolling sound for each ball
        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 50000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 5
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b)), 0, ballpitch, 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed control
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(maxvel / BOT(b).VelX)
            speedfactory = ABS(maxvel / BOT(b).VelY)
            If speedfactorx < 1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactorx
                BOT(b).VelY = BOT(b).VelY * speedfactorx
            End If
            If speedfactory < 1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactory
                BOT(b).VelY = BOT(b).VelY * speedfactory
            End If
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'***************************************
'     Collection collision sounds
'***************************************

Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
Sub aMetalWires_Hit(idx):PlaySoundAtBall "fx_MetalWire":End Sub
Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub   
Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

'***************************************************************************************************
' Only for VPX 10.2 and higher.
' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Return to previous State
'***************************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState)

    If TypeName(MyLight) = "Light" Then
        If FinalState = 2 Then
            FinalState = MyLight.State
        End If
        MyLight.BlinkInterval = BlinkPeriod
        MyLight.Duration 2, TotalPeriod, FinalState
    ElseIf TypeName(MyLight) = "Flasher" Then
        Dim steps
        ' Store all blink information
        steps = Int(TotalPeriod / BlinkPeriod + .5) 
        If FinalState = 2 Then                      
            FinalState = ABS(MyLight.Visible)
        End If
        MyLight.UserValue = steps * 10 + FinalState        
        MyLight.TimerInterval = BlinkPeriod
        MyLight.TimerEnabled = 0
        MyLight.TimerEnabled = 1
        ExecuteGlobal "Sub " & MyLight.Name & "_Timer:" & "Dim tmp, steps, fstate:tmp=me.UserValue:fstate = tmp MOD 10:steps= tmp\10 -1:Me.Visible = steps MOD 2:me.UserValue = steps *10 + fstate:If Steps = 0 then Me.Visible = fstate:Me.TimerEnabled=0:End if:End Sub"
    End If
End Sub

'****************************************
' Init table for a new game
'****************************************

Sub ResetForNewGame()
    'debug.print "ResetForNewGame"
    Dim i

    bGameInPLay = True
    bBallSaverActive = False

    StopAttractMode
    GiOn

    CurrentPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = True
    For i = 1 To MaxPlayers
        Score(i) = 0
        ExtraBallsAwards(i) = 0
        Special1Awarded(i) = False
        Special2Awarded(i) = False
        Special3Awarded(i) = False
        Special4Awarded(i) = False
        Special5Awarded(i) = False
        BallsRemaining(i) = BallsPerGame
    Next
    BonusMultiplier = 1
    UpdateBallInPlay
	UpdatePlayers
    Clear_Match

    ' init other variables
    Tilt = 0

    ' init game variables
    Game_Init()

    ' start a music?
    ' first ball
    vpmtimer.addtimer 2000, "FirstBall '"
End Sub

Sub FirstBall
    'debug.print "FirstBall"
    ' reset table for a new ball, rise droptargets, etc
    ResetForNewPlayerBall()
    CreateNewBall()
End Sub

' (Re-)init table for a new ball or player

Sub ResetForNewPlayerBall()
    'debug.print "ResetForNewPlayerBall"
    UpdatePlayers
    AddScore 0

    ' reset multiplier to 1x
    BonusMultiplier = 1

    ' turn on lights, and variables
    bExtraBallWonThisBall = False
    ResetNewBallVariables
    ResetNewBallLights

End Sub

' Create new ball

Sub CreateNewBall()
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass
    BallsOnPlayfield = BallsOnPlayfield + 1
    UpdateBallInPlay
	vpmtimer.addtimer 1000, "PlaySoundAt SoundFXDOF (""fx_Ballrel"", 104, DOFPulse, DOFContactors), Plunger : BallRelease.Kick 90, 4 '"
End Sub

' player lost the ball

Sub EndOfBall
    'debug.print "EndOfBall"

    ' Lost the first ball, now it cannot accept more players
    bOnTheFirstBall = False
	
	' Bonus Count
    If NOT Tilted Then
		'BonusMultiplier = 2 'for test x2
        Select Case BonusMultiplier
            Case 1:BonusCountTimer.Interval = 300
			Case 2:BonusCountTimer.Interval = 800
		End Select
		CBonus = 0
		KickerTBonus = 0
        BonusCountTimer.Enabled = 1
    Else 
    vpmtimer.addtimer 250, "EndOfBall2 '"
	Playsound"MotorLeer2"
	End If

End Sub

Sub BonusCountTimer_Timer 'The bonus are count when the ball is lost
    'debug.print "BonusCount_Timer"
    If Bonus> 0 Then
		If BonusMultiplier = 1 or KickerTBonus = 1 Then
			CBonus = CBonus + 1
			If CBonus = 5 Then BonusCountTimer.Interval = BonusCountTimer.Interval + 200
			If CBonus = 6 Then 
				BonusCountTimer.Interval = BonusCountTimer.Interval - 200
				CBonus = 0
			End If
		End If
        If KickerTBonus = 0 Then AddScore 2000 * BonusMultiplier Else AddScore 2000
        Bonus = Bonus - 1
        UpdateBonusLights
    Else 
        BonusCountTimer.Enabled = 0
        If KickerTBonus = 0 Then
			vpmtimer.addtimer 250, "EndOfBall2 '" 
			Playsound"MotorLeer2"
		Else 
			KickerTBonus = 0
			Addbonus				
			vpmtimer.addtimer 300, "PlaySoundAt SoundFXDOF (""fx_kicker"", 133, DOFPulse, DOFContactors), kickerT : kickerT.kick 135, 14 + RndNbr(3) '"
		End If
    End If
End Sub

Sub UpdateBonusLights
    Select Case Bonus
        Case 0:BL2K.State = 0:BL4K.State = 0:BL6K.State = 0:BL8K.State = 0:BL10K.State = 0:BL12K.State = 0:BL14K.State = 0:BL16K.State = 0:BL18K.State = 0:BL20K.State = 0:BL40K.State = 0
        Case 1:BL2K.State = 1:BL4K.State = 0:BL6K.State = 0:BL8K.State = 0:BL10K.State = 0:BL12K.State = 0:BL14K.State = 0:BL16K.State = 0:BL18K.State = 0:BL20K.State = 0:BL40K.State = 0
        Case 2:BL2K.State = 0:BL4K.State = 1:BL6K.State = 0:BL8K.State = 0:BL10K.State = 0:BL12K.State = 0:BL14K.State = 0:BL16K.State = 0:BL18K.State = 0:BL20K.State = 0:BL40K.State = 0
        Case 3:BL2K.State = 0:BL4K.State = 0:BL6K.State = 1:BL8K.State = 0:BL10K.State = 0:BL12K.State = 0:BL14K.State = 0:BL16K.State = 0:BL18K.State = 0:BL20K.State = 0:BL40K.State = 0
        Case 4:BL2K.State = 0:BL4K.State = 0:BL6K.State = 0:BL8K.State = 1:BL10K.State = 0:BL12K.State = 0:BL14K.State = 0:BL16K.State = 0:BL18K.State = 0:BL20K.State = 0:BL40K.State = 0
        Case 5:BL2K.State = 0:BL4K.State = 0:BL6K.State = 0:BL8K.State = 0:BL10K.State = 1:BL12K.State = 0:BL14K.State = 0:BL16K.State = 0:BL18K.State = 0:BL20K.State = 0:BL40K.State = 0
        Case 6:BL2K.State = 0:BL4K.State = 0:BL6K.State = 0:BL8K.State = 0:BL10K.State = 0:BL12K.State = 1:BL14K.State = 0:BL16K.State = 0:BL18K.State = 0:BL20K.State = 0:BL40K.State = 0
        Case 7:BL2K.State = 0:BL4K.State = 0:BL6K.State = 0:BL8K.State = 0:BL10K.State = 0:BL12K.State = 0:BL14K.State = 1:BL16K.State = 0:BL18K.State = 0:BL20K.State = 0:BL40K.State = 0
        Case 8:BL2K.State = 0:BL4K.State = 0:BL6K.State = 0:BL8K.State = 0:BL10K.State = 0:BL12K.State = 0:BL14K.State = 0:BL16K.State = 1:BL18K.State = 0:BL20K.State = 0:BL40K.State = 0
        Case 9:BL2K.State = 0:BL4K.State = 0:BL6K.State = 0:BL8K.State = 0:BL10K.State = 0:BL12K.State = 0:BL14K.State = 0:BL16K.State = 0:BL18K.State = 1:BL20K.State = 0:BL40K.State = 0
        Case 10:BL2K.State = 0:BL4K.State = 0:BL6K.State = 0:BL8K.State = 0:BL10K.State = 0:BL12K.State = 0:BL14K.State = 0:BL16K.State = 0:BL18K.State = 0:BL20K.State = 1:BL40K.State = 0
        Case 11:BL2K.State = 1:BL4K.State = 0:BL6K.State = 0:BL8K.State = 0:BL10K.State = 0:BL12K.State = 0:BL14K.State = 0:BL16K.State = 0:BL18K.State = 0:BL20K.State = 1:BL40K.State = 0
        Case 12:BL2K.State = 0:BL4K.State = 1:BL6K.State = 0:BL8K.State = 0:BL10K.State = 0:BL12K.State = 0:BL14K.State = 0:BL16K.State = 0:BL18K.State = 0:BL20K.State = 1:BL40K.State = 0
        Case 13:BL2K.State = 0:BL4K.State = 0:BL6K.State = 1:BL8K.State = 0:BL10K.State = 0:BL12K.State = 0:BL14K.State = 0:BL16K.State = 0:BL18K.State = 0:BL20K.State = 1:BL40K.State = 0
        Case 14:BL2K.State = 0:BL4K.State = 0:BL6K.State = 0:BL8K.State = 1:BL10K.State = 0:BL12K.State = 0:BL14K.State = 0:BL16K.State = 0:BL18K.State = 0:BL20K.State = 1:BL40K.State = 0
        Case 15:BL2K.State = 0:BL4K.State = 0:BL6K.State = 0:BL8K.State = 0:BL10K.State = 1:BL12K.State = 0:BL14K.State = 0:BL16K.State = 0:BL18K.State = 0:BL20K.State = 1:BL40K.State = 0
        Case 16:BL2K.State = 0:BL4K.State = 0:BL6K.State = 0:BL8K.State = 0:BL10K.State = 0:BL12K.State = 1:BL14K.State = 0:BL16K.State = 0:BL18K.State = 0:BL20K.State = 1:BL40K.State = 0
        Case 17:BL2K.State = 0:BL4K.State = 0:BL6K.State = 0:BL8K.State = 0:BL10K.State = 0:BL12K.State = 0:BL14K.State = 1:BL16K.State = 0:BL18K.State = 0:BL20K.State = 1:BL40K.State = 0
        Case 18:BL2K.State = 0:BL4K.State = 0:BL6K.State = 0:BL8K.State = 0:BL10K.State = 0:BL12K.State = 0:BL14K.State = 0:BL16K.State = 1:BL18K.State = 0:BL20K.State = 1:BL40K.State = 0
        Case 19:BL2K.State = 0:BL4K.State = 0:BL6K.State = 0:BL8K.State = 0:BL10K.State = 0:BL12K.State = 0:BL14K.State = 0:BL16K.State = 0:BL18K.State = 1:BL20K.State = 1:BL40K.State = 0
        Case 20:BL2K.State = 0:BL4K.State = 0:BL6K.State = 0:BL8K.State = 0:BL10K.State = 0:BL12K.State = 0:BL14K.State = 0:BL16K.State = 0:BL18K.State = 0:BL20K.State = 0:BL40K.State = 1
		Case 21:BL2K.State = 1:BL4K.State = 0:BL6K.State = 0:BL8K.State = 0:BL10K.State = 0:BL12K.State = 0:BL14K.State = 0:BL16K.State = 0:BL18K.State = 0:BL20K.State = 0:BL40K.State = 1
        Case 22:BL2K.State = 0:BL4K.State = 1:BL6K.State = 0:BL8K.State = 0:BL10K.State = 0:BL12K.State = 0:BL14K.State = 0:BL16K.State = 0:BL18K.State = 0:BL20K.State = 0:BL40K.State = 1
        Case 23:BL2K.State = 0:BL4K.State = 0:BL6K.State = 1:BL8K.State = 0:BL10K.State = 0:BL12K.State = 0:BL14K.State = 0:BL16K.State = 0:BL18K.State = 0:BL20K.State = 0:BL40K.State = 1
        Case 24:BL2K.State = 0:BL4K.State = 0:BL6K.State = 0:BL8K.State = 1:BL10K.State = 0:BL12K.State = 0:BL14K.State = 0:BL16K.State = 0:BL18K.State = 0:BL20K.State = 0:BL40K.State = 1
        Case 25:BL2K.State = 0:BL4K.State = 0:BL6K.State = 0:BL8K.State = 0:BL10K.State = 1:BL12K.State = 0:BL14K.State = 0:BL16K.State = 0:BL18K.State = 0:BL20K.State = 0:BL40K.State = 1
        Case 26:BL2K.State = 0:BL4K.State = 0:BL6K.State = 0:BL8K.State = 0:BL10K.State = 0:BL12K.State = 1:BL14K.State = 0:BL16K.State = 0:BL18K.State = 0:BL20K.State = 0:BL40K.State = 1
        Case 27:BL2K.State = 0:BL4K.State = 0:BL6K.State = 0:BL8K.State = 0:BL10K.State = 0:BL12K.State = 0:BL14K.State = 1:BL16K.State = 0:BL18K.State = 0:BL20K.State = 0:BL40K.State = 1
        Case 28:BL2K.State = 0:BL4K.State = 0:BL6K.State = 0:BL8K.State = 0:BL10K.State = 0:BL12K.State = 0:BL14K.State = 0:BL16K.State = 1:BL18K.State = 0:BL20K.State = 0:BL40K.State = 1
        Case 29:BL2K.State = 0:BL4K.State = 0:BL6K.State = 0:BL8K.State = 0:BL10K.State = 0:BL12K.State = 0:BL14K.State = 0:BL16K.State = 0:BL18K.State = 1:BL20K.State = 0:BL40K.State = 1
    End Select
End Sub

Sub EndOfBall2
    'debug.print "EndOfBall2"

    Tilted = False
    Tilt = 0
    TiltReel.SetValue 0
    If B2SOn then
        Controller.B2SSetTilt 0
    end if
    DisableTable False

   ' win extra ball?
    If(ExtraBallsAwards(CurrentPlayer)> 0) Then
        'debug.print "Extra Ball"

        ' if so then give it
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1

        ' turn off light if no more extra balls
        If(ExtraBallsAwards(CurrentPlayer) = 0) Then
			LightShootAgain.State = 0
			ShootAgainR.SetValue 0
			If B2SOn then
				Controller.B2SSetShootAgain 0
			End If
        End If

        ' extra ball sound or light?

        ' reset as in a new ball
        ResetForNewPlayerBall()
        CreateNewBall()

    Else ' no extra ball

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1

        ' last ball?
        If(BallsRemaining(CurrentPlayer) <= 0) Then
            CheckHighScore()
        End If

        ' this is not the last ball, check for new player
        EndOfBallComplete()
    End If
End Sub

Sub EndOfBallComplete()
    'debug.print "EndOfBallComplete"
    Dim NextPlayer

    ' other players?
    If(PlayersPlayingGame> 1) Then
        NextPlayer = CurrentPlayer + 1
        ' if it is the last player then go to the first one
        If(NextPlayer> PlayersPlayingGame) Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' end of game?
    If((BallsRemaining(CurrentPlayer) <= 0) AND(BallsRemaining(NextPlayer) <= 0) ) Then

      ' match if playing with coins
        If bFreePlay = False Then
            Verification_Match
        End If

        ' end of game
        EndOfGame()
    Else
        ' next player
        CurrentPlayer = NextPlayer

        ' update score
        AddScore 0

        ' reset table for new player
        ResetForNewPlayerBall()
        CreateNewBall()
    End If
End Sub

' Called at the end of the game

Sub EndOfGame()
    'debug.print "EndOfGame"

    bGameInPLay = False
    bJustStarted = False

    ' turn off flippers
    SolLFlipper 0
    SolRFlipper 0

    ' start the attract mode
    StartAttractMode
End Sub

' Function to calculate the balls left
Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp> BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function

' check the highscore
Sub CheckHighscore
    Dim playertops, si, sj, i, stemp, stempplayers
    For i = 1 to 4
        sortscores(i) = 0
        sortplayers(i) = 0
    Next
    playertops = 0
    For i = 1 to PlayersPlayingGame
        sortscores(i) = Score(i)
        sortplayers(i) = i
    Next
    For si = 1 to PlayersPlayingGame
        For sj = 1 to PlayersPlayingGame-1
            If sortscores(sj)> sortscores(sj + 1) then
                stemp = sortscores(sj + 1)
                stempplayers = sortplayers(sj + 1)
                sortscores(sj + 1) = sortscores(sj)
                sortplayers(sj + 1) = sortplayers(sj)
                sortscores(sj) = stemp
                sortplayers(sj) = stempplayers
            End If
        Next
    Next
    HighScoreTimer.interval = 100
    HighScoreTimer.enabled = True
    ScoreChecker = 4
    CheckAllScores = 1
    NewHighScore sortscores(ScoreChecker), sortplayers(ScoreChecker)
End Sub

'******************
'      Match
'******************

Sub Verification_Match()
	Playsound"MotorLeer2"
    Match = INT(RND(1) * 10) * 10 ' random between 00 and 90
    Display_Match
    If(Score(CurrentPlayer) MOD 100) = Match Then
        AwardSpecial      
    End If
End Sub

Sub Clear_Match()
    MatchReel1.SetValue 0
    MatchReel2.SetValue 0
    If B2SOn then
        Controller.B2SSetMatch 0
    end if
End Sub

Sub Display_Match()
	If (Match \ 10) < 5 Then  MatchReel1.SetValue 1 + (Match \ 10)  Else  MatchReel2.SetValue 1 + (Match \ 10)
    If B2SOn then
        If Match = 0 then
            Controller.B2SSetMatch 100
        else
            Controller.B2SSetMatch Match
        end if
    end if
End Sub

' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

Sub Drain_Hit()
    Drain.DestroyBall
    BallsOnPlayfield = BallsOnPlayfield - 1
    PlaySoundAt "fx_drain", Drain
	DOF 250, DOFPulse

    'tilted?
    If Tilted Then
        StopEndOfBallMode
    End If

    ' if still playing and not tilted
    If(bGameInPLay = True) AND (Tilted = False) Then

        ' ballsaver?
        If(bBallSaverActive = True) Then
            CreateNewBall()
        Else
            ' last ball?
            If(BallsOnPlayfield = 0) Then
                StopEndOfBallMode
                vpmtimer.addtimer 500, "EndOfBall '" 
                Exit Sub
            End If
        End If
    End If
End Sub

Sub swPlungerRest_Hit()
    bBallInPlungerLane = True
End Sub

Sub swPlungerRest_UnHit()
    bBallInPlungerLane = False
End Sub

' ***************************************
'               Score functions
' ***************************************

Sub AddScore(Points)
    If Tilted Then Exit Sub
    Select Case Points
        Case 10, 100, 1000, 10000
            Score(CurrentPlayer) = Score(CurrentPlayer) + points
            UpdateScore points
        ' sounds
			If Points = 10000 Then
                PlaySound SoundFXDOF ("fx_segasachimelow", 303, DOFPulse, DOFChimes)  
			ElseIf Points = 1000 Then
                PlaySound SoundFXDOF ("fx_segasachimehigh", 301, DOFPulse, DOFChimes)
			ElseIf Points = 100 Then
                PlaySound SoundFXDOF ("fx_segasachimehigh", 301, DOFPulse, DOFChimes)          
			Else '10
                PlaySound SoundFXDOF ("fx_segasachimemid", 302, DOFPulse, DOFChimes)
            End If 
		Case 20, 30, 40, 50
            Add10 = Add10 + Points \ 10
            AddScore10Timer.Enabled = TRUE         
        Case 200, 300, 400, 500
            Add100 = Add100 + Points \ 100
            AddScore100Timer.Enabled = TRUE   
        Case 2000, 3000, 4000, 5000
            Add1000 = Add1000 + Points \ 1000
            AddScore1000Timer.Enabled = TRUE
        Case 20000, 30000, 40000, 50000
            Add10000 = Add10000 + Points \ 10000
            AddScore10000Timer.Enabled = TRUE
    End Select

    ' check for higher score and specials for 3 Balls
	If BallsPerGame = 3 Then
		If Score(CurrentPlayer) >= Special1 AND Special1Awarded(CurrentPlayer) = False Then
			AwardSpecial
			Special1Awarded(CurrentPlayer) = True
		End If
		If Score(CurrentPlayer) >= Special2 AND Special2Awarded(CurrentPlayer) = False Then
			AwardSpecial
			Special2Awarded(CurrentPlayer) = True
		End If
		If Score(CurrentPlayer) >= Special3 AND Special3Awarded(CurrentPlayer) = False Then
			AwardSpecial
			Special3Awarded(CurrentPlayer) = True
		End If
	End If
   ' check for higher score and specials for 5 Balls
	If BallsPerGame = 5 Then
		If Score(CurrentPlayer) >= Special4 AND Special4Awarded(CurrentPlayer) = False Then
			AwardSpecial
			Special4Awarded(CurrentPlayer) = True
		End If
		If Score(CurrentPlayer) >= Special5 AND Special5Awarded(CurrentPlayer) = False Then
			AwardSpecial
			Special5Awarded(CurrentPlayer) = True
		End If
	End If
End Sub

'************************************
'       Score sound Timers
'************************************

Sub AddScore10Timer_Timer()
    if Add10> 0 then
        AddScore 10
        Add10 = Add10 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore100Timer_Timer()
    if Add100> 0 then
        AddScore 100
        Add100 = Add100 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore1000Timer_Timer()
    if Add1000> 0 then
        AddScore 1000
        Add1000 = Add1000 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore10000Timer_Timer()
    if Add10000> 0 then
        AddScore 10000
        Add10000 = Add10000 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub


'*******************
'     Bonus
'*******************

Sub AddBonus
    If Tilted Then Exit Sub
	If Bonus < 29 Then
		Bonus = Bonus + 1
		If Bonus = 20 Then LightSpinner.State = 1 Else LightSpinner.State = 0
		vpmtimer.addtimer 200, "UpdateBonusLights '"
    End If
End Sub

'***********************************************************************************
'        Score EM reels - puntuaciones - y actualiza otras luces del backdrop
'***********************************************************************************
'esta es al rutina que actualiza la puntuación del jugador

Sub UpdateScore(playerpoints)
    Select Case CurrentPlayer
        Case 1:ScoreReel1.Addvalue playerpoints
		Case 2:ScoreReel2.Addvalue playerpoints
		Case 3:ScoreReel3.Addvalue playerpoints
		Case 4:ScoreReel4.Addvalue playerpoints
    End Select
    If B2SOn then
        Controller.B2SSetScorePlayer CurrentPlayer, Score(CurrentPlayer)
    end if
End Sub

Sub ResetScores
    ScoreReel1.ResetToZero
    ScoreReel2.ResetToZero
    ScoreReel3.ResetToZero
    ScoreReel4.ResetToZero
    If B2SOn then
        Controller.B2SSetScorePlayer1 0
        Controller.B2SSetScorePlayer2 0
        Controller.B2SSetScorePlayer3 0
        Controller.B2SSetScorePlayer4 0
		Controller.B2SSetData 81,0
		Controller.B2SSetData 82,0
		Controller.B2SSetData 83,0
		Controller.B2SSetData 84,0
    end if
End Sub

Sub AddCredits(value) 'limit to 15 credits
    If Credits <15 Then
        Credits = Credits + value
        UpdateCredits
    end if
End Sub

Sub UpdateCredits
    If Credits> 0 Then 'in Bally tables
        CreditLight.State = 1
    Else
        CreditLight.State = 0
    End If
    PlaySound "fx_relay"
    CreditsReel.SetValue credits
   If B2SOn Then
		If Credits < 10 Then Controller.B2SSetCredits Credits 'limited to 9 for backglass
		If Credits = 0 then
            Controller.B2SSetdata 20,0
        else
            Controller.B2SSetdata 20,1
        end if
    end if
End Sub

Sub UpdateBallInPlay
    Select Case(Balls)
        Case 1:BallInPlayR.SetValue 1
        Case 2:BallInPlayR.SetValue 2
        Case 3:BallInPlayR.SetValue 3
        Case 4:BallInPlayR.SetValue 4
        Case 5:BallInPlayR.SetValue 5
    End Select
    If B2SOn Then
        Controller.B2SSetBallInPlay Balls
    End If
End Sub

Sub UpdatePlayers
    Select case CurrentPlayer
        Case 0:pl1.State = 0:pl2.State = 0:pl3.State = 0:pl4.State = 0
        Case 1:pl1.State = 1:pl2.State = 0:pl3.State = 0:pl4.State = 0
        Case 2:pl1.State = 0:pl2.State = 1:pl3.State = 0:pl4.State = 0
        Case 3:pl1.State = 0:pl2.State = 0:pl3.State = 1:pl4.State = 0
        Case 4:pl1.State = 0:pl2.State = 0:pl3.State = 0:pl4.State = 1
    End Select
    If B2SOn Then	
		Controller.B2SSetPlayerUp CurrentPlayer
		Controller.B2SSetData 81,0
		Controller.B2SSetData 82,0
		Controller.B2SSetData 83,0
		Controller.B2SSetData 84,0
		Controller.B2SSetData 80 + CurrentPlayer,1
    End If
    Select Case PlayersPlayingGame
        Case 0:cp1.State = 0:cp2.State = 0:cp3.State = 0:cp4.State = 0
        Case 1:cp1.State = 1:cp2.State = 0:cp3.State = 0:cp4.State = 0
        Case 2:cp1.State = 0:cp2.State = 1:cp3.State = 0:cp4.State = 0
        Case 3:cp1.State = 0:cp2.State = 0:cp3.State = 1:cp4.State = 0
        Case 4:cp1.State = 0:cp2.State = 0:cp3.State = 0:cp4.State = 1
    End Select
    If B2SOn Then
        Controller.B2SSetCanPlay PlayersPlayingGame
    End If
End Sub


'*************************
'        Specials
'*************************

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
		LightShootAgain.State = 1
		ShootAgainR.SetValue 1
        If B2SOn Then
            Controller.B2SSetShootAgain 1
        End If		
    End If
End Sub

Sub AwardSpecial()
    PlaySound SoundfXDOF ("fx_knocker", 110, DOFPulse, DOFKnocker)
    AddCredits 1
End Sub

' ********************************
'        Attract Mode
' ********************************
' use the"Blink Pattern" of each light

Sub StartAttractMode()
	Dim x
	If AttractMode = 1 Then
		bAttractMode = True
		For each x in aLights
			x.State = 2
		Next
	End If
    If B2SOn then
        Controller.B2SSetGameOver 1
        Controller.B2SSetBallInPlay 0
		Controller.B2SSetPlayerUp 0
        Controller.B2SSetCanPlay 1
		Controller.B2SSetShootAgain 0
        Controller.B2SSetTilt 0
		Controller.B2SSetData 81,1
		Controller.B2SSetData 82,1
		Controller.B2SSetData 83,1
		Controller.B2SSetData 84,1
    end if
    GameOverR.SetValue 1
    BallInPlayR.SetValue 0
	ShootAgainR.SetValue 0
	pl1.State = 0:pl2.State = 0:pl3.State = 0:pl4.State = 0
    cp1.State = 1 : cp2.State = 0 : cp3.State = 0 : cp4.State = 0
	TiltReel.SetValue 0	
End Sub

Sub StopAttractMode()
    Dim x
    bAttractMode = False
    TurnOffPlayfieldLights
    ResetScores
    GameOverR.SetValue 0
    If B2SOn then
        Controller.B2SSetGameOver 0
    end if
End Sub

'************************************************
'    Load / Save / Highscore
'************************************************

Sub Loadhs
    ' Based on Black's Highscore routines
    Dim FileObj
    Dim ScoreFile, TextStr
    Dim temp1
    Dim temp2
    Dim temp3
    Dim temp4
    Dim temp5
    Dim temp6
    Dim temp8
    Dim temp9
    Dim temp10
    Dim temp11
    Dim temp12
    Dim temp13
    Dim temp14
    Dim temp15
    Dim temp16
    Dim temp17

    Set FileObj = CreateObject("Scripting.FileSystemObject")
    If Not FileObj.FolderExists(UserDirectory) then
        Credits = 0
        Exit Sub
    End If
    If Not FileObj.FileExists(UserDirectory & TableName& ".txt") then
        Credits = 0
        Exit Sub
    End If
    Set ScoreFile = FileObj.GetFile(UserDirectory & TableName& ".txt")
    Set TextStr = ScoreFile.OpenAsTextStream(1, 0)
    If(TextStr.AtEndOfStream = True) then
        Exit Sub
    End If
    temp1 = TextStr.ReadLine
    temp2 = textstr.readline

    HighScore = cdbl(temp1)
    If HighScore <1 then
        temp8 = textstr.readline
        temp9 = textstr.readline
        temp10 = textstr.readline
        temp11 = textstr.readline
        temp12 = textstr.readline
        temp13 = textstr.readline
        temp14 = textstr.readline
        temp15 = textstr.readline
        temp16 = textstr.readline
        temp17 = textstr.readline
    End If
    TextStr.Close
    Credits = cdbl(temp2)

    If HighScore <1 then
        HSScore(1) = int(temp8)
        HSScore(2) = int(temp9)
        HSScore(3) = int(temp10)
        HSScore(4) = int(temp11)
        HSScore(5) = int(temp12)

        HSName(1) = temp13
        HSName(2) = temp14
        HSName(3) = temp15
        HSName(4) = temp16
        HSName(5) = temp17
    End If
    Set ScoreFile = Nothing
    Set FileObj = Nothing
    SortHighscore 'added to fix a previous error
End Sub

Sub Savehs
    ' Based on Black's Highscore routines
    Dim FileObj
    Dim ScoreFile
    Dim xx
    Set FileObj = CreateObject("Scripting.FileSystemObject")
    If Not FileObj.FolderExists(UserDirectory) then
        Exit Sub
    End If
    Set ScoreFile = FileObj.CreateTextFile(UserDirectory & TableName& ".txt", True)
    ScoreFile.WriteLine 0
    ScoreFile.WriteLine Credits
    For xx = 1 to 5
        scorefile.writeline HSScore(xx)
    Next
    For xx = 1 to 5
        scorefile.writeline HSName(xx)
    Next
    ScoreFile.Close
    Set ScoreFile = Nothing
    Set FileObj = Nothing
End Sub

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 1 to 5
        For j = 1 to 4
            If HSScore(j) <HSScore(j + 1) Then
                tmp = HSScore(j + 1)
                tmp2 = HSName(j + 1)
                HSScore(j + 1) = HSScore(j)
                HSName(j + 1) = HSName(j)
                HSScore(j) = tmp
                HSName(j) = tmp2
            End If
        Next
    Next
End Sub

'****************************************
' Realtime updates
'****************************************

Function PI()
	PI = 4*Atn(1)
End Function

Sub GameTimer_Timer
    RollingUpdate
    LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle
    RightFlipperTop.RotZ = RightFlipper.CurrentAngle
    Pgate1.rotx = Gate1.currentangle*.6
    Pgate2.rotx = Gate2.currentangle*.6

	Dim SpinnerRadius: SpinnerRadius=7
	SpinnerRod.TransZ = (cos((leftspinner.CurrentAngle + 180) * (PI/180))+1) * SpinnerRadius
	SpinnerRod.TransY = sin((leftspinner.CurrentAngle) * (PI/180)) * -SpinnerRadius
End Sub

'***********************************************************************
' *********************************************************************
'  *********     G A M E  C O D E  S T A R T S  H E R E      *********
' *********************************************************************
'***********************************************************************

Sub VPObjects_Init 'init objects
    TurnOffPlayfieldLights()
End Sub

' Dim all the variables

Sub Game_Init() 'called at the start of a new game
    'Start music?
    'Init variables?
	KickerTBonus = 0
	SpecialCenter = 0
	SpinCounter = 1
	CenterBonusCounter = 1
	Bonus = 1
    'Start or init timers
    'Init lights?
    TurnOffPlayfieldLights()
End Sub

Sub StopEndOfBallMode() 'called when the last ball is drained

End Sub

Sub ResetNewBallVariables() 'init variables new ball/player	
	'SpinCounter = 1	
	'CenterBonusCounter = 1
	KickerTBonus = 0
	SpecialCenter = 0
	Bonus = 1
End Sub

Sub ResetNewBallLights()    'init lights for new ball/player
	TurnOffPlayfieldLights()
	TurnOnNewBallLights()
	UpdateBonusLights()
	UpdateCenterBonusLights
	UpdateSpinnerLight
End Sub

Sub TurnOnNewBallLights()
    Dim a
    For each a in aNewBallLights
        a.State = 1
    Next
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

' *********************************************************************
'                        Table Object Hit Events
' *********************************************************************
' Any target hit Sub will do this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/modes this trigger is a member of
' - set the "LastSwicthHit" variable in case it is needed later
' *********************************************************************

'**********************
'       Slingshots
'**********************

Dim LStep, RStep

Sub LeftSlingShot_Slingshot   'left slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_slingshot",103,DOFPulse,DOFcontactors), Lemk
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add points
    AddScore 10
    ' some effect
End Sub

Sub LeftSlingShot_Timer    
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing2.Visible = 0:Lemk.RotX = -20:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot    'right slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_slingshot",104,DOFPulse,DOFcontactors), Remk
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add points
    AddScore 10
    ' some effect
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -20:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'***********************
'       Rubbers
'***********************

Dim Rub1, Rub2, Rub3, Rub4, Rub5, Rub6, Rub7

Sub RubberBand001_Hit   'center / Counter
    If Tilted then Exit Sub
	AddCenterBonus
	Select case CenterBonusCounter
		case 3:
			LightKickerLeft.State = 0
			LightKickerRight.State = 0
		case 6:
			SpecialCenter = 0
			LightSpecialLeft.State = 0
			LightSpecialRight. State = 0
		case 9 :
			LightExtraBallFeature.State = 0
	end select
    Rub7 = 1:RubberBand001_Timer
End Sub

Sub RubberBand001_Timer
    Select Case Rub7
        Case 1:r013.Visible = 0:r035.Visible = 1:RubberBand001.TimerEnabled = 1
        Case 2:r035.Visible = 0:r036.Visible = 1
        Case 3:r036.Visible = 0:r013.Visible = 1:RubberBand001.TimerEnabled = 0
    End Select
    Rub7 = Rub7 + 1
End Sub

Sub RubberBand005_Hit   'left
    If Tilted then Exit Sub
    AddScore 10
    Rub4 = 1:RubberBand005_Timer
End Sub

Sub RubberBand005_Timer
    Select Case Rub4
        Case 1:r011.Visible = 0:r029.Visible = 1:RubberBand005.TimerEnabled = 1
        Case 2:r029.Visible = 0:r030.Visible = 1
        Case 3:r030.Visible = 0:r011.Visible = 1:RubberBand005.TimerEnabled = 0
    End Select
    Rub4 = Rub4 + 1
End Sub

Sub RubberBand006_Hit  '
    If Tilted then Exit Sub
    AddScore 10
    Rub1 = 1:RubberBand006_Timer
End Sub

Sub RubberBand006_Timer
    Select Case Rub1
        Case 1:r009.Visible = 0:r020.Visible = 1:RubberBand006.TimerEnabled = 1
        Case 2:r020.Visible = 0:r019.Visible = 1
        Case 3:r019.Visible = 0:r009.Visible = 1:RubberBand006.TimerEnabled = 0
    End Select
    Rub1 = Rub1 + 1
End Sub

Sub RubberBand007_Hit  '
    If Tilted then Exit Sub
    AddScore 10
    Rub6 = 1:RubberBand007_Timer
End Sub

Sub RubberBand007_Timer
    Select Case Rub6
        Case 1:r016.Visible = 0:r031.Visible = 1:RubberBand007.TimerEnabled = 1
        Case 2:r020.Visible = 0:r028.Visible = 1
        Case 3:r031.Visible = 0:r016.Visible = 1:RubberBand007.TimerEnabled = 0
    End Select
    Rub6 = Rub6 + 1
End Sub

Sub RubberBand008_Hit   '
    If Tilted then Exit Sub
    AddScore 10
    Rub2 = 1:RubberBand008_Timer
End Sub

Sub RubberBand008_Timer
    Select Case Rub2
        Case 1:r010.Visible = 0:r022.Visible = 1:RubberBand008.TimerEnabled = 1
        Case 2:r022.Visible = 0:r021.Visible = 1
        Case 3:r021.Visible = 0:r010.Visible = 1:RubberBand008.TimerEnabled = 0
    End Select
    Rub2 = Rub2 + 1
End Sub

Sub RubberBand009_Hit   'right
    If Tilted then Exit Sub
    AddScore 10
    Rub5 = 1:RubberBand009_Timer
End Sub

Sub RubberBand009_Timer
    Select Case Rub5
        Case 1:r025.Visible = 0:r027.Visible = 1:RubberBand009.TimerEnabled = 1
        Case 2:r027.Visible = 0:r026.Visible = 1
        Case 3:r026.Visible = 0:r025.Visible = 1:RubberBand009.TimerEnabled = 0
    End Select
    Rub5 = Rub5 + 1
End Sub

Sub RubberBand010_Hit   '
    If Tilted then Exit Sub
    AddScore 10
    Rub3 = 1:RubberBand010_Timer
End Sub

Sub RubberBand010_Timer
    Select Case Rub3
        Case 1:r014.Visible = 0:r024.Visible = 1:RubberBand010.TimerEnabled = 1
        Case 2:r024.Visible = 0:r023.Visible = 1
        Case 3:r023.Visible = 0:r014.Visible = 1:RubberBand010.TimerEnabled = 0
    End Select
    Rub3 = Rub3 + 1
End Sub


'*********
' Bumpers
'*********

Sub Bumper001_Hit 'left
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",105,DOFPulse,DOFContactors), bumper001
    AddScore 100
End Sub

Sub Bumper002_Hit ' right
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",106,DOFPulse,DOFContactors), bumper002
    AddScore 100
End Sub

'*****************
'     Triggers
'*****************

' out lanes

Sub Trigger001_Hit  'Left
    PlaySoundAt "fx_sensor", Trigger001
	DOF 204, DOFPulse
    If Tilted Then Exit Sub
	Addscore 10000
	AddBonus
	If LightSpecialLeft.State = 1 Then AwardSpecial
End Sub

Sub Trigger002_Hit 'Left/Right
    PlaySoundAt "fx_sensor", Trigger002
	DOF 205, DOFPulse
    If Tilted Then Exit Sub	
	Addscore 5000
	Alternate
End Sub

Sub Trigger003_Hit 'Right/Left
    PlaySoundAt "fx_sensor", Trigger003
	DOF 206, DOFPulse
    If Tilted Then Exit Sub
	Addscore 5000
	Alternate
End Sub

Sub Trigger004_Hit  'Right
    PlaySoundAt "fx_sensor", Trigger004
	DOF 207, DOFPulse
    If Tilted Then Exit Sub
	Addscore 10000
	AddBonus
	If LightSpecialRight.State =1 Then AwardSpecial
End Sub

' top lanes

Sub Trigger006_Hit 'Left
    PlaySoundAt "fx_sensor", Trigger006
	DOF 208, DOFPulse
    If Tilted Then Exit Sub	
	Addscore 5000
	If LightTopLeft.State = 1  Then
		AddBonus
		vpmtimer.addtimer 250, "AddBonus '"
		vpmtimer.addtimer 250, "AddBonus '"
	End If
	Alternate
End Sub

Sub Trigger007_Hit  'Center
    PlaySoundAt "fx_sensor", Trigger007
	DOF 209, DOFPulse
    If Tilted Then Exit Sub
	Addscore 10000	
	If LightExtraBallFeature.State = 1 Then
		AwardExtraBall
		LightExtraBallFeature.State =0
	End If
	AddBonus
	Alternate
End Sub

Sub Trigger008_Hit  'Right
    PlaySoundAt "fx_sensor", Trigger008
	DOF 210, DOFPulse
    If Tilted Then Exit Sub
	Addscore 5000
	If LightTopRight.State = 1 Then
		AddBonus
		vpmtimer.addtimer 250, "AddBonus '"
		vpmtimer.addtimer 250, "AddBonus '"
	End If
	Alternate
End Sub

'************************
'       Targets
'************************

' Target Rectangle

Sub Target1_hit 'left
    PlaySoundAtBall "fx_target"
	DOF 202, DOFPulse
    If Tilted Then Exit Sub
	Addscore 1000
	If LightTargetLeft.State = 1 Then AddBonus
	Alternate
End Sub

Sub Target2_hit 'right
    PlaySoundAtBall "fx_target"
	DOF 203, DOFPulse
    If Tilted Then Exit Sub
	Addscore 1000
	If LightTargetRight.State = 1 Then AddBonus
	Alternate
End Sub

'***********************
'     Moving Target
'***********************

sub MovingTargetTimer_timer
	if MoveDirection=1 Then	
		Collection2(MoveTarget).visible=0
		Collection2(MoveTarget).collidable=False
		Collection2(MoveTarget).hashitevent=False
		MoveTarget=MoveTarget+1
'		Collection2(movetarget).visible=1 '(Set to Debug)
		Collection2(MoveTarget).collidable=True
		Collection2(MoveTarget).hashitevent=True

		if MoveTarget>48 then 
			MoveDirection=0
		end If
		PtargetC.objroty=((MoveTarget*.8)-20.5)
		exit sub
	Else
		Collection2(MoveTarget).Visible=0
		Collection2(MoveTarget).collidable=False
		Collection2(MoveTarget).hashitevent=False		
		MoveTarget=MoveTarget-1
'		Collection2(movetarget).visible=1 '(Set to Debug)
		Collection2(MoveTarget).collidable=True
		Collection2(MoveTarget).hashitevent=True

		if MoveTarget<1 then 
			MoveDirection=1
		end If
	end If
	PtargetC.objroty=((MoveTarget*.8)-20.5)
end sub 

Sub Collection2_Hit(idx)
    PlaySoundAt "fx_target",PtargetC
	DOF 201, DOFPulse
    If Tilted Then Exit Sub
	Select case CenterBonusCounter
		case 1,3,4,6,7,9:
			AddScore 5000
		case 2:
			AddScore 1000
			If LightDoubleBonus.State = 0 Then
				LightKickerLeft.State = 1
				LightKickerRight.State = 1
			End If		
		case 5:
			AddScore 1000
			SpecialCenter = 1
			LightSpecialLeft.State = 1
			LightSpecialRight.State = 0
		case 8:
			AddScore 1000
			If LightShootAgain.State = 0 Then LightExtraBallFeature.State = 1
	end select
	Alternate
end sub

'************************
'       Kickers
'************************

'Left

Sub KickerL_Hit
    PlaySoundAt "fx_kicker_enter", kickerL
	Playsound"MotorLeer2"
	If Tilted Then
		vpmtimer.addtimer 250, "KickBall '"
        Exit Sub
    End If
	AddScore 5000
	If LightKickerLeft.State = 1 Then
		LightDoubleBonus.State = 1
		BonusMultiplier = 2
		LightKickerLeft.State = 0
		LightKickerRight.State = 0		
	End If
	Alternate
	vpmtimer.addtimer 500, "PlaySoundAt SoundFXDOF (""fx_kicker"", 131, DOFPulse, DOFContactors), kickerL : kickerL.kick 125, 14 + RndNbr(3) '"
End Sub

'Right

Sub KickerR_Hit
    PlaySoundAt "fx_kicker_enter", kickerR
	Playsound"MotorLeer2"
	If Tilted Then
		vpmtimer.addtimer 250, "KickBall '"
        Exit Sub
    End If
	AddScore 5000
	If LightKickerRight.State = 1 Then
		LightDoubleBonus.State = 1
		BonusMultiplier = 2
		LightKickerLeft.State = 0
		LightKickerRight.State = 0		
	End If
	Alternate
	vpmtimer.addtimer 500, "PlaySoundAt SoundFXDOF (""fx_kicker"", 132, DOFPulse, DOFContactors), kickerR : kickerR.kick 235, 14 + RndNbr(3)'"
End Sub

'Top

Sub KickerT_Hit
    PlaySoundAt "fx_kicker_enter", kickerT
	Playsound"MotorLeer2"
	If Tilted Then
		vpmtimer.addtimer 250, "KickBall '"
        Exit Sub
    End If
	KickerTBonus = 1
	BonusCountTimer.Interval = 300
	CBonus = 0
    BonusCountTimer.Enabled = 1    
	Alternate
End Sub

'Kick all ball

Sub KickBall
	vpmtimer.addtimer 500, "PlaySoundAt SoundFXDOF (""fx_kicker"", 132, DOFPulse, DOFContactors), kickerT :  kickerL.kick 125, 10 : kickerR.kick 235, 10 : kickerT.kick 135, 10 '"
End Sub

'***********************************
'            Spinners
'***********************************

Sub LeftSpinner_Spin
    If Tilted Then Exit Sub
	If LightSpinner.state = 1 then 	AddScore 1000 Else 	AddScore 100
	If SpinCounter < 10 Then SpinCounter = SpinCounter + 1 Else SpinCounter = 1
	UpdateSpinnerLight
	Select Case SpinCounter 
		Case 2 : AddBonus
		Case 4 : AddBonus
		Case 6 : AddBonus
		Case 8 : AddBonus
		Case 10 : AddBonus
	End Select
End Sub

'******************************
'      Extra routines
'******************************

Sub AddCenterBonus()
	If CenterBonusCounter < 9 Then CenterBonusCounter = CenterBonusCounter + 1 Else CenterBonusCounter = 1
	UpdateCenterBonusLights
End Sub

Sub UpdateCenterBonusLights
	Select Case CenterBonusCounter
	Case 1 : BL01.State = 1 : BL02.State = 0 : BL03.State = 0 : BL04.State = 0 : BL05.State = 0 : BL06.State = 0 : BL07.State = 0 : BL08.State = 0 : BL09.State = 0
	Case 2 : BL01.State = 0 : BL02.State = 1 : BL03.State = 0 : BL04.State = 0 : BL05.State = 0 : BL06.State = 0 : BL07.State = 0 : BL08.State = 0 : BL09.State = 0
	Case 3 : BL01.State = 0 : BL02.State = 0 : BL03.State = 1 : BL04.State = 0 : BL05.State = 0 : BL06.State = 0 : BL07.State = 0 : BL08.State = 0 : BL09.State = 0
	Case 4 : BL01.State = 0 : BL02.State = 0 : BL03.State = 0 : BL04.State = 1 : BL05.State = 0 : BL06.State = 0 : BL07.State = 0 : BL08.State = 0 : BL09.State = 0
	Case 5 : BL01.State = 0 : BL02.State = 0 : BL03.State = 0 : BL04.State = 0 : BL05.State = 1 : BL06.State = 0 : BL07.State = 0 : BL08.State = 0 : BL09.State = 0
	Case 6 : BL01.State = 0 : BL02.State = 0 : BL03.State = 0 : BL04.State = 0 : BL05.State = 0 : BL06.State = 1 : BL07.State = 0 : BL08.State = 0 : BL09.State = 0
	Case 7 : BL01.State = 0 : BL02.State = 0 : BL03.State = 0 : BL04.State = 0 : BL05.State = 0 : BL06.State = 0 : BL07.State = 1 : BL08.State = 0 : BL09.State = 0
	Case 8 : BL01.State = 0 : BL02.State = 0 : BL03.State = 0 : BL04.State = 0 : BL05.State = 0 : BL06.State = 0 : BL07.State = 0 : BL08.State = 1 : BL09.State = 0
	Case 9 : BL01.State = 0 : BL02.State = 0 : BL03.State = 0 : BL04.State = 0 : BL05.State = 0 : BL06.State = 0 : BL07.State = 0 : BL08.State = 0 : BL09.State = 1
	End Select
End Sub

Sub UpdateSpinnerLight
	Select Case SpinCounter
	Case 1 : LS01.State = 1 : LS02.State = 0 : LS03.State = 0 : LS04.State = 0 : LS05.State = 0 : LS06.State = 0 : LS07.State = 0 : LS08.State = 0 : LS09.State = 0 : LS10.State = 0
	Case 2 : LS01.State = 0 : LS02.State = 1 : LS03.State = 0 : LS04.State = 0 : LS05.State = 0 : LS06.State = 0 : LS07.State = 0 : LS08.State = 0 : LS09.State = 0 : LS10.State = 0
	Case 3 : LS01.State = 0 : LS02.State = 0 : LS03.State = 1 : LS04.State = 0 : LS05.State = 0 : LS06.State = 0 : LS07.State = 0 : LS08.State = 0 : LS09.State = 0 : LS10.State = 0
	Case 4 : LS01.State = 0 : LS02.State = 0 : LS03.State = 0 : LS04.State = 1 : LS05.State = 0 : LS06.State = 0 : LS07.State = 0 : LS08.State = 0 : LS09.State = 0 : LS10.State = 0
	Case 5 : LS01.State = 0 : LS02.State = 0 : LS03.State = 0 : LS04.State = 0 : LS05.State = 1 : LS06.State = 0 : LS07.State = 0 : LS08.State = 0 : LS09.State = 0 : LS10.State = 0
	Case 6 : LS01.State = 0 : LS02.State = 0 : LS03.State = 0 : LS04.State = 0 : LS05.State = 0 : LS06.State = 1 : LS07.State = 0 : LS08.State = 0 : LS09.State = 0 : LS10.State = 0
	Case 7 : LS01.State = 0 : LS02.State = 0 : LS03.State = 0 : LS04.State = 0 : LS05.State = 0 : LS06.State = 0 : LS07.State = 1 : LS08.State = 0 : LS09.State = 0 : LS10.State = 0
	Case 8 : LS01.State = 0 : LS02.State = 0 : LS03.State = 0 : LS04.State = 0 : LS05.State = 0 : LS06.State = 0 : LS07.State = 0 : LS08.State = 1 : LS09.State = 0 : LS10.State = 0
	Case 9 : LS01.State = 0 : LS02.State = 0 : LS03.State = 0 : LS04.State = 0 : LS05.State = 0 : LS06.State = 0 : LS07.State = 0 : LS08.State = 0 : LS09.State = 1 : LS10.State = 0
	Case 10 : LS01.State = 0 : LS02.State = 0 : LS03.State = 0 : LS04.State = 0 : LS05.State = 0 : LS06.State = 0 : LS07.State = 0 : LS08.State = 0 : LS09.State = 0 : LS10.State = 1
	End Select
End Sub

Sub Alternate()
	If LightTopLeft.State = 1 Then
		LightTopLeft.State = 0 : LightTopRight.State = 1
		LightTargetLeft.State = 1 : LightTargetRight.State = 0	
	Else
		LightTopLeft.State = 1 : LightTopRight.State = 0
		LightTargetLeft.State = 0 : LightTargetRight.State = 1
	End If
	If SpecialCenter = 1 Then
		If LightSpecialLeft.State = 1 Then
			LightSpecialLeft.State = 0 : LightSpecialRight.State = 1
		Else
			LightSpecialLeft.State = 1 : LightSpecialRight.State = 0
		End If
	End If
End Sub

' ============================================================================================
' GNMOD - Multiple High Score Display and Collection
' jpsalas: changed ramps by flashers
' ============================================================================================

Dim EnteringInitials ' Normally zero, set to non-zero to enter initials
EnteringInitials = False
Dim ScoreChecker
ScoreChecker = 0
Dim CheckAllScores
CheckAllScores = 0
Dim sortscores(4)
Dim sortplayers(4)

Dim PlungerPulled
PlungerPulled = 0

Dim SelectedChar   ' character under the "cursor" when entering initials

Dim HSTimerCount   ' Pass counter For HS timer, scores are cycled by the timer
HSTimerCount = 5   ' Timer is initially enabled, it'll wrap from 5 to 1 when it's displayed

Dim InitialString  ' the string holding the player's initials as they're entered

Dim AlphaString    ' A-Z, 0-9, space (_) and backspace (<)
Dim AlphaStringPos ' pointer to AlphaString, move Forward and backward with flipper keys
AlphaString = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_<"

Dim HSNewHigh      ' The new score to be recorded

Dim HSScore(5)     ' High Scores read in from config file
Dim HSName(5)      ' High Score Initials read in from config file

' default high scores, remove this when the scores are available from the config file
HSScore(1) = 230000
HSScore(2) = 210000
HSScore(3) = 190000
HSScore(4) = 170000
HSScore(5) = 150000

HSName(1) = "AAA"
HSName(2) = "ZZZ"
HSName(3) = "XXX"
HSName(4) = "ABC"
HSName(5) = "BBB"

Sub HighScoreTimer_Timer
    If EnteringInitials then
        If HSTimerCount = 1 then
            SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
            HSTimerCount = 2
        Else
            SetHSLine 3, InitialString
            HSTimerCount = 1
        End If
    ElseIf bGameInPlay then
        SetHSLine 1, "HIGH SCORE1"
        SetHSLine 2, HSScore(1)
        SetHSLine 3, HSName(1)
        HSTimerCount = 5 ' set so the highest score will show after the game is over
        HighScoreTimer.enabled = false
    ElseIf CheckAllScores then
        NewHighScore sortscores(ScoreChecker), sortplayers(ScoreChecker)
    Else
        ' cycle through high scores
        HighScoreTimer.interval = 2000
        HSTimerCount = HSTimerCount + 1
        If HsTimerCount> 5 then
            HSTimerCount = 1
        End If
        SetHSLine 1, "HIGH SCORE" + FormatNumber(HSTimerCount, 0)
        SetHSLine 2, HSScore(HSTimerCount)
        SetHSLine 3, HSName(HSTimerCount)
    End If
End Sub

Function GetHSChar(String, Index)
    Dim ThisChar
    Dim FileName
    ThisChar = Mid(String, Index, 1)
    FileName = "PostIt"
    If ThisChar = " " or ThisChar = "" then
        FileName = FileName & "BL"
    ElseIf ThisChar = "<" then
        FileName = FileName & "LT"
    ElseIf ThisChar = "_" then
        FileName = FileName & "SP"
    Else
        FileName = FileName & ThisChar
    End If
    GetHSChar = FileName
End Function

Sub SetHsLine(LineNo, String)
    Dim Letter
    Dim ThisDigit
    Dim ThisChar
    Dim StrLen
    Dim LetterLine
    Dim Index
    Dim StartHSArray
    Dim EndHSArray
    Dim LetterName
    Dim xFor
    StartHSArray = array(0, 1, 12, 22)
    EndHSArray = array(0, 11, 21, 31)
    StrLen = len(string)
    Index = 1

    For xFor = StartHSArray(LineNo) to EndHSArray(LineNo)
        Eval("HS" &xFor).imageA = GetHSChar(String, Index)
        Index = Index + 1
    Next
End Sub

Sub NewHighScore(NewScore, PlayNum)
    If NewScore> HSScore(5) then
        HighScoreTimer.interval = 500
        HSTimerCount = 1
        AlphaStringPos = 1      ' start with first character "A"
        EnteringInitials = true ' intercept the control keys while entering initials
        InitialString = ""      ' initials entered so far, initialize to empty
        SetHSLine 1, "PLAYER " + FormatNumber(PlayNum, 0)
        SetHSLine 2, "ENTER NAME"
        SetHSLine 3, MID(AlphaString, AlphaStringPos, 1)
        HSNewHigh = NewScore
        PlaySound "sfx_Enter"
    End If
    ScoreChecker = ScoreChecker-1
    If ScoreChecker = 0 then
        CheckAllScores = 0
    End If
End Sub

Sub CollectInitials(keycode)
    Dim i
    If keycode = LeftFlipperKey Then
        ' back up to previous character
        AlphaStringPos = AlphaStringPos - 1
        If AlphaStringPos <1 then
            AlphaStringPos = len(AlphaString) ' handle wrap from beginning to End
            If InitialString = "" then
                ' Skip the backspace If there are no characters to backspace over
                AlphaStringPos = AlphaStringPos - 1
            End If
        End If
        SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
        PlaySound "sfx_Previous"
    ElseIf keycode = RightFlipperKey Then
        ' advance to Next character
        AlphaStringPos = AlphaStringPos + 1
        If AlphaStringPos> len(AlphaString) or(AlphaStringPos = len(AlphaString) and InitialString = "") then
            ' Skip the backspace If there are no characters to backspace over
            AlphaStringPos = 1
        End If
        SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
        PlaySound "sfx_Next"
    ElseIf keycode = StartGameKey or keycode = PlungerKey Then
        SelectedChar = MID(AlphaString, AlphaStringPos, 1)
        If SelectedChar = "_" then
            InitialString = InitialString & " "
            PlaySound("sfx_Esc")
        ElseIf SelectedChar = "<" then
            InitialString = MID(InitialString, 1, len(InitialString) - 1)
            If len(InitialString) = 0 then
                ' If there are no more characters to back over, don't leave the < displayed
                AlphaStringPos = 1
            End If
            PlaySound("sfx_Esc")
        Else
            InitialString = InitialString & SelectedChar
            PlaySound("sfx_Enter")
        End If
        If len(InitialString) <3 then
            SetHSLine 3, InitialString & SelectedChar
        End If
    End If
    If len(InitialString) = 3 then
        ' save the score
        For i = 5 to 1 step -1
            If i = 1 or(HSNewHigh> HSScore(i) and HSNewHigh <= HSScore(i - 1) ) then
                ' Replace the score at this location
                If i <5 then
                    HSScore(i + 1) = HSScore(i)
                    HSName(i + 1) = HSName(i)
                End If
                EnteringInitials = False
                HSScore(i) = HSNewHigh
                HSName(i) = InitialString
                HSTimerCount = 5
                HighScoreTimer_Timer
                HighScoreTimer.interval = 2000
                PlaySound("fx_Bong")
                Exit Sub
            ElseIf i <5 then
                ' move the score in this slot down by 1, it's been exceeded by the new score
                HSScore(i + 1) = HSScore(i)
                HSName(i + 1) = HSName(i)
            End If
        Next
    End If
End Sub
' End GNMOD