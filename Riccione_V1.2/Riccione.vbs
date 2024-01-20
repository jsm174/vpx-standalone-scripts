' ****************************************************************
'               VISUAL PINBALL X EM Script by JPSalas
' 		Basic Script for EM Script games up to 4 players
' 				uses core.vbs for extra functions
'      Thanks goes to Fusionwerks for Cabana and Parasol objects
' 
' ****************************************************************

Option Explicit
Randomize
'
' DOF config - leeoneil
'
' Option for more lights effects with DOF (Undercab effects on bumpers and slingshots) 
' Replace "False" by "True" to activate (False by default)
Const Epileptikdof = False
'
' Flippers L/R - 101/102
' Slingshot L/R - 103/104
' Bumpers  - 105/106/107/108/109
' Targets - 110/111/112/113/114
'
' LED backboard
' Flasher Outside Left - 203/209/214/218/225/227/240/250
' Flasher left - 207/213/215/218/222/229/240/250
' Flasher center - 211/212/215/217/218/221/223/232/240/250
' Flasher right - 208/213/217/221/224/231/240/250
' Flasher Outside - 205/210/216/220/226/228/240/250
' PF Flashers L/R - 204/206
' Start Button - 200
' Undercab - 201/202
' Strobe - 230
' Knocker - 300
' Chimes - 301/302/303

' Constant values of the flippers physics - used in the creation of the balls, they have to be loaded before core.vbs
Const BallSize = 25 ' the radius of the ball. normal size is 50 units of VP
Const BallMass = 1 ' the ball weight, this value is according to the strength of the flippers and the plunger.

' Load the core.vbs to be able to use its functions, especially the vpintimer.addtimer.
LoadCoreFiles

Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    On Error Resume Next
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "Can't open controller.vbs"
End Sub

' Changeable values
Const Tiempo = 27 '30 seconds, 20 seconds o 10 seconds

' Constant values
' for DOF users: make a new line in VPMAlias.txt "T1D_123_riccione,T1D_123" to use existing DOF values from 1-2-3

Const TableName = "Riccione" ' used to load and record highscore and credits
Const cGameName = "T1D_123_riccione" ' for B2S
Const MaxPlayers = 1        
Const MaxMultiplier = 5     
Const BallsPerGame = 5      
Const Special1 = 30000      ' score to be obtained for bonus game
Const Special2 = 50000      ' score to be obtained for bonus game
Const Special3 = 100000     ' score to be obtained for bonus game

' Global values
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim Bonus
Dim DoubleBonus
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Special1Awarded(4)
Dim Special2Awarded(4)
Dim Special3Awarded(4)
Dim Score(4)
Dim HighScore
Dim Match
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim Add10
Dim Add100
Dim Add1000
Dim x

' Control values
Dim LastSwitchHit
Dim BallsOnPlayfield
Dim BallsLocked

' Boolean values (True or False)
Dim bAttractMode
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bExtraBallWonThisBall
Dim bJustStarted
Dim bBallInPlungerLane
Dim bBallSaverActive


' core.vbs variables, like magnets, impulse plunger

' *********************************************************************
'                Common routines for all tables
' *********************************************************************

Sub Table1_Init()
    Dim x

    ' Initialize various objects on the table, such as droptargets, animations....
    VPObjects_Init
    LoadEM

    ' Load highscore and credits recorded values
    Loadhs

    UpdateCredits

    ' Free play or with coins: if True, then no coins will be used.
    bFreePlay = False 

    ' Initialize the global variables of the table
    bAttractMode = False
    bOnTheFirstBall = False
    bGameInPlay = False
    bBallInPlungerLane = False
    LastSwitchHit = ""
    BallsOnPlayfield = 0
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    Match = 0
    bJustStarted = True
    Add10 = 0
    Add100 = 0
    Add1000 = 0

    ' puts the table in standby mode
    EndOfGame

    'Turns GI lights on after one second
    vpmtimer.addtimer 1000, "GiOn '"

    ' Removes sides and scores when the table is played in FS mode
    If Table1.ShowDT then
        lrail.Visible = True
        rrail.Visible = True
        For each x in aReels
            x.Visible = 1
        Next
    Else
        lrail.Visible = False
        rrail.Visible = False
        For each x in aReels
            x.Visible = 0
        Next
    End If

   LoadLUT
End Sub

'**************
'Strandkabine - Cabana
'**************


Sub Door1_Hit ()
	Door1.isdropped = True
	AddScore 500
	li5.State = 0
	PlaySoundAtBall SoundFXDOF ("fx_target", 110, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 212, DOFPulse
    If Epileptikdof = True Then DOF 213, DOFPulse End If
    LastSwitchHit = "target1"
	CheckDoors()
End Sub

Sub Door2_Hit ()
	Door2.isdropped = True
	AddScore 500
	li6.State = 0
	PlaySoundAtBall SoundFXDOF ("fx_target", 110, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 212, DOFPulse
    If Epileptikdof = True Then DOF 213, DOFPulse End If
    LastSwitchHit = "target2"
	CheckDoors()
End Sub

Sub Door3_Hit ()
	Door3.isdropped = True
	AddScore 500
	li7.State = 0
	PlaySoundAtBall SoundFXDOF ("fx_target", 110, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 212, DOFPulse
    If Epileptikdof = True Then DOF 213, DOFPulse End If
    LastSwitchHit = "target3"
	CheckDoors()
End Sub

Sub CheckDoors()
	If li5.State = 0 AND li6.State = 0 AND li7.State = 0 Then 'den Extra-Ball zum Leuchten bringen
        li3.State = 1
        li4.State = 1
    End If
End Sub

Sub Kicker001_Hit ()
	BallsLocked = BallsLocked + 1
	CheckBallsInPlay()
	AddScore 500
End Sub

Sub Kicker002_Hit ()
	BallsLocked = BallsLocked + 1
	CheckBallsInPlay()
	AddScore 500
End Sub

Sub Kicker003_Hit ()
	BallsLocked = BallsLocked + 1
	CheckBallsInPlay()
	AddScore 500
End Sub

Sub CheckBallsInPlay()
	If BallsLocked = 3 then ' All locked up, kick 'em out
		PlaySound "seagull"
		vpmtimer.addtimer 2000, "ReleaseMultiball '"
		
	Else 
		If BallsOnPlayfield = 1 Then
		BallRelease.CreateSizedBallWithMass BallSize, BallMass
		PlaySoundAt SoundFXDOF ("fx_Ballrel", 104, DOFPulse, DOFContactors), BallRelease
		BallRelease.Kick 90, 4
		
		Else 
			If BallsOnPlayfield > 1 Then ' when ball locked during multiball
			BallsOnPlayfield = BallsOnPlayfield - 1
			End If
		End If
	End If
End Sub

Sub ReleaseMultiball()
	TurnOffBallSaver
		li3.State = 0 
		li4.State = 0
		Kicker001.Kick 190, 3	' Kick the ball (direction, force)
		Kicker002.Kick 190, 4
		Kicker003.Kick 170, 3
		AddScore 1000
		BallsLocked = 0		' No more locked balls now
		BallsOnPlayfield = BallsOnPlayfield + 2
		vpmtimer.addtimer 500, "ShutDoors '"
End Sub

Sub ShutDoors()
	Door1.isdropped = False
	Door2.isdropped = False
	Door3.isdropped = False
	ResetNewBallLights
End Sub

'******
' Aquafun
'******

Sub Hole_Hit ()
	AddScore 1000
	PlaySoundAt "water-jump", Drain
    DOF 232, DOFPulse
	Hole.DestroyBall
	vpmtimer.addtimer 2000, "AquafunRelease '"
End Sub

Sub AquafunRelease()
	Aquafun.CreateSizedBallWithMass BallSize, BallMass
	PlaySoundAt SoundFXDOF ("water-jump", 104, DOFPulse, DOFContactors), Aquafun
	Aquafun.Kick 180,2
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)
    If EnteringInitials then
        CollectInitials(keycode)
        Exit Sub
    End If

    If keycode = LeftMagnaSave Then bLutActive = True: Lutbox.text = "level of darkness " & LUTImage + 1
    If keycode = RightMagnaSave Then
        If bLutActive Then NextLUT:End If
    End If

    ' Credits hinzufügen
    If Keycode = AddCreditKey Then
        If(Tilted = False)Then
            AddCredits 1
            PlaySound "fx_coin"
            DOF 200, DOFOn
        End If
    End If

    ' Plunger
    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
    End If

    ' Normal operation of flippers and other keys during game play

    If bGameInPlay AND NOT Tilted Then
        If Credits = 0 Then DOF 200, DOFOff
        ' default key
        If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
        If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt

        ' flipers
        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1

        ' start key
        If keycode = StartGameKey Then
            If((PlayersPlayingGame < MaxPlayers)AND(bOnTheFirstBall = True))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    PlayersReel.SetValue, PlayersPlayingGame
                'PlaySound "so_fanfare1"
                Else
                    If(Credits > 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        Credits = Credits - 1
                        UpdateCredits
                        UpdateBallInPlay
                    Else
                    DOF 200, DOFOff
                    ' there are not enough credits to start the game.
                    'PlaySound "so_nocredits".
                    End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = True)Then
                    If(BallsOnPlayfield = 0)Then
                        ResetScores
                        ResetForNewGame()
                    End If
                Else
                    If(Credits > 0)Then
                        If(BallsOnPlayfield = 0)Then
                            Credits = Credits - 1
                            UpdateCredits
                            ResetScores
                            ResetForNewGame()
                        End If
                    Else
                    ' Not Enough Credits to start a game.
                    'PlaySound "so_nocredits"
                    End If
                End If
            End If
    End If ' If (GameInPlay)
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If EnteringInitials then
        Exit Sub
    End If

    If keycode = LeftMagnaSave Then bLutActive = False: LutBox.text = ""

    If bGameInPlay AND NOT Tilted Then
        ' flipper keys
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

'*************
' For the table
'*************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub table1_Exit
    Savehs
Controller.Stop
End Sub

'***************************
'   LUT - Darkness control 
'***************************

Dim bLutActive, LUTImage

Sub LoadLUT
    bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "")Then LUTImage = x Else LUTImage = 0
    UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT:LUTImage = (LUTImage + 1)MOD 15:UpdateLUT:SaveLUT:Lutbox.text = "level of darkness " & LUTImage + 1:End Sub

Sub UpdateLUT
    Select Case LutImage
        Case 0:table1.ColorGradeImage = "LUT0":GiIntensity = 1:ChangeGIIntensity 1
        Case 1:table1.ColorGradeImage = "LUT1":GiIntensity = 1.05:ChangeGIIntensity 1
        Case 2:table1.ColorGradeImage = "LUT2":GiIntensity = 1.1:ChangeGIIntensity 1
        Case 3:table1.ColorGradeImage = "LUT3":GiIntensity = 1.15:ChangeGIIntensity 1
        Case 4:table1.ColorGradeImage = "LUT4":GiIntensity = 1.2:ChangeGIIntensity 1
        Case 5:table1.ColorGradeImage = "LUT5":GiIntensity = 1.25:ChangeGIIntensity 1
        Case 6:table1.ColorGradeImage = "LUT6":GiIntensity = 1.3:ChangeGIIntensity 1
        Case 7:table1.ColorGradeImage = "LUT7":GiIntensity = 1.35:ChangeGIIntensity 1
        Case 8:table1.ColorGradeImage = "LUT8":GiIntensity = 1.4:ChangeGIIntensity 1
        Case 9:table1.ColorGradeImage = "LUT9":GiIntensity = 1.45:ChangeGIIntensity 1
        Case 10:table1.ColorGradeImage = "LUT10":GiIntensity = 1.5:ChangeGIIntensity 1
        Case 11:table1.ColorGradeImage = "LUT11":GiIntensity = 1.55:ChangeGIIntensity 1
        Case 12:table1.ColorGradeImage = "LUT12":GiIntensity = 1.6:ChangeGIIntensity 1
        Case 13:table1.ColorGradeImage = "LUT13":GiIntensity = 1.65:ChangeGIIntensity 1
        Case 14:table1.ColorGradeImage = "LUT14":GiIntensity = 1.7:ChangeGIIntensity 1
    End Select
End Sub

Dim GiIntensity
GiIntensity = 1   'used for the LUT changing to increase the GI lights when the table is darker

Sub ChangeGiIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in aGiLights
        bulb.IntensityScale = GiIntensity * factor
    Next
End Sub

'********************
'     Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF ("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
        LeftFlipper.EOSTorque = 0.65:LeftFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFXDOF ("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        LeftFlipper.EOSTorque = 0.15:LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF ("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        RightFlipper.EOSTorque = 0.65:RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFXDOF ("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.EOSTorque = 0.15:RightFlipper.RotateToStart
    End If
End Sub

' the sound of the ball hitting the flippers

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'*******************
' GI Lights
'*******************

Sub GiOn 'turns on the GI lights
    DOF 201, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff 'turn off the GI lights
    DOF 201, DOFOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
End Sub

'**************
' TILT - Foul
'**************

'the "timer" TiltDecreaseTimer subtracts .01 from the "Tilt" variable each round

Sub CheckTilt                     'this routine is called every time you hit the table
    Tilt = Tilt + TiltSensitivity 'adds a value to the counter "Tilt".
    TiltDecreaseTimer.Enabled = True
    If Tilt > 15 Then             'If the "Tilt" variable is greater than 15 then you need to
        Tilted = True
        TiltReel.SetValue 1       'displays Tilt on the screen
        If B2SOn then
            Controller.B2SSetTilt 1
        end if
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'starts a pause so that all the balls are filtered out
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt > 0 Then
        Tilt = Tilt - 0.1
    Else
        TiltDecreaseTimer.Enabled = False
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
        'Turn off all Gi lights on the table
        GiOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        Bumper1.Threshold = 100
        Bumper2.Threshold = 100
        Bumper3.Threshold = 100
        Bumper4.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
        DOF 101, DOFOff
        DOF 102, DOFOff
    Else
        'relights all GI lights, bumpers and slingshots
        GiOn
        Bumper1.Threshold = 1
        Bumper2.Threshold = 1
        Bumper3.Threshold = 1
        Bumper4.Threshold = 1
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all of the balls have been strained, then ..
    If(BallsOnPlayfield = 0)Then
        '... make the end of normal ball
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
    End If
' otherwise this routine continues until all the balls have been strained.
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

'***********************************************
'   JP's VP10 Rolling Sounds + Ballshadow v4.0
'   uses a collection of shadows, aBallShadow
'***********************************************

Const tnob = 19   'total number of balls
Const lob = 0     'number of locked balls
Const maxvel = 26 'max ball velocity
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
        aBallShadow(b).Y = 3000
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y
        aBallShadow(b).Height = BOT(b).Z -Ballsize/2

        If BallVel(BOT(b))> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 50000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 10
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
        If BOT(b).VelZ <-1 and BOT(b).z <55 and BOT(b).z> 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed control
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(maxvel / BOT(b).VelX)
            speedfactory = ABS(maxvel / BOT(b).VelY)
            If speedfactorx <1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactorx
                BOT(b).VelY = BOT(b).VelY * speedfactorx
            End If
            If speedfactory <1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactory
                BOT(b).VelY = BOT(b).VelY * speedfactory
            End If
        End If
    Next
End Sub

'*****************************
' Sound of colliding balls
'*****************************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'***************************************
' Sounds from collections of objects
' such as metals, rubbers, plastics, etc.
'***************************************

Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

'************************************************************************************************************************
' Only for VPX 10.2 and later.
' FlashForMs will flash a light or flasher for a few milliseconds "TotalPeriod" every so many milliseconds "BlinkPeriod".
' When the "TotalPeriod" has finished, the light or flasher will be set to the state specified by the "FinalState" value.
' The value of "FinalState" can be: 0=off, 1=on, 2=return to the previous state.
'************************************************************************************************************************

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

        ' starts the blinks and creates the routine to be executed as a timer that will execute the blinks.
        MyLight.TimerInterval = BlinkPeriod
        MyLight.TimerEnabled = 0
        MyLight.TimerEnabled = 1
        ExecuteGlobal "Sub " & MyLight.Name & "_Timer:" & "Dim tmp, steps, fstate:tmp=me.UserValue:fstate = tmp MOD 10:steps= tmp\10 -1:Me.Visible = steps MOD 2:me.UserValue = steps *10 + fstate:If Steps = 0 then Me.Visible = fstate:Me.TimerEnabled=0:End if:End Sub"
    End If
End Sub

'****************************************
' Initializes the table for a new game
'****************************************

Sub ResetForNewGame()
    'debug.print "ResetForNewGame"
    Dim i

    bGameInPLay = True
    bBallSaverActive = False

    StopAttractMode
    If B2SOn then
        Controller.B2SSetGameOver 0
    end if
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
        BallsRemaining(i) = BallsPerGame
    Next
    DoubleBonus = 1
    Bonus = 0
    UpdateBallInPlay

    Clear_Match

    Tilt = 0

    Game_Init()

    ' jetzt können Sie eine Musik starten, wenn Sie wollen
	PlaySound "riccione_instrumental",-1
    ' nach einer kurzen Pause die "Firstball"-Routine starten.
    vpmtimer.addtimer 2000, "FirstBall '"
    If Credits = 0 Then DOF 200, DOFOff
End Sub

' this pause is to allow the table time to reset the markers and update the lights.

Sub FirstBall
    'debug.print "FirstBall"
    ' adjust the table for a new ball, raise the flip-up targets, etc.
    ResetForNewPlayerBall()
    ' creates a new ball in the plunger zone
    CreateNewBall()
    DOF 250, DOFPulse
End Sub

' (Re-)initialize the table for a new ball, whether you have lost the ball or it is the other player's turn.

Sub ResetForNewPlayerBall()
    'debug.print "ResetForNewPlayerBall"
    ' Ensures that the markers are activated for the player on duty
    AddScore 0

       ' turn on the lights, reset the game variables, etc.
    bExtraBallWonThisBall = False
    'ResetNewBallLights
    ResetNewBallVariables
End Sub

' Creates a new ball on the table

Sub CreateNewBall()
    ' creates a new ball based on the size and mass of the ball specified at the beginning of the script
    BallRelease.CreateSizedBallWithMass BallSize, BallMass

    ' increases the number of balls on the board, as they have to be counted.
    BallsOnPlayfield = BallsOnPlayfield + 1
	
    ' update the backdrop lights
    UpdateBallInPlay

    PlaySoundAt SoundFXDOF ("fx_Ballrel", 104, DOFPulse, DOFContactors), BallRelease
    BallRelease.Kick 90, 4
	
	bBallSaverActive = True
	BallSaver.State = 1
	vpmtimer.addtimer 15000, "TurnOffBallSaver '"
End Sub

Sub TurnOffBallSaver()
	bBallSaverActive = False
	BallSaver.State = 0
End Sub

' The player has lost his ball, and there are no more balls in play.
' Start counting bonuses

Sub EndOfBall()
    'debug.print "EndOfBall"
    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 0
    ' The first one has been lost. From here on, no more players can be accepted
    bOnTheFirstBall = False

    ' only collect bonuses if there is no foul
    ' the foul system will take care of new balls or end of the game

    If NOT Tilted Then
        If DoubleBonus = 2 Then
            BonusCountTimer.Interval = 400
        Else
            BonusCountTimer.Interval = 250
        End If
        BonusCountTimer.Enabled = 1
    Else 'If there is a foul, simply wait a moment and go straight to the second part after losing the ball.
        vpmtimer.addtimer 400, "EndOfBall2 '"
    End If
End Sub

Sub BonusCountTimer_Timer
    'debug.print "BonusCount_Timer"
    If Bonus > 0 Then
        Bonus = Bonus -1
        AddScore 1000 * DoubleBonus
        UpdateBonusLights
    Else
        BonusCountTimer.Enabled = 0
        vpmtimer.addtimer 1000, "EndOfBall2 '"
    End If
End Sub

Sub UpdateBonusLights 'schaltet die Bonuslichter entsprechend der Variable "Bonus" ein oder aus.
    Select Case Bonus
        Case 0:li2.State = 0:li3.State = 0:li4.State = 0:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 0
        Case 1:li2.State = 1:li3.State = 0:li4.State = 0:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 0
        Case 2:li2.State = 0:li3.State = 1:li4.State = 0:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 0
        Case 3:li2.State = 0:li3.State = 0:li4.State = 1:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 0
        Case 4:li2.State = 0:li3.State = 0:li4.State = 0:li5.State = 1:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 0
        Case 5:li2.State = 0:li3.State = 0:li4.State = 0:li5.State = 0:li6.State = 1:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 0
        Case 6:li2.State = 0:li3.State = 0:li4.State = 0:li5.State = 0:li6.State = 0:li7.State = 1:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 0
        Case 7:li2.State = 0:li3.State = 0:li4.State = 0:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 1:li9.State = 0:li10.State = 0:li11.State = 0
        Case 8:li2.State = 0:li3.State = 0:li4.State = 0:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 1:li10.State = 0:li11.State = 0
        Case 9:li2.State = 0:li3.State = 0:li4.State = 0:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 1:li11.State = 0
        Case 10:li2.State = 0:li3.State = 0:li4.State = 0:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 1
        Case 11:li2.State = 1:li3.State = 0:li4.State = 0:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 1
        Case 12:li2.State = 0:li3.State = 1:li4.State = 0:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 1
        Case 13:li2.State = 0:li3.State = 0:li4.State = 1:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 1
        Case 14:li2.State = 0:li3.State = 0:li4.State = 0:li5.State = 1:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 1
        Case 15:li2.State = 0:li3.State = 0:li4.State = 0:li5.State = 0:li6.State = 1:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 1
    End Select
End Sub

' The bonus count is over. See if the player has won extra balls
' and if not see if he is the last player or the last ball.
'
Sub EndOfBall2()
    'debug.print "EndOfBall2"
    ' if there is a foul, remove it, and reset the foul count to zero for the next player, or ball

    Tilted = False
    Tilt = 0
    TiltReel.SetValue 0
    If B2SOn then
        Controller.B2SSetTilt 0
    end if
    DisableTable False 're-enable bumpers and slingshots

    ' has the player earned an extra ball?
    If(ExtraBallsAwards(CurrentPlayer) > 0)Then
        'debug.print "Extra Ball"

        ' yes? then you give it to the player
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1
		
        ' if there are no more balls turn the play light off again
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
		li3.State = 0 
		li4.State = 0
        'LightShootAgain.State = 0
        'If B2SOn then
        '    Controller.B2SSetShootAgain 0
        'end if
        End If

' here you could put some extra ball sound or some flashing light.

' In this table we make the extra ball the same as if it were the next ball, resetting the variables and targets.
        ResetForNewPlayerBall()

        ' we create a new ball in the firing corridor
        CreateNewBall()
    Else 

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1

        If(BallsRemaining(CurrentPlayer) <= 0)Then

            CheckHighScore()
        End If

        EndOfBallComplete()
    End If
End Sub

' Diese Routine wird am Ende der Bonuszählung aufgerufen
' und macht mit dem nächsten Ball oder dem nächsten Spieler weiter.
'
Sub EndOfBallComplete()
    'debug.print "EndOfBallComplete"
    Dim NextPlayer

    'debug.print "EndOfBall - Complete"

    ' Gibt es noch andere Spieler?
    If(PlayersPlayingGame > 1)Then
        
        NextPlayer = CurrentPlayer + 1
        
        If(NextPlayer > PlayersPlayingGame)Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' Haben wir das Ende des Spiels erreicht (alle Bälle wurden von allen Spielern gespielt)?
    If((BallsRemaining(CurrentPlayer) <= 0)AND(BallsRemaining(NextPlayer) <= 0))Then

        ' Hier beginnt die Lotterie, wenn Sie mit Münzen spielen.
        If bFreePlay = False Then
            Verification_Match
        End If

        ' jetzt ist der Tisch im EndOfGame Modus
        EndOfGame()
    Else
        
        CurrentPlayer = NextPlayer

        AddScore 0

        ResetForNewPlayerBall()

        CreateNewBall()
    End If
End Sub

' Diese Funktion wird am Ende des Spiels aufgerufen

Sub EndOfGame()
    'debug.print "EndOfGame"
    bGameInPlay = False
    bJustStarted = False
	ResetNewBallLights
	StopSound "riccione_instrumental"
	
	'reset BAR Lights
		li10.State = 1
		li11.State = 1
		li12.State = 1

	'Strandkabine zurücksetzen
		Kicker001.DestroyBall	' Kick the ball (direction, force)
		Kicker002.DestroyBall
		Kicker003.DestroyBall
		BallsLocked = 0
		BallsOnPlayfield = 0
		'vpmtimer.addtimer 500, "ShutDoors '"
		Door1.isdropped = False
		Door2.isdropped = False
		Door3.isdropped = False

    If B2SOn then
        Controller.B2SSetGameOver 1
        Controller.B2SSetBallInPlay 0
        Controller.B2SSetPlayerUp 0
        Controller.B2SSetCanPlay 0
    end if
    
    SolLFlipper 0
    SolRFlipper 0
    DOF 250, DOFPulse

	
	BallsOnPlayfield = 0
    StartAttractMode
End Sub

' This function calculates the number of balls remaining
Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp > BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function

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
            If sortscores(sj) > sortscores(sj + 1)then
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
'  Match - Loteria
'******************

Sub IncreaseMatch
    Match = (Match + 10)MOD 100
End Sub

Sub Verification_Match()
    PlaySound "fx_match"
    Display_Match
    If(Score(CurrentPlayer)MOD 100) = Match Then
        PlaySound SoundFXDOF ("fx_knocker", 300, DOFPulse, DOFKnocker)
        DOF 230, DOFPulse
        AddCredits 1
        DOF 200, DOFOn
    End If
End Sub

Sub Clear_Match()
    MatchReel.SetValue 0
    If B2SOn then
        Controller.B2SSetMatch 0
    end if
End Sub

Sub Display_Match()
    MatchReel.SetValue 1 + (Match \ 10)
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

' you have lost the ball ;-( see how many balls are on the board.
' if there is only one then reduce the number of balls and see if it is the last ball to end the game.
' if there is more than one, it means there is multiball, then continue the game.
'
Sub Drain_Hit()
    Drain.DestroyBall

    BallsOnPlayfield = BallsOnPlayfield - 1

    ' make the sound of the ball
    PlaySoundAt "fx_drain", Drain
    DOF 232, DOFPulse

    'if there is a foul, the tilt system will continue with the next ball/player.
    If Tilted Then
        StopEndOfBallMode
    End If

    ' if you are playing and there is no foul
    If(bGameInPLay = True)AND(Tilted = False)Then

        ' ist der BallSaver aktiv?
        If(bBallSaverActive = True)Then

            ' dann gibt es einen neuen Ball
            CreateNewBall()
        Else
            ' war es der letzte Ball im Spiel?
            If(BallsOnPlayfield = 0)Then
                StopEndOfBallMode
                vpmtimer.addtimer 500, "EndOfBall '" 'we take a short pause before continuing with the end of ball
                Exit Sub
            End If
        End If
    End If
End Sub

Sub swPlungerRest_Hit()
    bBallInPlungerLane = True
End Sub

' The ball has been shot, so we change the variable, which in this table is used only so that the sound of the trigger changes depending on whether there is a ball there or not.
' In other tables it can be used to start a counter to save the ball.

Sub swPlungerRest_UnHit()
    bBallInPlungerLane = False
End Sub

' *********************************************************************
'               Functions for counting points
' *********************************************************************

' Adds points to player, rings bells and updates backdrop

Sub AddScore(Points)
    If Tilted Then Exit Sub
    Select Case Points
        Case 10, 100, 1000
            
            Score(CurrentPlayer) = Score(CurrentPlayer) + points
            
            UpdateScore points
            
            If Points = 100 AND(Score(CurrentPlayer)MOD 1000) \ 100 = 0 Then  '
                PlaySound SoundFXDOF ("chime1000", 303, DOFPulse, DOFChimes)
            ElseIf Points = 10 AND(Score(CurrentPlayer)MOD 100) \ 10 = 0 Then 
                PlaySound SoundFXDOF ("chime100", 302, DOFPulse, DOFChimes)
            Else
                PlaySound SoundFXDOF ("chime10", 301, DOFPulse, DOFChimes) &Points
            End If
        Case 50
            Add10 = Add10 + 5
            AddScore10Timer.Enabled = TRUE
        Case 300
            Add100 = Add100 + 3
            AddScore100Timer.Enabled = TRUE
        Case 500
            Add100 = Add100 + 5
            AddScore100Timer.Enabled = TRUE
        Case 2000, 3000, 4000, 5000
            Add1000 = Add1000 + Points \ 1000
            AddScore1000Timer.Enabled = TRUE
    End Select

    ' here you can check if the player has earned a high score and give him a credit or extra ball.
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
End Sub

'******************************
'TIMER OF 10, 100 and 1000 POINTS
'******************************

' rings the chimes according to the points

Sub AddScore10Timer_Timer()
    if Add10 > 0 then
        AddScore 10
        Add10 = Add10 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore100Timer_Timer()
    if Add100 > 0 then
        AddScore 100
        Add100 = Add100 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore1000Timer_Timer()
    if Add1000 > 0 then
        AddScore 1000
        Add1000 = Add1000 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

'*******************
'     BONUS
'*******************

' THIS TABLE DOES NOT USE BONUS

' advances the bonus and upgrades the lights
' Bonuses are limited to 1500 points

Sub AddBonus(bonuspoints)
    If(Tilted = False)Then
        
        Bonus = Bonus + bonuspoints
        If Bonus > 15 Then
            Bonus = 15
        End If
        
        UpdateBonusLights
    End if
End Sub

'***********************************************************************************
'        Score reels - scores - and update other backdrop lights
'***********************************************************************************


Sub UpdateScore(playerpoints)
    Select Case CurrentPlayer
        Case 1:ScoreReel1.Addvalue playerpoints
    ' Case 2:ScoreReel2.Addvalue playerpoints
    ' Case 3:ScoreReel3.Addvalue playerpoints
    ' Case 4:ScoreReel4.Addvalue playerpoints
    End Select
    If B2SOn then
        Controller.B2SSetScorePlayer CurrentPlayer, Score(CurrentPlayer)
        If Score(CurrentPlayer) >= 100000 then
            Controller.B2SSetScoreRollover 24 + CurrentPlayer, 1
        end if
    end if
End Sub

Sub ResetScores
    ScoreReel1.ResetToZero

    If B2SOn then
        Controller.B2SSetScorePlayer1 0
        Controller.B2SSetScoreRolloverPlayer1 0
    end if
End Sub

Sub AddCredits(value)
    If Credits < 9 Then
        Credits = Credits + value
        CreditsReel.SetValue Credits
        UpdateCredits
    end if
End Sub

Sub UpdateCredits
    If Credits > 0 Then
    'CreditLight.State = 1
    Else
    'CreditLight.State = 0
    End If
    PlaySound "fx_relay"
    CreditsReel.SetValue credits
    If B2SOn then
        Controller.B2SSetCredits Credits
    end if
End Sub

Sub UpdateBallInPlay
    Select Case Balls
        Case 0:BallDisplay.ImageA = "Ballnr0"
        Case 1:BallDisplay.ImageA = "Ballnr1"
        Case 2:BallDisplay.ImageA = "Ballnr2"
        Case 3:BallDisplay.ImageA = "Ballnr3"
        Case 4:BallDisplay.ImageA = "Ballnr4"
        Case 5:BallDisplay.ImageA = "Ballnr5"
    End Select
    If B2SOn then
        Controller.B2SSetBallInPlay Balls
    end if
End Sub

'*************************
' Extra games and balls
'*************************

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        PlaySound SoundFXDOF ("fx_knocker", 300, DOFPulse, DOFKnocker)
        DOF 230, DOFPulse
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
		li3.State = 0 
		li4.State = 0
    'LightShootAgain.State = 1
    'If B2SOn then
    '    Controller.B2SSetShootAgain 1
    'end if
    END If
End Sub

Sub AwardSpecial()
    PlaySound SoundFXDOF ("fx_knocker", 300, DOFPulse, DOFKnocker)
    DOF 230, DOFPulse
    AddCredits 1
End Sub

' ********************************
'        Attract Mode
' ********************************
' the lights simply blink according to the values we have set in the "Blink Pattern" of each light

Sub StartAttractMode()
    Dim x
    bAttractMode = True
    StartLightSeq
    GameOverR.SetValue 1
    BallDisplay.ImageA = "ballnr0"
    If Credits > 0 Then DOF 200, DOFOn
    DOF 201, DOFOff
End Sub

Sub StopAttractMode()
    Dim x
    bAttractMode = False
    LightSeqAttract.StopPlay
	LightSeqRing.StopPlay
    ResetScores
    GameOverR.SetValue 0
End Sub

Sub StartLightSeq()
    'lights sequences
    LightSeqAttract.UpdateInterval = 250
    LightSeqAttract.Play SeqRandom, 10, , 50000
	LightSeqRing.UpdateInterval = 300
    LightSeqRing.Play SeqRandom, 10, , 50000
End Sub

Sub LightSeqAttract_PlayDone()
    StartLightSeq()
End Sub



'****************************************
' Real-time updates
'****************************************
' is mainly used to make animations or sounds that change in realtime
' For example to synchronize flippers, doors or grinders with primitives.

Sub GameTimer_Timer
    RollingUpdate 'updates the sound of the ball rolling
					'and also some animations, especially of primitives.
End Sub

'***********************************************************************
' *********************************************************************
'            Here begins the code particular to the table
' (so far all the routines have been very general for all the tables)
' (and there are very few routines that need to change from table to table)
' *********************************************************************
'***********************************************************************

' starts flip-up targets, primitives, etc.
' although in VPX there are not many objects that need to be initialized.

Sub VPObjects_Init 

    TurnOffPlayfieldLights()
    'Obere Bahnen, nur einschalten, wenn der Tisch hochgefahren wird
    li10.State = 1
    li11.State = 1
    li12.State = 1
'
End Sub



Sub Game_Init() 'this routine is called at the beginning of a new game

'Start some music, if there was music in this table.

'start some variables, in this table there are very few variables since we use the lights, and the UserValue of the targets

'start some timer

'start some lights
'TurnOffPlayfieldLights()

End Sub

Sub StopEndOfBallMode()     'this sub is called after the last ball is drained

End Sub

Sub ResetNewBallVariables() 'starts the variable for a new ball or player

End Sub

Sub ResetNewBallLights()    'Lichter ein- oder ausschalten für einen neuen Ball
    li5.State = 1           'Zentrale Ziele
    li6.State = 1
    li7.State = 1
    li3.State = 0 'Extra Ball
    li4.State = 0
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

' *********************************************************************
'       Table events - ball collision against targets
'
' At each target or object with which the ball collides, it will be necessary to make:
' - sound a physical sound
' - make some movement, if necessary
' - add a score
' - turn on/off a light
' - make a check to see if the player has completed anything
' *********************************************************************

' the ball hits the Slingshots
' - do a manual animation of the slingshots using rubber bands
Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAtBall SoundFXDOF ("fx_slingshot",103,DOFPulse,DOFcontactors)
    DOF 203, DOFPulse
    If Epileptikdof = True Then DOF 204, DOFPulse End If
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    AddScore 10

End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing2.Visible = 0:Lemk.RotX = -20:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAtBall SoundFXDOF ("fx_slingshot",104,DOFPulse,DOFcontactors)
    DOF 205, DOFPulse
    If Epileptikdof = True Then DOF 206, DOFPulse End If
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    AddScore 10

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
'  10 point rubber bands
'***********************

Sub RubberBand4_Hit
    If Tilted Then Exit Sub
    LastSwitchHit = "RubberBand4"
    AddScore 10
End Sub

Sub RubberBand14_Hit
    If Tilted Then Exit Sub
    LastSwitchHit = "RubberBand14"
    AddScore 10
End Sub

Sub RubberBand6_Hit
    If Tilted Then Exit Sub
    LastSwitchHit = "RubberBand6"
    AddScore 10
End Sub

Sub RubberBand7_Hit
    If Tilted Then Exit Sub
    LastSwitchHit = "RubberBand7"
    AddScore 10
End Sub

Sub RubberBand8_Hit
    If Tilted Then Exit Sub
    LastSwitchHit = "RubberBand8"
    AddScore 10
End Sub

Sub RubberBand9_Hit
    If Tilted Then Exit Sub
    LastSwitchHit = "RubberBand9"
    AddScore 10
End Sub

Sub RubberBand10_Hit
    If Tilted Then Exit Sub
    LastSwitchHit = "RubberBand10"
    AddScore 10
End Sub

Sub RubberBand11_Hit
    If Tilted Then Exit Sub
    LastSwitchHit = "RubberBand11"
    AddScore 10
End Sub

'******************************
'     DOF lights ball entrance
'******************************
'
Sub Trigger001_Hit
    DOF 240, DOFPulse
End sub

'*********
' Bumpers
'*********

Sub Bumper1_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",105,DOFPulse,DOFContactors), bumper1
    DOF 207, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    AddScore 10 + 90 * LightBumper1.State
    LastSwitchHit = "bumper1"
End Sub

Sub Bumper2_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",107,DOFPulse,DOFContactors), bumper2
    DOF 208, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    AddScore 10 + 90 * LightBumper2.State
    LastSwitchHit = "bumper2"
End Sub

Sub Bumper3_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",108,DOFPulse,DOFContactors), bumper3
    DOF 209, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    AddScore 10 + 90 * LightBumper3.State
    LastSwitchHit = "bumper3"
End Sub

Sub Bumper4_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",109,DOFPulse,DOFContactors), bumper4
    DOF 210, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    AddScore 10 + 90 * LightBumper4.State
    LastSwitchHit = "bumper4"
End Sub

Sub Bumper5_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",106,DOFPulse,DOFContactors), bumper5
    DOF 211, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    AddScore 100 + 900 * LightBumper5.State
    IncreaseMatch 'is the only thing that increases the value of the lottery.
    LastSwitchHit = "bumper5"
End Sub

'*****************
'     Corridors
'*****************

' top

Sub Trigger5_Hit
    PlaySoundAt "fx_sensor", Trigger5
    If Tilted Then Exit Sub
    DOF 222, DOFPulse
    AddScore 100
    LastSwitchHit = "trigger5"
    li10.State = 0
    'check?
    CheckTopLights
End Sub

Sub Trigger6_Hit
    PlaySoundAt "fx_sensor", Trigger6
    If Tilted Then Exit Sub
    DOF 223, DOFPulse
    AddScore 100
    LastSwitchHit = "trigger6"
    li11.State = 0
    'check?
    CheckTopLights
End Sub

Sub Trigger7_Hit
    PlaySoundAt "fx_sensor", Trigger7
    If Tilted Then Exit Sub
    DOF 224, DOFPulse
    AddScore 100
    LastSwitchHit = "trigger7"
    li12.State = 0
    'check?
    CheckTopLights
End Sub

' top sides

Sub Trigger8_Hit
    PlaySoundAt "fx_sensor", Trigger8
    If Tilted Then Exit Sub
    DOF 225, DOFPulse
    AddScore 500
    LastSwitchHit = "trigger8"
    'check?
    If li8.State Then
        AwardSpecial
        ResetAdvance
		AddScore 2000
    End If
End Sub

Sub Trigger9_Hit
    PlaySoundAt "fx_sensor", Trigger9
    If Tilted Then Exit Sub
    DOF 226, DOFPulse
    AddScore 500
    LastSwitchHit = "trigger9"
    'check?
    If li9.State Then
        AwardSpecial
        ResetAdvance
		AddScore 2000
    End If
End Sub

' outlanes

Sub Trigger1_Hit
    PlaySoundAt "fx_sensor", Trigger1
    If Tilted Then Exit Sub
    DOF 227, DOFPulse
    LastSwitchHit = "trigger1"
    AddScore 1000
    'check?
    If li1.State Then
        AwardSpecial
        StopSpecialTimer
    End If
    If li3.State Then
        AwardExtraBall
    End If
End Sub

Sub Trigger4_Hit
    PlaySoundAt "fx_sensor", Trigger4
    If Tilted Then Exit Sub
    DOF 228, DOFPulse
    AddScore 1000
    LastSwitchHit = "trigger4"
    'check?
    If li2.State Then
        AwardSpecial
        StopSpecialTimer
    End If
    If li4.State Then
        AwardExtraBall
    End If
End Sub

' inlanes

Sub Trigger2_Hit
    PlaySoundAt "fx_sensor", Trigger1
    If Tilted Then Exit Sub
    DOF 229, DOFPulse
    AddScore 500
    LastSwitchHit = "trigger2"
'check?
End Sub

Sub Trigger3_Hit
    PlaySoundAt "fx_sensor", Trigger3
    If Tilted Then Exit Sub
    DOF 231, DOFPulse
    LastSwitchHit = "trigger3"
    AddScore 500
'check?
End Sub

Sub CheckTopLights 'BAR Lichter beleuchten die Bumper und die speziellen Outlanes, und der 30-Sekunden-Timer beginnt.
    Dim x
    IF li10.State + li11.State + li12.State = 0 Then
		
        If NOT SpecialTimerStarted Then
            For each x in bumperlights
                x.State = 1
            Next
            li1.State = 1
            li2.State = 1
            SpecialTimerStep = 9
            SpecialTimerStarted = True
			StopSound "riccione_instrumental"
			PlaySound "sottoilsole"
            SpecialTimer_Timer
            SpecialTimer.Enabled = 1
			If B2sOn then
			Controller.B2SSetData 77, 1
			End If
        End If
    End If
End Sub

Dim SpecialTimerStep
Dim SpecialTimerStarted
SpecialTimerStarted = False
SpecialTimer.Interval = Tiempo * 100

Sub SpecialTimer_Timer
    If SpecialTimerStep = -1 Then
        StopSpecialTimer
    Else
        
        'PlaySound "sottoilsole"
        If B2sOn then
            Controller.B2SSetData 50, 0
            Controller.B2SSetData 51, 0
            Controller.B2SSetData 52, 0
            Controller.B2SSetData 53, 0
            Controller.B2SSetData 54, 0
            Controller.B2SSetData 55, 0
            Controller.B2SSetData 56, 0
            Controller.B2SSetData 57, 0
            Controller.B2SSetData 58, 0
            Controller.B2SSetData 59, 0
            Controller.B2SSetData 50 + SpecialTimerStep, 1
        end if
        Select Case SpecialTimerStep
            Case 9:tl9.State = 1
            Case 8:tl9.State = 0:tl8.State = 1
            Case 7:tl8.State = 0:tl7.State = 1
            Case 6:tl7.State = 0:tl6.State = 1
            Case 5:tl6.State = 0:tl5.State = 1
            Case 4:tl5.State = 0:tl4.State = 1
            Case 3:tl4.State = 0:tl3.State = 1
            Case 2:tl3.State = 0:tl2.State = 1
            Case 1:tl2.State = 0:tl1.State = 1
            Case 0:tl1.State = 0:tl0.State = 1
        End Select
        SpecialTimerStep = SpecialTimerStep - 1
    End If
End Sub

Sub StopSpecialTimer
    StopSound "sottoilsole"
	
	PlaySound "riccione_instrumental",-1
    Dim x
    For each x in bumperlights
        x.State = 0
    Next
    For each x in TimerLights
        x.State = 0
    Next
    If B2SOn then
	Controller.B2SSetData 77, 0
        For x = 50 to 59
            Controller.B2SSetData x, 0
        next
    end if
    li1.State = 0
    li2.State = 0
    li10.State = 1
    li11.State = 1
    li12.State = 1
    SpecialTimer.Enabled = 0
    SpecialTimerStarted = False
End Sub

Sub ResetAdvance
    Dim x
    For each x in AdvanceLights
        x.State = 0
    Next
    Li8.State = 0
    Li9.State = 0
End Sub



'******
' Targets
'******

Sub ch1_hit 'green
    PlaySoundAtBall SoundFXDOF ("fx_target", 111, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 214, DOFPulse
    If Epileptikdof = True Then DOF 215, DOFPulse End If
    LastSwitchHit = "ch1"
    AddScore 100
    If Li21.State = 0 Then li21.State = 1:Exit Sub
    If Li17.State = 0 Then li17.State = 1:Exit Sub
    If Li13.State = 0 Then li13.State = 1
End Sub

Sub ch2_hit 'blue
    PlaySoundAtBall SoundFXDOF ("fx_target", 112, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 216, DOFPulse
    If Epileptikdof = True Then DOF 217, DOFPulse End If
    LastSwitchHit = "ch2"
    AddScore 100
    If Li15.State AND li24.State = 0 Then li24.State = 1:Exit Sub
    If Li24.State AND Li20.State = 0 Then li20.State = 1:Exit Sub
    If Li20.State AND Li16.State = 0 Then
        li16.State = 1
        li8.State = 1
        li9.State = 1
    End If
End Sub

Sub ch3_hit 'yellow
    PlaySoundAtBall SoundFXDOF ("fx_target", 113, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 218, DOFPulse
    If Epileptikdof = True Then DOF 219, DOFPulse End If
    LastSwitchHit = "ch3"
    AddScore 100
    If Li13.State AND li22.State = 0 Then li22.State = 1:Exit Sub
    If Li22.State AND Li18.State = 0 Then li18.State = 1:Exit Sub
    If Li18.State AND Li14.State = 0 Then li14.State = 1
End Sub

Sub ch4_hit 'red
    PlaySoundAtBall SoundFXDOF ("fx_target", 114, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 220, DOFPulse
    If Epileptikdof = True Then DOF 221, DOFPulse End If
    LastSwitchHit = "ch4"
    AddScore 100
    If Li14.State AND li23.State = 0 Then li23.State = 1:Exit Sub
    If Li23.State AND Li19.State = 0 Then li19.State = 1:Exit Sub
    If Li19.State AND Li15.State = 0 Then li15.State = 1
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
    If Not FileObj.FolderExists(UserDirectory)then
        Credits = 0
        Exit Sub
    End If
    If Not FileObj.FileExists(UserDirectory & TableName& ".txt")then
        Credits = 0
        Exit Sub
    End If
    Set ScoreFile = FileObj.GetFile(UserDirectory & TableName& ".txt")
    Set TextStr = ScoreFile.OpenAsTextStream(1, 0)
    If(TextStr.AtEndOfStream = True)then
        Exit Sub
    End If
    temp1 = TextStr.ReadLine
    temp2 = textstr.readline

    HighScore = cdbl(temp1)
    If HighScore < 1 then
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

    If HighScore < 1 then
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
End Sub

Sub Savehs
    ' Based on Black's Highscore routines
    Dim FileObj
    Dim ScoreFile
    Dim xx
    Set FileObj = CreateObject("Scripting.FileSystemObject")
    If Not FileObj.FolderExists(UserDirectory)then
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


' ============================================================================================
' GNMOD - Multiple High Score Display and Collection
' changed ramps by flashers, jpsalas
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
HSScore(1) = 75000
HSScore(2) = 70000
HSScore(3) = 60000
HSScore(4) = 55000
HSScore(5) = 50000

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
        If HsTimerCount > 5 then
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

    For xFor = StartHSArray(LineNo)to EndHSArray(LineNo)
        Eval("HS" &xFor).imageA = GetHSChar(String, Index)
        Index = Index + 1
    Next
End Sub

Sub NewHighScore(NewScore, PlayNum)
    If NewScore > HSScore(5)then
        HighScoreTimer.interval = 500
        HSTimerCount = 1
        AlphaStringPos = 1      ' start with first character "A"
        EnteringInitials = true ' intercept the control keys while entering initials
        InitialString = ""      ' initials entered so far, initialize to empty
        SetHSLine 1, "PLAYER " + FormatNumber(PlayNum, 0)
        SetHSLine 2, "ENTER NAME"
        SetHSLine 3, MID(AlphaString, AlphaStringPos, 1)
        HSNewHigh = NewScore
        AwardSpecial
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
        If AlphaStringPos < 1 then
            AlphaStringPos = len(AlphaString) ' handle wrap from beginning to End
            If InitialString = "" then
                ' Skip the backspace If there are no characters to backspace over
                AlphaStringPos = AlphaStringPos - 1
            End If
        End If
        SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
        PlaySound "fx_Previous"
    ElseIf keycode = RightFlipperKey Then
        ' advance to Next character
        AlphaStringPos = AlphaStringPos + 1
        If AlphaStringPos > len(AlphaString)or(AlphaStringPos = len(AlphaString)and InitialString = "")then
            ' Skip the backspace If there are no characters to backspace over
            AlphaStringPos = 1
        End If
        SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
        PlaySound "fx_Next"
    ElseIf keycode = StartGameKey or keycode = PlungerKey Then
        SelectedChar = MID(AlphaString, AlphaStringPos, 1)
        If SelectedChar = "_" then
            InitialString = InitialString & " "
            PlaySound("fx_Esc")
        ElseIf SelectedChar = "<" then
            InitialString = MID(InitialString, 1, len(InitialString)- 1)
            If len(InitialString) = 0 then
                ' If there are no more characters to back over, don't leave the < displayed
                AlphaStringPos = 1
            End If
            PlaySound("fx_Esc")
        Else
            InitialString = InitialString & SelectedChar
            PlaySound("fx_Enter")
        End If
        If len(InitialString) < 3 then
            SetHSLine 3, InitialString & SelectedChar
        End If
    End If
    If len(InitialString) = 3 then
        ' save the score
        For i = 5 to 1 step -1
            If i = 1 or(HSNewHigh > HSScore(i)and HSNewHigh <= HSScore(i - 1))then
                ' Replace the score at this location
                If i < 5 then
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
            ElseIf i < 5 then
                ' move the score in this slot down by 1, it's been exceeded by the new score
                HSScore(i + 1) = HSScore(i)
                HSName(i + 1) = HSName(i)
            End If
        Next
    End If
End Sub
' End GNMOD