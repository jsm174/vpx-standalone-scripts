' ***********************************************************************
'               VISUAL PINBALL X EM Script by JPSalas
'                  Basic EM script up to 4 players
'		          uses core.vbs for extra functions
'
'  Stardust / IPD No. 2359 / August 05, 1971 / 4 Players / Williams
'  VPX - version by JPSalas 2022, version 4.0.0
' ***********************************************************************

Option Explicit
Randomize
'
' DOF config - leeoneil
'
' Option for more lights effects with DOF (Undercab effects on bumpers and slingshots) 
' "True" to activate (False by default)
Const Epileptikdof = False
'
' Flippers L/R - 101/102
' Slingshot L/R - 103/104
' Bumpers L/R - 105/106/107/108/109
' Targets L/R - 110/111/112/113
' Kickers - 114
'
' LED backboard
' Flasher Outside Left - 203/206/212/215/217/240/250
' Flasher left - 209/212/213/220/240/250
' Flasher center - 207/219220/240/250
' Flasher right - 210/211/214/220/240/250
' Flasher Outside - 205/208/211/216/218/221/240/250
' Flashers MX L/R - 204/206
' Start Button - 200
' Undercab - 201/202
' Strobe - 230
' Knocker - 300
' Chimes - 301/302

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

' Constants
Const TableName = "Stardust" ' file name to save highscores and other variables
Const cGameName = "Stardust" ' B2S name
Const MaxPlayers = 4         ' 1 to 4 can play
Const MaxMultiplier = 3      ' limit bonus multiplier
Const BallsPerGame = 5       ' 3 or 5
Const FreePlay = False       ' Free play or coins
Const Special1 = 42000       ' extra ball or credit
Const Special2 = 63000
Const Special3 = 87000

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
    ' Init som objects, like walls, targets
    VPObjects_Init
    LoadEM

    ' load highscore
    Credits = 1
    Loadhs
    ScoreReel1.SetValue HSScore(1)
    If B2SOn then
        Controller.B2SSetScorePlayer 1, HSScore(1)
    End If
    UpdateCredits

    ' init all the global variables
    bFreePlay = FreePlay
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

    If keycode = LeftMagnaSave Then bLutActive = True: Lutbox.text = "level of darkness " & LUTImage + 1
    If keycode = RightMagnaSave Then
        If bLutActive Then
            NextLUT
        Else
            KickBall ' kick balls from kicker in case they are stuck
        End If
    End If

    ' add coins
    If Keycode = AddCreditKey Then
        If(Tilted = False)Then
            AddCredits 1
            PlaySound "fx_coin"
            DOF 200, DOFOn
        End If
    End If

    ' plunger
    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
    End If

    ' tilt keys
    If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
    If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
    If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt

    ' keys during game

    If bGameInPlay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1

        If keycode = StartGameKey Then
            If((PlayersPlayingGame < MaxPlayers)AND(bOnTheFirstBall = True))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    UpdatePlayers
                'PlaySound "so_fanfare1"
                Else
                    If(Credits > 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        Credits = Credits - 1
                        UpdateCredits
                        UpdateBallInPlay
                        UpdatePlayers
                    Else
                    DOF 200, DOFOff
                    ' Not Enough Credits to start a game.
                    'PlaySound "so_nocredits"
                    End If
                End If
            End If
        End If
        Else

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
                    DOF 200, DOFOff
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
        ' teclas de los flipers
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
GiIntensity = 1   'used by the LUT changing to increase the GI lights when the table is darker

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

Sub LeftFlipper_Collide(parm)
    PlaySoundAtBall "fx_rubber_flipper"
End Sub

Sub RightFlipper_Collide(parm)
    PlaySoundAtBall "fx_rubber_flipper"
End Sub

'***********
' GI lights
'***********

Sub GiOn 'enciende las luces GI
    DOF 201, DOFOn
    Dim bulb
	PlaySound"fx_gion"
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff 'apaga las luces GI
    DOF 201, DOFOff
    Dim bulb
	PlaySound"fx_gioff"
    For each bulb in aGiLights
        bulb.State = 0
    Next
End Sub

'**************
'    TILT
'**************

Sub CheckTilt
    Tilt = Tilt + TiltSensitivity
    TiltDecreaseTimer.Enabled = True
    If Tilt > 15 Then
        Tilted = True
        TiltReel.SetValue 1
        If B2SOn then
            Controller.B2SSetTilt 1
        end if
        DisableTable True
        ' BallsRemaining(CurrentPlayer) = 0 'player looses the game
        TiltRecoveryTimer.Enabled = True 'wait for all the balls to drain
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    If Tilt > 0 Then
        Tilt = Tilt - 0.1
    Else
        TiltDecreaseTimer.Enabled = False
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
        GiOff
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        Bumper001.Threshold = 100
        Bumper002.Threshold = 100
        Bumper003.Threshold = 100
        Bumper004.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
        PostDown
        kickball
        DOF 101, DOFOff
        DOF 102, DOFOff
    Else
        GiOn
        Bumper001.Threshold = 1
        Bumper002.Threshold = 1
        Bumper003.Threshold = 1
        Bumper004.Threshold = 1
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' all the balls have drained
    If(BallsOnPlayfield = 0)Then
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
    End If
' otherwise repeat
End Sub

'***************************************************************
'             Supporting Ball & Sound Functions v4.0
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
' Ball 2 Ball Collision Sound
'*****************************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'***************************************
'     Collection collision sounds
'***************************************

Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

'************************************************************************************************************************
' Only for VPX 10.2 and higher.
' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Return to previous State
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
        BallsRemaining(i) = BallsPerGame
    Next
    BonusMultiplier = 1
    Bonus = 0
    UpdateBallInPlay
    UpdatePLayers
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
    ' reset table for a new ball, rise droptargets ++
    ResetForNewPlayerBall()
    CreateNewBall()
    DOF 250, DOFPulse
End Sub

' (Re-)init table for a new ball or player

Sub ResetForNewPlayerBall()
    'debug.print "ResetForNewPlayerBall"
    UpdatePlayers
    AddScore 0

    ' reset multiplier to 1x

    ' turn on lights, and variables
    bExtraBallWonThisBall = False
    ResetNewBallLights
    ResetNewBallVariables
End Sub

' Crete new ball

Sub CreateNewBall()
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass
    BallsOnPlayfield = BallsOnPlayfield + 1
    UpdateBallInPlay
    PlaySoundAt SoundFXDOF ("fx_Ballrel", 104, DOFPulse, DOFContactors), BallRelease
    If Epileptikdof = True Then DOF 221, DOFPulse End If
    BallRelease.Kick 90, 4
End Sub

' player lost the ball

Sub EndOfBall()
    'debug.print "EndOfBall"
    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 0
    ' Lost the first ball, now it cannot accept more players
    bOnTheFirstBall = False

    'No bonus in this table after the balls drain

    ' add bonus if no tilt
    ' tilt system will take care of the next ball

    'If NOT Tilted AND LightShootAgain.State Then
    'If NOT Tilted Then

    '    BonusCountTimer.Enabled = 1
    'Else
    vpmtimer.addtimer 400, "EndOfBall2 '"
'End If
End Sub

Sub CollectBonus
    Select Case BonusMultiplier
        Case 1:BonusCountTimer.Interval = 250
        Case 2:BonusCountTimer.Interval = 500
        Case 3:BonusCountTimer.Interval = 750
    End Select
    BonusCountTimer.Enabled = 1
End Sub

Sub BonusCountTimer_Timer
    'debug.print "BonusCount_Timer"
    If Bonus > 0 Then
        Bonus = Bonus -1
        AddScore 1000 * BonusMultiplier
        UpdateBonusLights
    Else
        BonusCountTimer.Enabled = 0
        Bonus = 1:UpdateBonusLights
        KickBall
    End If
End Sub

Sub UpdateBonusLights
    Select Case Bonus
        Case 0:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 1:bl1.State = 1:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 2:bl1.State = 0:bl2.State = 1:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 3:bl1.State = 0:bl2.State = 0:bl3.State = 1:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 4:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 1:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 5:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 1:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 6:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 1:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 7:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 1:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 8:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 1:bl9.State = 0:bl10.State = 0
        Case 9:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 1:bl10.State = 0
        Case 10:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 1
    End Select
End Sub

Sub EndOfBall2()
    'debug.print "EndOfBall2"
    Tilted = False
    Tilt = 0
    TiltReel.SetValue 0
    If B2SOn then
        Controller.B2SSetTilt 0
    end if
    DisableTable False

    'debug.print "Extra Ball"
    ' win extra ball?
    If(ExtraBallsAwards(CurrentPlayer) > 0)Then
        ' if so then give it
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1

        ' turn off light if no more extra balls
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
            ShootAgainR.SetValue 0
            light012.State = 0
            If B2SOn then
                Controller.B2SSetShootAgain 0
            end if
        End If

        ' extra ball sound?

        ' reset as in a new ball
        ResetForNewPlayerBall()
        CreateNewBall()
    Else ' no extra ball

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1

        ' last ball?
        If(BallsRemaining(CurrentPlayer) <= 0)Then
            CheckHighScore()
        End If

        ' this is not the last ball, chack for new player
        EndOfBallComplete()
    End If
End Sub

Sub EndOfBallComplete()
    'debug.print "EndOfBallComplete"
    Dim NextPlayer

    ' other players?
    If(PlayersPlayingGame > 1)Then
        NextPlayer = CurrentPlayer + 1
        ' if it is the last player then go to the first one
        If(NextPlayer > PlayersPlayingGame)Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' end of game?
    If((BallsRemaining(CurrentPlayer) <= 0)AND(BallsRemaining(NextPlayer) <= 0))Then

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
    DOF 250, DOFPulse

    ' start the attract mode
    StartAttractMode
End Sub

' Fuction to calculate the balls left
Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp > BallsPerGame Then
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
'     Match
'******************

Sub Verification_Match()
    PlaySound "fx_match"
    Match = INT(RND(1) * 10) * 10 ' random between 10 and 90
    Display_Match
    If(Score(CurrentPlayer)MOD 100) = Match Then
        PlaySound SoundFXDOF ("fx_knocker",300,DOFPulse,DOFKnocker)
        DOF 230, DOFPulse
        DOF 200, DOFOn
        AddCredits 1
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

Sub Drain_Hit()
    Drain.DestroyBall
    BallsOnPlayfield = BallsOnPlayfield - 1
    PlaySoundAt "fx_drain", Drain
    DOF 220, DOFPulse

    'tilted?
    If Tilted Then
        StopEndOfBallMode
    End If

    ' if still playing and not tilted
    If(bGameInPLay = True)AND(Tilted = False)Then

        ' ballsaver?
        If(bBallSaverActive = True)Then
            CreateNewBall()
        Else
            ' last ball?
            If(BallsOnPlayfield = 0)Then
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

'******************************
'     DOF lights ball entrance
'******************************
'
Sub Trigger004_Hit
    DOF 240, DOFPulse
End sub

' ****************************************
'             Score functions
' ****************************************

Sub AddScore(Points)
    If Tilted Then Exit Sub
    If Points = 10 OR Points = 100 Or Points = 1000 Then
        Score(CurrentPlayer) = Score(CurrentPlayer) + points
        UpdateScore points
        ' sounds
        If Points > 99 Then
            PlaySound SoundFXDOF ("fx_bell100",301,DOFPulse,DOFChimes)
        Else
            PlaySound SoundFXDOF ("fx_bell10",302,DOFPulse,DOFChimes)
        End If
    End If

    If Points > 10 And Points < 100 Then
        Add10 = Add10 + Points \ 10
        AddScore10Timer.Enabled = TRUE
    End If
    If Points > 100 AND Points < 1000 Then
        Add100 = Add100 + Points \ 100
        AddScore100Timer.Enabled = TRUE
    End If
    If Points > 1000 Then
        Add1000 = Add1000 + Points \ 1000
        AddScore1000Timer.Enabled = TRUE
    End If

    ' check for higher score and specials
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

'************************************
'       Score sound Timers
'************************************

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

Sub AddBonus(bonuspoints)
    If(Tilted = False)Then
        Bonus = Bonus + bonuspoints
        If Bonus > 10 Then
            Bonus = 10
        End If
        UpdateBonusLights
    End if
End Sub

Sub AddBonusMultiplier(multi)
    If(Tilted = False)Then
        BonusMultiplier = BonusMultiplier + multi
        If BonusMultiplier > MaxMultiplier Then
            BonusMultiplier = MaxMultiplier
        End If
        UpdateMultiplierLights
    End if
End Sub

Sub UpdateMultiplierLights
    Select Case BonusMultiplier
    'Case 1:li2.State = 1:li3.State = 0:li4.State = 0
    'Case 2:li2.State = 0:li3.State = 1:li4.State = 0
    'Case 3:li2.State = 0:li3.State = 0:li4.State = 1
    End Select
End Sub

'**********************************
'        Score EM reels
'**********************************

Sub UpdateScore(playerpoints)
    Select Case CurrentPlayer
        Case 1:ScoreReel1.Addvalue playerpoints
        Case 2:ScoreReel2.Addvalue playerpoints
        Case 3:ScoreReel3.Addvalue playerpoints
        Case 4:ScoreReel4.Addvalue playerpoints
    End Select
    If B2SOn then
        Controller.B2SSetScorePlayer CurrentPlayer, Score(CurrentPlayer)
        If Score(CurrentPlayer) >= 100000 then
            Controller.B2SSetScoreRollover 24 + CurrentPlayer, 1
        end if
    end if
End Sub

Sub ResetScores
    ScoreReel1.setvalue 11111
    ScoreReel2.setvalue 11111
    ScoreReel3.setvalue 11111
    ScoreReel4.setvalue 11111
    ScoreReel1.ResetToZero
    ScoreReel2.ResetToZero
    ScoreReel3.ResetToZero
    ScoreReel4.ResetToZero
    If B2SOn then
        Controller.B2SSetScorePlayer1 0
        Controller.B2SSetScoreRolloverPlayer1 0
        Controller.B2SSetScorePlayer2 0
        Controller.B2SSetScoreRolloverPlayer2 0
        Controller.B2SSetScorePlayer3 0
        Controller.B2SSetScoreRolloverPlayer3 0
        Controller.B2SSetScorePlayer4 0
        Controller.B2SSetScoreRolloverPlayer4 0
    end if
End Sub

Sub AddCredits(value) 'limit to 15 credits
    If Credits < 15 Then
        Credits = Credits + value
        CreditsReel.SetValue Credits
        UpdateCredits
    end if
End Sub

Sub UpdateCredits
    If Credits > 0 Then 'in Bally tables
        CreditLight.State = 1
    Else
        CreditLight.State = 0
    End If
    PlaySound "fx_relay"
    CreditsReel.SetValue credits
    If B2SOn then
        Controller.B2SSetCredits Credits
    end if
End Sub

Sub UpdateBallInPlay
    BallInPlayR.SetValue Balls
    If B2SOn then
        Controller.B2SSetBallInPlay Balls
    end if
End Sub

Sub UpdatePlayers
    Select case CurrentPlayer
        Case 0:pl1.State = 0:pl2.State = 0:pl3.State = 0:pl4.State = 0
        Case 1:pl1.State = 1:pl2.State = 0:pl3.State = 0:pl4.State = 0
        Case 2:pl1.State = 0:pl2.State = 1:pl3.State = 0:pl4.State = 0
        Case 3:pl1.State = 0:pl2.State = 0:pl3.State = 1:pl4.State = 0
        Case 4:pl1.State = 0:pl2.State = 0:pl3.State = 0:pl4.State = 1
    End Select
    If B2SOn then
        B2SPlayerLights(CurrentPlayer)
    end if
    Select Case PlayersPlayingGame
        Case 0:cp1.State = 0:cp2.State = 0:cp3.State = 0:cp4.State = 0
        Case 1:cp1.State = 1:cp2.State = 0:cp3.State = 0:cp4.State = 0
        Case 2:cp1.State = 0:cp2.State = 1:cp3.State = 0:cp4.State = 0
        Case 3:cp1.State = 0:cp2.State = 0:cp3.State = 1:cp4.State = 0
        Case 4:cp1.State = 0:cp2.State = 0:cp3.State = 0:cp4.State = 1
    End Select
    If B2SOn then
        Controller.B2SSetCanPlay PlayersPlayingGame
    end if
End Sub

'*************************
'        Specials
'*************************

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        'PlaySound "fx_knocker"
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
        ShootAgainR.SetValue 1
        light012.State = 1
        If B2SOn then
            Controller.B2SSetShootAgain 1
        end if
    Else
        Addscore 5000
    END If
End Sub

Sub AwardSpecial()
    PlaySound SoundFXDOF ("fx_knocker",300,DOFPulse,DOFKnocker)
    DOF 230, DOFPulse
    DOF 200, DOFOn
    AddCredits 1
End Sub

' ********************************
'        Attract Mode
' ********************************
' use the"Blink Pattern" of each light

Sub B2SPlayerLights(pp)
    Controller.B2SSetData 81, 0
    Controller.B2SSetData 82, 0
    Controller.B2SSetData 83, 0
    Controller.B2SSetData 84, 0
    If pp > 0 Then Controller.B2SSetData 80 + CurrentPlayer, 1
End Sub

Sub StartAttractMode()
    Dim x
    bAttractMode = True
    For each x in aLights
        x.State = 2
    Next
    GameOverR.SetValue 1
    BallInPlayR.SetValue 0
    ShootAgainR.SetValue 0
    If B2SOn then
        Controller.B2SSetGameOver 1
        Controller.B2SSetBallInPlay 0
        B2SPlayerLights(0)
        Controller.B2SSetCanPlay 0
    end if
    If Credits > 0 Then DOF 200, DOFOn End If
    If Credits = 0 Then DOF 200, DOFOff End If
    DOF 201, DOFOff
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

'*********************************
'    Load / Save / Highscore
'*********************************

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
    SortHighscore 'added to fix a previous error
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

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 1 to 5
        For j = 1 to 4
            If HSScore(j) < HSScore(j + 1)Then
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

'*******************
' Realtime updates
'*******************

Sub GameTimer_Timer
    RollingUpdate
    LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle
    RightFlipperTop.RotZ = RightFlipper.CurrentAngle
    gatep.RotZ = gatef.CurrentAngle
    gatep001.RotZ = gatef001.CurrentAngle
    gatep002.RotZ = gatef002.CurrentAngle
End Sub

'***********************************************************************
' *********************************************************************
'  *********     G A M E  C O D E  S T A R T S  H E R E      *********
' *********************************************************************
'***********************************************************************

Sub VPObjects_Init 'init objects
    Post.IsDropped = 1
    TurnOffPlayfieldLights()
End Sub

' Dim all the variables

Sub Game_Init() 'called at the start of a new game
    'Start music?
    'Init variables?
    'Start or init timers
    'Init lights?
    TurnOffPlayfieldLights()
End Sub

Sub StopEndOfBallMode() 'called when the last ball is drained
    PostDown
End Sub

Sub ResetNewBallVariables() 'init variables new ball/player
    Bonus = 1:UpdateBonusLights
    vpmtimer.addtimer 1000, "PlaySoundAt ""fx_solenoidoff"", gatef:gatef.RotateToStart '"
    vpmtimer.addtimer 1000, "PlaySoundAt ""fx_solenoidoff"", gatef002:gatef002.RotateToStart '"
    vpmtimer.addtimer 1000, "PlaySoundAt ""fx_solenoidoff"", gatef001:gatef001.RotateToStart '"
End Sub

Sub ResetNewBallLights() 'init lights for new ball/player
    TurnOffPlayfieldLights()
    LightBumper002.State = 1
    Light026.State = 1
    Light027.State = 1
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

' Slingshots
Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_slingshot",103,DOFPulse,DOFcontactors), Lemk
    DOF 203, DOFPulse
    If Epileptikdof = True Then DOF 204, DOFPulse End If
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add points
    AddScore 10
    ' some effect?
    RandomExtraBall
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
    PlaySoundAt SoundFXDOF ("fx_slingshot",104,DOFPulse,DOFcontactors), Remk
    DOF 205, DOFPulse
    If Epileptikdof = True Then DOF 206, DOFPulse End If
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add points
    AddScore 10
    ' add effect?
    RandomExtraBall
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

Dim Rub1

Sub RubberBand019_Hit
    If Tilted then Exit Sub
    AddScore 10
    Rub1 = 1:RubberBand019_Timer
    RandomExtraBall
End Sub

Sub RubberBand019_Timer
    Select Case Rub1
        Case 1:r1.Visible = 0:r2.Visible = 1:RubberBand019.TimerEnabled = 1
        Case 2:r2.Visible = 0:r3.Visible = 1
        Case 3:r3.Visible = 0:r1.Visible = 1:RubberBand019.TimerEnabled = 0
    End Select
    Rub1 = Rub1 + 1
End Sub

Dim Rub2

Sub RubberBand009_Hit
    If Tilted then Exit Sub
    AddScore 10
    Rub2 = 1:RubberBand009_Timer
    RandomExtraBall
End Sub

Sub RubberBand009_Timer
    Select Case Rub2
        Case 1:r4.Visible = 0:r5.Visible = 1:RubberBand009.TimerEnabled = 1
        Case 2:r5.Visible = 0:r6.Visible = 1
        Case 3:r6.Visible = 0:r4.Visible = 1:RubberBand009.TimerEnabled = 0
    End Select
    Rub2 = Rub2 + 1
End Sub

Dim Rub3

Sub RubberBand013_Hit
    If Tilted then Exit Sub
    AddScore 10
    Rub3 = 1:RubberBand013_Timer
    RandomExtraBall
End Sub

Sub RubberBand013_Timer
    Select Case Rub3
        Case 1:r7.Visible = 0:r8.Visible = 1:RubberBand013.TimerEnabled = 1
        Case 2:r8.Visible = 0:r9.Visible = 1
        Case 3:r9.Visible = 0:r7.Visible = 1:RubberBand013.TimerEnabled = 0
    End Select
    Rub3 = Rub3 + 1
End Sub

Dim Rub4

Sub RubberBand011_Hit
    If Tilted then Exit Sub
    AddScore 10
    Rub4 = 1:RubberBand011_Timer
    RandomExtraBall
End Sub

Sub RubberBand011_Timer
    Select Case Rub4
        Case 1:r10.Visible = 0:r11.Visible = 1:RubberBand011.TimerEnabled = 1
        Case 2:r11.Visible = 0:r12.Visible = 1
        Case 3:r12.Visible = 0:r10.Visible = 1:RubberBand011.TimerEnabled = 0
    End Select
    Rub4 = Rub4 + 1
End Sub

Dim Rub5

Sub RubberBand014_Hit
    If Tilted then Exit Sub
    AddScore 100
    AddBonus 1
    AdvanceArrow
    Rub5 = 1:RubberBand014_Timer
End Sub

Sub RubberBand014_Timer
    Select Case Rub5
        Case 1:r13.Visible = 0:r14.Visible = 1:RubberBand014.TimerEnabled = 1
        Case 2:r14.Visible = 0:r15.Visible = 1
        Case 3:r15.Visible = 0:r13.Visible = 1:RubberBand014.TimerEnabled = 0
    End Select
    Rub5 = Rub5 + 1
End Sub

Dim Rub6

Sub RubberBand016_Hit
    If Tilted then Exit Sub
    AddScore 100
    AddBonus 1
    AdvanceArrow
    Rub6 = 1:RubberBand016_Timer
End Sub

Sub RubberBand016_Timer
    Select Case Rub6
        Case 1:r16.Visible = 0:r17.Visible = 1:RubberBand016.TimerEnabled = 1
        Case 2:r17.Visible = 0:r18.Visible = 1
        Case 3:r18.Visible = 0:r16.Visible = 1:RubberBand016.TimerEnabled = 0
    End Select
    Rub6 = Rub6 + 1
End Sub

Dim Rub7

Sub RubberBand012_Hit
    If Tilted then Exit Sub
    AddScore 10
    Rub7 = 1:RubberBand012_Timer
End Sub

Sub RubberBand012_Timer
    Select Case Rub7
        Case 1:r19.Visible = 0:r20.Visible = 1:RubberBand012.TimerEnabled = 1
        Case 2:r20.Visible = 0:r21.Visible = 1
        Case 3:r21.Visible = 0:r19.Visible = 1:RubberBand012.TimerEnabled = 0
    End Select
    Rub7 = Rub7 + 1
End Sub

Dim Rub8

Sub RubberBand007_Hit
    If Tilted then Exit Sub
    AddScore 10
    Rub8 = 1:RubberBand007_Timer
End Sub

Sub RubberBand007_Timer
    Select Case Rub8
        Case 1:r22.Visible = 0:r23.Visible = 1:RubberBand007.TimerEnabled = 1
        Case 2:r23.Visible = 0:r24.Visible = 1
        Case 3:r24.Visible = 0:r22.Visible = 1:RubberBand007.TimerEnabled = 0
    End Select
    Rub8 = Rub8 + 1
End Sub

Sub RubberBand005_Hit
    If Tilted then Exit Sub
    AddBonus 1
    AdvanceArrow
    Addscore 100
End Sub

Sub RubberBand015_Hit
    If Tilted then Exit Sub
    AddBonus 1
    AdvanceArrow
    Addscore 100
End Sub

'*********
' Bumpers
'*********

Sub Bumper001_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",105,DOFPulse,DOFContactors), bumper001
    DOF 206, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    AddScore 10 + 90 * LightBumper001.State
    RandomExtraBall
End Sub

Sub Bumper002_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",106,DOFPulse,DOFContactors), bumper002
    DOF 207, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    AddScore 100
    RandomExtraBall
End Sub

Sub Bumper003_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",107,DOFPulse,DOFContactors), bumper003
    DOF 208, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    AddScore 10 + 90 * LightBumper003.State
    RandomExtraBall
End Sub

Sub Bumper004_Hit 'always on
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",108,DOFPulse,DOFContactors), bumper004
    DOF 209, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    AddScore 10 + 90 * LightBumper004.State
    RandomExtraBall
End Sub

Sub Bumper005_Hit 'always on
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",109,DOFPulse,DOFContactors), bumper005
    DOF 210, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    AddScore 10 + 90 * LightBumper005.State
    RandomExtraBall
End Sub

'*****************
'     Lanes
'*****************

' outlanes

Sub Trigger001_Hit
    PlaySoundAt "fx_sensor", Trigger001
    If Tilted Then Exit Sub
    DOF 215, DOFPulse
    AddScore 1000
End Sub

Sub Trigger002_Hit
    PlaySoundAt "fx_sensor", Trigger002
    If Tilted Then Exit Sub
    DOF 216, DOFPulse
    AddScore 1000 + 4000 * light013.State
    light013.State = 0
    vpmtimer.addtimer 1000, "gatef.RotateToStart '"
End Sub

' center lanes

Sub Trigger003_Hit
    PlaySoundAt "fx_sensor", Trigger003
    If Tilted Then Exit Sub
    DOF 217, DOFPulse
    AddScore 1000 + 4000 * light011.State
    light011.State = 0
    vpmtimer.addtimer 1000, "gatef002.RotateToStart '"
End Sub

Sub Trigger008_Hit
    PlaySoundAt "fx_sensor", Trigger008
    If Tilted Then Exit Sub
    DOF 218, DOFPulse
    AddScore 1000 + 4000 * light014.State
    light014.State = 0
    vpmtimer.addtimer 1000, "gatef001.RotateToStart '"
End Sub

'************************
'       Targets
'************************

'top

Sub Target001_hit
    PlaySoundAtBall SoundFXDOF ("fx_target", 110, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 211, DOFPulse
    AddScore 100
    LightBumper003.State = 1
    LightBumper004.State = 1
End Sub

Sub Target003_hit
    PlaySoundAtBall SoundFXDOF ("fx_target", 111, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 212, DOFPulse
    AddScore 100
    LightBumper001.State = 1
    LightBumper005.State = 1
End Sub

' center

Sub Target002_hit
    PlaySoundAtBall SoundFXDOF ("fx_target", 112, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 213, DOFPulse
    AddScore 100
    AddBonus 1
    AdvanceArrow
    PostUp
End Sub

Sub Target004_hit
    PlaySoundAtBall SoundFXDOF ("fx_target", 113, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 214, DOFPulse
    AddScore 100
    AddBonus 1
    AdvanceArrow
    PostUp
End Sub

'*******************
' Rollover - bonus
'*******************

Sub sw001_Hit
    PlaySoundAt "fx_sensor", sw001
    PostDown
    If Tilted Then Exit Sub
    AddScore 10
End Sub

Sub sw002_Hit
    PlaySoundAt "fx_sensor", sw002
    PostDown
    If Tilted Then Exit Sub
    AddScore 10
End Sub

'*****************
'    kickers
'*****************
Sub kicker1_Hit
    PlaySoundAt "fx_kicker_enter", kicker1
    If Tilted Then kicker1.kick 190, 20: Exit Sub
    SpotCard
End Sub

Sub kicker2_Hit
    PlaySoundAt "fx_kicker_enter", kicker2
    If Tilted Then kicker2.kick 190, 20: Exit Sub
    SpotCard
End Sub

Sub KickBall
    vpmtimer.addtimer 500, "DOF 219, DOFPulse:DOF 230, DOFPulse:DOF 114, DOFPulse:PlaySoundAt SoundFX(""fx_kicker"",DOFContactors), kicker2: kicker1.kick 190, 20: kicker2.kick 190, 20 '"
 End Sub

'******************************
'      Extra routines
'******************************

Dim ArrowPos, ArrowNewPos

ArrowPos = 0
ArrowNewPos = 0

Sub AdvanceArrow
    PlaySoundAt "fx_diverter", Arrow
    ArrowNewPos = ArrowNewPos + 36
    MoveArrow.Enabled = 1
End Sub

Sub RandomExtraBall
    Dim tmp
    tmp = INT(RND(1) * 6)
    light015.State = 0
    light016.State = 0
    light017.State = 0
    Select Case tmp
        Case 0:light015.State = 1
        Case 1:light016.State = 1
        Case 2:light017.State = 1
    End Select
End Sub

Sub MoveArrow_Timer
    'debug.print Arrowpos
    ArrowPos = ArrowPos + 6
    If ArrowPos <= ArrowNewPos Then
        Arrow.Rotz = ArrowPos
    Else
        ArrowNewPos = ArrowNewPos MOD 360
        ArrowPos = ArrowNewPos
        Me.Enabled = 0
    End If
End Sub

Sub PostUp
    Post.IsDropped = 0
    postlight2.State = 1
    postlight.State = 1
    PlaySoundAt "fx_solenoidon2", postlight2
End Sub

Sub PostDown
    Post.IsDropped = 1
    postlight2.State = 0
    postlight.State = 0
    PlaySoundAt "fx_solenoidoff2", postlight2
End Sub

Sub SpotCard
    Select Case ArrowNewPos \ 36
        Case 0: 'Double Bonus
            BonusMultiplier = 2
            CollectBonus
        Case 1: 'Extra ball when Lit
            If Light017.State Then
                AwardExtraBall
            End If
            KickBall
        Case 2: ' Opens right gate
            light014.State = 1
            gatef001.RotateToEnd
            PlaySoundAt "fx_solenoidon", gatef001
            KickBall
        Case 3: 'Bonus
            BonusMultiplier = 1
            CollectBonus
        Case 4: 'opens bottom gate
            light013.State = 1
            gatef.RotateToEnd
            PlaySoundAt "fx_solenoidon", gatef
            KickBall
        Case 5: 'Tripple Bonus
            BonusMultiplier = 3
            CollectBonus
        Case 6: 'Extra ball when Lit
            If Light015.State Then
                AwardExtraBall
            End If
            KickBall
        Case 7: 'Bonus
            BonusMultiplier = 1
            CollectBonus
        Case 8: ' Opens right gate
            light011.State = 1
            gatef002.RotateToEnd
            PlaySoundAt "fx_solenoidon", gatef002
            KickBall
        Case 9: 'Extra ball when Lit
            If Light016.State Then
                AwardExtraBall
            End If
            KickBall
    End Select
End Sub

' ***************************************************
' GNMOD - Multiple High Score Display and Collection
' jpsalas: changed ramps by flashers
' ***************************************************

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
HSScore(1) = 25000
HSScore(2) = 24000
HSScore(3) = 23000
HSScore(4) = 22000
HSScore(5) = 21000

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