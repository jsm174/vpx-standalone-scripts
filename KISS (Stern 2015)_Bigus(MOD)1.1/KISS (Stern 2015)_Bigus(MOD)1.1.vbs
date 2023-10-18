' ****************************************************************
'                       VISUAL PINBALL X
' Author: Allknowing2012 2105/2016
'
' Credits to 
'   JPSalas - refactored my code/table with his Pokemon table
'   VANLION on vpinball.com for his Gene Head 3D model
'   UltraPeepi for Ultradmd
'   Dark for the Speaker Model
'   Aldiode for the PF artwork
' ****************************************************************
' 20161025 - bonus animation, dmd background border, no credit audio, bsp vid
' 20161030 - apron mod (RustyCardores) and code version check added
'          - reset lockballs after gameover and store for each player
'          - code for super bumpers, super targets, combo shot on super ramps
' 20161031 - track shots made by player by song as some songs require more/less shots to complete
'          - save demon lock state
' 20161101 - dont stop/reset music on a ball save
' 20161102 - New Match Gifs - be sure to download them and put in ultradmd directory.
' 20161113 - New UDMD Location code from Seraph74, strengthen demon kicker, ballsaver on for start of multiball, random first song 
' 20161119 - frontrow ball save, turn off super ramps between games, extend pause for DMB
' 20161120 - updated the ultradmd code from Seraph74
' 20161122 - pf/light update from Aldiode
' 20161122 - Bugs: Reset BumperLights, Front Row Save loops endlessly + KISSRules.vbs
' 20220301 - First release POST Vpinball.org

Option Explicit
Randomize

If Table1.ShowDT then
  PaulStanleyDesktop.visible=True 
  PaulStanleyFS.visible=False
Light001.intensity = 30
Else
  PaulStanleyDesktop.visible=False
  PaulStanleyFS.visible=True
End if

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0


'ExecuteGlobal GetTextFile("KISSCore.vbs")
' *************************************
'    KISS Core Functions 
'
Const KissCoreV = 1.02
' *************************************
' 20161031 - changes to reduce UltraDMD DMD flicker
' 20161101 - audio additions
Dim DesktopMode:DesktopMode = Table1.ShowDT

Dim MusicVol:MusicVol = 0.2      'Music Volume 0-1
Dim MusicVolMB:MusicVolMB = 0.7  'Multiball Music Volume 0-1

'*************
' Pause Table
'*************
Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub table1_Exit
    Savehs
'    If B2SOn Then Controller.stop
    if UseUDMD then 
      UltraDMD.clear
      UltraDMD.uninit
    end if
End Sub

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFXDOF("fx_flipperup",101,DOFOn,DOFFlippers), 0, 1, -0.05, 0.15
        LeftFlipper.RotateToEnd
    Else
        PlaySound SoundFXDOF("fx_flipperdown",101,DOFOff,DOFFlippers), 0, 1, -0.05, 0.15
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFXDOF("fx_flipperup",102,DOFOn,DOFFlippers), 0, 1, 0.05, 0.15
        RightFlipper.RotateToEnd
    Else
        PlaySound SoundFXDOF("fx_flipperdown",102,DOFOff,DOFFlippers), 0, 1, 0.05, 0.15
        RightFlipper.RotateToStart
    End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, -0.05, 0.25
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, 0.05, 0.25
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
        If UBound(tmp) = -1 Then
            GiOff
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    Dim bulb
    DOF 150, DOFOn
    For each bulb in aGiLights
        bulb.State = 1
    Next
Table1.colorgradeimage = "ColorGradeLUT256x16_extraConSat"
End Sub

Sub GiOff
    Dim bulb
    DOF 150, DOFOff
    For each bulb in aGiLights
        bulb.State = 0
    Next
Table1.colorgradeimage = "ColorGrade_3"
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

'*********************************************************************
'                 Positional Sound Playback Functions
'*********************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
	PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
Sub PlaySoundAt(soundname, tableobj)
    PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub


'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / table1.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / table1.width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 8 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_Timer()
    Dim BOT, b
    BOT = GetBalls

	' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

	' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

    For b = 0 to UBound(BOT)
      If BallVel(BOT(b) ) > 1 Then
        rolling(b) = True
        if BOT(b).z < 30 Then ' Ball on playfield
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.5, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
        End If
      Else
        If rolling(b) = True Then
          StopSound("fx_ballrolling" & b)
          rolling(b) = False
        End If
      End If
 ' play ball drop sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

 '**********************
'Flipper Shadows
'***********************
Sub RealTime_Timer
'  lfs.RotZ = LeftFlipper.CurrentAngle
'  rfs.RotZ = RightFlipper.CurrentAngle
BallShadowUpdate
End Sub


Sub BallShadowUpdate()
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6,BallShadow7,BallShadow8)
    Dim BOT, b
    BOT = GetBalls
    ' hide shadow of deleted balls
    If UBound(BOT)<(tnob-1) Then
        For b = (UBound(BOT) + 1) to (tnob-1)
            BallShadow(b).visible = 0
        Next
    End If
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
    ' render the shadow for each ball
    For b = 0 to UBound(BOT)
		BallShadow(b).X = BOT(b).X
		ballShadow(b).Y = BOT(b).Y + 10                       
        If BOT(b).Z > 20 and BOT(b).Z < 200 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
if BOT(b).z > 30 Then 
ballShadow(b).height = BOT(b).Z - 20
ballShadow(b).opacity = 80
Else
ballShadow(b).height = BOT(b).Z - 24
ballShadow(b).opacity = 90
End If
    Next	
End Sub

'******************************
' Diverse Collection Hit Sounds
'******************************

Sub aMetals_Hit(idx):PlaySound "fx_MetalHit", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Bands_Hit(idx):PlaySound "fx_rubber", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Posts_Hit(idx):PlaySound "fx_postrubber", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Pins_Hit(idx):PlaySound "fx_postrubber", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aYellowPins_Hit(idx):PlaySound "fx_postrubber", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aPlastics_Hit(idx):PlaySound "fx_PlasticHit", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aGates_Hit(idx):PlaySound "fx_Gate", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aWoods_Hit(idx):PlaySound "fx_Woodhit", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
debug.print "Loadhs"
    x = LoadValue(cGameName, "HighScore1")
    If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 100000 End If
    x = LoadValue(cGameName, "HighScore1Name")
    If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(cGameName, "HighScore2")
    If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 100000 End If
    x = LoadValue(cGameName, "HighScore2Name")
    If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(cGameName, "HighScore3")
    If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 100000 End If
    x = LoadValue(cGameName, "HighScore3Name")
    If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(cGameName, "HighScore4")
    If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 100000 End If
    x = LoadValue(cGameName, "HighScore4Name")
    If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(cGameName, "Credits")
    If(x <> "") then Credits = CInt(x) Else Credits = 0 End If
    x = LoadValue(cGameName, "TotalGamesPlayed")
    If(x <> "") then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 End If

    x = LoadValue(cGameName, "HighCombo")
    If(x <> "") then HighCombo = CInt(x) Else HighCombo = 5 End If
    x = LoadValue(cGameName, "HighComboName")
    If(x <> "") then HighComboName = x Else HighComboName = "AAA" End If

    x = LoadValue(cGameName, "LastScoreP1")
    If(x <> "") then LastScoreP1 = CDbl(x) Else LastScoreP1 = 0 End If
    x = LoadValue(cGameName, "LastScoreP2")
    If(x <> "") then LastScoreP2 = CDbl(x) Else LastScoreP2 = 0 End If
    x = LoadValue(cGameName, "LastScoreP3")
    If(x <> "") then LastScoreP3 = CDbl(x) Else LastScoreP3 = 0 End If
    x = LoadValue(cGameName, "LastScoreP4")
    If(x <> "") then LastScoreP4 = CDbl(x) Else LastScoreP4 = 0 End If

'HighScore(0)=100
'HighScore(1)=100
'HighScore(2)=100
'HighScore(3)=100
'HighCombo=3
End Sub

Sub Savehs
debug.print "Savehs"
    SaveValue cGameName, "HighScore1", HighScore(0)
    SaveValue cGameName, "HighScore1Name", HighScoreName(0)
    SaveValue cGameName, "HighScore2", HighScore(1)
    SaveValue cGameName, "HighScore2Name", HighScoreName(1)
    SaveValue cGameName, "HighScore3", HighScore(2)
    SaveValue cGameName, "HighScore3Name", HighScoreName(2)
    SaveValue cGameName, "HighScore4", HighScore(3)
    SaveValue cGameName, "HighScore4Name", HighScoreName(3)
    SaveValue cGameName, "Credits", Credits
    SaveValue cGameName, "TotalGamesPlayed", TotalGamesPlayed
    SaveValue cGameName, "HighCombo", HighCombo
    SaveValue cGameName, "HighComboName", HighComboName

    SaveValue cGameName, "LastScoreP1", LastScoreP1
    SaveValue cGameName, "LastScoreP2", LastScoreP2
    SaveValue cGameName, "LastScoreP3", LastScoreP3
    SaveValue cGameName, "LastScoreP4", LastScoreP4
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

    tmp = Score(1):hsPlayer=1
    If Score(2) > tmp Then tmp = Score(2):hsPlayer=2
    If Score(3) > tmp Then tmp = Score(3):hsPlayer=3
    If Score(4) > tmp Then tmp = Score(4):hsPlayer=4


    If tmp > HighScore(1) Then 'add 1 credit for beating the highscore
        Credits = Credits + 1
        DOF 125, DOFOn
        PlaySound "audio581"
    else
      if tmp > HighScore(3) then
         PlaySound "audio582"
      end if      
    End If



    If tmp > HighScore(3) Then
        PlaySound SoundFXDOF("fx_Knocker",122,DOFPulse,DOFKnocker)
	DOF 121, DOFPulse
        HighScore(3) = tmp
        'enter player's name
        HSMode=1:HighScoreEntryInit()
    Else
        CheckHighComboCnt()
    End If
End Sub

Dim hsPlayer

Sub CheckHighComboCnt()
    Dim tmp
 
    tmp = ComboCnt(1):hsPlayer=1
    If ComboCnt(2) > tmp Then tmp = ComboCnt(2):hsPlayer=2
    If ComboCnt(3) > tmp Then tmp = ComboCnt(3):hsPlayer=3
    If ComboCnt(4) > tmp Then tmp = ComboCnt(4):hsPlayer=4

    If tmp > HighCombo Then 'add 1 credit for beating the high combo count
        Credits = Credits + 1
        DOF 125, DOFOn
    End If

    If tmp > HighCombo Then
        PlaySound SoundFXDOF("fx_Knocker",122,DOFPulse,DOFKnocker)
	DOF 121, DOFPulse
        HighCombo = tmp
        'enter player's name
        HSMode=2:HighScoreEntryInit()
    Else
        EndOfBallComplete()
    End If
End Sub

Sub HighScoreEntryInit()
    hsbModeActive = True
    hsLetterFlash = 0

    hsEnteredDigits(0) = " "
    hsEnteredDigits(1) = " "
    hsEnteredDigits(2) = " "
    hsCurrentDigit = 0

    hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ<>+=0123456789" ' ` is back arrow
    hsCurrentLetter = 1
    UDMDTimer.enabled=False
    DMDFlush()
    HighScoreDisplayNameNow()

    if UseUDMD Then UltraDMD.Clear
    HighScoreFlashTimer.Interval = 200
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
        if(hsCurrentLetter > len(hsValidLetters) ) then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = PlungerKey or keycode=StartGameKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "`") then
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
            if(hsCurrentDigit > 0) then
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

    debug.print "HighScoreDisplayName"
    if hsMode = 1 then
      TempTopStr = "P#" & hsPlayer & " Great Score!"
    else
      TempTopStr = "P#" & hsPlayer & " Great Combo!"
    end if
    TempBotStr = "> "
    if(hsCurrentDigit > 0) then TempBotStr = TempBotStr & hsEnteredDigits(0)
    if(hsCurrentDigit > 1) then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit > 2) then TempBotStr = TempBotStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3) then
        if(hsLetterFlash <> 0) then
            TempBotStr = TempBotStr & "-"
        else
            TempBotStr = TempBotStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit < 1) then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit < 2) then TempBotStr = TempBotStr & hsEnteredDigits(2)

    TempBotStr = TempBotStr & " <"
    debug.print "HighScoreDisplayName :" & TempTopStr & ":" & TempBotStr & ":"
    if UseUDMD then
       if CurScene="HS" then
         UltraDMD.ModifyScene00 "HS", TempTopStr, TempBotStr
         debug.print "modify call to hs"
       else
         CurScene="HS"
         UltraDMD.DisplayScene00ExWithID "HS", FALSE, "scene09.gif", TempTopStr, 15, 2, TempBotStr, 15, 2, UltraDMD_Animation_None, 50000, UltraDMD_Animation_None
         debug.print "new call to hs"
       end if
     end if
End Sub

Sub HighScoreFlashTimer_Timer()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = hsLetterFlash + 1
    if(hsLetterFlash = 2) then hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreCommitName()
    debug.print "HighScoreCommitName"
    CurScene=""
    HighScoreFlashTimer.Enabled = False
    hsbModeActive = False
    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ") then
        hsEnteredName = "YOU"
    end if
    if UseDMD then UltraDMD.CancelRenderingWithID("HS")

    if HSMode=1 Then
       HighScoreName(3) = hsEnteredName
       SortHighscore
       CheckHighComboCnt()
    else
       HighComboName=hsEnteredName
       EndOfBallComplete()
    end if
End Sub

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 0 to 3
        For j = 0 to 2
            If HighScore(j) < HighScore(j + 1) Then
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

Sub DMDFlush()
    If UseUDMD Then debug.print "DMDFlush":UltraDMD.CancelRendering
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
            FinalState = MyLight.State 				'Keep the current light state
        End If
        MyLight.BlinkInterval = BlinkPeriod
        MyLight.Duration 2, TotalPeriod, FinalState
    ElseIf TypeName(MyLight) = "Flasher" Then
		
        Dim steps
			Dim flasherNumber
			flasherNumber = Split(MyLight.Name,"r")
			DOF CInt(flasherNumber(1))+200, DOFPulse
		
		' Store all blink information
        steps = Int(TotalPeriod / BlinkPeriod + .5) 'Number of ON/OFF steps to perform
        If FinalState = 2 Then						'Keep the current flasher state
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

'******************************************
' Change light color - simulate color leds
' changes the light color and state
' colors: red, orange, yellow, green, blue, white
'******************************************
DIM RGBColors(5)
RGBColors(1)="red"
RGBColors(2)="orange"
RGBColors(3)="yellow"
RGBColors(4)="green"
RGBColors(5)="blue"

Sub SetLightColor(n, col, stat)
    Select Case col
        Case "red"
            n.color = RGB(18, 0, 0)
            n.colorfull = RGB(255, 0, 0)
        Case "orange"
            n.color = RGB(18, 3, 0)
            n.colorfull = RGB(255, 64, 0)
        Case "yellow"
            n.color = RGB(18, 18, 0)
            n.colorfull = RGB(255, 255, 0)
        Case "green"
            n.color = RGB(0, 18, 0)
            n.colorfull = RGB(0, 255, 0)
        Case "blue"
            n.color = RGB(0, 18, 18)
            n.colorfull = RGB(0, 255, 255)
        Case "white"
            n.color = RGB(255, 234, 215)
            n.colorfull = RGB(255, 255, 255)
        Case "purple"
            n.color = RGB(174, 35, 250)
            n.colorfull = RGB(255, 255, 255)
    End Select
    If stat <> -1 Then
        n.State = 0
        n.State = stat
    End If
End Sub

Sub StartAttractMode(dummy)
    StartLightSeq
    DMDFlush:debug.print "Flush 12"
    am=0
    AttractMode.Interval=1500
    AttractMode.enabled=TRUE
End Sub

Sub StopAttractMode
    Dim bulb
    AttractMode.enabled=False
    DMDFlush
    If UseUDMD then UltraDMD.CancelRendering
    If useUDMD then UltraDMD.clear
    LightSeqAttract.StopPlay
'StopSong
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


'ExecuteGlobal GetTextFile("KISSMusic.vbs")
' *************************************
'    KISS Music V1.02
'
Const KissMusicV=1.02
' *************************************
Dim Track(10)

Track(1) =  "bgout_song-0x1E"  
Track(2) =  "bgout_song-0x20"  
Track(3) =  "bgout_song-0x1A"  
Track(4) =  "bgout_song-0x22"  
Track(5) =  "bgout_song-0x24"  
Track(6) =  "bgout_song-0x28"  
Track(7) =  "bgout_song-0x2A"  
Track(8) =  "bgout_song-0x2D"   

Track(9) =  "bgout_song-0x1C"  
Track(10)=  "bgout_song-0x26" 

Dim Title(10)
Title(1)  = "Detroit Rock City"
Title(2)  = "Deuce"
Title(3)  = "Black Diamond"
Title(4)  = "Hotter Than Hell"
Title(5)  = "Lick It Up"
Title(6)  = "Love It Loud"
Title(7)  = "Rock + Roll"
Title(8) = "Shout It Out"

Title(9)  = "Calling Dr. Love"
Title(10) = "Love Gun"

Dim City(16)
City(1) = "Detroit" ' 0x288
City(2) = "Chicago"
City(3) = "Pittsburgh"
City(4) = "Seattle"
City(5) = "Portland"
City(6) = "Los Angeles"
City(7) = "Houston"
City(8) = "New Orleans"
City(9) = "Atlanta"
City(10) = "Orlando"
City(11) = "Tokyo"
City(12) = "London"
City(13) = "New York"
City(14) = "San Francisco"
City(15) = "Mexico City"

Dim slen(100) ' Song Length
slen(01)=1120
slen(02)=4750
slen(03)=680
slen(04)=1360
slen(05)=1120
slen(06)=8575  ' Match Vid
slen(07)=9480
slen(08)=1360 'missing 
slen(09)=76960  ' high score entry
slen(10)=560
slen(11)=1160
slen(12)=10880
slen(13)=4300
slen(14)=3080
slen(15)=4280
slen(16)=1400  ' missing
slen(17)=2950   'missing
slen(18)=440
slen(19)=920
slen(20)=1400
slen(21)=3780
slen(22)=6360
slen(23)=3000
slen(24)=1560
slen(25)=1600
slen(26)=2760
slen(27)=920
slen(28)=520
slen(29)=1640
slen(30)=2360
slen(31)=2800
slen(32)=1520
slen(33)=1560
slen(34)=3700
slen(35)=1320
slen(36)=18600
slen(37)=900
slen(38)=500
slen(39)=920
slen(40)=2460
slen(41)=2480
slen(42)=2600
slen(43)=2500
slen(44)=470
slen(45)=4280
slen(46)=2360
slen(47)=3480
slen(48)=3480
slen(49)=3480
slen(50)=3480
slen(51)=1000 ' 10120
slen(52)=130
slen(53)=5160
slen(54)=240
slen(55)=3360
slen(56)=3320
slen(57)=240
slen(58)=2080
slen(59)=240
slen(60)=9880
slen(61)=1400
slen(62)=2800
slen(63)=4000
slen(64)=6600
slen(65)=1040
slen(66)=1960
slen(67)=7320
slen(68)=4800
slen(69)=2320
slen(70)=2840
slen(71)=4600
slen(72)=1240
slen(73)=1520
slen(74)=3160
slen(75)=00
slen(76)=00
slen(77)=00
slen(78)=00
slen(79)=00
slen(80)=700
slen(81)=680
slen(82)=600
slen(83)=13850
slen(84)=00
slen(85)=00
slen(86)=00
slen(87)=00
slen(88)=00
slen(89)=00
slen(100)=4490



'ExecuteGlobal GetTextFile("KISSRules.vbs")
' *************************************
'    KISS Game Rules
'
Const KissRulesV=1.05
' *************************************
' 20161030 - Super targets,bumpers
' 20161031 - save demon lock state between players
' 20161113 - strengthen demon kicker and enable ball save during multiball
' 20161119 - ball audio on right ramp
' 20161122 - front row bug 

Dim cRGB,xx
cRGB=1 ' Starting Colour

Sub InitMode(tr)
  debug.print "InitMode " & tr
  for each xx in ShotsColl
    xx.state=LightStateOff
  Next
  i35.state=LightStateOff ' KISS Targets
  i36.state=LightStateOff
  i37.state=LightStateOff
  i38.state=LightStateOff
  ModeInProgress=True
  Select Case tr
    case 1: ' two shots  lo-58, sc-68
      SetLightColor i58, RGBColors(cRGB), 2
      SetLightColor i68, RGBColors(cRGB), 2
    case 2: ' lo, sc
      SetLightColor i58, RGBColors(cRGB), 2
      SetLightColor i68, RGBColors(cRGB), 2
    case 3: ' ro
      SetLightColor i98, RGBColors(cRGB), 2
    case 4: ' kiss targets
      debug.print "KISS Targets Flash here..."
      SetLightColor i35, "white", 2
      SetLightColor i36, "white", 2
      SetLightColor i37, "white", 2
      SetLightColor i38, "white", 2
    case 5: ' sc, mr
      for each xx in ShotsColl
        SetLightColor xx, RGBColors(cRGB), 2
      Next
    case 6: ' lo, ro
      SetLightColor i58, RGBColors(cRGB), 2
      SetLightColor i98, RGBColors(cRGB), 2
    case 7: ' mid ramp
      SetLightColor i73, RGBColors(cRGB), 2
    case 8: ' mid ramp
      SetLightColor i73, RGBColors(cRGB), 2
    End Select
   cRGB=cRGB+1
   if cRGB>5 then cRGB=1
End Sub

'   ******************************************************
'   Rollover Switches

Sub sw1_hit  ' Leftmost Inlane
  DOF 133, DOFPulse
  PlaySound "fx_sensor", 0, 1, AudioPan(ActiveBall)
  if Tilted Then Exit Sub
  If i7.state <> LightStateOff then 'Light Bumper Shot
    SetLightColor i7, "white", 0
   ' light one of the "Light bumper" lights   i101,102,103,104, pf 17,18,19,20
   ' if any pf & upper lights are off then choose it
    if i17.state=LightStateOff or i18.state=lightstateoff or i19.state=lightstateoff or i20.state=lightstateoff then
      if i17.state=lightstateoff and i101.state=lightstateoff then
           SetLightColor i101, "white", 1
      else
        if i18.state=lightstateoff and i102.state=lightstateoff then
             SetLightColor i102, "white", 1
        else
          if i19.state=lightstateoff and i103.state=lightstateoff then
                SetLightColor i103, "white", 1
          else
            if i20.state=lightstateoff and i104.state=lightstateoff then
                SetLightColor i104, "white", 1
            end if
          end if
        end if
      end if
    else ' all of the lights were already done atleast once
      if i17.state=LightStateBlinking or i18.state=lightstateBlinking or i19.state=lightstateBlinking or i20.state=lightstateBlinking then
        if i17.state=lightstateBlinking and i101.state=lightstateoff then
                SetLightColor i101, "white", 1
        else
          if i18.state=lightstateBlinking and i102.state=lightstateoff then
                SetLightColor i102, "white", 1
          else
            if i19.state=lightstateBlinking and i103.state=lightstateoff then
                SetLightColor i103, "white", 1
            else
              if i20.state=lightstateBlinking and i104.state=lightstateoff then
                SetLightColor i104, "white", 1
              end if
            end if
          end if
        end if
      else ' already got all the pf lights so lets just build them up for increased bumper points and light colours
        if i101.state=lightstateoff and bumpercolor(4) < 4 then   ' probably should light them based on bumper colours!!
                SetLightColor i101, "white", 1
        else
          if i102.state=lightstateoff and bumpercolor(3) < 4  then
              SetLightColor i102, "white", 1
          else
            if i103.state=lightstateoff and bumpercolor(1) < 4 then
                SetLightColor i103, "white", 1
            else
              if i104.state=lightstateoff and bumpercolor(2) < 4 then
                SetLightColor i104, "white", 1
              end if
            end if
          end if
        end if
      end if
    end if
  End if
  AddScore(5000)
End Sub

Sub sw2_hit ' Right Inlane Kiss Combo
  DOF 134, DOFPulse
  PlaySound "fx_sensor", 0, 1, AudioPan(ActiveBall)
  if Tilted Then Exit Sub

  If i33.state=LightStateOff then
    KissCombo.Interval=10000
    KissCombo.enabled=True
    SetLightColor i33, "white", 2
    if i9.state = LightStateOff or i10.state = LightStateOff or i11.state = LightStateOff or i12.state = LightStateOff then
      DisplayI(5)
    else  ' you hit all the targets so now its a hurry up bonus
      SetLightColor i115, "white", 2
      DMDTextI "KISS COMBO","HURRY UP!", bgi                          ' right Lane
      KISSBonus=1000000
      KISSHurryUp.Interval=400
      KISSHurryUp.Enabled=True
    End if
  End if
  AddScore(5000)
End Sub

Sub sw3_hit
  DOF 132, DOFPulse
  PlaySound "fx_sensor", 0, 1, AudioPan(ActiveBall)
  if Tilted Then Exit Sub
  if LightRockAgain.state=LightStateOff then ' Save BallHit 
    if i6.state <> LightStateOff then
      if not DemonMBMode then
        DisplayI(13)
        PlaySound "audio113"
        Debug.print "Exit via Front Row"
        FrontRowSave=True ' Next Drain is just a drain .. 
      End IF
    end if
  end if
  AddScore(5000)
End Sub

Sub sw4_hit
  DOF 135, DOFPulse
  PlaySound "fx_sensor", 0, 1, AudioPan(ActiveBall)
  if Tilted Then Exit Sub
  AddScore(5000)
End Sub

Sub sw14_hit   ' left orbit
  DOF 130, DOFPulse
  PlaySound "fx_sensor", 0, 1, AudioPan(ActiveBall)
  If Tilted Then Exit Sub
  If BallLooping.enabled=False then ' Ignore Initial ball plunge
    BallLooping.enabled=True ' Ignore next switch if looping around 
    if KISSHurryUp.enabled=True Then ' Score KISSHurryUp
      KISSHurryUp.Enabled=False
      AddScore(KISSBONUS) ' HurryUp Bonus Award
      KissCombo.enabled=False
      debug.print "kiss Hurry Up"
      DMDFlush()
      DMDTextI ">KISS Hurry Up!<", KISSBONUS, bgi
      ComboCnt(CurPlayer)=ComboCnt(CurPlayer)+1
      i33.state=LightStateOff
    else
      DMDGif "scene51.gif","YOU ROCK!","",slen(51)
    end if
    AddScore(10000)
    if NOT i58.state=LightStateOff then
      processLO()
    else
      LastShot(CurPlayer)=-1
    end if
    if NOT i62.state=LightStateOff then ' Collect Instrument Drums
      DMDGif "scene58.gif","","",slen(58)
      i62.state=LightStateOff  
      SetLightColor i24, "white", 1  ' pf instrument light 
      i97.state=LightStateBlinking  ' Light Instrument Light
      instruments(CurPlayer)=instruments(CurPlayer)+1
      CheckInstruments()
    end if
    if i7.state=LightStateOff then ' Light Bumper Shot
      SetLightColor i7, "white", 1
    end if
  end if
End Sub

Sub sw24_hit
  debug.print "sw24_hit()"
  DOF 131, DOFPulse
  PlaySound "fx_sensor", 0, 1, AudioPan(ActiveBall)
  If Tilted Then Exit Sub
  if BallLooping.enabled=False then ' Ignore if this is the initial ball plunge
    BallLooping.enabled=True ' Ignore next switch if looping around 
    AddScore(10000)
    if NOT i98.state=LightStateOff then
      processRO()
    else
      LastShot(CurPlayer)=-1
    end if

    if i101.state=LightStateOn then  ' for catman bumper4 
      i101.state=LightStateOff
      if i17.state=LightStateOff then
          SetLightColor i17, "white", 2
    else
      if i17.state=LightStateBlinking then
            SetLightColor i17, "white", 1
      end if
    end if

    ' add one to the bumper light!

    if BumperColor(4)=0 then  ' None
       flasher4.visible=True
       flasher4.color=RGB(255, 255, 255) ' white
       SetLightColor B4L, "white", 1
    else
      if BumperColor(4)=1 Then
         debug.print "green"
         flasher4.color=RGB(0, 255, 0) ' green
         SetLightColor B4L, "green", 1
      else
         if BumperColor(4)=2 then
           flasher4.color=RGB(0, 0, 128) ' blue
           SetLightColor B4L, "blue", 1
           debug.print "purple"
         else
           if BumperColor(4)=3 then
             flasher4.color=RGB(255,0,0) ' red
             SetLightColor B4L, "red", 1
             debug.print "red"
           end if
         end if
      end if
    End If

    BumperColor(4)=BumperColor(4)+1
  end if

  if i102.state=LightStateOn then  ' for Spaceman  bumper3 
    i102.state=LightStateOff
    if i18.state=LightStateOff then
          SetLightColor i18, "white", 2
    else
      if i18.state=LightStateBlinking then
            SetLightColor i18, "white", 1
      end if
    end if

    ' add one to the bumper light!

    if BumperColor(3)=0 then  ' None
       flasher3.visible=True
       flasher3.color=RGB(255, 255, 255) ' white
       SetLightColor B3L, "white", 1
    else
      if BumperColor(3)=1 Then
         debug.print "green"
         flasher3.color=RGB(0, 255, 0) ' green
         SetLightColor B3L, "green", 1
      else
         if BumperColor(3)=2 then
           flasher3.color=RGB(0, 0, 128) ' blue
           SetLightColor B3L, "blue", 1
           debug.print "purple"
         else
           if BumperColor(3)=3 then
             flasher3.color=RGB(255,0,0) ' red
             SetLightColor B3L, "red", 1
             debug.print "red"
           end if
         end if
      end if
    End If

    BumperColor(3)=BumperColor(3)+1
  end if

  if i103.state=LightStateOn then   ' for Starchild   bumper1 
    i103.state=LightStateOff
    if i19.state=LightStateOff then
          SetLightColor i19, "white", 2
    else
      if i19.state=LightStateBlinking then
            SetLightColor i19, "white", 1
      end if
    end if

    ' add one to the bumper light!

    if BumperColor(1)=0 then  ' None
       flasher1.visible=True
       flasher1.color=RGB(255, 255, 255) ' white
       SetLightColor B1L, "white", 1
    else
      if BumperColor(1)=1 Then
         debug.print "green"
         flasher1.color=RGB(0, 255, 0) ' green
         SetLightColor B1L, "green", 1
      else
         if BumperColor(1)=2 then
           flasher1.color=RGB(0, 0, 128) ' blue
           SetLightColor B1L, "blue", 1
           debug.print "purple"
         else
           if BumperColor(1)=3 then
             flasher1.color=RGB(255,0,0) ' red
             SetLightColor B1L, "red", 1
             debug.print "red"
           end if
         end if
      end if
    End If

    BumperColor(1)=BumperColor(1)+1
  end if
'
   if i104.state=LightStateOn then    ' for demon   bumper2 
    i104.state=LightStateOff
    if i20.state=LightStateOff then
          SetLightColor i20, "white", 2
    else
      if i20.state=LightStateBlinking then
            SetLightColor i20, "white", 1
      end if
    end if

    ' add one to the bumper light!

    if BumperColor(2)=0 then  ' None
       flasher2.visible=True
       flasher2.color=RGB(255, 255, 255) ' white
       SetLightColor B2L, "white", 1
    else
      if BumperColor(2)=1 Then
         debug.print "green"
         flasher2.color=RGB(0, 255, 0) ' green
         SetLightColor B2L, "green", 1
      else
         if BumperColor(2)=2 then
           flasher2.color=RGB(0, 0, 128) ' blue
           SetLightColor B2L, "blue", 1
           debug.print "purple"
         else
           if BumperColor(2)=3 then
             flasher2.color=RGB(255,0,0) ' red
             SetLightColor B2L, "red", 1
             debug.print "red"
           end if
         end if
      end if
    End If

    BumperColor(2)=BumperColor(2)+1
  end if
   CheckBumpers()
 end if
End Sub

Sub BallLooping_Timer()
  BallLooping.enabled=False
End Sub

' ***********************
' STAR Targets
' ***********************
Sub sw25_hit  ' S-tar
  PlaySound SoundFXDOF("audio22",117,DOFPulse,DOFTargets), 0, 1, AudioPan(ActiveBall)
  If Tilted Then Exit Sub
  if Not i95.state=LightStateOff then
    targetCnt(CurPlayer)=targetCnt(CurPlayer)+1
    if targetCnt(CurPlayer)=100 then
      DisplayI(30)
      AddScore(40000)
      i95.state=LightStateOff
    else
      if TargetCnt(CurPlayer) < 100 then
          AddScore(40000)
          if rnd*10 > 4 then 
            UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("40k",INT(RND*8)+3," "), 15, 14, 100, 14
          Else  
            DisplayI(34)
          end if
      Else  
          AddScore(5000)
          UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("5k",INT(RND*8)+3," "), 15, 14, 100, 14
      End If
    End if
  Else
    AddScore(5000)
  End If

  i64.state=LightStateOn   ' S
  CheckInstrument1()
  CheckLoveGun()
End Sub

Sub sw26_hit  ' s-T-tar
  PlaySound SoundFXDOF("audio22",117,DOFPulse,DOFTargets), 0, 1, AudioPan(ActiveBall)
  If Tilted Then Exit Sub
  if Not i95.state=LightStateOff then
    targetCnt(CurPlayer)=targetCnt(CurPlayer)+1
    if targetCnt(CurPlayer)=100 then
      DisplayI(30)
      AddScore(40000)
      i95.state=LightStateOff
    else
      if TargetCnt(CurPlayer) < 100 then
          AddScore(40000)
          if rnd*10 > 4 then 
            UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("40k",INT(RND*8)+3," "), 15, 14, 100, 14
          Else  
            DisplayI(34)
          end if
      Else  
          AddScore(5000)
          UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("5k",INT(RND*8)+3," "), 15, 14, 100, 14
      End If
    End If
  Else
    AddScore(5000)
  End If

  I65.state=LightStateOn   ' S
  CheckInstrument1()
  CheckLoveGun()
End Sub
Sub sw27_hit  ' st-A-r
  PlaySound SoundFXDOF("audio22",117,DOFPulse,DOFTargets), 0, 1, AudioPan(ActiveBall)
  If Tilted Then Exit Sub
  if Not i95.state=LightStateOff then
    targetCnt(CurPlayer)=targetCnt(CurPlayer)+1
    if targetCnt(CurPlayer)=100 then
      DisplayI(30)
      AddScore(40000)
      i95.state=LightStateOff
    else
      if TargetCnt(CurPlayer) < 100 then
          AddScore(40000)
          if rnd*10 > 4 then 
            UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("40k",INT(RND*8)+3," "), 15, 14, 100, 14
          Else  
            DisplayI(34)
          end if
      Else  
          AddScore(5000)
          UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("5k",INT(RND*8)+3," "), 15, 14, 100, 14
      End If
    End if
  Else
    AddScore(5000)
  End If

  I66.state=LightStateOn   ' S
  CheckInstrument1()
  CheckLoveGun()
End Sub
Sub sw28_hit  ' sta-R
  PlaySound SoundFXDOF("audio22",117,DOFPulse,DOFTargets), 0, 1, AudioPan(ActiveBall)
  If Tilted Then Exit Sub
  if Not i95.state=LightStateOff then
    targetCnt(CurPlayer)=targetCnt(CurPlayer)+1
    if targetCnt(CurPlayer)=100 then
      DisplayI(30)
      AddScore(40000)
      i95.state=LightStateOff
    else
      if TargetCnt(CurPlayer) < 100 then
          AddScore(40000)
          if rnd*10 > 4 then 
            UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("40k",INT(RND*8)+3," "), 15, 14, 100, 14
          Else  
            DisplayI(34)
          end if
      Else  
          AddScore(5000)
          UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("5k",INT(RND*8)+3," "), 15, 14, 100, 14
      End If
    End if
  Else
    AddScore(5000)
  End If

  I67.state=LightStateOn   ' S
  CheckInstrument1()
  CheckLoveGun()
End Sub

'***************
' Ramp Switches
'***************

Sub sw56_Hit() ' Right Ramp1  RightRampDone
  PlaySound "fx_metalrolling", 0, 1, AudioPan(ActiveBall)
  DOF 131, DOFPulse
  PlaySound "fx_sensor", 0, 1, AudioPan(ActiveBall)
  If Tilted Then Exit Sub
  PlaySound "audio167",0,0.2
FlashForMs Flasher10, 800, 50, 0
FlashForMs Flasher10a, 800, 50, 0

  if NOT i118.state=LightStateOff then ' Super Ramps
      debug.print "Ramp Cnt is " & RampCnt(CurPlayer)
      RampCnt(CurPlayer)=RampCnt(CurPlayer)+1
      if lastShot(CurPlayer)=3 then ' if prior shot was mid ramp
        AddScore(150000)
      else
        AddScore(75000)
      End if
      if RampCnt(CurPlayer) > 9 Then  
        i118.state=LightStateOff
        DisplayI(27) ' Completed

        SpinCnt(CurPlayer) = 0
      else
        DisplayI(31)
      end if
  else
      AddScore(10000)
  end if

  if NOT i92.state=LightStateOff then
    processRR()
  else
    LastShot(CurPlayer)=-1
  end if

  if i97.state=LightStateBlinking then ' Light Instrument
    debug.print "Light an instrument"
    i97.state=LightStateOff
    if NOT i21.state=LightStateOn then
      SetLightColor i72, "white", 2   ' Light  star child
      DisplayI(14)
    else
      if NOT i22.state=LightStateOn then
        SetLightColor i78, "white", 2   ' Light center Ramp
        DisplayI(14)
      else
        if NOT i23.state=LightStateOn then
          SetLightColor i91, "white", 2   ' Light  Demon 
          DisplayI(3)
        else
          if NOT i24.state=LightStateOn then
            SetLightColor i62, "white", 2   ' Light 
            DisplayI(4)
          end if
        end if
      end if
    end if
  end if
  if i7.state=LightStateOff then ' Light Bumper Shot
    SetLightColor i7, "white", 2   ' Light Bumper
  end if
  RandomScene()
End sub

Sub sw64_Hit  ' Center Ramp
  debug.print "sw64_hit"
  PlaySound "fx_metalrolling", 0, 1, AudioPan(ActiveBall)
  If Tilted Then Exit Sub
  PlaySound "audio166",0,0.2
FlashForMs Flasher9, 1000, 50, 0
FlashForMs Flasher9a, 1000, 50, 0

  if NOT i118.state=LightStateOff then ' Super Ramps
      debug.print "Ramp Cnt is " & RampCnt(CurPlayer)
      RampCnt(CurPlayer)=RampCnt(CurPlayer)+1
      if lastShot(CurPlayer)=5 then  ' If prior shot was RR
        AddScore(150000)
      else
        AddScore(75000)
      End if
      if RampCnt(CurPlayer) > 9 Then  
        i118.state=LightStateOff
        DisplayI(27) ' Completed
        SpinCnt(CurPlayer) = 0
      else
        DisplayI(31) ' SuperRamps
      end if
  else
      AddScore(10000)
  end if

  if NOT i73.state=LightStateOff then
    debug.print "process MR()"
    processMR()
  else
    LastShot(CurPlayer)=-1
  end if

  if ArmyHurryUp.enabled then ' ArmyHurryUp 
    AddScore(ArmyBonus)
    DMDFlush()
    DMDTextPause ">Army Hurry Up!<", ArmyBonus, 500
    i77.state=LightStateOff
    ArmyHurryUp.enabled=False
  end if

  if NOT i78.state=LightStateOff then ' Collect Instrument Aces Guitar
    DMDGif "scene53.gif","","",slen(53) 
    i78.state=LightStateOff
    i22.state=LightStateOn        ' pf instrument light 
    i97.state=LightStateBlinking  ' Light Instrument Light
    instruments(CurPlayer)=instruments(CurPlayer)+1
    CheckInstruments()
  end if	
  if i7.state=LightStateOff then ' Light Bumper Shot
    SetLightColor i7,"white",2
  end if      
  RandomScene()
End sub

'   *********************************
'   Demon Lock Targets
Sub sw44_Hit
  PlaySound SoundFXDOF("audio22",128,DOFPulse,DOFTargets), 0, 1, AudioPan(ActiveBall)
  if Tilted Then Exit Sub
  if I82.state=LightStateBlinking then
    if Not i95.state=LightStateOff then
      targetCnt(CurPlayer)=targetCnt(CurPlayer)+1
      if targetCnt(CurPlayer)=100 then
        DisplayI(30)
        AddScore(40000)
        i95.state=LightStateOff
      else
        if TargetCnt(CurPlayer) < 100 then
            AddScore(40000)
            if rnd*10 > 4 then 
              UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("40k",INT(RND*8)+3," "), 15, 14, 100, 14
            Else  
              DisplayI(34)
            end if
        Else  
            AddScore(5000)
            UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("5k",INT(RND*8)+3," "), 15, 14, 100, 14
        End If
      End if
    Else
      AddScore(5000)
    End If
    SetLightColor i82, "white", 1
    CheckLock
  else
    AddScore(100)
  end if
End Sub

Sub sw45_Hit
  PlaySound SoundFXDOF("audio22",128,DOFPulse,DOFTargets), 0, 1, AudioPan(ActiveBall)
  if Tilted Then Exit Sub
  if I84.state=LightStateBlinking then
    if Not i95.state=LightStateOff then
      targetCnt(CurPlayer)=targetCnt(CurPlayer)+1
      if targetCnt(CurPlayer)=100 then
        DisplayI(30)
        AddScore(40000)
        i95.state=LightStateOff
      else
        if TargetCnt(CurPlayer) < 100 then
            AddScore(40000)
            if rnd*10 > 4 then 
              UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("40k",INT(RND*8)+3," "), 15, 14, 100, 14
            Else  
              DisplayI(34)
            end if
        Else  
            AddScore(5000)
            UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("5k",INT(RND*8)+3," "), 15, 14, 100, 14
        End If
      End if
    Else
      AddScore(5000)
    End If
    SetLightColor i84, "white", 1
    CheckLock
  else
    AddScore(100)
  end if
End Sub

Sub CheckLock()
    If bLockEnabled Then Exit Sub
    If i82.State + i84.State = 2 Then
        bLockEnabled = True
        SetLightColor i90, "white", 2 
    End If
End Sub

' ***********************************
Sub sw46_hit ' Rightmost Left Inlane Army Combo
  DOF 133, DOFPulse
  PlaySound "fx_sensor", 0, 1, AudioPan(ActiveBall)
  if Tilted Then Exit Sub
  ArmyCombo.Interval=10000
  ArmyCombo.enabled=True
  SetLightColor i8, "white", 2

  if i13.state=LightStateOff or i14.state=LightStateOff or i15.state=LightStateOff or i16.state=LightStateOff then
    AddScore(5000) ' just a normal combo shot lit
    DisplayI(11)
  else
    DMDTextI "ARMY", "HURRYUP!", bgi
    SetLightColor i77, "white", 2 ' Kiss Army Lights 
        ' start hurry Update  
     ArmyBonus=900000
     ArmyHurryUp.Interval=400
     ArmyHurryUp.Enabled=True
  end if
End Sub

Sub ArmyCombo_Timer()
  i8.state=LightStateOff
  ArmyCombo.enabled=False
End Sub

Sub KissCombo_Timer()
  i33.state=LightStateOff
  KissCombo.enabled=False
End Sub

'  *******************************************************
'     Right StandUp Targets
Sub sw58_hit
  PlaySound SoundFXDOF("audio134",116,DOFPulse,DOFTargets), 0, 1, AudioPan(ActiveBall)
  if Tilted Then Exit Sub
  FlashForMs SmallFlasher2, 600, 50, 0
  if Not i95.state=LightStateOff then
    targetCnt(CurPlayer)=targetCnt(CurPlayer)+1
    if targetCnt(CurPlayer)=100 then
      DisplayI(30)
      AddScore(40000)
      i95.state=LightStateOff
    else
      if TargetCnt(CurPlayer) < 100 then
          AddScore(40000)
          if rnd*10 > 4 then 
            UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("40k",INT(RND*8)+3," "), 15, 14, 100, 14
          Else  
            DisplayI(34)
          end if
      Else  
          AddScore(5000)
          UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("5k",INT(RND*8)+3," "), 15, 14, 100, 14
      End If
    End if
  Else
    AddScore(5000)
  End If
  SetLightColor i106, "yellow", 1
  SetLightColor i13, "yellow", 2
' Check for a Combo bonus
  if NOT i8.state = lightStateOff then ' check if this is a hurryup or just a combo 
    if i13.state=LightStateOff or i14.state=LightStateOff or i15.state=LightStateOff or i16.state=LightStateOff then
      ArmyCombo.enabled=False
      debug.print "army combo"
      DMDTextI "ARMY COMBO", "100000", bgi
      ComboCnt(CurPlayer)=ComboCnt(CurPlayer)+1
      i8.state=LightStateOff
      AddScore(100000)
    End If
  end if
  CheckBackStage()
End Sub

Sub sw59_hit
  PlaySound SoundFXDOF("audio134",116,DOFPulse,DOFTargets), 0, 1, AudioPan(ActiveBall)
  if Tilted Then Exit Sub
  if Not i95.state=LightStateOff then
    targetCnt(CurPlayer)=targetCnt(CurPlayer)+1
    if targetCnt(CurPlayer)=100 then
      DisplayI(30)
      AddScore(40000)
      i95.state=LightStateOff
    else
      if TargetCnt(CurPlayer) < 100 then
          AddScore(40000)
          if rnd*10 > 4 then 
            UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("40k",INT(RND*8)+3," "), 15, 14, 100, 14
          Else  
            DisplayI(34)
          end if
      Else  
          AddScore(5000)
          UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("5k",INT(RND*8)+3," "), 15, 14, 100, 14
      End If
    End if
  Else
    AddScore(5000)
  End If
  FlashForMs SmallFlasher2, 600, 50, 0
  SetLightColor i107, "yellow", 1
  SetLightColor i14, "yellow", 2
' Check for a Combo bonus
  if NOT i8.state = lightStateOff then ' check if this is a hurryup or just a combo 
    if i13.state=LightStateOff or i14.state=LightStateOff or i15.state=LightStateOff or i16.state=LightStateOff then
      ArmyCombo.enabled=False
      debug.print "army combo"
      DMDTextI "ARMY COMBO", "100000", bgi
      ComboCnt(CurPlayer)=ComboCnt(CurPlayer)+1
      i8.state=LightStateOff
      AddScore(100000)
    End If
  end if
  CheckBackStage()
End Sub

Sub sw60_hit
  PlaySound SoundFXDOF("audio134",116,DOFPulse,DOFTargets), 0, 1, AudioPan(ActiveBall)
  if Tilted Then Exit Sub
  if Not i95.state=LightStateOff then
    targetCnt(CurPlayer)=targetCnt(CurPlayer)+1
    if targetCnt(CurPlayer)=100 then
      DisplayI(30)
      AddScore(40000)
      i95.state=LightStateOff
    else
      if TargetCnt(CurPlayer) < 100 then
          AddScore(40000)
          if rnd*10 > 4 then 
            UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("40k",INT(RND*8)+3," "), 15, 14, 100, 14
          Else  
            DisplayI(34)
          end if
      Else  
          AddScore(5000)
          UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("5k",INT(RND*8)+3," "), 15, 14, 100, 14
      End If
    End if
  Else
    AddScore(5000)
  End If
  FlashForMs SmallFlasher2, 600, 50, 0
  SetLightColor i108, "yellow", 1
  SetLightColor i15, "yellow", 2
' Check for a Combo bonus
  if NOT i8.state = lightStateOff then ' check if this is a hurryup or just a combo 
    if i13.state=LightStateOff or i14.state=LightStateOff or i15.state=LightStateOff or i16.state=LightStateOff then
      ArmyCombo.enabled=False
      debug.print "army combo"
      DMDTextPause "ARMY COMBO", ArmyBonus,500
      ComboCnt(CurPlayer)=ComboCnt(CurPlayer)+1
      i8.state=LightStateOff
      AddScore(100000)
    End If
  end if
  CheckBackStage()
End Sub

Sub sw61_hit
  PlaySound SoundFXDOF("audio134",116,DOFPulse,DOFTargets), 0, 1, AudioPan(ActiveBall)
  if Tilted Then Exit Sub
  if Not i95.state=LightStateOff then
    targetCnt(CurPlayer)=targetCnt(CurPlayer)+1
    if targetCnt(CurPlayer)=100 then
      DisplayI(30)
      AddScore(40000)
      i95.state=LightStateOff
    else
      if TargetCnt(CurPlayer) < 100 then
          AddScore(40000)
          if rnd*10 > 4 then 
            UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("40k",INT(RND*8)+3," "), 15, 14, 100, 14
          Else  
            DisplayI(34)
          end if
      Else  
          AddScore(5000)
          UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("5k",INT(RND*8)+3," "), 15, 14, 100, 14
      End If
    End if
  Else
    AddScore(5000)
  End If
  FlashForMs SmallFlasher2, 600, 50, 0
  SetLightColor i109, "yellow", 1
  SetLightColor i16, "yellow", 2
' Check for a Combo bonus
  if NOT i8.state = lightStateOff then ' check if this is a hurryup or just a combo 
    if i13.state=LightStateOff or i14.state=LightStateOff or i15.state=LightStateOff or i16.state=LightStateOff then
      ArmyCombo.enabled=False
      debug.print "army combo"
      DMDTextI "ARMY COMBO", "100000", bgi
      ComboCnt(CurPlayer)=ComboCnt(CurPlayer)+1
      i8.state=LightStateOff
      AddScore(100000)
    End If
  End if
  CheckBackStage()
End Sub

'  *******************************************************
'     Left Drop Targets

Sub sw38_hit
  PlaySound SoundFXDOF("audio22",116,DOFPulse,DOFTargets), 0, 1, AudioPan(ActiveBall)
PlaySound SoundFXDOF("fx_droptarget",116,DOFPulse,DOFTargets), 0, 1, AudioPan(ActiveBall)
End Sub

Sub sw38_Dropped
  debug.print "Target Dropped"
  if Tilted Then Exit Sub
  if NOT i33.state=LightStateOff then
    if i9.state = LightStateOff or i10.state = LightStateOff or i11.state = LightStateOff or i12.state = LightStateOff then
      AddScore(100000)  ' Combo but No HurryUp
      KissCombo.enabled=False
      debug.print "kiss combo"
      DMDTextI "KISS COMBO", "100000", bgi
      ComboCnt(CurPlayer)=ComboCnt(CurPlayer)+1
      i33.state=LightStateOff
    end if 
  end if

  if i35.state=LightStateBlinking then 'Hotter Than Hell Mode
    ProcessTarget()
  end if

  SetLightColor i35, "white", 1
  SetLightColor i9, "white", 1
'  if B2SOn=True then
'    Controller.B2SSetData 1, 1
'  end if
  if Not i95.state=LightStateOff then
    targetCnt(CurPlayer)=targetCnt(CurPlayer)+1
    if targetCnt(CurPlayer)=100 then
      DisplayI(30)
      AddScore(40000)
      i95.state=LightStateOff
    else
      if TargetCnt(CurPlayer) < 100 then
          AddScore(40000)
          if rnd*10 > 4 then 
            UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("40k",INT(RND*8)+3," "), 15, 14, 100, 14
          Else  
            DisplayI(34)
          end if
      Else  
          AddScore(5000)
          UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("5k",INT(RND*8)+3," "), 15, 14, 100, 14
      End If
    End if
  Else
    AddScore(5000)
  End If
  If sw38.IsDropped=1 and sw39.IsDropped=1 and sw40.IsDropped=1 and sw41.IsDropped=1 then 
    debug.print "All targets dropped"
    KissTargetStack=KissTargetStack+1
    FrontRowStack=FrontRowStack+1
    ResetTargets()
  End if
  CheckBackStage():CheckFrontRow()
End Sub

Sub sw39_hit
  PlaySound SoundFXDOF("audio22",116,DOFPulse,DOFTargets), 0, 1, AudioPan(ActiveBall)
PlaySound SoundFXDOF("fx_droptarget",116,DOFPulse,DOFTargets), 0, 1, AudioPan(ActiveBall)
End Sub

Sub sw39_Dropped
  debug.print "Target Dropped"
  if Tilted Then Exit Sub
  if NOT i33.state=LightStateOff then
    if i9.state = LightStateOff or i10.state = LightStateOff or i11.state = LightStateOff or i12.state = LightStateOff then
      AddScore(100000)  ' Combo but No HurryUp
      KissCombo.enabled=False
      debug.print "kiss combo"
      DMDTextI "KISS COMBO", "100000", bgi
      ComboCnt(CurPlayer)=ComboCnt(CurPlayer)+1
      i33.state=LightStateOff
    end if 
  end if

  if i36.state=LightStateBlinking then 'Hotter Than Hell Mode
    ProcessTarget()
  end if

  SetLightColor i36, "white", 1
  SetLightColor i10, "white", 1
'  if B2SOn=True then
'    Controller.B2SSetData 2, 1
'  end if
  if Not i95.state=LightStateOff then
    targetCnt(CurPlayer)=targetCnt(CurPlayer)+1
    if targetCnt(CurPlayer)=100 then
      DisplayI(30)
      AddScore(40000)
      i95.state=LightStateOff
    else
      if TargetCnt(CurPlayer) < 100 then
          AddScore(40000)
          if rnd*10 > 4 then 
            UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("40k",INT(RND*8)+3," "), 15, 14, 100, 14
          Else  
            DisplayI(34)
          end if
      Else  
          AddScore(5000)
          UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("5k",INT(RND*8)+3," "), 15, 14, 100, 14
      End If
    End if
  Else
    AddScore(5000)
  End If
  If sw38.IsDropped=1 and sw39.IsDropped=1 and sw40.IsDropped=1 and sw41.IsDropped=1 then 
    debug.print "All targets dropped"
    KissTargetStack=KissTargetStack+1
    FrontRowStack=FrontRowStack+1
    ResetTargets()
  End if
  CheckBackStage():CheckFrontRow()
End Sub

Sub sw40_hit
  PlaySound SoundFXDOF("audio22",116,DOFPulse,DOFTargets), 0, 1, AudioPan(ActiveBall)
PlaySound SoundFXDOF("fx_droptarget",116,DOFPulse,DOFTargets), 0, 1, AudioPan(ActiveBall)
End Sub

Sub sw40_Dropped
  debug.print "Target Dropped"
  if Tilted Then Exit Sub
  if NOT i33.state=LightStateOff then
    if i9.state = LightStateOff or i10.state = LightStateOff or i11.state = LightStateOff or i12.state = LightStateOff then
      AddScore(100000)  ' Combo but No HurryUp
      KissCombo.enabled=False
      debug.print "kiss combo"
      DMDTextI "KISS COMBO", "100000", bgi
      ComboCnt(CurPlayer)=ComboCnt(CurPlayer)+1
      i33.state=LightStateOff
    end if 
  end if

  if i37.state=LightStateBlinking then 'Hotter Than Hell Mode
    ProcessTarget()
  end if

  SetLightColor i37, "white", 1
  SetLightColor i11, "white", 1
'  if B2SOn=True then
'    Controller.B2SSetData 3, 1
'  end if
  if Not i95.state=LightStateOff then
    targetCnt(CurPlayer)=targetCnt(CurPlayer)+1
    if targetCnt(CurPlayer)=100 then
      DisplayI(30)
      AddScore(40000)
      i95.state=LightStateOff
    else
      if TargetCnt(CurPlayer) < 100 then
          AddScore(40000)
          if rnd*10 > 4 then 
            UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("40k",INT(RND*8)+3," "), 15, 14, 100, 14
          Else  
            DisplayI(34)
          end if
      Else  
          AddScore(5000)
          UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("5k",INT(RND*8)+3," "), 15, 14, 100, 14
      End If
    End if
  Else
    AddScore(5000)
  End If
  If sw38.IsDropped=1 and sw39.IsDropped=1 and sw40.IsDropped=1 and sw41.IsDropped=1 then 
    debug.print "All targets dropped"
    KissTargetStack=KissTargetStack+1
    FrontRowStack=FrontRowStack+1
    ResetTargets()
  End if
  CheckBackStage():CheckFrontRow()
End Sub

Sub sw41_hit
  PlaySound SoundFXDOF("audio22",116,DOFPulse,DOFTargets), 0, 1, AudioPan(ActiveBall)
PlaySound SoundFXDOF("fx_droptarget",116,DOFPulse,DOFTargets), 0, 1, AudioPan(ActiveBall)
End Sub

Sub sw41_Dropped
  debug.print "Target Dropped"
  if Tilted Then Exit Sub
  if NOT i33.state=LightStateOff then
    if i9.state = LightStateOff or i10.state = LightStateOff or i11.state = LightStateOff or i12.state = LightStateOff then
      AddScore(100000)  ' Combo but No HurryUp
      KissCombo.enabled=False
      debug.print "kiss combo"
      DMDTextI "KISS COMBO", "100000", bgi
      ComboCnt(CurPlayer)=ComboCnt(CurPlayer)+1
      i33.state=LightStateOff
    end if 
  end if

  if i38.state=LightStateBlinking then 'Hotter Than Hell Mode
    ProcessTarget()
  end if

  SetLightColor i38, "white", 1
  SetLightColor i12, "white", 1
'  if B2SOn=True then
'    Controller.B2SSetData 4, 1
'  end if
  if Not i95.state=LightStateOff then
    targetCnt(CurPlayer)=targetCnt(CurPlayer)+1
    if targetCnt(CurPlayer)=100 then
      DisplayI(30)
      AddScore(40000)
      i95.state=LightStateOff
    else
      if TargetCnt(CurPlayer) < 100 then
          AddScore(40000)
          if rnd*10 > 4 then 
            UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("40k",INT(RND*8)+3," "), 15, 14, 100, 14
          Else  
            DisplayI(34)
          end if
      Else  
          AddScore(5000)
          UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("5k",INT(RND*8)+3," "), 15, 14, 100, 14
      End If
    End if
  Else
    AddScore(5000)
  End If
  If sw38.IsDropped=1 and sw39.IsDropped=1 and sw40.IsDropped=1 and sw41.IsDropped=1 then 
    debug.print "All targets dropped"
    KissTargetStack=KissTargetStack+1
    FrontRowStack=FrontRowStack+1
    ResetTargets()
  End if
  CheckBackStage():CheckFrontRow()
End Sub


' *********************************************************************
'                        General Routines
' *********************************************************************

Sub CheckBackStage
   debug.print "CheckBackStage"
   if I106.state=LightStateOn and I107.state=LightStateOn and I108.state=LightStateOn and I109.state=LightStateOn then
     ArmyTargetStack=ArmyTargetStack+1
     I106.state=LightStateOff:I107.state=LightStateOff:I108.state=LightStateOff:I109.state=LightStateOff
   end if

   if i41.state=LightStateOff then ' Not already lit
     if KissTargetStack > 0 then
       debug.print "Kiss Targets Complete"
       KissTargetStack=KissTargetStack-1
       DMDGif "scene37.gif"," ","LIT",slen(37)
       i41.state=LightStateBlinking:i42.state=LightStateBlinking
       SetLightColor i41, "white", 2
       SetLightColor i42, "white", 2
     else
       if ArmyTargetStack > 0 then 
         debug.print "Army targets complete"
         ArmyTargetStack=ArmyTargetStack-1
         DMDGif "scene37.gif"," ","LIT",slen(37)
         SetLightColor i41, "white", 2
         SetLightColor i42, "white", 2
       '  textline1.text="backstage pass"
       end if
     end if
   end if
End Sub

Sub CheckFrontRow
   if i6.state=LightStateOff and FrontRowStack > 0 and Not LoveGunMode and Not DemonMBMode then
     FrontRowStack=FrontRowStack-1
     SetLightColor i6, "orange", 2
     DMDTextI "FRONT ROW", "", bgi
   end if
End Sub

Sub ArmyHurryUp_timer
  ArmyBonus=ArmyBonus-15000
  DMDTextPause "ARMY Hurry Up!", ArmyBonus, 0
  if ArmyBonus <=0 then
    ArmyHurryUp.Enabled=False
    i77.state=LightStateOff
  end if
End Sub


Sub KISSHurryUp_timer
  KISSBonus=KISSBonus- 25000
  DMDTextPause "KISS Hurry Up!", KISSBonus,0
  if KISSBonus <=0 then
    KISSHurryUp.Enabled=False
    i115.state=LightStateOff  ' Doublecheck that light
  end if
End Sub

Sub sctarget_hit
  PlaySound SoundFXDOF("audio22",116,DOFPulse,DOFTargets), 0, 1, AudioPan(ActiveBall)
PlaySound SoundFXDOF("fx_droptarget",116,DOFPulse,DOFTargets), 0, 1, AudioPan(ActiveBall)
  if Tilted Then Exit Sub
  AddScore(15000)
End Sub

Sub ResetTargets()
  debug.print "ResetTargets()"
PlaySound "fx_resetdrop"
  sw38.isDropped = False ' KISS Drop Targets
  sw39.isDropped = False
  sw40.isDropped = False
  sw41.isDropped = False
  if cursong(CurPlayer)=4 then ' Hotter Than Hell means they need to flash on Reset 
    debug.print "Target lights blink for Hotter Than Hell"
    I35.state=LightStateBlinking:I36.state=LightStateBlinking:I37.state=LightStateBlinking:I38.state=LightStateBlinking
  else
    I35.state=LightStateOff:I36.state=LightStateOff:I37.state=LightStateOff:I38.state=LightStateOff
  end if
'  if B2SOn=True then
'    Controller.B2SSetData 1, 0
'    Controller.B2SSetData 2, 0
'    Controller.B2SSetData 3, 0
'    Controller.B2SSetData 4, 0
'  end if
End Sub

'***********************************
'Demon Lock
'***********************************

Dim bLockEnabled

Sub sw42_Hit()
    PlaySound "fx_kicker_enter", 0, 1, 0.1
    If Tilted Then Exit Sub
    if NOT i79.state=LightStateOff then
      processD()
    else
      LastShot(CurPlayer)=-1
    end if
    Addscore 10000
    if NOT i91.state=LightStateOff then ' Collect Instrument Genes Bass
           DMDGif "scene56.gif","","",slen(56) 
           i91.state=LightStateOff
           SetLightColor i23, "white", 1  ' pf instrument light 
           SetLightColor i97, "white", 2  ' Light Instrument Light
           instruments(CurPlayer)=instruments(CurPlayer)+1
           CheckInstruments()
    end if
    if i85.state=lightstateoff then  'D-E-M-O-N lights
          SetLightColor i85, "white", 1:DisplayI(6)
    else
      if i86.state=lightstateoff then
            SetLightColor i86, "white", 1:DisplayI(6)
      else
            if i87.state=lightstateoff then
              SetLightColor i87, "white", 1:DisplayI(6)
            else
              if i88.state=lightstateoff then
                SetLightColor i88, "white", 1:DisplayI(6)
              else
                if i89.state=lightstateoff then
                  SetLightColor i89, "white", 1:DisplayI(6)
                  PlaySound "audio429"
                end if
              end if
            end if
      end if
    end if
    If bLockEnabled and NOT demonMBMode and NOT LoveGunMode Then
            LockedBalls(CurPlayer) = LockedBalls(CurPlayer) + 1
            bLockEnabled=False
            Playsound "audio" & 657 + LockedBalls(CurPlayer)
            SetLightColor i82, "orange", 2
            SetLightColor i84, "orange", 2
            i90.State = 0
            DMDTextI "BALL " & LockedBalls(CurPlayer), "IS LOCKED", bgi
            If LockedBalls(CurPlayer) = 3 Then
                MBPauseTimer.interval=2000:MBPauseTimer.enabled=True
                vpmtimer.addtimer 2000, "DemonKickBall '"
            else
                vpmtimer.addtimer 1500, "DemonKickBall '"
            End IF
    Else
       vpmtimer.addtimer 1500, "DemonKickBall '"
    End If

End Sub

Sub MBPauseTimer_Timer()
  MBPauseTimer.enabled=False
  DemonMultiball
End Sub

Sub DemonMultiball()
    FlashForMs Flasher9, 1000, 50, 0
FlashForMs Flasher9a, 1000, 50, 0
    FlashForMs Flasher10, 1000, 50, 0 
FlashForMs Flasher10a, 1000, 50, 0
    DemonMBMode=True
EndMusic
    SaveSong=cursong(CurPlayer)
    debug.print "Saving Song #" & SaveSong
    SaveShots()
    AddMultiball 3
    if Not bBallSaverActive then
      EnableBallSaver(9) ' Multiball BallSaver
    end if

    LockedBalls(CurPlayer) = 0
    bLockEnabled = False
    i85.State = 0
    i86.State = 0
    i87.State = 0
    i88.State = 0
    i89.State = 0

    ' turn off the lock lights
    SetLightColor i82, "orange", 0
    SetLightColor i84, "orange", 0
    i90.State = 0

   ' turn off front row ball Save
    i6.state=lightStateOff
    i44.state=lightstateOff
   
   'Turn On the Jackpot lights
    for each xx in ShotsColl
      SetLightColor xx, "red", 2
    Next
for each xx in aGiLights
      SetLightColor xx, "red", 2
    Next
    for xx = 1 to 4
      LastShot(xx)=-1  ' Clear JackPot Shots
    Next
   
    debug.print "Start DEMON MB" 
    DMDGif "scene64.gif","","",slen(64)
    StopSound Track(cursong(CurPlayer))
  ' play speech
    PlaySound "audio662"
    cursong(CurPlayer)=9  ' Calling DR Love
    Song = track(CurSong(CurPlayer))
    playsound song, 0, MusicVolMB
    debug.print "Show Saved Song #" & SaveSong
End Sub


Sub Start_LoveGun()
    SetLightColor i25, "white", 2    ' Flash Start Child
    if NOT LoveGunMode and NOT DemonMBMode then ' Not already in MB
      debug.print "Start LoveGun()"
      LoveGunMode=True
      DMDGif "scene50.gif","LOVE GUN","",slen(50) 
      EndMusic
      ' save the previous states
      SaveSong=cursong(CurPlayer)
      SaveShots()
      AddMultiball 3
      if Not bBallSaverActive then
        EnableBallSaver(9) ' Multiball BallSaver
      end if
for each xx in aGiLights
      SetLightColor xx, "purple", 2
    Next

      'Turn Off Demon Locks
      SetLightColor i82, "orange", 0
      SetLightColor i84, "orange", 0
      i90.State = 0
      bLockEnabled = False    ' Lose the locks 
      
      PlaySound "audio472" ' Love Gun

      for each xx in ShotsColl
        SetLightColor xx, "blue", 1
      Next
      for xx = 1 to 4
        LastShot(xx)=-1  ' Clear JackPot Shots
      Next
      StopSound Track(cursong(CurPlayer))
      cursong(CurPlayer)=10
      Song = track(CurSong(CurPlayer))
      playsound song, 0, MusicVolMB
    end if
End Sub

Sub DemonKickBall()
Dim bulb
    PlaySound "fx_kicker", 0, 1, 0.1
    debug.print "DemonKickball Destroyball"
    sw42.destroyball
    sw42top.createball
    sw42top.Kick 200, 20	
    PlaySound "audio65"
FlashForMs Flasher10, 1000, 50, 0
FlashForMs Flasher10a, 1000, 50, 0
FlashForMs GeneLight, 1000, 50, 0
GeneHead.blenddisablelighting = 0.1:vpmtimer.addtimer 900, "GeneHead.blenddisablelighting = 0 '"
For each bulb in aGILights:bulb.duration 2, 1000, 1: Next
End Sub

Sub ResetJackpotLights()
    debug.print "ResetJackpotLights()"
    i118.State = 0
    RightRampLight.State = 0

    LoveGunMode=False ' Turn off the All Jackpot Mode
    DemonMBMode=False

    PlaySound "audio501" ' that was insane
    debug.print "Done MB .. start original track #" & SaveSong

    StopSound Track(cursong(CurPlayer))
    cursong(CurPlayer)=SaveSong
    Song = track(CurSong(CurPlayer))
    playsound song, 0, MusicVol

    LoadShots() ' should restore last state here
    SetLightColor i82, "orange", 2  ' Demon Lock Light
    SetLightColor i84, "orange", 2  ' Demon Lock Light

    if cursong(CurPlayer)=4 then ' Hotter Than Hell means they need to flash on Reset 
      if sw38.isdropped=1 then i35.state=LightStateOn else i35.state=2 end if
      if sw39.isdropped=1 then i36.state=LightStateOn else i36.state=2 end if
      if sw40.isdropped=1 then i37.state=LightStateOn else i37.state=2 end if
      if sw41.isdropped=1 then i38.state=LightStateOn else i38.state=2 end if
    else
      if sw38.isdropped=1 then i35.state=LightStateOn else i35.state=LightStateOff end if
      if sw39.isdropped=1 then i36.state=LightStateOn else i36.state=LightStateOff end if
      if sw40.isdropped=1 then i37.state=LightStateOn else i37.state=LightStateOff end if
      if sw41.isdropped=1 then i38.state=LightStateOn else i38.state=LightStateOff end if
    end if
End Sub

Sub CheckRockCity
  if i25.state=LightStateOn and i26.state=LightStateOn and i27.state=LightStateOn and i28.state=LightStateOn then
    SetLightColor i43, "yellow", 2   ' Rock City
  end if
End Sub

Sub CheckKissArmy
  if i25.state=LightStateBlinking or i26.state=LightStateBlinking or i27.state=LightStateBlinking or i28.state=LightStateBlinking then '1 flashing
    if i25.state <> LightStateOff and i26.state <> LightStateOff and i27.state <> LightStateOff and i28.state <> LightStateOff then
      PlaySound "audio560"
      SetLightColor i40, "yellow", 2 ' KissArmy
    end if
  end if
End Sub

' *********************************************************************
'                        Game Choices
' *********************************************************************

Sub NextCity
  CurCity(CurPlayer)=CurCity(CurPlayer)+1
  if CurCity(CurPlayer) > 15 then
    CurCity(CurPlayer)=1
  end if
  debug.print "Change of city to " & CurCity(CurPlayer)
  PlaySound "audio" & 429+CurCity(CurPlayer),0,1,0.25,0.25
End Sub

Sub NextSong
  debug.print "Stopping "  & Track(cursong(CurPlayer)) 
  StopSound song   
  if NewTrackTimer.enabled then NewTrackTimer.enabled=False:NewTrackTimer.enabled=True  ' auto plunge in NewTrack Mode
  cursong(CurPlayer) = cursong(CurPlayer) + 1
  if cursong(CurPlayer) > 8 then 
    cursong(CurPlayer) = 1
  end if
  If UseUDMD Then UltraDMD.SetScoreboardBackgroundImage CurSong(CurPlayer) & ".png",15,13:UltraDMD.Clear:OnScoreboardChanged()
  if cursong(CurPlayer) = SaveShot(CurPlayer,9) then
    LoadShots()
  else
    InitMode(cursong(CurPlayer))
  End if
  Song=Track(CurSong(CurPlayer))
  SongPause.interval=500
  SongPause.enabled=True ' Play song after short pause in case they change their mind

 End Sub

' ***********************************************************************************
' Save and Load the Shots from player to player
' ***********************************************************************************

Dim SaveShot(4,25), SaveCol(4,6), SaveColf(4,6)

' Save the status of the 6 shots for MB start
Sub SaveShots() 'after MB
  debug.print "SaveShots for player " & CurPlayer
  SaveShot(CurPlayer,1)=i58.state
  SaveShot(CurPlayer,2)=i68.state
  SaveShot(CurPlayer,3)=i73.state
  SaveShot(CurPlayer,4)=i79.state
  SaveShot(CurPlayer,5)=i92.state
  SaveShot(CurPlayer,6)=i98.state
  SaveShot(CurPlayer,7)=i6.state ' front row
  SaveShot(CurPlayer,8)=i44.state 'next track
  SaveShot(CurPlayer,9)=CurSong(CurPlayer)
  if i35.state=LightStateBlinking or i36.state=LightStateBlinking or i37.state=LightStateBlinking or i38.state=LightStateBlinking then
    debug.print "SaveShots .. blinking"
    SaveShot(CurPlayer,10)=1
  else
    SaveShot(CurPlayer,10)=0
  end if
  SaveCol(CurPlayer,1)=i58.color
  SaveCol(CurPlayer,2)=i68.color
  SaveCol(CurPlayer,3)=i73.color
  SaveCol(CurPlayer,4)=i79.color
  SaveCol(CurPlayer,5)=i92.color
  SaveCol(CurPlayer,6)=i98.color
  SaveColf(CurPlayer,1)=i58.colorfull
  SaveColf(CurPlayer,2)=i68.colorfull
  SaveColf(CurPlayer,3)=i73.colorfull
  SaveColf(CurPlayer,4)=i79.colorfull
  SaveColf(CurPlayer,5)=i92.colorfull
  SaveColf(CurPlayer,6)=i98.colorfull
End Sub

' Restore the status of the 6 shots after MB Ends
Sub LoadShots() ' after MB
  debug.print "Load Shots for Player " & CurPlayer
  i58.state=SaveShot(CurPlayer,1)
  i68.state=SaveShot(CurPlayer,2)
  i73.state=SaveShot(CurPlayer,3)
  i79.state=SaveShot(CurPlayer,4)
  i92.state=SaveShot(CurPlayer,5)
  i98.state=SaveShot(CurPlayer,6)
  i6.state =SaveShot(CurPlayer,7)
  i44.state=SaveShot(CurPlayer,8)

  debug.print "Loadshots - targets if 1" & SaveShot(CurPlayer,10)
  if SaveShot(CurPlayer,10)=1 then
    i35.state=LightStateBlinking 
    i36.state=LightStateBlinking 
    i37.state=LightStateBlinking 
    i38.state=LightStateBlinking 
  end if
  i58.color = SaveCol(CurPlayer,1)
  i68.color = SaveCol(CurPlayer,2)
  i73.color = SaveCol(CurPlayer,3)
  i79.color = SaveCol(CurPlayer,4)
  i92.color = SaveCol(CurPlayer,5)
  i98.color = SaveCol(CurPlayer,6)

  i58.colorfull = SaveColf(CurPlayer,1)
  i68.colorfull = SaveColf(CurPlayer,2)
  i73.colorfull = SaveColf(CurPlayer,3)
  i79.colorfull = SaveColf(CurPlayer,4)
  i92.colorfull = SaveColf(CurPlayer,5)
  i98.colorfull = SaveColf(CurPlayer,6)
End Sub

Sub SaveState 'at ball end   ' BSP, SuperTargets, Super Ramps, SuperBumpers, SuperSpinner
  if i41.state=LightStateBlinking then ' BSP
    SaveShot(CurPlayer,11)=1
  else
    SaveShot(CurPlayer,11)=0
  end if
  if i122.state=LightStateBlinking then ' 
    SaveShot(CurPlayer,12)=1
  else
    SaveShot(CurPlayer,12)=0
  end if
  if i95.state=LightStateBlinking then ' 
    SaveShot(CurPlayer,13)=1
  else
    SaveShot(CurPlayer,13)=0
  end if
  if i118.state=LightStateBlinking then ' BSP
    SaveShot(CurPlayer,14)=1
  else
    SaveShot(CurPlayer,14)=0
  end if
  if i61.state=LightStateBlinking then ' Bumpers
    SaveShot(CurPlayer,15)=1
  else
    SaveShot(CurPlayer,15)=0
  end if
  SaveShot(CurPlayer,16)=i90.state
End Sub

Sub LoadState ' At InitBall
  if SaveShot(CurPlayer,11)=1 then
    i41.state=LightStateBlinking 
    i42.state=LightStateBlinking 
  end if
  if SaveShot(CurPlayer,12)=1 then
    i122.state=LightStateBlinking
  end if
  if SaveShot(CurPlayer,13)=1 then
    i95.state=LightStateBlinking
  end if
  if SaveShot(CurPlayer,14)=1 then
    i118.state=LightStateBlinking
  end if
  if SaveShot(CurPlayer,15)=1 then
    i61.state=LightStateBlinking
  end if
  i90.state=SaveShot(CurPlayer,16)
End Sub


'ExecuteGlobal GetTextFile("KISSCode.vbs")
' *************************************
'    KISS Code 
'
Const KissCodeV=1.02
' *************************************
' Code to handle the various modes
' *************************************
' 20161031 track shots made by player by song as some songs require more/less shots to complete

' LO-sw14/58, sc-68, MR-sw64/73, D-sw42/79, RR-sw56/92, ro-sw24/98


Dim Curcolor, Curcolorfull

Sub SaveRGB(ll)
  CurColor=ll.color
  Curcolorfull=ll.colorfull
End Sub

Sub RestoreRGB(ll)
  ll.color=Curcolor
  ll.colorfull=Curcolorfull
End Sub

Sub ProcessLO
  Debug.print "ProcessLO Cursong is " & CurSong(CurPlayer)
  SaveRGB(i58):i58.state=LightStateOff
  if DemonMBMode then
    if LastShot(CurPlayer) = -1 then
      DisplayI(18)
      AddScore(1000000)
    else
     DisplayI(19)
      AddScore(2000000)
    end if
    LastShot(CurPlayer)=1
    Shots(CurPlayer,CurSong(CurPlayer))=Shots(CurPlayer,CurSong(CurPlayer))+1
    exit sub
  else
    if LoveGunMode then
      if LastShot(CurPlayer) = -1 then
        DisplayI(17)
        AddScore(1000000)
      else
        DisplayI(19)
        AddScore(2000000)
      end if
      LastShot(CurPlayer)=1
      Shots(CurPlayer,CurSong(CurPlayer))=Shots(CurPlayer,CurSong(CurPlayer))+1
      exit sub
    end if
  end if
  LastShot(CurPlayer)=1
  Shots(CurPlayer,CurSong(CurPlayer))=Shots(CurPlayer,CurSong(CurPlayer))+1

  Select Case CurSong(CurPlayer)
    case 5: 
      ShowShot(Shots(CurPlayer,CurSong(CurPlayer))*200000)
      if i58.state=LightStateOff and i68.state=LightStateOff and i73.state=LightStateOff and i79.state=LightStateOff and i92.state=LightStateOff and i98.state=LightStateOff  then ' reset lights
        InitMode(CurSong(CurPlayer))
      end if
      if Shots(CurPlayer,CurSong(CurPlayer)) Mod 10 = 0 then SongComplete()
    case 2:
      ShowShot(100000)
      if i68.state=LightStateBlinking then
        RestoreRGB(i68):i68.state=LightStateBlinking:RestoreRGB(i73):i73.state=LightStateBlinking:
        SetLightColor i58, "white", 0
      else
        RestoreRGB(i58):i58.state=LightStateBlinking:RestoreRGB(i68):i68.state=LightStateBlinking
        SetLightColor i98, "white", 0
      end if
        DisplayI(8) ' Deuce
      if Shots(CurPlayer,CurSong(CurPlayer)) Mod 12 = 0 then SongComplete()
    case 3:
      ShowShot(100000):i98.state=LightStateBlinking:RestoreRGB(i98)
      if Shots(CurPlayer,CurSong(CurPlayer)) Mod 12 = 0 then SongComplete()
    case 4:
      if Shots(CurPlayer,CurSong(CurPlayer)) > 17 then
        ShowShot(2000000)
      else
        ShowShot(250000+(100000*Shots(CurPlayer,CurSong(CurPlayer))))
      end if
      if sw38.isdropped=False then i35.state=LightStateBlinking end if
      if sw39.isdropped=False then i36.state=LightStateBlinking end if
      if sw40.isdropped=False then i37.state=LightStateBlinking end if
      if sw41.isdropped=False then i38.state=LightStateBlinking end if
      if Shots(CurPlayer,CurSong(CurPlayer)) > 10 then SongComplete()
    case 1:
      if Shots(CurPlayer,CurSong(CurPlayer)) > 20 then
        ShowShot(2000000)
      else
        ShowShot(50000+ (50000*Shots(CurPlayer,CurSong(CurPlayer))))
      end if
      if NOT i68.state=LightStateOff then  ' Move Left
        i98.state=LightStateBlinking:RestoreRGB(i98):i58.state=LightStateBlinking:RestoreRGB(i58)
        SetLightColor i68, "white", 0
      else
        SetLightColor i98, "white", 0
        RestoreRGB(i58):i58.state=LightStateBlinking:RestoreRGB(i68):i68.state=LightStateBlinking
      end if
      if Shots(CurPlayer,CurSong(CurPlayer)) Mod 12 = 0 then SongComplete()
    case 6:
      ShowShot(300000)
      if i58.state=LightStateOff and i98.state=LightStateoff then ' Both Orbits complete
        RestoreRGB(i68):RestoreRGB(i73):RestoreRGB(i79):RestoreRGB(i92)
        i68.state=LightStateBlinking:i73.state=LightStateBlinking:i79.state=LightStateBlinking:i92.state=LightStateBlinking
      end if
    case 7:
      ShowShot(100000):RestoreRGB(i73):i73.state=LightStateBlinking
    case 8:
       ShowShot(100000)
       if i58.state=LightStateOff and i98.state=LightStateOff then 
          RestoreRGB(i68):i68.state=LightStateBlinking
          RestoreRGB(i79):i79.state=LightStateBlinking
          RestoreRGB(i92):i92.state=LightStateBlinking
       end if
    case 9:
    case 10:
  End Select
End Sub

Sub ProcessSC
  debug.print "ProcessSC"
  SaveRGB(i68)
  SetLightColor i68, "white", 0
  LastShot(CurPlayer)=2
  Shots(CurPlayer,CurSong(CurPlayer))=Shots(CurPlayer,CurSong(CurPlayer))+1
  debug.print "Shots=" & Shots(CurPlayer,CurSong(CurPlayer))
  Select Case CurSong(CurPlayer)
    case 5: 
      ShowShot(Shots(CurPlayer,CurSong(CurPlayer))*200000)
      if i58.state=LightStateOff and i68.state=LightStateOff and i73.state=LightStateOff and i79.state=LightStateOff and i92.state=LightStateOff and i98.state=LightStateOff  then ' reset lights
        InitMode(CurSong(CurPlayer))
      end if
      if Shots(CurPlayer,CurSong(CurPlayer)) = 10 then SongComplete()
    case 2:
      ShowShot(100000)
      if i73.state=LightStateBlinking then
        RestoreRGB(i73):i73.state=LightStateBlinking:RestoreRGB(i79):i79.state=LightStateBlinking:SetLightColor i68, "white", 0
      else
        RestoreRGB(i68):i68.state=LightStateBlinking:RestoreRGB(i73):i73.state=LightStateBlinking:SetLightColor i58, "white", 0
      end if
   '   DisplayI(8) ' Deuce
      if Shots(CurPlayer,CurSong(CurPlayer)) Mod 12 = 0 then SongComplete()
    case 3:
      ShowShot(100000):i98.state=LightStateBlinking:RestoreRGB(i98)
      if Shots(CurPlayer,CurSong(CurPlayer)) Mod 12 = 0 then SongComplete()
    case 4:
      if Shots(CurPlayer,CurSong(CurPlayer)) > 17 then
        ShowShot(2000000)
      else
        ShowShot(250000+(100000*Shots(CurPlayer,CurSong(CurPlayer))))
      end if
      if sw38.isdropped=False then i35.state=LightStateBlinking end if
      if sw39.isdropped=False then i36.state=LightStateBlinking end if
      if sw40.isdropped=False then i37.state=LightStateBlinking end if
      if sw41.isdropped=False then i38.state=LightStateBlinking end if
      if Shots(CurPlayer,CurSong(CurPlayer)) > 10 then SongComplete()
    case 1:
      if Shots(CurPlayer,CurSong(CurPlayer)) > 20 then
        ShowShot(2000000)
      else
        ShowShot(50000+ (50000*Shots(CurPlayer,CurSong(CurPlayer))))
      end if
      if NOT i73.state=LightStateOff then  ' Move Left
        RestoreRGB(i58):i58.state=LightStateBlinking:RestoreRGB(i68):i68.state=LightStateBlinking:SetLightColor i73, "white", 0
      else
        SetLightColor i58, "white", 0:RestoreRGB(i68):i68.state=LightStateBlinking:RestoreRGB(i73):i73.state=LightStateBlinking
      end if
      if Shots(CurPlayer,CurSong(CurPlayer)) Mod 12 = 0 then SongComplete()
    case 6:
      ShowShot(200000*(Shots(CurPlayer,CurSong(CurPlayer))-1))
      if Shots(CurPlayer,CurSong(CurPlayer)) > 10 then
        SongComplete()
        SetLightColor i68, "white", 0
        SetLightColor i73, "white", 0
        SetLightColor i79, "white", 0
        SetLightColor i92, "white", 0
      else
        RestoreRGB(i73):RestoreRGB(i79):RestoreRGB(i92)
        SetLightColor i68, "white", 0
        i73.state=LightStateBlinking:i79.state=LightStateBlinking:i92.state=LightStateBlinking
      end if
    case 7:
      ShowShot(100000):RestoreRGB(i73):i73.state=LightStateBlinking
    case 8:
      ShowShot(100000)
      if i68.state=LightStateOff and i79.state=LightStateOff and i92.state=LightStateOff then
         RestoreRGB(i73):i73.state=LightStateBlinking
         SongComplete()
      End If
    case 9:
    case 10:
  End Select
End Sub

Sub RandomBD ' When Bumper is hit & Black Diamond then target moves if its not the RO
  if i98.state=LightStateOff and CurSong(CurPlayer) = 3 then
    debug.print "RandomBD"
    SetLightColor i58,"white", 0
    SetLightColor i68,"white", 0
    SetLightColor i73,"white", 0
    SetLightColor i79,"white", 0
    SetLightColor i92,"white", 0
      Select Case INT(RND*5)+1
        case 1: SetLightColor i58,RGBColors(cRGB), 2
        case 2: SetLightColor i68,RGBColors(cRGB), 2
        case 3: SetLightColor i73,RGBColors(cRGB), 2
        case 4: SetLightColor i79,RGBColors(cRGB), 2
        case 5: SetLightColor i92,RGBColors(cRGB), 2
      End Select
  End If
End Sub

Sub ProcessTarget
   LastShot(CurPlayer)=7 
   ' If Random Shot not already lit then light one
   if i58.state=LightStateOff and i68.state=LightStateOff and i73.state=LightStateOff and i79.state=LightStateOff and i92.state=LightStateOff and i98.state=LightStateOff then
     Shots(CurPlayer,CurSong(CurPlayer))=Shots(CurPlayer,CurSong(CurPlayer))+1
     if Shots(CurPlayer,CurSong(CurPlayer)) > 5 then
       ShowShot(2000000)
     else
       ShowShot(250000+(100000*Shots(CurPlayer,CurSong(CurPlayer))))
     end if
    ' light random shot
     debug.print "Light Random shot "
   
     Select Case INT(RND*6)+1
      case 1: SetLightColor i58, "red", 2
      case 2: SetLightColor i68, "red", 2
      case 3: SetLightColor i73, "red", 2
      case 4: SetLightColor i79, "red", 2
      case 5: SetLightColor i92, "red", 2
      case 6: SetLightColor i98, "red", 2
     End Select
  End If
  if Shots(CurPlayer,CurSong(CurPlayer)) > 10 then SongComplete()
End Sub

Sub ProcessMR
  SaveRGB(i73):SetLightColor i73, "white", 0
  if DemonMBMode then
    if LastShot(CurPlayer) = -1 then
      DisplayI(18)
      AddScore(1000000)
    else
      DisplayI(19)
      AddScore(2000000)
    end if
    LastShot(CurPlayer)=3
    Shots(CurPlayer,CurSong(CurPlayer))=Shots(CurPlayer,CurSong(CurPlayer))+1
    exit sub
  else
    if LoveGunMode then
      if LastShot(CurPlayer) = -1 then
        DisplayI(17)
        AddScore(1000000)
      else
        DisplayI(19)
        AddScore(2000000)
      end if
      LastShot(CurPlayer)=3
      Shots(CurPlayer,CurSong(CurPlayer))=Shots(CurPlayer,CurSong(CurPlayer))+1
      exit sub
    end if
  end if
  LastShot(CurPlayer)=3
  Shots(CurPlayer,CurSong(CurPlayer))=Shots(CurPlayer,CurSong(CurPlayer))+1
  debug.print "Shots=" & Shots(CurPlayer,CurSong(CurPlayer))
  Select Case CurSong(CurPlayer)
    case 5: 
      ShowShot(Shots(CurPlayer,CurSong(CurPlayer))*200000)
      if i58.state=LightStateOff and i68.state=LightStateOff and i73.state=LightStateOff and i79.state=LightStateOff and i92.state=LightStateOff and i98.state=LightStateOff  then ' reset lights
        InitMode(CurSong(CurPlayer))
      end if
      if Shots(CurPlayer,CurSong(CurPlayer)) = 10 then SongComplete()
    case 2:
   '   DisplayI(8) ' Deuce
      ShowShot(100000)
       if i79.state=LightStateBlinking then
        RestoreRGB(i79):i79.state=LightStateBlinking:RestoreRGB(i92):i92.state=LightStateBlinking:SetLightColor i73, "white", 0
      else
        RestoreRGB(i73):i73.state=LightStateBlinking:RestoreRGB(i79):i79.state=LightStateBlinking:SetLightColor i68, "white", 0
      end if
      if Shots(CurPlayer,CurSong(CurPlayer)) Mod 12 = 0 then SongComplete()
    case 3:
      ShowShot(100000):i98.state=LightStateBlinking:RestoreRGB(i98)
      if Shots(CurPlayer,CurSong(CurPlayer)) Mod 12 = 0 then SongComplete()
    case 4:
      if Shots(CurPlayer,CurSong(CurPlayer)) > 17 then
        ShowShot(2000000)
      else
        ShowShot(250000+(100000*Shots(CurPlayer,CurSong(CurPlayer))))
      end if
      if sw38.isdropped=False then i35.state=LightStateBlinking end if
      if sw39.isdropped=False then i36.state=LightStateBlinking end if
      if sw40.isdropped=False then i37.state=LightStateBlinking end if
      if sw41.isdropped=False then i38.state=LightStateBlinking end if
      if Shots(CurPlayer,CurSong(CurPlayer)) > 10 then SongComplete()
    case 1:
      if Shots(CurPlayer,CurSong(CurPlayer)) > 20 then
        ShowShot(2000000)
      else
        ShowShot(50000+ (50000*Shots(CurPlayer,CurSong(CurPlayer))))
      end if
      if Not i79.state=LightStateOff then  ' Move Left
        RestoreRGB(i68):i68.state=LightStateBlinking:RestoreRGB(i73):i73.state=LightStateBlinking:SetLightColor i79, "white", 0
      else
        SetLightColor i68, "white", 0:RestoreRGB(i73):i73.state=LightStateBlinking:RestoreRGB(i79):i79.state=LightStateBlinking
      end if
      if Shots(CurPlayer,CurSong(CurPlayer)) Mod 12 = 0 then SongComplete()
    case 6:
      ShowShot(200000*(Shots(CurPlayer,CurSong(CurPlayer))-1))
      if Shots(CurPlayer,CurSong(CurPlayer)) > 10 then
        SongComplete()
        SetLightColor i68, "white", 0
        SetLightColor i73, "white", 0
        SetLightColor i79, "white", 0
        SetLightColor i92, "white", 0
      else
        RestoreRGB(i68):RestoreRGB(i79):RestoreRGB(i92)
        SetLightColor i73, "white", 0
        i68.state=LightStateBlinking:i79.state=LightStateBlinking:i92.state=LightStateBlinking
      end if
    case 7:
      ShowShot(100000)
      ' Light Next Light In Sequence
      Select Case Shots(CurPlayer,CurSong(CurPlayer))
        Case 1: RestoreRGB(i68):i68.state=LightStateBlinking
        Case 3: RestoreRGB(i79):i79.state=LightStateBlinking
        Case 5: RestoreRGB(i92):i92.state=LightStateBlinking
        Case 7: RestoreRGB(i98):i98.state=LightStateBlinking
        Case 9: SongComplete()
      End Select  
    case 8:
      debug.print "R&R All night light Orbits"
      ShowShot(100000)
      SaveRGB(i58):i58.state=LightStateBlinking
      RestoreRGB(i98):i98.state=LightStateBlinking  ' Two orbits
    case 9:
    case 10:
  End Select
End Sub

Sub ProcessD
  SaveRGB(i79):SetLightColor i79, "white", 0
  if DemonMBMode then
    if LastShot(CurPlayer) = -1 then
      DisplayI(18)
      AddScore(1000000)
    else
      DisplayI(19)
      AddScore(2000000)
    end if
    LastShot(CurPlayer)=4
    Shots(CurPlayer,CurSong(CurPlayer))=Shots(CurPlayer,CurSong(CurPlayer))+1
    Exit sub
  else
    if LoveGunMode then
      if LastShot(CurPlayer) = -1 then
        DisplayI(17)
        AddScore(1000000)
      else
        DisplayI(19)
        AddScore(2000000)
      end if
      LastShot(CurPlayer)=4
      Shots(CurPlayer,CurSong(CurPlayer))=Shots(CurPlayer,CurSong(CurPlayer))+1
      exit sub
    end if
  end if
  LastShot(CurPlayer)=4
  Shots(CurPlayer,CurSong(CurPlayer))=Shots(CurPlayer,CurSong(CurPlayer))+1
  debug.print "Shots=" & Shots(CurPlayer,CurSong(CurPlayer))
  Select Case CurSong(CurPlayer)
    case 5: 
      ShowShot(Shots(CurPlayer,CurSong(CurPlayer))*200000)
      if i58.state=LightStateOff and i68.state=LightStateOff and i73.state=LightStateOff and i79.state=LightStateOff and i92.state=LightStateOff and i98.state=LightStateOff  then ' reset lights
        InitMode(CurSong(CurPlayer))
      end if
      if Shots(CurPlayer,CurSong(CurPlayer)) = 10 then SongComplete()
    case 2:
      ShowShot(100000)
       if i92.state=LightStateBlinking then
        RestoreRGB(i92):i92.state=LightStateBlinking:SaveRGB(i98):i98.state=LightStateBlinking:SetLightColor i79, "white", 0
      else
        RestoreRGB(i79):i79.state=LightStateBlinking:RestoreRGB(i92):i92.state=LightStateBlinking:SetLightColor i73, "white", 0
      end if
      if Shots(CurPlayer,CurSong(CurPlayer)) Mod 12 = 0 then SongComplete()
    case 3:
      ShowShot(100000):i98.state=LightStateBlinking:RestoreRGB(i98)
      if Shots(CurPlayer,CurSong(CurPlayer)) Mod 12 = 0 then SongComplete()
    case 4:
      if Shots(CurPlayer,CurSong(CurPlayer)) > 17 then
        ShowShot(2000000)
      else
        ShowShot(250000+(100000*Shots(CurPlayer,CurSong(CurPlayer))))
      end if
      if sw38.isdropped=False then i35.state=LightStateBlinking end if
      if sw39.isdropped=False then i36.state=LightStateBlinking end if
      if sw40.isdropped=False then i37.state=LightStateBlinking end if
      if sw41.isdropped=False then i38.state=LightStateBlinking end if
      if Shots(CurPlayer,CurSong(CurPlayer)) > 10 then SongComplete()
    case 1:
      if Shots(CurPlayer,CurSong(CurPlayer)) > 20 then
        ShowShot(2000000)
      else
        ShowShot(50000+ (50000*Shots(CurPlayer,CurSong(CurPlayer))))
      end if
      if NOT i92.state=LightStateOff then  ' Move Left
        RestoreRGB(i73):i73.state=LightStateBlinking:RestoreRGB(i79):i79.state=LightStateBlinking:SetLightColor i92, "white", 0
      else
        SetLightColor i73, "white", 0:RestoreRGB(i79):i79.state=LightStateBlinking:RestoreRGB(i92):i92.state=LightStateBlinking
      end if
      if Shots(CurPlayer,CurSong(CurPlayer)) Mod 12 = 0 then SongComplete()
    case 6:
      ShowShot(200000*(Shots(CurPlayer,CurSong(CurPlayer))-1))
      if Shots(CurPlayer,CurSong(CurPlayer)) > 10 then 
        SongComplete()
        SetLightColor i68, "white", 0
        SetLightColor i73, "white", 0
        SetLightColor i79, "white", 0
        SetLightColor i92, "white", 0
      else
        RestoreRGB(i68):RestoreRGB(i73):RestoreRGB(i92)
        SetLightColor i79, "white", 0
        i68.state=LightStateBlinking:i73.state=LightStateBlinking:i92.state=LightStateBlinking
      end if
    case 7:
      ShowShot(100000):RestoreRGB(i73):i73.state=LightStateBlinking
    case 8:
      ShowShot(100000)
      if i68.state=LightStateOff and i79.state=LightStateOff and i92.state=LightStateOff then
        RestoreRGB(i73):i73.state=LightStateBlinking
        SongComplete()
      End If
    case 9:
    case 10:
  End Select
End Sub

Sub ProcessRR
  SaveRGB(i92):SetLightColor i92, "white", 0
  if DemonMBMode then
    if LastShot(CurPlayer) = -1 then
      DisplayI(17)
      AddScore(1000000)
    else
      DisplayI(28)
      AddScore(2000000)
    end if
    LastShot(CurPlayer)=5
    Shots(CurPlayer,CurSong(CurPlayer))=Shots(CurPlayer,CurSong(CurPlayer))+1
    exit sub
  else
    if LoveGunMode then
      if LastShot(CurPlayer) = -1 then
        DisplayI(19)
        AddScore(1000000)
      else
        DisplayI(19)
        AddScore(2000000)
      end if
      LastShot(CurPlayer)=5
      Shots(CurPlayer,CurSong(CurPlayer))=Shots(CurPlayer,CurSong(CurPlayer))+1
      exit sub
    end if
  end if
  LastShot(CurPlayer)=5
  Shots(CurPlayer,CurSong(CurPlayer))=Shots(CurPlayer,CurSong(CurPlayer))+1
  debug.print "Shots=" & Shots(CurPlayer,CurSong(CurPlayer))
  Select Case CurSong(CurPlayer)
    case 5: 
      ShowShot(Shots(CurPlayer,CurSong(CurPlayer))*200000)
      if i58.state=LightStateOff and i68.state=LightStateOff and i73.state=LightStateOff and i79.state=LightStateOff and i92.state=LightStateOff and i98.state=LightStateOff  then ' reset lights
        InitMode(CurSong(CurPlayer))
      end if
      if Shots(CurPlayer,CurSong(CurPlayer)) = 10 then SongComplete()
    case 2:
   '   DisplayI(8) ' Deuce
      ShowShot(100000)
       if i98.state=LightStateBlinking then
        RestoreRGB(i98):i98.state=LightStateBlinking:RestoreRGB(i58):i58.state=LightStateBlinking:SetLightColor i92, "white", 0
      else
        SetLightColor i79, "white", 0:RestoreRGB(i98):i98.state=LightStateBlinking:RestoreRGB(i92):i92.state=LightStateBlinking
      end if
      if Shots(CurPlayer,CurSong(CurPlayer)) Mod 12 = 0 then SongComplete()
    case 3:
      ShowShot(100000):i98.state=LightStateBlinking:RestoreRGB(i98)
      if Shots(CurPlayer,CurSong(CurPlayer)) Mod 12 = 0 then SongComplete()
    case 4:
      if Shots(CurPlayer,CurSong(CurPlayer)) > 17 then
        ShowShot(2000000)
      else
        ShowShot(250000+(100000*Shots(CurPlayer,CurSong(CurPlayer))))
      end if
      if sw38.isdropped=False then i35.state=LightStateBlinking end if
      if sw39.isdropped=False then i36.state=LightStateBlinking end if
      if sw40.isdropped=False then i37.state=LightStateBlinking end if
      if sw41.isdropped=False then i38.state=LightStateBlinking end if
      if Shots(CurPlayer,CurSong(CurPlayer)) > 10 then SongComplete()
    case 1:
      if Shots(CurPlayer,CurSong(CurPlayer)) > 20 then
        ShowShot(2000000)
      else
        ShowShot(50000+ (50000*Shots(CurPlayer,CurSong(CurPlayer))))
      end if
      if i98.state=LightStateBlinking then  ' Move Left
        RestoreRGB(i79):i79.state=LightStateBlinking:RestoreRGB(i92):i92.state=LightStateBlinking:SetLightColor i98, "white", 0
      else
        SetLightColor i79, "white", 0:RestoreRGB(i92):i92.state=LightStateBlinking:RestoreRGB(i98):i98.state=LightStateBlinking
      end if
      if Shots(CurPlayer,CurSong(CurPlayer)) Mod 12 = 0 then SongComplete()
    case 6:
      ShowShot(200000*(Shots(CurPlayer,CurSong(CurPlayer))-1))
      if Shots(CurPlayer,CurSong(CurPlayer)) > 10 then
        SongComplete()
        SetLightColor i68, "white", 0
        SetLightColor i73, "white", 0
        SetLightColor i79, "white", 0
        SetLightColor i92, "white", 0
      else
        RestoreRGB(i68):RestoreRGB(i73):RestoreRGB(i79):SetLightColor i92, "white", 0
        i68.state=LightStateBlinking:i73.state=LightStateBlinking:i79.state=LightStateBlinking
      end if
    case 7:
      ShowShot(100000):RestoreRGB(i73):i73.state=LightStateBlinking
    case 8:
      ShowShot(100000)
      if i68.state=LightStateOff and i79.state=LightStateOff and i92.state=LightStateOff then
         RestoreRGB(i73):i73.state=LightStateBlinking
         SongComplete()
     End if
    case 9:
    case 10:
  End Select
End Sub

Sub ProcessRO
  debug.print "ProcessRO"
  SaveRGB(i98):SetLightColor i98, "white", 0
  if DemonMBMode then
    if LastShot(CurPlayer) = -1 then
      DisplayI(18)
      AddScore(1000000)
    else
      DisplayI(19)
      AddScore(2000000)
    end if
    LastShot(CurPlayer)=6
    Shots(CurPlayer,CurSong(CurPlayer))=Shots(CurPlayer,CurSong(CurPlayer))+1
    exit sub
  else
    if LoveGunMode then
      if LastShot(CurPlayer) = -1 then
        DisplayI(17)
        AddScore(1000000)
      else
        DisplayI(19)
        AddScore(2000000)
      end if
      LastShot(CurPlayer)=6
      Shots(CurPlayer,CurSong(CurPlayer))=Shots(CurPlayer,CurSong(CurPlayer))+1
      exit sub
    end if
  end if
  LastShot(CurPlayer)=6
  Shots(CurPlayer,CurSong(CurPlayer))=Shots(CurPlayer,CurSong(CurPlayer))+1
  debug.print "Shots=" & Shots(CurPlayer,CurSong(CurPlayer))
  Select Case CurSong(CurPlayer)
    case 5: 
      ShowShot(Shots(CurPlayer,CurSong(CurPlayer))*200000)
      if i58.state=LightStateOff and i68.state=LightStateOff and i73.state=LightStateOff and i79.state=LightStateOff and i92.state=LightStateOff and i98.state=LightStateOff  then ' reset lights
        InitMode(CurSong(CurPlayer))
      end if
      if Shots(CurPlayer,CurSong(CurPlayer)) = 10 then SongComplete()
    case 2:
   '   DisplayI(8) ' Deuce
      ShowShot(100000)
      if i58.state=LightStateBlinking then
        RestoreRGB(i58):i58.state=LightStateBlinking:RestoreRGB(i68):i68.state=LightStateBlinking:SetLightColor i98, "white", 0
      else
        RestoreRGB(i98):i98.state=LightStateBlinking:RestoreRGB(i58):i58.state=LightStateBlinking:SetLightColor i98, "white", 0
      end if
      if Shots(CurPlayer,CurSong(CurPlayer)) Mod 12 = 0 then SongComplete()
    case 3: ' Goes to a random light
      ShowShot(100000)
      Select Case INT(RND*5)+1
        case 1: i58.state=LightStateBlinking:RestoreRGB(i58)
        case 2: i68.state=LightStateBlinking:RestoreRGB(i68)
        case 3: i73.state=LightStateBlinking:RestoreRGB(i73)
        case 4: i79.state=LightStateBlinking:RestoreRGB(i79)
        case 5: i92.state=LightStateBlinking:RestoreRGB(i92)
      End Select
      if Shots(CurPlayer,CurSong(CurPlayer)) Mod 12 = 0 then SongComplete()
    case 4: 
      if Shots(CurPlayer,CurSong(CurPlayer)) > 17 then
        ShowShot(2000000)
      else
        ShowShot(250000+(100000*Shots(CurPlayer,CurSong(CurPlayer))))
      end if
      if sw38.isdropped=False then i35.state=LightStateBlinking end if
      if sw39.isdropped=False then i36.state=LightStateBlinking end if
      if sw40.isdropped=False then i37.state=LightStateBlinking end if
      if sw41.isdropped=False then i38.state=LightStateBlinking end if
      if Shots(CurPlayer,CurSong(CurPlayer)) > 10 then SongComplete()
    case 1:
      if Shots(CurPlayer,CurSong(CurPlayer)) > 20 then
        ShowShot(2000000)
      else
        ShowShot(50000+ (50000*Shots(CurPlayer,CurSong(CurPlayer))))
      end if
      if i58.state=LightStateBlinking then  ' Move Left
        RestoreRGB(i92):i92.state=LightStateBlinking:RestoreRGB(i98):i98.state=LightStateBlinking:SetLightColor i58, "white", 0
      else
        SetLightColor i92, "white", 0:RestoreRGB(i98):i98.state=LightStateBlinking:RestoreRGB(i58):i58.state=LightStateBlinking
      end if
      if Shots(CurPlayer,CurSong(CurPlayer)) Mod 12 = 0 then 
        SongComplete()
      end if
    case 6:
      ShowShot(300000)
      if i58.state=LightStateOff and i98.state=LightStateoff Then ' Both Orbits complete
        RestoreRGB(i68):RestoreRGB(i73):RestoreRGB(i79):RestoreRGB(i92)
        i68.state=LightStateBlinking:i73.state=LightStateBlinking:i79.state=LightStateOn:i92.state=LightStateBlinking
      end if
    case 7:
      ShowShot(100000):RestoreRGB(i73):i73.state=LightStateBlinking
    case 8:
       ShowShot(100000)
       if i58.state=LightStateOff then 
          RestoreRGB(i68):RestoreRGB(i68):i68.state=LightStateBlinking
          RestoreRGB(i79):RestoreRGB(i79):i79.state=LightStateBlinking
          RestoreRGB(i92):RestoreRGB(i92):i92.state=LightStateBlinking
       end if
    case 9:
    case 10:
  End Select
End Sub

Sub sw43HoleExit()
    If BallInHole > 0 Then
' check extraball
' check kiss Army
' check rock City
        ProcessScoop()
    End If
End Sub

Sub NewTrackTimer_Timer
   NewTrackTimer.enabled=False
   ChooseSongMode=False
   ScoopDelay.interval=500
   ScoopDelay.Enabled = True
   vpmtimer.addtimer 500, "FlashForMs FlasherExitHole, 1500, 30, 0 '"
   vpmtimer.addtimer 500, "FlashForMs SmallFlasher1, 1500, 30, 0 '"
End Sub

Sub ProcessScoop
Dim bulb
  InScoop=True
  ScoopDelay.interval=4000

  if Not i40.state=LightStateOff then ' Kiss ArmyBonus  
    i40.state=LightStateOff 
    msgbox "NOT IMPLEMENTED YET"
  else
    if NOT i43.state=LightStateOff then ' Rock City
      debug.print "Rock City"
      i43.state=LightStateOff
      i25.state=LightStateOff:i26.state=LightStateOff:i27.state=LightStateOff:i28.state=LightStateOff ' Characters
      ' Light All Jackpots
      StopSound Track(cursong(CurPlayer))
      cursong(CurPlayer)=1
      PlaySound Track(cursong(CurPlayer)) ' "bg_music1" 'PlaySound track(1) ' Detroit Rock City
    else
      if NOT i44.state=LightStateOff then ' New Track
        debug.print "New Track"
        KissHurryUp.enabled=False  ' Need to stop the hurry up and the countdown graphics
        ArmyHurryUp.enabled=False
        DMDTextPause "Select","New Track",3000
        StopSound song
        ChooseSongMode=TRUE
        NewTrackTimer.interval=6000:NewTrackTimer.enabled=True  'auto plunge in NewTrack Mode
        i44.state=LightStateOff
        AddScore(10000)
        Exit Sub
      end if 
      if NOT i39.state=LightStateOff then ' Extra BallHit 
          i39.state=LightStateOff
          debug.print "Extra Ball"
          AwardExtraBall
          LightRockAgain.state=LightStateON ' Rock Again lit
'For each bulb in aGILights:bulb.duration 2, 2000, 1: Next
      else ' check backstage pass
        if NOT i41.state=LightStateOff then
          debug.print "BackStage Pass (video)"
          UDMDTimer.enabled=False
          i41.state=LightStateOff:i42.state=LightStateOff
          AddScore(5000)
          BSP.interval=2000:BSP.enabled=True
          Exit Sub ' 
        Else
          debug.print "Scoop - nothing lit"
          AddScore(5000)
          ScoopDelay.Interval = 1500
        End If
      End If
    End if
  End If
  ScoopDelay.Enabled = True
  vpmtimer.addtimer 500, "FlashForMs FlasherExitHole, 1500, 30, 0 '"
  vpmtimer.addtimer 500, "FlashForMs SmallFlasher1, 1500, 30, 0 '"

End Sub

Sub BSP_Timer 
  BSP.enabled=False
  vpmtimer.addtimer 1500, "FlashForMs FlasherExitHole, 1000, 30, 0 '"
  vpmtimer.addtimer 1500, "FlashForMs SmallFlasher1, 1000, 30, 0 '"
  ScoopCB() ' After Backstage Pass video is shown
  DMDGif "scene38.gif", "", "", slen(38)
End Sub

Sub Resync_targets()
    ' resync target lights in case we may have chose Hotter Than Hell
    debug.print "resync_targets"
    if sw38.isdropped=True then i35.state=LightStateOn else i35.state=LightStateOff end if
    if sw39.isdropped=True then i36.state=LightStateOn else i36.state=LightStateOff end if
    if sw40.isdropped=True then i37.state=LightStateOn else i37.state=LightStateOff end if
    if sw41.isdropped=True then i38.state=LightStateOn else i38.state=LightStateOff end if
End Sub

Sub ScoopCB() ' Play after BSP Scene completes
dim rr
   debug.print "ScoopCB()"
   rr=Int(Rnd*6)+1
   if rr > 6 then rr=6
   debug.print "Random Choice of " & rr
   ' Check that we dont already have the random choice
   if rr=1 and i118.state=LightStateBlinking then 
     rr=2
   end if
   if rr=2 and i95.state=LightStateBlinking then 
      rr=3
   end if
   if rr=3 and i122.state=LightStateBlinking then 
      rr=4
   end if
   if rr=5 and i29.state=LightStateOn and i30.state=LightStateOn and i31.state=LightStateOn then 
     rr=6
   end if
   if rr=6 and i61.state=LightStateOn then 
     rr = 4
   end if
Select case rr
     		  Case 1 : DMDGif "scene40.gif", "", "", slen(40)
                            ScoopDelay.interval=4000:ScoopDelay.enabled=True
                            debug.print "Superramps"
                            SetLightColor i118, "white", 2   ' Super Ramps
                            RampCnt(CurPlayer)=0
	   	  Case 2 : DMDGif "scene42.gif", "", "", slen(42)
                           ScoopDelay.interval=4000:ScoopDelay.enabled=True
                           SetLightColor i95, "white", 2   '
                           debug.print "SuperTargets"
                           TargetCnt(CurPlayer)=0
		  Case 3 : DMDGif "scene43.gif", "", "", slen(43)
                           ScoopDelay.interval=4000:ScoopDelay.enabled=True
                           debug.print "Super Spinner"
                           SetLightColor i122, "white", 2  
                           SpinCnt(CurPlayer)=0
                  Case 4 : DMDGif "scene62.gif", "2 MILLION", "", slen(62)
                           ScoopDelay.interval=slen(62)+500:ScoopDelay.enabled=True
                           debug.print "2 Million"
                           AddScore(2000000)
                  Case 5 : if i29.state=LightStateOff then
                             DMDGif "scene62.gif", "2X", "BONUS", slen(62)
                             ScoopDelay.interval=slen(62)+500:ScoopDelay.enabled=True
                             debug.print "2X"
                             BonusMultiplier(CurPlayer)=2
                             i29.state=LightStateOn
                           else
                             if i30.state=LightStateOff then
                               DMDGif "scene62.gif", "3X", "BONUS", slen(62)
                               ScoopDelay.interval=slen(62)+500:ScoopDelay.enabled=True
                               debug.print "3X"
                               BonusMultiplier(CurPlayer)=3
                               i30.state=LightStateOn
                             else
                               DMDGif "scene62.gif", "COLOSSAL", "BONUS", slen(62)
                               ScoopDelay.interval=slen(62)+500:ScoopDelay.enabled=True
                               debug.print "Colossal"
                               i31.state=LightStateOn
                               BonusMultiplier(CurPlayer)=5
                             end if
                           end if
                    Case 6 : DMDGif "scene41.gif", "", "", slen(41)
                           SetLightColor i61, "white", 2  
                           BumperCnt(CurPlayer)=0
                           debug.print "Super Bumpers"                       
                           ScoopDelay.interval=3000:ScoopDelay.enabled=True
     End Select
End Sub

Sub ScoopDelay_Timer
  debug.print "ScoopDelay_Timer()"
  ScoopDelay.Enabled = False
  BallInHole = BallInHole - 1
  sw43.CreateSizedball BallSize / 2
  PlaySound "fx_popper", 0, 1, -0.1, 0.25
  sw43.Kick 175, 14, 1
  vpmtimer.addtimer 1000, "sw43HoleExit '" 'repeat until all the balls are kicked out
  InScoop=False
End Sub

Sub SongComplete()
  DMDTextPause "Song Complete","Total " & ModeScore,1700
  ModeInProgress=False
  i44.state=LightStateBlinking ' Next Track
  for each xx in ShotsColl
    SetLightColor xx, "white", 0
  Next
  Resync_targets
End Sub

Sub ShowShot(points)
  DMDTextPause Title(cursong(CurPlayer)),points,1200
  AddScore(points)
End Sub

' STAR Targets
Sub CheckInstrument1 ' Pauls Guitar
  debug.print "CheckInstrument1"
  if NOT i72.state=LightStateOff then ' Collect Instrument
    debug.print "If STAR then collect Instrument"
    if i64.state=1 and i65.state=1 and i66.state=1 and i67.state=1 then 'STAR
      debug.print "STAR: Play Instrument Video"
      DMDGif "scene55.gif","","",slen(55) 
      i72.state=LightStateOff
      i21.state=LightStateOn  ' pf instrument light 
      i97.state=LightStateBlinking  ' Light Instrument Light
      instruments(CurPlayer)=instruments(CurPlayer)+1
      CheckInstruments()
    end if
  end if	      
End Sub

Sub CheckInstruments
  debug.print "CheckInstruments"
   if i21.state=LightStateOn and i22.state=LightStateOn and i23.state=LightStateOn and i24.state=LightStateOn then
     debug.print "Instrument Cnt is " & instruments(CurPlayer)
     if instruments(CurPlayer)=4 then
'       TextLine1.text="Extra Ball Light"
'       textline2.text="Instrument BONUS"
       I39.state=LightStateBlinking ' Light Extra Ball
       DisplayI(25) ' Extra Ball Lit
     end if
     i21.state=LightStateBlinking:i22.state=LightStateBlinking:i23.state=LightStateBlinking:i24.state=LightStateBlinking
   end if
   if instruments(CurPlayer)=6 then
     DMDGif "scene49.gif","","",slen(49)
     I26.blinkInterval=100
     I26.state=LightStateBlinking ' Spaceman
   end if
   if instruments(CurPlayer)=12 then
     DMDGif "scene49.gif","","",slen(49) 
     I26.blinkInterval=100
     I26.state=LightStateOn ' Spaceman
   end if
   if instruments(CurPlayer)=48 then ' Extra Ball
'     TextLine1.text="Extra Ball Light"
'     textline2.text="Instrument BONUS"
     I39.state=LightStateOn ' Light Extra Ball
   end if
End Sub

Sub CheckLoveGun
   debug.print "CheckLoveGun"
   if i64.state=1 and i65.state=1 and i66.state=1 and i67.state=1 then ' not already LG Ready
     debug.print "Turn off STAR"
     i64.state=0:i65.state=0:i66.state=0:i67.state=0
     if NOT i68.state=LightStateOff then
       processSC()
     end if
     if NOT LoveGunMode then   ' Dont turn on LG Mode while in LG
       if F116.state=LightStateOff then
         debug.print "Play LG Ready"
         F116.state=1                   ' star flasher
         DMDGif "scene26.gif","","",slen(26) 
       End If
    Else
      debug.print "Already in LG Mode"
    End if
   end if
End Sub

Sub CheckBumpers  
  if i17.state=LightStateBlinking and i18.state=LightStateBlinking and i19.state=LightStateBlinking and i20.state=LightStateBlinking and i28.state = LightStateOff then
    debug.print "CatMAN"
    DMDGif "scene47.gif","","",slen(47) 
    i28.blinkinterval=100
    i28.state=LightStateBlinking
    PlaySound "audio377" ' crowd noise
    CheckKissArmy()
  end if
  if i17.state=LightStateOn and i18.state=LightStateOn and i19.state=LightStateOn and i20.state=LightStateOn and NOT i28.state=LightStateOn then
    debug.print "CatMAN"
    DMDGif "scene47.gif","","",slen(47)
    i28.state=LightStateOn
    PlaySound "audio377" ' crowd noise
    CheckRockCity()
  end if
End Sub


'ExecuteGlobal GetTextFile("KISSDMDV2.vbs")
' *************************************
'    KISS UltraDMDCode 
'
Const KissDMDV=1.08
' *************************************
'
' 20161031 - use ModifyScene to reduce flicker
' 20161101 - End of game audio
' 20161102 - New Match Gifs - be sure to download them and put in ultradmd directory.
' 20161113 - New UDMD Location code from Seraph74
' 20161120 - Updated UDMD Location code from Seraph74

Dim UltraDMD
Dim BonusLights1, BonusLights2, BonusLights3, BonusLights4, ImgList

Const UltraDMD_VideoMode_Stretch = 0
Const UltraDMD_VideoMode_Top = 1
Const UltraDMD_VideoMode_Middle = 2
Const UltraDMD_VideoMode_Bottom = 3


Const UltraDMD_Animation_FadeIn = 0
Const UltraDMD_Animation_FadeOut = 1
Const UltraDMD_Animation_ZoomIn = 2
Const UltraDMD_Animation_ZoomOut = 3
Const UltraDMD_Animation_ScrollOffLeft = 4
Const UltraDMD_Animation_ScrollOffRight = 5
Const UltraDMD_Animation_ScrollOnLeft = 6
Const UltraDMD_Animation_ScrollOnRight = 7
Const UltraDMD_Animation_ScrollOffUp = 8
Const UltraDMD_Animation_ScrollOffDown = 9
Const UltraDMD_Animation_ScrollOnUp = 10
Const UltraDMD_Animation_ScrollOnDown = 11
Const UltraDMD_Animation_None = 14

Sub LoadUltraDMD
    Set UltraDMD = CreateObject("UltraDMD.DMDObject")
    UltraDMD.Init

    Dim fso, curDir
    Set fso = CreateObject("Scripting.FileSystemObject")
    curDir = fso.GetAbsolutePathName(".")
    Set fso = nothing

    ' A Major version change indicates the version is no longer backward compatible
    If Not UltraDMD.GetMajorVersion = 1 Then
        MsgBox "Incompatible Version of UltraDMD found."
        Exit Sub
    End If

    'A Minor version change indicates new features that are all backward compatible
    If UltraDMD.GetMinorVersion < 3 Then
        MsgBox "Incompatible Version of UltraDMD found.  Please update to version 1.4 or newer."
        Exit Sub
    End If

    UltraDMD.SetProjectFolder curDir & "\KISS.UltraDMD"
    UltraDMD.SetVideoStretchMode UltraDMD_VideoMode_Middle
    UltraDMD.SetScoreboardBackgroundImage "1.png",15,13

    ImgList = "kiss.png,kiss.png,kiss.png,kiss.png,kiss1.png,kiss2.png,kiss3.png,kiss4.png,kiss.png,kiss.png,kiss.png,kiss.png"
    BonusLights1 = UltraDMD.CreateAnimationFromImages(4, false, imgList)
    ImgList = "Army.png,Army.png,Army.png,Army.png,Army1.png,Army2.png,Army3.png,Army4.png,Army.png,Army.png,Army.png,Army.png"
    BonusLights2 = UltraDMD.CreateAnimationFromImages(4, false, imgList)
    ImgList = "faces.png,faces.png,faces.png,faces.png,faces1.png,faces2.png,faces3.png,faces4.png,faces.png,faces.png,faces.png,faces.png"
    BonusLights3 = UltraDMD.CreateAnimationFromImages(4, false, imgList)
    ImgList = "inst.png,inst.png,inst.png,inst.png,inst1.png,inst2.png,inst3.png,inst4.png,inst.png,inst.png,inst.png,inst.png"
    BonusLights4 = UltraDMD.CreateAnimationFromImages(4, false, imgList)

    OnScoreboardChanged()
End Sub

'---------- UltraDMD Unique Table Color preference -------------
' http://www.vpforums.org/index.php?showtopic=26602&page=21#entry362581
'
Dim DMDColor, DMDColorSelect, UseFullColor
Dim DMDPosition, DMDPosX, DMDPosY

Sub GetDMDColor
    'Dim WshShell,filecheck,directory
    'Set WshShell = CreateObject("WScript.Shell")
    'If DMDPosition then
    '    WshShell.RegWrite "HKCU\Software\UltraDMD\x",DMDPosX,"REG_DWORD"
    '    WshShell.RegWrite "HKCU\Software\UltraDMD\y",DMDPosY,"REG_DWORD"
    'End if
    'WshShell.RegWrite "HKCU\Software\UltraDMD\fullcolor",UseFullColor,"REG_SZ"
    'WshShell.RegWrite "HKCU\Software\UltraDMD\color",DMDColorSelect,"REG_SZ"
End Sub
'---------------------------------------------------
'---------------------------------------------------

' *****************************************
'    DMD Interactions
' *****************************************
Sub DMDTextI(txt1,txt2,img)  ' Pass in Image
  debug.print "Text: " & txt1 & " " & txt2 
  DMDTextPauseI txt1,txt2,500,img
End Sub

Sub DMDText(txt1,txt2)
  debug.print "Text: " & txt1 & " " & txt2 
  DMDTextPause txt1,txt2,500
End Sub

Sub DMDTextPauseI(txt1,txt2,pause,img)
dim PriorState
  debug.print "Text: " & txt1 & " " & txt2 
  PriorState=UDMDTimer.Enabled
  UDMDTimer.Enabled=False
  if UseUDMD then
    UltraDMD.DisplayScene00Ex img, txt1, 15, 2, txt2, 15, 2, UltraDMD_Animation_None, pause, UltraDMD_Animation_None
  End if
  UDMDTimer.interval=100:UDMDTimer.Enabled=PriorState
End Sub

Sub DMDTextPause(txt1,txt2,pause)
  debug.print "Text: " & txt1 & " " & txt2 
  UDMDTimer.Enabled=False
  if UseUDMD then
    UltraDMD.DisplayScene00Ex "scene01.gif", txt1, 15, 2, txt2, 15, 2, UltraDMD_Animation_None, pause, UltraDMD_Animation_None
    UDMDTimer.interval=100:UDMDTimer.Enabled = True
  End if
End Sub

Sub DMDGif(img1,txt1,txt2,p)
  debug.print "Txt: >" & txt1 & "< Gif:" & img1 & " Pause =" & p
  if NOT UseUDMD then Exit Sub

  UDMDTimer.Enabled=False
  if txt2 = "" then
    UltraDMD.DisplayScene00Ex img1, txt1, 0, 15, "", -1, -1, UltraDMD_Animation_None, p, UltraDMD_Animation_None
  else
    UltraDMD.DisplayScene00Ex img1, txt1, 15, 2, txt2, 13, 2, UltraDMD_Animation_None, p, UltraDMD_Animation_None
  end if
  UDMDTimer.interval=p:UDMDTimer.Enabled = True
End Sub

Sub DisplayI(id)
  debug.print "Showing ID" & id
  if NOT useUDMD Then Exit Sub

  DMDFlush()
  Select Case id
    Case 2: DMDTextI "DANGER","", bgi
    Case 1: DMDTextI "TILT","", bgi
    Case 3: DMDTextI "BASS INSTRUMENT","LIT", bgi
    Case 4: DMDTextI "DRUM INSTRUMENT","LIT", bgi
    Case 5: DMDTextI "KISS COMBO","LIT", bgi                          ' right Lane
    Case 6: DMDTextI "SPELL DEMON TO","RAISE JACKPOT!", bgi
    Case 7: DMDTextI "LOCK","LIT", bgi
'when both green lights hit
    Case 8: DMDTextI "DEUCE","", bgi
' ARMY COMPLETED SPINNER VALUE 24,000 (spinning army target) 4 right targets
    Case 9: DMDTextI "SPINNER VALUE 5,000","ARMY COMPLETED", bgi
' FRONT ROW IS LIT .. when all left kiss targets hit
    Case 10: DMDTextI "FRONT ROW IS","LIT", bgi
    Case 11: DMDTextI "ARMY COMBO IS","LIT", bgi
' Left out lane - shows crows cheer the "FRONT ROW AWARDED | ROCK AGAIN!", dont wait to drain just pop ball and autoplunge
    Case 13: DMDTextI "FRONT ROW","ROCK AGAIN!", bgi
    Case 14: DMDTextI "GUITAR INSTRUMENT","LIT", bgi
    Case 15: DMDTextI "EXTRA BALL","", bgi
    Case 16: DMDGif "scene10.gif","REPLAY","",slen(10)
             ' PlaySound "audio"  'x175

    Case 17: DMDTextI "JACKPOT","", bgi:PlaySound "audio663"
    Case 18: DMDTextI "DEMON","JACKPOT", bgi:PlaySound "audio663"
    Case 19: DMDTextI "DOUBLE","JACKPOT", bgi:PlaySound "audio666"
    Case 20: DMDTextI "SUPER","JACKPOT", bgi
    Case 21: DMDTextI "DOUBLE SUPER","JACKPOT", bgi
    Case 22: DMDTextI "BONUS","2X", bgi:PlaySound "audio681"
    Case 23: DMDTextI "BONUS","3X", bgi:PlaySound "audio682"
    Case 24: DMDTextI "COLOSSAL","BONUS", bgi

    Case 25: DMDGif  "scene11.gif","EXTRA BALL","LIT",slen(11)
    Case 26: DMDGif  "scene11.gif","ROCK AGAIN!","",slen(11)
    Case 27: DMDGif  "scene45.gif","SUPER RAMPS","COMPLETED",slen(45)
    Case 28: DMDGif  "scene45.gif","SUPER BUMPERS","COMPLETED",slen(45)
    Case 29: DMDGif  "scene45.gif","SUPER SPINNER","COMPLETED",slen(45)
    Case 30: DMDGif  "scene45.gif","SUPER TARGETS","COMPLETED",slen(45)
    Case 31: DMDGif  "scene45.gif","SUPER RAMPS", (10-RampCnt(CurPlayer)) & " REMAINING",300
    Case 32: DMDGif  "scene45.gif","SUPER BUMPERS", (50-BumperCnt(CurPlayer)) & " REMAINING",300
    Case 33: DMDGif  "scene45.gif","SUPER SPINNER", (100-SpinCnt(CurPlayer)) & " REMAINING",300
    Case 34: DMDGif  "scene45.gif","SUPER TARGETS", (100-TargetCnt(CurPlayer)) & " REMAINING",300
  End Select
End Sub

Sub UDMDTimer_Timer
    If Not UltraDMD.IsRendering and NOT hsbModeActive Then
        'When the scene finishes rendering, then immediately display the scoreboard
        UDMDTimer.Enabled = False:UDMDTimer.interval=100 
        OnScoreboardChanged()
    End If
End Sub

Sub BV(val)
' only show dmd if timer is active so as not to overwhelm the display
DIM tstr,bstr
    if NOT useUDMD Then Exit Sub
    if bvtimer.enabled=False and svtimer.enabled=False and UltraDMD.IsRendering then Exit Sub  ' Some other animation is going on

    if RND*10 < 5 then
      tstr=string(INT(5*RND)," ") & ((bumpercolor(val)+1)*5) & "K"
    else
      tstr=((bumpercolor(val)+1)*5) & "K" & string(INT(5*RND)," ") 
    end if

    bstr=" "

    UDMDTimer.Enabled=False

    bvtimer.enabled=False
    bvtimer.interval=100
    bvtimer.enabled=True   
    debug.print "BV()"
    if CurScene <> "bv" then
      CurScene="bv"
      debug.print "BV Display scene TEST"
      UltraDMD.CancelRendering:UltraDMD.Clear
      if RND*10 < 5 then
      	  UltraDMD.DisplayScene00ExWithId "bvid", FALSE, "scene18.gif", tstr, 14, INT(RND*4)-1, bstr, -1, -1, UltraDMD_Animation_None, 500, UltraDMD_Animation_None
      else
    	  UltraDMD.DisplayScene00ExWithId "bvid", FALSE, "scene19.gif", bstr, -1, -1, tstr, 14, INT(RND*4)-1, UltraDMD_Animation_None, 500, UltraDMD_Animation_None
      end if
    Else
      debug.print "ModifyScene"
      if NOT UltraDMD.IsRendering then debug.print "bv is no longer rendering??"
      if RND*10 < 5 then
      	  UltraDMD.ModifyScene00 "bvid",  tstr, bstr
      else
    	  UltraDMD.ModifyScene00 "bvid",  bstr, tstr
      end if
    End if
    UDMDTimer.interval=500:UDMDTimer.Enabled = True
End Sub

Sub bvtimer_timer()
  if Not UltraDMD.IsRendering Then
    bvtimer.enabled=False
    debug.print "BV timer expired"
    If CurScene="bv" then 
      CurScene=""
    End If
    OnScoreboardChanged()
    UDMDTimer.interval=100:UDMDTimer.Enabled = True
  End if
End Sub

Sub SpinV(txt)  ' spinner video
Dim bstr,tstr
    if NOT useUDMD Then Exit Sub
    if svtimer.enabled=False and bvtimer.enabled=False and UltraDMD.IsRendering then Exit Sub  ' Some other animation is going on

    if RND*10 < 7 then
      tstr=string(INT(5*RND)," ") & txt
    else
      tstr=txt & string(INT(5*RND)," ") 
    end if

    bstr=" "

    if left(txt,7)="Spinner" then
      tstr="Spinner Count"
      bstr=SpinCnt(CurPlayer)
    End if
    UDMDTimer.Enabled=False

    svtimer.enabled=False
    svtimer.interval=100
    svtimer.enabled=True   
    debug.print "SV() >" & tstr & "<" & "[" & bstr & "]"
    if CurScene <> "sv" then
      UltraDMD.CancelRendering:UltraDMD.Clear
      CurScene="sv"
      debug.print "New Scene"
      if RND*10 < 5 then
    	  UltraDMD.DisplayScene00ExWithId "sv", FALSE, "scene45.gif", tstr, 14, INT(RND*4)-1, bstr, -1, -1, UltraDMD_Animation_None, 500, UltraDMD_Animation_None
      else
    	  UltraDMD.DisplayScene00ExWithId "sv", FALSE, "scene45.gif", bstr, -1, -1, tstr, 14, INT(RND*4)-1, UltraDMD_Animation_None, 500, UltraDMD_Animation_None
      end if
    Else
      debug.print "ModifyScene"
      if NOT UltraDMD.IsRendering then debug.print "sv is no longer rendering??"
      if RND*10 < 5 then
      	  UltraDMD.ModifyScene00 "sv",  tstr, bstr
      else
    	  UltraDMD.ModifyScene00 "sv",  bstr, tstr
      end if
    End if
    UDMDTimer.interval=500:UDMDTimer.Enabled = True
End Sub

Sub svtimer_timer()
  if Not UltraDMD.IsRendering Then
    svtimer.enabled=False
    debug.print "SV timer expired"
    If CurScene="sv" then 
      CurScene=""
    End If
    OnScoreboardChanged()
    UDMDTimer.interval=100:UDMDTimer.Enabled = True
  End if
End Sub

Sub OnScoreboardChanged()
  if NOT useUDMD Then Exit Sub
  If UltraDMD.IsRendering Then Exit Sub
  if BVTimer.Enabled=True then Exit Sub ' Lets show the Bumper Animations
  if SVTimer.Enabled=True then Exit Sub ' Lets show the Spinner Animations

  debug.print "OnScoreboardChanged()"
  if UltraDMD.GetMinorVersion > 3 then
    if CurPlayer = 0 or BallsRemaining(CurPlayer)=0 then
      UltraDMD.DisplayScoreboard00 PlayersPlayingGame, 0, Score(1),  Score(2),  Score(3),  Score(4), "credits " & Credits, ""
    else
      UltraDMD.DisplayScoreboard00 PlayersPlayingGame, CurPlayer, Score(1),  Score(2),  Score(3),  Score(4), "credits " & Credits, "ball " & BallsPerGame-BallsRemaining(CurPlayer)+1
    end if
  else
    if CurPlayer = 0 or BallsRemaining(CurPlayer)=0 then
      UltraDMD.DisplayScoreboard PlayersPlayingGame, 0, Score(1),  Score(2),  Score(3),  Score(4), "credits " & Credits, ""
    else
      UltraDMD.DisplayScoreboard PlayersPlayingGame, CurPlayer, Score(1),  Score(2),  Score(3),  Score(4), "credits " & Credits, "ball " & BallsPerGame-BallsRemaining(CurPlayer)+1
    end if
  End if
End Sub

Sub RandomScene()
  RScene(INT(RND*20)+1)
End Sub

Sub RScene(x)
  if NOT useUDMD Then Exit Sub
  If UltraDMD.IsRendering Then Exit Sub
  UDMDTimer.Enabled=False
  debug.print "RandomScene " & x
  Select Case x
    Case 1: DMDGif "scene14.gif","","",slen(14)
    Case 2: DMDGif "scene24.gif","","",slen(24)
    Case 3: DMDGif "scene25.gif","","",slen(25)
    Case 4: DMDGif "scene27.gif","","",slen(27)
    Case 5: DMDGif "scene28.gif","","",slen(28)
    Case 6: DMDGif "scene29.gif","","",slen(29)
    Case 7: DMDGif "scene30.gif","","",slen(30)
    Case 8: DMDGif "scene31.gif","","",slen(31)
    Case 9: DMDGif "scene32.gif","","",slen(32)
    Case 10: DMDGif "scene33.gif","","",slen(33)
    Case 11: DMDGif "scene35.gif","","",slen(35)
    Case 12: DMDGif "scene36.gif","","",slen(36)
    Case 13: DMDGif "scene45.gif","","",slen(45)
    Case 14: DMDGif "scene51.gif","","",slen(51)
    Case 15: DMDGif "scene69.gif","","",slen(69)
    Case 16: DMDGif "scene70.gif","","",slen(70)
    Case 17: DMDGif "scene71.gif","","",slen(71)
    Case 18: DMDGif "scene72.gif","","",slen(72)
    Case 19: DMDGif "scene73.gif","","",slen(73)
    Case 20: DMDGif "scene74.gif","","",slen(74)
  End Select
End Sub

Sub AttractMode_Timer
  Dim AttractSpacer,z, Y

  if Not UseUDMD then Exit Sub
  if UltraDMD.IsRendering then Exit Sub

  AttractMode.enabled=False
  UltraDMD.DisplayScene00 "black.bmp", "  GAME OVER  ", 15, "", -1, UltraDMD_Animation_None, 2000, UltraDMD_Animation_None
  'If LastScoreP1 <> 0 Then
      UltraDMD.DisplayScene00 "black.bmp", "Player #1", 15, LastScoreP1, 12, UltraDMD_Animation_None, 2000, UltraDMD_Animation_None
  'End If
  If LastScoreP2 <> 0 Then
      UltraDMD.DisplayScene00 "black.bmp", "Player #2", 15, LastScoreP2, 12, UltraDMD_Animation_None, 2000, UltraDMD_Animation_None
  End If
  If LastScoreP3 <> 0 Then
      UltraDMD.DisplayScene00 "black.bmp", "Player #3", 15, LastScoreP3, 12, UltraDMD_Animation_None, 2000, UltraDMD_Animation_None
  End If
  If LastScoreP4 <> 0 Then
      UltraDMD.DisplayScene00 "black.bmp", "Player #4", 15, LastScoreP4, 12, UltraDMD_Animation_None, 2000, UltraDMD_Animation_None
  End If
	
  UltraDMD.DisplayScene00 "black.bmp", "REPLAY AT", 15, "15000000", 15, UltraDMD_Animation_None, 2000, UltraDMD_Animation_None

  z = 12 - Len(HighScore(0)):AttractSpacer = ""
  For Y = 0 to z
	AttractSpacer = AttractSpacer & " "
  Next
  UltraDMD.DisplayScene00 "black.bmp", "Grand Champion", 10, HighScoreName(0) & AttractSpacer & HighScore(0), 15, 14, 2000, 14

  z = 12 - Len(HighScore(1)):AttractSpacer = ""
  For Y = 0 to z
	AttractSpacer = AttractSpacer & " "
  Next
  UltraDMD.DisplayScene00 "black.bmp", "HIGH SCORE #1", 10, HighScoreName(1) & AttractSpacer & HighScore(1), 15, 14, 2000, 14

  z = 12 - Len(HighScore(2)):AttractSpacer = ""
  For Y = 0 to z
	AttractSpacer = AttractSpacer & " "
  Next
  UltraDMD.DisplayScene00 "black.bmp", "HIGH SCORE #2", 10, HighScoreName(2) & AttractSpacer & HighScore(2), 15, 14, 2000, 14

  z = 12 - Len(HighScore(3)):AttractSpacer = ""
  For Y = 0 to z
	AttractSpacer = AttractSpacer & " "
  Next
  UltraDMD.DisplayScene00 "black.bmp", "HIGH SCORE #3", 10, HighScoreName(3) & AttractSpacer & HighScore(3), 15, 14, 2000, 14

  z = 12 - Len(HighCombo):AttractSpacer = ""
  For Y = 0 to z
	AttractSpacer = AttractSpacer & " "
  Next
  UltraDMD.DisplayScene00 "black.bmp", "COMBO CHAMPION", 10, HighComboName & AttractSpacer & HighCombo, 15, 14, 2000, 14

  UltraDMD.DisplayScene00 "scene16CROP.gif", "", 10, "", -1, 14, slen(16), UltraDMD_Animation_FadeOut

  UltraDMD.DisplayScene00 "scene100.gif", "", 10, "", -1, 14, slen(100), UltraDMD_Animation_FadeOut

  UltraDMD.DisplayScene00 "black.bmp", "PARTICIPATE IN LOCAL", 10, "TOURNAMENTS", 10, 14, 3500, 14

  UltraDMD.DisplayScene00 "sternadj.gif", "", 10, "", -1, 10, 1500, UltraDMD_Animation_FadeOut

  If bFreePlay = 1 OR Credits > 0 Then
       UltraDMD.DisplayScene00 "scene02.gif", "PRESS START", 10, "", 10, UltraDMD_Animation_None, slen(02), UltraDMD_Animation_FadeOut
  Else
       UltraDMD.DisplayScene00 "scene02.gif", "INSERT COIN", 10, "", 10, UltraDMD_Animation_None, slen(02), UltraDMD_Animation_FadeOut
  End If

  if credits > 0 then
       UltraDMD.DisplayScene00 "scene02.gif", "PRESS START", 15, "CREDITS " & credits, -1, UltraDMD_Animation_None, slen(02), UltraDMD_Animation_None
  end if
  AttractMode.interval=500:AttractMode.enabled=True
  debug.print "Queued the attract dmd"
End Sub

Dim MatchFN,MatchFlag
Sub CheckMatch()
   Dim X, XX, Y, Z, ZZ, abortLoop, tempSort, tmpScore
   Dim Match, divider	
   Dim fname
Playsound "audio44"
   Debug.print "CheckMatch()"
   UDMDTimer.Enabled=False
   match = INT(RND*10)
   matchFlag = 0
   for x=1 To (PlayersPlayingGame)	
        tmpScore=score(x)							'Break player's scores down into 2 digit numbers for match
	divider = 1000000000							'Divider starts at 1 billion
  	for xx=0 To 7								'Seven places will get us the last 2 digits of a 10 digit score		
		if (tmpScore >= divider) Then
			tmpScore = tmpScore MOD divider
		End If
		divider = Divider / 10						
	Next
	if (tmpScore = (Match * 10)) Then	'Did we match?	
		matchFlag  = matchFlag  +  1						'Count it up!
	End If
   Next
   MatchFN = CStr(Match) & "0"
  ' fname="match" & MatchFN & ".gif"
   debug.print fname
   if UseUDMD then
     UltraDMD.CancelRendering:UltraDMD.Clear
     DMDGif "scene06.gif","","",9500
   end if
   UDMDTimer.Enabled=False
   MatchTimer.Interval=9500
   MatchTimer.Enabled=True
End Sub

Sub MatchTimer_Timer
    debug.print "MatchTimer()"
    MatchTimer.Enabled=False
    Debug.print  ">" & MatchFN & "<"
   if UseUDMD then
     UltraDMD.CancelRendering:UltraDMD.Clear
     DMDGif "black.bmp","MATCH",MatchFN,3000
   end if
    if (matchFlag) Then										'Does one of the player's scores match?
        PlaySound "audio746",0,4  ' Match      
        PlaySound SoundFXDOF("fx_kicker",141,DOFPulse,DOFContactors), 0, 1, 0.1, 0.1
	credits  = credits  +  matchFlag								'Award a credit for each match!
    End If
    ' set the machine into game over mode
   MatchTimer2.Interval=3000
   MatchTimer2.Enabled=True
End Sub

Sub MatchTimer2_Timer
    MatchTimer2.Enabled=False
    debug.print "MatchTimer2"
    UDMDTimer.interval=200:UDMDTimer.Enabled=True 
    If INT(RND*10) > 5 then
       PlaySound "audio415"
    Else  
      If Int(RND*10) > 5 Then  
        PlaySound "audio416"
      Else 
        if Int(RND*10) > 5 Then
          PlaySound "audio427"
        End If
      End If
    End If 
    EndOfGame()
End Sub




Const BallSize = 50 ' 50 is the normal size
Const UseUDMD = True  ' FALSE is only for testing .. not fully functional as FALSE
Const bgi = "Black.bmp"  ' or just "" if you want the border around the dmd 


' *************************************
' Modify UltraDMD setttings here
' *************************************

UseFullColor = "True"              '    "True" / "False"
DMDColorSelect = "OrangeRed"        '     Rightclick on UDMD window to get full list of colours

DMDPosition = True                 '     Use Manual DMD Position, True / False
DMDPosX = 10                      '     Position in Decimal,  Set a value here if you want to have a table specific value eg. 2303	
DMDPosY = 10                       '     Position in Decimal

' Rightclick on UDMD window to get full list of colours
GetDMDColor


' Load the core.vbs for supporting Subs and functions
On Error Resume Next
  ExecuteGlobal GetTextFile("core.vbs")
  If Err Then MsgBox "Can't open core.vbs"
On Error Goto 0

' Define any Constants
Const cGameName  = "kiss_original_2016"
Const myVersion = "1.0.0"
Const MaxPlayers = 4
Const BallSaverTime = 15   '15 'in seconds
Const MaxMultiplier = 99 'no limit in this game
Const BallsPerGame = 3   ' 3 or 5

'load up the backglass and DOF
LoadEM


' Define Global Variables
Dim PlayersPlayingGame
Dim CurPlayer
Dim Credits
Dim Bonus
Dim BonusPoints(4)
Dim BonusMultiplier(4)
Dim BallsRemaining(4),LockedBalls(4)
Dim ExtraBallsAwards(4)
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)

Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim bAutoPlunger

' Define Game Control Variables
Dim LastSwitchHit
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
Dim bMusicOn
Dim bExtraBallWonThisBall
Dim PI: PI = Round(4 * Atn(1), 6)

Dim plungerIM 'used mostly as an autofire plunger
Dim ttable

Dim LoveGunMode, DemonMBMode,InDemonLock,InScoop,KISSTargetStack,ArmyTargetStack,FrontRowStack
Dim am, ModeScore, ModeInProgress, MBScore, ChooseSongMode
Dim Spincnt(4)    ' how many times the spinner has went around
Dim DemonMB(4)
Dim Shots(4,15)   ' Players/Songs
Dim CreditAwarded(4)
Dim BumperCnt(4),BumperColor(4), ComboCnt(4)
Dim RampCnt(4)
Dim LastShot(4)
Dim TargetCnt(4)  
Dim Instruments(4)
Dim CurSong(4), CurCity(4)
Dim SaveSong   ' keep track of track number while in MB
Dim CurScene
Dim HighCombo, HighComboName, LastScoreP1, LastScoreP2, LastScoreP3, LastScoreP4
Dim KISSBonus,ArmyBonus, HSMode, dd, FrontRowSave




' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    Dim i
    dd=10 ' just for debugging
    Randomize
    ' initalise the DMD display
    If UseUDMD then LoadUltraDMD   

    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 45 ' Plunger Power
    Const IMTime = 1.1        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFXDOF("fx_kicker",141,DOFPulse,DOFContactors), SoundFXDOF("fx_solenoid",141,DOFPulse,DOFContactors)
        .CreateEvents "plungerIM"
    End With

    Set ttable = New cvpmTurnTable
    With ttable
        .InitTurnTable Magnet1, 90
        .spinCW = False
        .MotorOn = True
        .CreateEvents "ttable"
    End With

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    'load saved values, highscore, names
    Loadhs

    'Init main variables
    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 0
        BonusMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        LockedBalls(i)=0
        ExtraBallsAwards(i) = 0
        CurCity(i)=1
        spincnt(i)=0    ' how many times the spinner has went around
        DemonMB(i)=0
        for xx=1 to 15
          Shots(i,xx)=0      ' How many shots achieved
        next
        BumperCnt(i)=0:RampCnt(i)=0:TargetCnt(i)=0
        BumperColor(i)=0   ' 1 per bumper not per player
        CurSong(i)=1
        ComboCnt(i)=0
        CreditAwarded(i)=False
    Next
    ResetBumpers()

    if UseUDMD Then  ' wait for the intro video to finish
       do while UltraDMD.isRendering
       loop     
       UltraDMD.clear
    End if


    CurScene=""

    ' freeplay or coins
    bFreePlay = False 'we want coins

    ' initialse any other flags
    ChooseSongMode = False
    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bBallSaverActive = False
    bBallSaverReady = False
    bMultiBallMode = False
    FrontRowSave = False
    bGameInPlay = False
	bAutoPlunger = False
    bMusicOn = True
    BallsOnPlayfield = 0
    BallsInLock = 0
    BallsInHole = 0
    LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    EndOfGame()
End Sub

Function LPad(s, l, c)
  Dim n : n = 0
'  debug.print "LPAD S="  & s & " l=" & l & " c=" & c
  If l > Len(s) Then n = l - Len(s)
  LPad = String(n, c) & s
End Function

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)
    If Keycode = AddCreditKey Then
        Credits = Credits + 1
        DOF 125, DOFOn
        PlaySound "fx_coin",0,1,0.25,0.2
        If NOT bGameInPlay then
          If(Tilted = False) Then
            AttractMode.enabled=False
			'OnScoreboardChanged()
            If UseUDMD then 
               UltraDMD.CancelRendering:UltraDMD.Clear
               UltraDMD.DisplayScene00 "scene02.gif", "PRESS START", 15, "CREDITS " & credits, -1, UltraDMD_Animation_None, 3500, UltraDMD_Animation_None
            End if
            AttractMode.interval=1500:AttractMode.enabled=True
          End If
        Else
			OnScoreboardChanged()
        End If
    End If

    If keycode = PlungerKey Then
'        PlungerIM.AutoFire
        Plunger.PullBack
		PlaySound "fx_plungerpull",0,1,0.25,0.2
    End If

    If bGameInPlay AND NOT Tilted Then

    If keycode = LeftTiltKey Then   Nudge 90,  6:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
    If keycode = RightTiltKey Then  Nudge 270, 6:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
    If keycode = CenterTiltKey Then Nudge 0,   7:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt

dim xxx,yyy
if keycode = LeftMagnaSave and 1=2 Then   
   debug.print "Enable debugging stuff"
  'i39.state=LightStateOn   ' ExtraBalls
  'i41.state=LightStateOn ' BackStagePass
  ' i44.state=LightStateOn ' New Track
   'i43.state=LightStateOn  ' Rock City
   'i40.state=LightStateOn  ' Kiss ArmyBonus
   ' i101.state=LightStateOn  ' starchild bumper
   ' i102.state=LightStateOn  ' starchild bumper
   ' i103.state=LightStateOn  ' starchild bumper
   ' i104.state=LightStateOn  ' starchild bumper
  ' i61.state=LightStateOn ' Super Bumper
   ' i95.state=LightStateOn ' Super Targets
    'i118.state=LightStateOn ' Super Ramps
   ' i122.state=LightStateOn ' Super Targets
   dd=dd+1:if dd>74 then dd=10
   xxx="scene" & lpad(dd,2,"0") & ".gif"
   'debug.print dd & " " & xxx
   'DMDGif xxx,dd,"",slen(dd)
   'DMDGif "","DONE","DONE",1000
   'DisplayI(dd)
    'RScene(dd)
    'spinv("5K")
    'SpinV "Spinner Count" &  SpinCnt(CurPlayer)
  '  BV(1)
    'msgbox "HighCombo=" & HighCombo  
    'ComboCnt(1)=HighCombo+1
    'DMDGif "frame508.jpg", "", "9" + "0             ", 1000
    'FlashForMs SmallFlasher2, 500, 50, 0
'    UltraDMD.DisplayScene00  BonusLights1, "", 10, "", 10, UltraDMD_Animation_None, 1100, UltraDMD_Animation_None
'    UltraDMD.DisplayScene00  "black.bmp", "", 10, "", 10, UltraDMD_Animation_None, 1000, UltraDMD_Animation_None 
  ' i17.state = LightStateOn
  ' i21.state = LightStateOn
  ' i25.state = LightStateOn
  'i9.state = LightStateOn:i13.state = LightStateOn:I106.state=LightStateOn:i107.state=LightStateOn:I108.state=LightStateOn:I109.state=LightStateOn:CheckBackStage()
  ' for yyy=1 to 4
   '  for xxx=1 to 15 
    '   debug.print yyy & " " & xxx & "=" & Shots(yyy,xxx)
    ' Next
   'next
end if
if keycode=RightMagnaSave and 1=2 Then
  i6.state=LightStateBlinking
end if

         if (keycode = RightMagnaSave or keycode=LeftMagnaSave) and ChooseSongMode then
           NewTrackTimer.enabled=False
  	       ChooseSongMode=False
           ScoopDelay.interval=200
           ScoopDelay.Enabled = True
           vpmtimer.addtimer 500, "FlashForMs FlasherExitHole, 1500, 30, 0 '"
        end if

        If keycode = LeftFlipperKey Then 
           SolLFlipper 1
  		   'Leftflipper.RotateToStart
           Leftflipper.TimerEnabled = 1  ' nFozzy Flipper Code
           Leftflipper.TimerInterval = 16
           Leftflipper.return = returnspeed * 0.5
        End If
        If keycode = RightFlipperKey Then 
           SolRFlipper 1
  		   'Rightflipper.RotateToStart
           Rightflipper.TimerEnabled = 1  ' nFozzy Flipper Code
           Rightflipper.TimerInterval = 16
           Rightflipper.return = returnspeed * 0.5
        End if

        If keycode = StartGameKey and NOT hsbModeActive Then
            If((PlayersPlayingGame < MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                Else
                    If(Credits > 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        If Credits < 1 Then DOF 125, DOFOff
                    Else
                        ' Not Enough Credits to start a game.
                        DMDFlush:debug.print "Flush 2"
                        PlaySound "audio749"   ' You got to pay to play
                        DMDTextPauseI "INSERT COIN", "CREDITS " & Credits, 1000, "scene02.gif"
                        PlaySound "audio749"
                    End If
                End If
                If UseUDMD Then UltraDMD.CancelRendering:UltraDMD.Clear
                OnScoreboardChanged()
            End if
        End If
        If keycode = LeftFlipperKey and bBallInPlungerLane and BallsOnPlayfield = 1 Then  
            debug.print "Change City"
            DMDFlush():debug.print "Flush 3"
            NextCity
        End If 
        If keycode = RightFlipperKey and ((bBallInPlungerLane and BallsOnPlayfield = 1) or ChooseSongMode) Then 
            debug.print "Change Song"
            DMDFlush():debug.print "Flush 4"
            if AutoPlungeTimer.enabled Then
               AutoPlungeTimer.enabled=False:AutoPlungeTimer.enabled=True
            End If
            NextSong()
        End If
        Else ' If (GameInPlay)
            If keycode = StartGameKey and NOT hsbModeActive Then
                If(bFreePlay = True) Then
                    If(BallsOnPlayfield = 0) Then
                        ResetForNewGame()
                    End If
                Else
                    If(Credits > 0) Then
                        If(BallsOnPlayfield = 0) Then
                            Credits = Credits - 1
                            if Credits < 1 Then DOF 125, DOFOff
                            ResetForNewGame()
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        DMDFlush:debug.print "Flush 5"
                        DMDTextPauseI "INSERT COIN", "CREDITS " & Credits, 1000, "scene02.gif"
                        PlaySound "audio749"
                    End If
                End If
            End If

    End If ' If (GameInPlay)

    If hsbModeActive Then UDMDTimer.Enabled=False:EnterHighScoreKey(keycode)

' Table specific
End Sub

' nFozzy Flipper Code
dim returnspeed, lfstep, rfstep
returnspeed = leftflipper.return
lfstep = 1
rfstep = 1

sub leftflipper_timer()
	select case lfstep
		Case 1: Leftflipper.return = returnspeed * 0.6 :lfstep = lfstep + 1
		Case 2: Leftflipper.return = returnspeed * 0.7 :lfstep = lfstep + 1
		Case 3: Leftflipper.return = returnspeed * 0.8 :lfstep = lfstep + 1
		Case 4: Leftflipper.return = returnspeed * 0.9 :lfstep = lfstep + 1
		Case 5: Leftflipper.return = returnspeed * 1 :lfstep = lfstep + 1
		Case 6: Leftflipper.timerenabled = 0 : lfstep = 1
	end select
end sub

sub rightflipper_timer()
	select case rfstep
		Case 1: Rightflipper.return = returnspeed * 0.6 :rfstep = rfstep + 1
		Case 2: Rightflipper.return = returnspeed * 0.7 :rfstep = rfstep + 1
		Case 3: Rightflipper.return = returnspeed * 0.8 :rfstep = rfstep + 1
		Case 4: Rightflipper.return = returnspeed * 0.9 :rfstep = rfstep + 1
		Case 5: Rightflipper.return = returnspeed * 1 :rfstep = rfstep + 1
		Case 6: Rightflipper.timerenabled = 0 : rfstep = 1
	end select
end sub

Sub SongPause_Timer
  SongPause.enabled=False
  debug.print "Playing Song" & Song
  playsound song, 0, MusicVol
End Sub

Sub Table1_MusicDone
  playsound song, 0, MusicVol  ' continue on with the Song
End Sub

Sub Table1_KeyUp(ByVal keycode)
    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then SolLFlipper 0
        If keycode = RightFlipperKey Then SolRFlipper 0
    End If
	If keycode = PlungerKey Then
		Plunger.Fire
		PlaySound "fx_plunger",0,1,0.25,0.25
	End If
End Sub

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                      'Called when table is nudged
    Tilt = Tilt + TiltSensitivity                  'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt > TiltSensitivity) AND(Tilt < 15) Then 'show a warning
        DisplayI(2)
    End if
    If Tilt > 15 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
        DMDFlush:debug.print "Flush 6"
        DisplayI(1)
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt > 0 Then
        Tilt = Tilt - 0.1
    Else
        Me.Enabled = False
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
        'turn off GI and turn off all the lights
        GiOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        Bumper1.Force = 0
        Bumper2.Force = 0
        Bumper3.Force = 0
        Bumper4.Force = 0

        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
        LeftSlingshot1.Disabled = 1
        RightSlingshot1.Disabled = 1
    Else
        'turn back on GI and the lights
        'GiOn
        Bumper1.Force = 6
        Bumper2.Force = 6
        Bumper3.Force = 6
        Bumper4.Force = 6
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        LeftSlingshot1.Disabled = 0
        RightSlingshot1.Disabled = 0
        'clean up the buffer display
        DMDFlush:debug.print "Flush 7"
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield = 0) Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        EndOfBall()
        Me.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub

'********************
' Music as wav sounds
'********************

Dim Song
Song = ""

Sub PlaySoundEffect()
End Sub

Sub PlayFanfare()
End Sub

' Ramp Sounds
Sub RHelp1_Hit()
    StopSound "fx_metalrolling"
    PlaySound "fx_ballrampdrop", 0, 1, AudioPan(ActiveBall)
End Sub

Sub RHelp2_Hit()
    StopSound "fx_metalrolling"
    PlaySound "fx_ballrampdrop", 0, 1, AudioPan(ActiveBall)
End Sub


' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i

    bGameInPLay = True

    'resets the score display, and turn off attrack mode
    StopAttractMode
    GiOn

    TotalGamesPlayed = TotalGamesPlayed + 1
    CurPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = True
    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 0
        BonusMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
        CurCity(i)=1
        spincnt(i)=0    ' how many times the spinner has went around
        DemonMB(i)=0
        For xx=1 to 15
          Shots(i,xx)=0      ' How many shots achieved
        Next
        BumperCnt(i)=0:RampCnt(i)=0:TargetCnt(i)=0
        BumperColor(i)=0   ' 1 per bumper not per player
        CurSong(i)=Int(RND*7)+1
        ComboCnt(i)=0
        CreditAwarded(i)=False
        LockedBalls(i)=0
    Next
    If UseUDMD Then UltraDMD.SetScoreboardBackgroundImage CurSong(CurPlayer) & ".png",15,13:UltraDMD.Clear:OnScoreboardChanged()


    ' initialise any other flags
    bMultiBallMode = False
    Tilt = 0

    ' initialise Game variables
    Game_Init()
    OnScoreboardChanged()

    ' you may wish to start some music, play a sound, do whatever at this point
    PlaySound "audio451",0, 0.5  ' Are You Ready?

    ' set up the start delay to handle any Start of Game Attract Sequence
    vpmtimer.addtimer 1500, "FirstBall '"
End Sub

' This is used to delay the start of a game to allow any attract sequence to
' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
    ' reset the table for a new ball
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
    CreateNewBall()
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()
    Dim i
    
    DMDTextPauseI  "PLAYER " & CurPlayer, "", 1000, bgi
    UDMDTimer.interval=500:UDMDTimer.Enabled=True

    ' make sure the correct display is upto date
   ' AddScore 0

    ' reset any drop targets, lights, game modes etc..
    Bonus = 0
    bExtraBallWonThisBall = False
    ResetNewBallLights()

    'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    ModeScore=0
  ' Raise the Targets 
  ' Stop the timers
    ArmyCombo.enabled=False
    KissCombo.enabled=False
    KissHurryUp.enabled=False
    ArmyHurryUp.enabled=False
    LoveGunMode=False:DemonMBMode=False
    InDemonLock=False
    InScoop=False
    KissTargetStack=0
    ArmyTargetStack=0
    FrontRowStack=0
    MBScore=0   ' total score during DemonMB
    ResetTargets()
    sctarget.isdropped=False
    BallsInLock=0

    For i = 1 to MaxPlayers
       instruments(i)=0
       LastShot(i)=-1  ' Track what was hit last
       ComboCnt(i)=0
    next

    ResetBumpers()
    ModeInProgress=False  
End Sub


' Create a new ball on the Playfield


Sub CreateNewBall()
    ' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedball BallSize / 2
    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
    PlaySound SoundFXDOF("fx_Ballrel",123,DOFPulse,DOFContactors), 0, 1, 0.1, 0.1
    BallRelease.Kick 90, 4

    ' if there is 2 or more balls then set the multibal flag (remember to check for locked balls and other balls used for animations)
	' set the bAutoPlunger flag to kick the ball in play automatically
    If BallsOnPlayfield > 1 Then
        bMultiBallMode = True
		bAutoPlunger = True
    Else  
      ' If ballsaver or front row then dont reset song and music
      If MusicFlag=False then
        StopSound song
        Song = track(CurSong(CurPlayer))
        InitMode(CurSong(CurPlayer))
        SongPause.interval=100
        SongPause.enabled=True
      End If
      MusicFlag=False
'      AutoPlungeTimer.interval=10000 ' 60s then autoplunge
'      AutoPlungeTimer.enabled=True
    End If
End Sub

Sub AutoPlungeTimer_Timer
  debug.print "About to autoplunge"
  If bBallInPlungerLane Then  
    PlungerIM.AutoFire
    debug.print "PLUNGE!"
  Else
    debug.print "Already plunged"
  End If
End Sub

' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table

Sub AddMultiball(nballs)
    mBalls2Eject = mBalls2Eject + nballs
    CreateMultiballTimer.Enabled = True
End Sub

' Eject the ball after the delay, AddMultiballDelay
Sub CreateMultiballTimer_Timer()
    ' wait if there is a ball in the plunger lane
    If bBallInPlungerLane Then
        Exit Sub
    Else
        CreateNewBall()
        mBalls2Eject = mBalls2Eject -1
        If mBalls2Eject = 0 Then 'if there are no more balls to eject then stop the timer
            Me.Enabled = False
        End If
    End If
End Sub


'   if BonusCnt=1 then PlaySound "audio297",0,0.3,0.25,0.25
'   if BonusCnt=2 then PlaySound "audio298",0,0.3,0.25,0.25
'   if BonusCnt=3 then PlaySound "audio299",0,0.3,0.25,0.25
'   if BonusCnt=4 then PlaySound "audio300",0,0.3,0.25,0.25

' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded
'
Dim SoundLoops

Sub EndOfBall()
    Dim BonusDelayTime
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False
    KissHurryUp.enabled=False
    ArmyHurryUp.enabled=False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)
    SaveShots()
    SaveState()
    If(Tilted = False) Then
        Dim AwardPoints, TotalBonus
        AwardPoints = 0:TotalBonus = 0

' add in any bonus points (multipled by the bonus multiplier)
      if MBScore <> 0 then
        DMDTextPauseI "Demon Total", MBScore, 1500, bgi
        BonusDelayTime = BonusDelayTime+1000
      end If 
      if ModeScore <> 0 then
        DMDTextPauseI "Song Total", ModeScore, 1500, bgi
        BonusDelayTime = BonusDelayTime+1000
      end if
  
      SoundLoops=0
      AwardPoints = i9.state * 50000 + i10.state * 50000 + i11.state * 50000 + i12.state * 50000
      if AwardPoints <> 0 then
        TotalBonus = TotalBonus + AwardPoints
        SoundLoops=SoundLoops+1
        'DMDText "KISS Bonus", AwardPoints
        UltraDMD.DisplayScene00  BonusLights1, "", 10, "", 10, UltraDMD_Animation_None, 2000, UltraDMD_Animation_None
      '  UltraDMD.DisplayScene00  "black.bmp", "", 10, "", 10, UltraDMD_Animation_None, 0, UltraDMD_Animation_None
        BonusDelayTime = BonusDelayTime+1800
      end if

      AwardPoints = i13.state * 50000 + i14.state * 50000 + i15.state * 50000 + i16.state * 50000
      debug.print "army" & AwardPoints
      if AwardPoints <> 0 then
        TotalBonus = TotalBonus + AwardPoints
        'DMDText "Army Bonus", AwardPoints
        SoundLoops=SoundLoops+1
        UltraDMD.DisplayScene00  BonusLights2, "", 10, "", 10, UltraDMD_Animation_None, 2000, UltraDMD_Animation_None
        'UltraDMD.DisplayScene00  "black.bmp", "", 10, "", 10, UltraDMD_Animation_None, 0, UltraDMD_Animation_None
        BonusDelayTime = BonusDelayTime+1800
      end if

      AwardPoints = i17.state * 150000 + i18.state * 150000 + i19.state * 50000 + i20.state * 150000
      if AwardPoints <> 0 then
        TotalBonus = TotalBonus + AwardPoints
'        DMDText "Band Bonus", AwardPoints
        SoundLoops=SoundLoops+1
        UltraDMD.DisplayScene00  BonusLights3, "", 10, "", 10, UltraDMD_Animation_None, 2000, UltraDMD_Animation_None
       ' UltraDMD.DisplayScene00  "black.bmp", "", 10, "", 10, UltraDMD_Animation_None, 0, UltraDMD_Animation_None
        BonusDelayTime = BonusDelayTime+1800
      end if

      AwardPoints = Instruments(CurPlayer) * 100000
      if AwardPoints <> 0 then
        TotalBonus = TotalBonus + AwardPoints
'        DMDText "Instrument Bonus", AwardPoints
        SoundLoops=SoundLoops+1
        UltraDMD.DisplayScene00  BonusLights4, "", 10, "", 10, UltraDMD_Animation_None, 2000, UltraDMD_Animation_None
      '  UltraDMD.DisplayScene00  "black.bmp", "", 10, "", 10, UltraDMD_Animation_None, 0, UltraDMD_Animation_None
        BonusDelayTime = BonusDelayTime+1800
      end if

      if BonusMultiplier(CurPlayer) <> 1 then
        DMDTextI "Total Bonus X " & BonusMultiplier(CurPlayer), TotalBonus, bgi
      Else 
        DMDTextPauseI "Total Bonus", TotalBonus, 4000, bgi
      end if
      TotalBonus = TotalBonus * BonusMultiplier(CurPlayer)

      if SoundLoops > 0 Then
        EOBSoundTimer.interval=2000:EOBSoundTimer.enabled=True
      End if

      debug.print "Sent the bonus stuff to the dmd queue"
        ' add a bit of a delay to allow for the bonus points to be shown & added up
      BonusDelayTime = BonusDelayTime+4000
      vpmtimer.addtimer BonusDelayTime, "Addscore TotalBonus '"
    Else
      'no bonus to count so move quickly to the next stage
      BonusDelayTime = 100
    End If
    ' start the end of ball timer which allows you to add a delay at this point
  vpmtimer.addtimer 1000,  "StopSound song'"
   Playsound "audio44"
vpmtimer.addtimer BonusDelayTime,  "StopSound audio44'"
    vpmtimer.addtimer BonusDelayTime, "EndOfBall2 '"
End Sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the CurPlayer)
'
Sub EOBSoundTimer_Timer
  EOBSoundTimer.enabled=False
  PlaySound "audio297Loop",SoundLoops+1,0.3,0.25,0.25
End Sub

Sub EndOfBall2()
    StopSound "audio297Loop"
    ' if were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful if we are changing player LOL
    Tilted = False
    Tilt = 0
    DisableTable False 'enable again bumpers and slingshots

    ' has the player won an extra-ball ? (might be multiple outstanding)
debug.print "Checking if We won extra ball"
    If(ExtraBallsAwards(CurPlayer) <> 0) Then
        debug.print "Extra Ball"
        DisplayI(15)

        ' yep got to give it to them
        ExtraBallsAwards(CurPlayer) = ExtraBallsAwards(CurPlayer) - 1
        ' Turn on BallSaver again
        bBallSaverReady = True

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurPlayer) = 0) Then
            LightRockAgain.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
         Select case int(RND*2)+1
            case 1: PlaySound "audio570"
            case 2: PlaySound "audio573"
         End Select

        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls
debug.print "No extra ball"
        BallsRemaining(CurPlayer) = BallsRemaining(CurPlayer) - 1

        ' was that the last ball ?
        If(BallsRemaining(CurPlayer) <= 0) Then
            debug.print "No More Balls, High Score Entry"
            UDMDTimer.Enabled=False
            ' Submit the CurPlayers score to the High Score system
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

    debug.print "EndOfBall - Complete"

    ' are there multiple players playing this game ?
    If(PlayersPlayingGame > 1) Then
        ' then move to the next player
        NextPlayer = CurPlayer + 1
        ' are we going from the last player back to the first
        ' (ie say from player 4 back to player 1)
        If(NextPlayer > PlayersPlayingGame) Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurPlayer
    End If

    debug.print "Next Player = " & NextPlayer

    ' is it the end of the game ? (all balls been lost for all players)
    If((BallsRemaining(CurPlayer) <= 0) AND(BallsRemaining(NextPlayer) <= 0) ) Then
        ' you may wish to do some sort of Point Match free game award here
        ' generally only done when not in free play mode
        UDMDTimer.Enabled=False
        if UseUDMD then UltraDMD.CancelRendering
        CheckMatch()


    ' you may wish to put a Game Over message on the desktop/backglass

       LastScoreP1=Score(1)
       LastScoreP2=Score(2)
       LastScoreP3=Score(3)
       LastScoreP4=Score(4)

    Else
        ' set the next player
        CurPlayer = NextPlayer

        ' make sure the correct display is up to date
        AddScore 0

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
        CreateNewBall()       
        OnScoreboardChanged()
    End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    debug.print "End Of Game"
    bGameInPLay = False

    ' just ended your game then play the end of game tune

    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0

    ' terminate all modes - eject locked balls
    ' set any lights for the attract mode
    GiOff
    StartAttractMode 1
' you may wish to light any Game Over Light you may have
End Sub

Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurPlayer) + 1
    If tmp > BallsPerGame Then
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
Dim MusicFlag   ' Set this to true if you dont want the music to stop 
MusicFlag=False
Sub Drain_Hit()
    ' Destroy the ball
    Drain.DestroyBall
    BallsOnPlayfield = BallsOnPlayfield - 1
    ' pretend to knock the ball into the ball storage mech
    PlaySound "fx_drain"

    debug.print "Show Saved Song (c) #" & SaveSong

    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True) AND(Tilted = False) Then

        ' is the ball saver active,
        MusicFlag=False
        If(bBallSaverActive = True or FrontRowSave=True) Then
            debug.print "bBallSaverActive = True"
            if FrontRowSave=True then
              FrontRowSave=False
              i6.state=LightStateOff
            end if
            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
            AddMultiball 1
            MusicFlag=True
            Select case int(RND*2)+1
               case 1: PlaySound "audio570"   'Lets do that again
               case 2: PlaySound "audio573"
            End Select
			' we kick the ball with the autoplunger
			bAutoPlunger = True
            ' you may wish to put something on a display or play a sound at this point
            if UseUDMD then UltraDMD.CancelRendering
            DisplayI(26)
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1) Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True) then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ChangeGi "white"
                    ' you may wish to change any music over at this point and
                    ' turn off any multiball specific lights
    debug.print "Show Saved Song (d) #" & SaveSong
                    ResetJackpotLights
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0) Then
                ' handle the end of ball (change player, high score entry etc..)
                if UseUDMD then UltraDMD.CancelRendering
                ' End Modes and timers
                EndOfBall()
                ChangeGi "white"
            End If
      End if
    End If
    FrontRowSave=False
End Sub

' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.

Sub swPlungerRest_Hit()
    debug.print "ball in plunger lane"
    debug.print "Show Saved Song (b) #" & SaveSong
    ' some sound according to the ball position
    PlaySound "fx_sensor", 0, 1, 0.15, 0.25
    bBallInPlungerLane = True
    ' turn on Launch light is there is one
    LaunchLight.State = 2
    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
        debug.print "autofire the ball"
        PlungerIM.AutoFire
		DOF 124, DOFPulse
		DOF 121, DOFPulse
		bAutoPlunger = False
    End If
    ' resync target lights in case we may have chose Hotter Than Hell
    debug.print "swPlungerRest - resetting lights"
    if cursong(CurPlayer)=4 then ' Hotter Than Hell means they need to flash on Reset 
      if sw38.isdropped=1 then i35.state=LightStateOn else i35.state=2 end if
      if sw39.isdropped=1 then i36.state=LightStateOn else i36.state=2 end if
      if sw40.isdropped=1 then i37.state=LightStateOn else i37.state=2 end if
      if sw41.isdropped=1 then i38.state=LightStateOn else i38.state=2 end if
    else
      if sw38.isdropped=1 then i35.state=LightStateOn else i35.state=LightStateOff end if
      if sw39.isdropped=1 then i36.state=LightStateOn else i36.state=LightStateOff end if
      if sw40.isdropped=1 then i37.state=LightStateOn else i37.state=LightStateOff end if
      if sw41.isdropped=1 then i38.state=LightStateOn else i38.state=LightStateOff end if
    end if

    If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
      ' Just start the flash and not the timer itself
      LightRockAgain.BlinkInterval = 160
      LightRockAgain.State = 2
    End If
    BallLooping.enabled=False ' dont trigger switches on the plunge
End Sub

' The ball is released from the plunger turn off some flags 

Sub swPlungerRest_UnHit()
    bBallInPlungerLane = False
	DOF 141, DOFPulse
	DOF 121, DOFPulse
    ' turn off LaunchLight
    LaunchLight.State = 0
FlashForMs Flasher10, 800, 50, 0
FlashForMs Flasher10a, 800, 50, 0
    AutoPlungeTimer.enabled=False ' No longer autoplunge after 30s
    ' if there is a need for a ball saver, then start off a timer
    ' only start if it is ready, and it is currently not running, else it will reset the time period
    If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
        EnableBallSaver BallSaverTime
    End If
    BallLooping.Interval=1400:BallLooping.enabled=True ' dont trigger switches on the loop
End Sub

Sub EnableBallSaver(seconds)
    debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
     ' start the timer
    BallSaverTimer.Interval = 1000 * seconds
    BallSaverTimer.Enabled = True
    BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
    BallSaverSpeedUpTimer.Enabled = True
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    LightRockAgain.BlinkInterval = 160
    LightRockAgain.State = 2
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag
'
Sub BallSaverTimer_Timer()
    debug.print "Ballsaver ended"
    BallSaverTimer.Enabled = False
    ' clear the flag
    bBallSaverActive = False
    ' if you have a ball saver light then turn it off at this point or turn on if you have ExtraBall earned
    If(ExtraBallsAwards(CurPlayer) = 0) Then  
       LightRockAgain.State = 0
       debug.print "dont relight the extra ball"
    End if
End Sub

Sub BallSaverSpeedUpTimer_Timer()
    debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    LightRockAgain.BlinkInterval = 80
    LightRockAgain.State = 2
End Sub

' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board
'
Sub AddScore(points)
    If(Tilted = False) Then
        ' add the points to the current players score variable
        Score(CurPlayer) = Score(CurPlayer) + points 
        If ModeInProgress then
          ModeScore=ModeScore+points
        End if
        If DemonMBMode Then
          MBScore=MBScore+points
        End if
        ' update the score displays
        OnScoreboardChanged()
    End if

' you may wish to check to see if the player has gotten a replay
    if Score(CurPlayer) > 15000000 and creditawarded(CurPlayer) = False then
        credits = credits + 1
        creditawarded(CurPlayer)=True
        PlaySound SoundFXDOF("fx_Knocker",122,DOFPulse,DOFKnocker)
        DisplayI(16)
        OnScoreboardChanged
    end if
End Sub


'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' tables walls and animations
Sub VPObjects_Init
  sw38.isDropped = False ' KISS Drop Targets
  sw39.isDropped = False
  sw40.isDropped = False
  sw41.isDropped = False
End Sub

' tables variables and modes init

Sub Game_Init()
    bExtraBallWonThisBall = False
    TurnOffPlayfieldLights()
    'Play some Music
    'Init Variables
    BallInHole = 0
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

Sub ResetNewBallLights()
    'LightArrow1.State = 2
    'LightArrow6.State = 2
    I106.state = 0 ' ARMY
    I107.state = 0
    I108.state = 0
    I109.state = 0
    i64.state=0:i65.state=0:i66.state=0:i67.state=0
    SetLightColor i115,"white", 0  'HurryUp
    SetLightColor i77, "white",0   'HurryUp

    SetLightColor i82, "orange", 2  ' Demon Lock Light
    SetLightColor i84, "orange", 2  ' Demon Lock Light
    SetLightColor i7, "white", 2   ' Light Bumper
    SetLightColor i8, "white", 2   ' Combo
    SetLightColor i33, "white", 2   ' Combo
    SetLightColor i97, "white", 2   ' Instrument

    'Grid
    SetLightColor i9,  "white",0 
    SetLightColor i10, "white",0 
    SetLightColor i11, "white",0 
    SetLightColor i12, "white",0 
    SetLightColor i13, "yellow",0 
    SetLightColor i14, "yellow",0 
    SetLightColor i15, "yellow",0 
    SetLightColor i16, "yellow",0 
    SetLightColor i17, "white",0 
    SetLightColor i18, "white",0
    SetLightColor i19, "white",0
    SetLightColor i20, "white",0
    SetLightColor i21, "white",0
    SetLightColor i22, "white",0
    SetLightColor i23, "white",0
    SetLightColor i24, "white",0

    SetLightColor i25, "white",0
    SetLightColor i26, "white",0
    SetLightColor i27, "white",0
    SetLightColor i29, "white",0
    SetLightColor i30, "white",0
'    SetLightColor i31, "white",0
End Sub


' *********************************************************************
'                        Table Object Hit Events

Sub sw47_hit ' star child Kicker1_Hits
   PlaySound "fx_hole-enter", 0, 1, -0.1
   If f116.state=1 then
     FlashForMs Flasher9, 1000, 50, 0
FlashForMs Flasher9a, 1000, 50, 0
     FlashForMs Flasher10, 1000, 50, 0
FlashForMs Flasher10a, 1000, 50, 0
     Start_LoveGun()
     f116.state=0    ' turn off StarChild Flasher
     'i100.state=0   ' right orbit  - might not want to turn this off
   'else
     DMDGif "scene51.gif","STARCHILD","",slen(51)
   End if

   Me.TimerInterval = 1500
   Me.TimerEnabled = 1
End Sub

Sub sw47_unhit
    PlaySound SoundFXDOF("fx_popper",120,DOFPulse,DOFContactors), 0, 1, -0.1, 0.25
    DOF 121, DOFPulse
End Sub


sub sw47_timer
   debug.print "Exit star hole"
   Me.TimerEnabled=False
   sw47.kick 225,6,1
end sub


Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySound SoundFXDOF("fx_slingshot",103,DOFPulse,DOFcontactors), 0, 1, -0.05, 0.05
	DOF 105, DOFPulse
    LeftSling4.Visible = 1:LeftSling1.Visible = 0
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add some points
    AddScore 110
    RandomScene()
FlashForMs Flasher11, 200, 50, 0
FlashForMs Flasher11a, 200, 50, 0
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1
        Case 3:LeftSLing2.Visible = 0:LeftSling1.Visible = 1:Gi2.State = 1:LeftSlingShot.TimerEnabled = False
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySound SoundFXDOF("fx_slingshot",104,DOFPulse,DOFcontactors), 0, 1, 0.05, 0.05
	DOF 106, DOFPulse
    RightSling4.Visible = 1:RightSling1.Visible = 0
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add some points
    AddScore 110
    RandomScene()
FlashForMs Flasher12, 200, 50, 0
FlashForMs Flasher12a, 200, 50, 0
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1
        Case 3:RightSLing2.Visible = 0:RightSLing1.Visible = 1:Gi1.State = 1:RightSlingShot.TimerEnabled = False 
    End Select
    RStep = RStep + 1
End Sub

Dim LStep1, RStep1

Sub LeftSlingShot1_Slingshot
    If Tilted Then Exit Sub
    PlaySound SoundFXDOF("fx_slingshot",143,DOFPulse,DOFContactors), 0, 1, -0.05, 0.05
    Sling3a.Visible = 1:Sling3.Visible = 0
    Lemk1.RotX = 26
    LStep1 = 0
    LeftSlingShot1.TimerEnabled = True
    ' add some points
    AddScore 510
    ' remember last trigger hit by the ball
    LastSwitchHit = "LeftSlingShot1"
' add some effect to the table?
FlashForMs Flasher9, 200, 50, 0
FlashForMs Flasher9a, 200, 50, 0
End Sub

Sub LeftSlingShot1_Timer
    Select Case LStep1
        Case 1:Sling3a.Visible = 0:Sling3b.Visible = 1:Lemk1.RotX = 14
        Case 2:Sling3b.Visible = 0:Sling3c.Visible = 1:Lemk.RotX = 0
        Case 3:Sling3c.Visible = 0:Sling3.Visible = 1:Lemk1.RotX = -20:LeftSlingShot1.TimerEnabled = False
    End Select
    LStep1 = LStep1 + 1
End Sub

Sub RightSlingShot1_Slingshot
    If Tilted Then Exit Sub
    PlaySound SoundFXDOF("fx_slingshot",144,DOFPulse,DOFContactors), 0, 1, 1, 0.05
    Sling2a.Visible = 1:Sling2.Visible = 0
    Remk1.RotX = 26
    RStep1 = 0
    RightSlingShot1.TimerEnabled = True
    ' add some points
    AddScore 510
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightSlingShot1"
' add some effect to the table?
FlashForMs Flasher9, 200, 50, 0
FlashForMs Flasher9a, 200, 50, 0
End Sub

Sub RightSlingShot1_Timer
    Select Case RStep1
        Case 1:Sling2a.Visible = 0:Sling2b.Visible = 1:Remk.RotX = 14
        Case 2:Sling2b.Visible = 0:Sling2c.Visible = 1:Remk.RotX = 0
        Case 3:Sling2c.Visible = 0:Sling2.Visible = 1:Remk1.RotX = -20:RightSlingShot1.TimerEnabled = False
    End Select
    RStep1 = RStep1 + 1
End Sub

Sub UpdateFlipperLogo_Timer
    LFLogo.RotY = LeftFlipper.CurrentAngle
    RFlogo.RotY = RightFlipper.CurrentAngle
End Sub


Dim BallInHole, dBall, dZpos

Sub sw43_Hit
   PlaySound "fx_hole-enter", 0, 1, -0.1, 0.25
   Set dBall = ActiveBall
   dZpos=35
   sw43.TimerInterval = 4
   sw43.TimerEnabled = 1
End Sub

Sub sw43_Timer
    dBall.Z = dZpos
    dZpos = dZpos-4
    if dZpos < -30 Then  
      sw43.timerenabled = 0
      BallInHole = BallInHole + 1
      sw43.DestroyBall

      vpmtimer.addtimer 700, "sw43HoleExit '"
    end if
End Sub



' SpinnerRod code from Cyperpez and http://www.vpforums.org/index.php?showtopic=35497
Sub CheckSpinnerRod_timer()
	SpinnerRod.TransZ = sin( (spinner.CurrentAngle+180) * (2*PI/360)) * 5
	SpinnerRod.TransX = -1*(sin( (spinner.CurrentAngle- 90) * (2*PI/360)) * 5)
End Sub


'**********
' Spinner
'**********

Sub Spinner_Spin()
    PlaySound "fx_spinner", 0, 1, 0.1
	DOF 136, DOFPulse
    If Tilted Then Exit Sub
    FlashForMs SmallFlasher2, 500, 50, 0
    if NOT i122.state=LightStateOff then ' Super Spinner
      debug.print "Spinner Cnt is " & SpinCnt(CurPlayer) 
      Spincnt(CurPlayer)=Spincnt(CurPlayer)+1
      AddScore 1000*SpinCnt(CurPlayer)
      if SpinCnt(CurPlayer) > 50 Then  
        i122.state=LightStateOff
        DisplayI(29) ' Completed
        SpinCnt(CurPlayer) = 0
      else
        SpinV "Spinner Count" &  SpinCnt(CurPlayer)
      end if   
    else
      SpinV "2K"
      AddScore 2000
    end if
End Sub

'********
' Bumper
'********

Sub Bumper1_Hit
    If Tilted Then Exit Sub
    PlaySound SoundFXDOF("fx_bumper",109,DOFPulse,DOFContactors), 0, 1, AudioPan(ActiveBall)
	DOF 138, DOFPulse
    RandomBD() ' If BD then move shot
    if NOT i61.state=LightStateOff then ' Super Bumpers score 200K for upto 50Hits
      BumperCnt(CurPlayer)=BumperCnt(CurPlayer)+1
      if BumperCnt(CurPlayer)=50 then 
        DisplayI(28)
        AddScore(200000)
        i61.state=LightStateOff
      else
        if BumperCnt(CurPlayer) < 50 then
          AddScore(200000)
          if rnd*10 > 4 then 
            UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("200k",INT(RND*8)+3," "), 15, 14, 100, 14
          Else  
            DisplayI(32)
          end if
        Else  
          AddScore(50000)
          UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("50k",INT(RND*8)+3," "), 15, 14, 100, 14
        End If
      End if
    Else
      AddScore 5000 * (BumperColor(1) +1)
      bv(1)
    End If
End Sub

Sub Bumper2_Hit
    If Tilted Then Exit Sub
    PlaySound SoundFXDOF("fx_bumper",110,DOFPulse,DOFContactors), 0, 1, AudioPan(ActiveBall)
	DOF 140, DOFPulse
    RandomBD() ' If BD then move shot
    if NOT i61.state=LightStateOff then ' Super Bumpers score 200K for upto 50Hits
      BumperCnt(CurPlayer)=BumperCnt(CurPlayer)+1
      if BumperCnt(CurPlayer)=50 then 
        DisplayI(28)
        AddScore(200000)
        i61.state=LightStateOff
      else
        if BumperCnt(CurPlayer) < 50 then
          AddScore(200000)
          if rnd*10 > 4 then 
            UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("200k",INT(RND*8)+3," "), 15, 14, 100, 14
          Else  
            DisplayI(32)
          end if
        Else  
          AddScore(50000)
          UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("50k",INT(RND*8)+3," "), 15, 14, 100, 14
        End If
      End if
    Else
      AddScore 5000 * (BumperColor(2) +1)
      bv(2)
    End If
End Sub

Sub Bumper3_Hit
    If Tilted Then Exit Sub
    PlaySound SoundFXDOF("fx_bumper",107,DOFPulse,DOFContactors), 0, 1, AudioPan(ActiveBall)
	DOF 137, DOFPulse
    RandomBD() ' If BD then move shot
    if NOT i61.state=LightStateOff then ' Super Bumpers score 200K for upto 50Hits
      BumperCnt(CurPlayer)=BumperCnt(CurPlayer)+1
      if BumperCnt(CurPlayer)=50 then 
        DisplayI(28)
        AddScore(200000)
        i61.state=LightStateOff
      else
        if BumperCnt(CurPlayer) < 50 then
          AddScore(200000)
          if rnd*10 > 4 then 
            UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("200k",INT(RND*8)+3," "), 15, 14, 100, 14
          Else  
            DisplayI(32)
          end if
        Else  
          AddScore(50000)
          UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("50k",INT(RND*8)+3," "), 15, 14, 100, 14
        End If
      End if
    Else
      AddScore 5000 * (BumperColor(3) +1)
      bv(3)
    End if
End Sub

Sub Bumper4_Hit
    If Tilted Then Exit Sub
    PlaySound SoundFXDOF("fx_bumper",108,DOFPulse,DOFContactors), 0, 1, AudioPan(ActiveBall)
	DOF 139, DOFPulse
    RandomBD() ' If BD then move shot
    if NOT i61.state=LightStateOff then ' Super Bumpers score 200K for upto 50Hits
      BumperCnt(CurPlayer)=BumperCnt(CurPlayer)+1
      if BumperCnt(CurPlayer)=50 then 
        DisplayI(28)
        AddScore(200000)
        i61.state=LightStateOff
      else
        if BumperCnt(CurPlayer) < 50 then
          AddScore(200000)
          if rnd*10 > 4 then 
            UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("200k",INT(RND*8)+3," "), 15, 14, 100, 14
          Else  
            DisplayI(32)
          end if
        Else  
          AddScore(50000)
          UltraDMD.DisplayScene00 "scene19.gif", LPad("",INT(RND*3)," "), 10, Lpad("50k",INT(RND*8)+3," "), 15, 14, 100, 14
        End If
      End if
    Else
      AddScore 5000 * (BumperColor(4) +1)
      bv(4)
    End If
End Sub

Sub ResetBumpers()
Dim i
    BumperCnt(CurPlayer) = 100
    Flasher1.Visible = 0
    Flasher2.Visible = 0
    Flasher3.Visible = 0
    Flasher4.Visible = 0
    SetLightColor B1L, "white", 0
    SetLightColor B2L, "white", 0
    SetLightColor B3L, "white", 0
    SetLightColor B4L, "white", 0
    for i = 1 to 4
      BumperColor(i)=0
    next
End Sub

Sub AwardExtraBall()
  debug.print "AwardExtraBall..."
'    If NOT bExtraBallWonThisBall Then
	    DOF 121, DOFPulse
        ExtraBallsAwards(CurPlayer) = ExtraBallsAwards(CurPlayer) + 1
        bExtraBallWonThisBall = True
        DisplayI(15)
        GiEffect 1
  '      LightEffect 2
'    END If
     debug.print "XBalls=" & ExtraBallsAwards(CurPlayer)
End Sub


Sub EffectTrigger1_Hit()
    FlashForMs Flasher11, 600, 50, 0
FlashForMs Flasher11a, 600, 50, 0
End Sub

Sub EffectTrigger2_Hit()
    FlashForMs Flasher12, 600, 50, 0
FlashForMs Flasher12a, 600, 50, 0
End Sub
'============================
sub flippertimer_timer()
		FlipperLSh.RotZ = LeftFlipper.currentangle
		FlipperRSh.RotZ = RightFlipper.currentangle
End Sub
