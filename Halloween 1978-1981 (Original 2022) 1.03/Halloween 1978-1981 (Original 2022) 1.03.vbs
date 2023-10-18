'
'                       _ _    John Carpenter's        __ 
'            /\  /\__ _| | | _____      _____  ___  /\ \ \
'           / /_/ / _` | | |/ _ \ \ /\ / / _ \/ _ \/  \/ /
'          / __  / (_| | | | (_) \ V  V /  __/  __/ /\  / 
'          \/ /_/ \__,_|_|_|\___/ \_/\_/ \___|\___\_\ \/  
'
'                  By HiRez00, Apophis, and Xenonph
'
' Based on Medusa - Bally 1981
' http://www.ipdb.org/machine.cgi?id=1565
' Medusa / IPD No. 1565 / February 04, 1981 / 4 Players
' Original Medus VPX version by JPSalas 2017, version 1.0.4
' Uses the Left and Right Magna saves keys to activate the save post (gun)


Option Explicit
Randomize
Dim colornow, CDMD


'   USER OPTIONS
'***************************************************
'
' - Change LUTs buy holding down left MagnaSaves and then pressing right MagnaSaves
'   You should adjust the LUT before playing an actual game because MagnaSaves are used for 'Shoot Gun' post during gameplay.
' - Change the general illumination lights color by pressing the Extra Ball Buy-in button (or the number 2 button)
' - View a zoomed in Rules Card by pressing right MagnaSaves button before starting a game.
'

'----- General Sound Options -----
Const VolumeDial = 0.8				'Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Const BallRollVolume = 0.4 			'Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 0.7 			'Level of ramp rolling volume. Value between 0 and 1

'----- Game Sound Option -----
Const MusicVol = 0.8                'Level of music volume. Value between 0 and 1
Const CalloutVol = 0.8              'Level of callout volume. Value between 0 and 1

'- Simulated ROM Sound Option -
Const ROMSim = 1                    '0 = Off, 1 = On (Simulates 80's Rom Sound Beeping When Score Increases)

'----- GI Color Options ----- 
colornow = 2    					'0=Normal,1=Blue,2=Orange (Use extra ball buying button to change while in game - only the value set here is saved.)

'----- GI Dimming Option ----- 
Const GIDim = 1 					'0=Normal GI lights behavior, 1=Dim GI with flipper current draw

'----- ColorDMD --------
CDMD = 4   							'0=Yellow,  1=Red,  2=White,  3=Blue,  4=Reg Orange

'----- Flipper Colors --------
Const TopFlipperColor = 1			'1=White,  2=White No Text, 3=Orange, 4=Halloween
Const BottomFlipperColor = 1		'1=White and Black, 2=White and Orange, 3=Halloween

'----- Spinner Style --------
Const SpinnerStyle = 1			    '1=Halloween II Logo, 2=Halloween Logo, 3=The Shape (MM Mask)

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									'2 = flasher image shadow, but it moves like ninuzzu's

'----- ROM Option ------- So as to not interfere with existing Medusa VPX table settings -----
Const cGameName = "medusa"          ' Regular Version
'Const cGameName = "medusaa" 		' Free Play


'    END USER OPTIONS
'***************************************************


'****** PuP Variables ******

Dim usePUP: Dim cPuPPack: Dim PuPlayer: Dim PUPStatus: PUPStatus=false ' dont edit this line!!!

'*************************** PuP Settings for this table ********************************

usePUP   = false               ' enable Pinup Player functions for this table
cPuPPack = "halloween"    ' name of the PuP-Pack / PuPVideos folder for this table

'//////////////////// PINUP PLAYER: STARTUP & CONTROL SECTION //////////////////////////

' This is used for the startup and control of Pinup Player

Sub PuPStart(cPuPPack)
    If PUPStatus=true then Exit Sub
    If usePUP=true then
        Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")
        If PuPlayer is Nothing Then
            usePUP=false
            PUPStatus=false
        Else
            PuPlayer.B2SInit "",cPuPPack 'start the Pup-Pack
            PUPStatus=true
        End If
    End If
End Sub

Sub pupevent(EventNum)
    if (usePUP=false or PUPStatus=false) then Exit Sub
    PuPlayer.B2SData "E"&EventNum,1  'send event to Pup-Pack
End Sub

'************ PuP-Pack Startup **************

PuPStart(cPuPPack) 'Check for PuP - If found, then start Pinup Player / PuP-Pack

'********************************************

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0


If colornow=1 Then Dim w:For each w in aGiLights:w.color=RGB(0,0,255):w.intensity=40:gi47.intensity=10:gi48.intensity=10:gi49.intensity=10:gi60.intensity=10:gi61.intensity=10:Next
If colornow=2 Then Dim y:For each y in aGiLights:y.color=RGB(255,0,0):y.intensity=40:gi47.intensity=10:gi48.intensity=10:gi49.intensity=10:gi60.intensity=10:gi61.intensity=10:Next
If colornow=3 Then Dim z:For each z in aGiLights:z.color=RGB(255,255,155):z.intensity=25:gi47.intensity=10:gi48.intensity=10:gi49.intensity=10:gi60.intensity=10:gi61.intensity=10:Next


Dim TotalColor
TotalColor=3

Sub colorgi
	colornow = colornow + 1
	If colornow=1 Then Dim x:For each x in aGiLights:x.color=RGB(0,0,255):x.intensity=40:gi47.intensity=10:gi48.intensity=10:gi49.intensity=10:gi60.intensity=10:gi61.intensity=10:Next:Exit Sub
	If colornow=2 Then Dim y:For each y in aGiLights:y.color=RGB(255,0,0):y.intensity=40:gi47.intensity=10:gi48.intensity=10:gi49.intensity=10:gi60.intensity=10:gi61.intensity=10:Next:Exit Sub
	If colornow=3 Then Dim z:For each z in aGiLights:z.color=RGB(255,255,155):z.intensity=25:gi47.intensity=10:gi48.intensity=10:gi49.intensity=10:gi60.intensity=10:gi61.intensity=10:Next
	If colornow = TotalColor Then colornow = 0
End Sub

Const RampSpeedFactor = 0.5 
Const SaucerKickDelay = 500 

Const BallSize = 50
Const BallMass = 1

Const tnob = 2 ' total number of balls
Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height

LoadVPM "01550000", "Bally.vbs", 3.26

Dim bsTrough ', dtRBank, dtTBank ,bsSaucer
Dim x
Dim ScoreSound
Dim prevHDR, prevHTLK, prevHTRK, prevHOLTD, prevHTAL, prevHTAR, prevEndMusic


Const UseSolenoids = 0
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0
Const HandleMech = 0

Dim VarHidden
If Table1.ShowDT = true then
    VarHidden = 1
else
    VarHidden = 0
    For each x in aReels
        x.Visible = 0
    Next
    lrail.Visible = 0
    rrail.Visible = 0
    gi62.Visible = 0
    gi63.Visible = 0
    gi64.Visible = 0
    gi65.Visible = 0
    gi66.Visible = 0
    gi67.Visible = 0
    gi68.Visible = 0
    gi69.Visible = 0
    gi70.Visible = 0
    gi71.Visible = 0
    gi72.Visible = 0
    gi73.Visible = 0
    gi74.Visible = 0
    gi75.Visible = 0
    gi76.Visible = 0
    gi77.Visible = 0
    gi78.Visible = 0
    gi79.Visible = 0
    gi80.Visible = 0
    gi81.Visible = 0
    gi82.Visible = 0
    gi83.Visible = 0
    gi84.Visible = 0
    gi85.Visible = 0
    gi86.Visible = 0
    gi87.Visible = 0
    gi88.Visible = 0
    gi89.Visible = 0
    gi90.Visible = 0
    gi91.Visible = 0
    gi92.Visible = 0
    gi93.Visible = 0
    gi94.Visible = 0
    gi95.Visible = 0
    l11.Visible = 0
    l13.Visible = 0
    l27.Visible = 0
    l29.Visible = 0
    l45.Visible = 0
    l61.Visible = 0
end if

Set MotorCallback = GetRef("UpdateSolenoids")

' Standard Sounds
Const SSolenoidOn = ""
Const SSolenoidOff = ""
Const SCoin = ""

'Flipper primitive texture
Select Case BottomFlipperColor
	Case 1: LFLogo.image = "Hbatwhiteblack" : RFLogo.image = "Hbatwhiteblack"
	Case 2: LFLogo.image = "Hbatwhiteorange" : RFLogo.image = "Hbatwhiteorange"
	Case Else: LFLogo.image = "Hbatorange" : RFLogo.image = "Hbatorange"
End Select

Select Case TopFlipperColor
	Case 1: 
		LFlip2.image = "FL-Normal" : LFlipR2.image = "FL-Normal" : LFlip3.image = "FL-Normal" : LFlipR3.image = "FL-Normal"
		RFlip2.image = "FR-Normal" : RFlipR2.image = "FR-Normal" : RFlip3.image = "FR-Normal" : RFlipR3.image = "FR-Normal"
	Case 2: 
		LFlip2.image = "FL-No-Text" : LFlipR2.image = "FL-No-Text" : LFlip3.image = "FL-No-Text" : LFlipR3.image = "FL-No-Text"
		RFlip2.image = "FR-No-Text" : RFlipR2.image = "FR-No-Text" : RFlip3.image = "FR-No-Text" : RFlipR3.image = "FR-No-Text"
	Case 3: 
		LFlip2.image = "FL-Orange" : LFlipR2.image = "FL-Orange" : LFlip3.image = "FL-Orange" : LFlipR3.image = "FL-Orange"
		RFlip2.image = "FR-Orange" : RFlipR2.image = "FR-Orange" : RFlip3.image = "FR-Orange" : RFlipR3.image = "FR-Orange"
	Case Else: 
		LFlip2.image = "FL-Halloween-O" : LFlipR2.image = "FL-Halloween-O" : LFlip3.image = "FL-Halloween-O" : LFlipR3.image = "FL-Halloween-O"
		RFlip2.image = "FR-Halloween-O" : RFlipR2.image = "FR-Halloween-O" : RFlip3.image = "FR-Halloween-O" : RFlipR3.image = "FR-Halloween-O"
End Select

Select Case SpinnerStyle
	Case 1: 
		sw33.image = "h-spinner-1"
    Case 2:
		sw33.image = "h-spinner-2"
    Case Else:
		sw33.image = "h-spinner-3"
End Select


Dim bFlippersEnabled
Dim PlungerIM ' autofire plunger
Dim MBS

'************
' Table init.
'************

Sub table1_Init
    vpmInit me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Halloween (Original) v1.0" & vbNewLine & "VPX table by HiRez00 Apophis and Xenonph"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = VarHidden
        If CDMD=0 Then Controller.Games("medusa").Settings.Value("dmd_red") = 255:Controller.Games("medusa").Settings.Value("dmd_green") = 255:Controller.Games("medusa").Settings.Value("dmd_blue") = 0
        If CDMD=1 Then Controller.Games("medusa").Settings.Value("dmd_red") = 255:Controller.Games("medusa").Settings.Value("dmd_green") = 0:Controller.Games("medusa").Settings.Value("dmd_blue") = 0
        If CDMD=2 Then Controller.Games("medusa").Settings.Value("dmd_red") = 255:Controller.Games("medusa").Settings.Value("dmd_green") = 255:Controller.Games("medusa").Settings.Value("dmd_blue") = 255
        If CDMD=3 Then Controller.Games("medusa").Settings.Value("dmd_red") = 0:Controller.Games("medusa").Settings.Value("dmd_green") = 0:Controller.Games("medusa").Settings.Value("dmd_blue") = 255 
        If CDMD=4 Then Controller.Games("medusa").Settings.Value("dmd_red") = 255:Controller.Games("medusa").Settings.Value("dmd_green") = 69:Controller.Games("medusa").Settings.Value("dmd_blue") = 0
         Controller.Games("medusa").Settings.Value("sound")=0
         Controller.Games("medusa").Settings.Value("samples")=1
        '.Games(cGameName).Settings.Value("rol") = 0 '1= rotated display, 0= normal
        '.SetDisplayPosition 0,0, GetPlayerHWnd 'restore dmd window position
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
        LoadLUT
        AttractMusic
    End With

    ' Nudging
    vpmNudge.TiltSwitch = 15
    vpmNudge.Sensitivity = 5
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, Bumper4, LeftSlingshot, RightSlingshot, sw34)

    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0, 8, 0, 0, 0, 0, 0, 0
        .InitKick BallRelease, 80, 6
        .Balls = 1
        .IsTrough = True
    End With

	'Impulse Plunger as autoplunger
	Const IMPowerSetting = 50 ' Plunger Power
	Const IMTime = 1.1        ' Time in seconds for Full Plunge
	Set plungerIM = New cvpmImpulseP
	With plungerIM
		.InitImpulseP swPlungerIM, IMPowerSetting, IMTime
		.Random 1.5
		.InitExitSnd SoundFX("Saucer_Kick", DOFContactors), SoundFX("Relay_On", DOFContactors)
		.CreateEvents "plungerIM"
	End With

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1:

    ' init shield
    post2.IsDropped = 1
    post2rubber.visible = 0
    Post.Pullback

	MoveDiverter 0 'Close the diverter
	bFlippersEnabled = False
	LMultiballReady.state = 0

	'Apron Cards
	ReadNumGameBalls
	Select Case NumBallsPerGame
		Case 3: apron3.image = "Apron-3-Balls"
		Case 5: apron3.image = "Apron-5-Balls"
		Case Else: apron3.image = "Apron-5-Balls"
	End Select



	SetDefaultDips

End Sub

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub


'***************************
'Frame Timer

Sub FrameTimer_Timer()
	diverter.rotz = DiverterFlipper.currentangle
	LFLogo.RotZ = LeftFlipper.CurrentAngle
	RFlogo.RotZ = RightFlipper.CurrentAngle
	FlipperLSh.RotZ = LeftFlipper.CurrentAngle
	FlipperRSh.RotZ = RightFlipper.CurrentAngle
	LFlip2.RotZ = LeftFlipper2.CurrentAngle
	LFlipR2.RotZ = LeftFlipper2.CurrentAngle
	LFlip3.RotZ = LeftFlipper3.CurrentAngle
	LFlipR3.RotZ = LeftFlipper3.CurrentAngle
	RFlip2.RotZ = RightFlipper2.CurrentAngle
	RFlipR2.RotZ = RightFlipper2.CurrentAngle
	RFlip3.RotZ = RightFlipper3.CurrentAngle
	RFlipR3.RotZ = RightFlipper3.CurrentAngle
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
End Sub

'***************************
'Attract Music

Dim AttMusicNum
Sub AttractMusic
    If AttMusicNum = 0 then PlayMusic"Halloween/AH01.mp3",MusicVol: End If
    If AttMusicNum = 1 then PlayMusic"Halloween/AH02.mp3",MusicVol: End If
    If AttMusicNum = 2 then PlayMusic"Halloween/AH03.mp3",MusicVol: End If
    If AttMusicNum = 3 then PlayMusic"Halloween/AH04.mp3",MusicVol: End If
    If AttMusicNum = 4 then PlayMusic"Halloween/AH05.mp3",MusicVol: End If
    If AttMusicNum = 5 then PlayMusic"Halloween/AH06.mp3",MusicVol: End If

    AttMusicNum = (AttMusicNum + 1) mod 6
End Sub

Sub Table1_MusicDone
    If Controller.lamp(45) = True then AttractMusic: End If
    If Controller.lamp(45) = False then BallNumberMusic: End If
End Sub


'***************************
' Plunger Timers

Sub BallNumberMusic
    Dim bn1,bn2,bn3,bn4,bn5
    If NumBallsPerGame = 3 then
      If BallNumber = 1 then
        bn1 = INT(3 * RND(1) )
        If bn1=0 then PlayMusic"Halloween/HBA01.mp3",MusicVol: End If
		If bn1=1 then PlayMusic"Halloween/HBA01a.mp3",MusicVol: End If
		If bn1=2 then PlayMusic"Halloween/HBA01b.mp3",MusicVol: End If
      End If
      If BallNumber = 2 then
        bn2 = INT(5 * RND(1) )
        If bn2=0 then PlayMusic"Halloween/HBA02.mp3",MusicVol: End If
		If bn2=1 then PlayMusic"Halloween/HBA02a.mp3",MusicVol: End If
        If bn2=2 then PlayMusic"Halloween/HBA02b.mp3",MusicVol: End If
        If bn2=3 then PlayMusic"Halloween/HBA02c.mp3",MusicVol: End If
        If bn2=4 then PlayMusic"Halloween/HBA02d.mp3",MusicVol: End If
        If bn2=5 then PlayMusic"Halloween/HBA02e.mp3",MusicVol: End If
      End If
      If BallNumber = 3 then
        bn3 = INT(2 * RND(1) )
        If bn3=0 then PlayMusic"Halloween/HBA03.mp3",MusicVol: End If
		If bn3=1 then PlayMusic"Halloween/HBA03a.mp3",MusicVol: End If
      End If
     End If
    If NumBallsPerGame = 5 then
      If BallNumber = 1 then
        bn1 = INT(3 * RND(1) )
        If bn1=0 then PlayMusic"Halloween/HBA01.mp3",MusicVol: End If
		If bn1=1 then PlayMusic"Halloween/HBA01a.mp3",MusicVol: End If
		If bn1=2 then PlayMusic"Halloween/HBA01b.mp3",MusicVol: End If
      End If
      If BallNumber = 2 then
        bn2 = INT(2 * RND(1) )
        If bn2=0 then PlayMusic"Halloween/HBA02.mp3",MusicVol: End If
		If bn2=1 then PlayMusic"Halloween/HBA02a.mp3",MusicVol: End If
      End If
       If BallNumber = 3 then
        bn3 = INT(2 * RND(1) )
        If bn3=0 then PlayMusic"Halloween/HBA02b.mp3",MusicVol: End If
        If bn3=1 then PlayMusic"Halloween/HBA02c.mp3",MusicVol: End If
      End If
       If BallNumber = 4 then
        bn4 = INT(2 * RND(1) )
        If bn4=0 then PlayMusic"Halloween/HBA02d.mp3",MusicVol: End If
        If bn4=1 then PlayMusic"Halloween/HBA02e.mp3",MusicVol: End If
      End If
      If BallNumber = 5 then
        bn5 = INT(2 * RND(1) )
        If bn5=0 then PlayMusic"Halloween/HBA03.mp3",MusicVol: End If
		If bn5=1 then PlayMusic"Halloween/HBA03a.mp3",MusicVol: End If
      End If
     End If

end sub

'************************
Sub StopSounds()
     StopSound"HTA 7"
     StopSound"HTA 8"
     StopSound"HTA 9"
     StopSound"HTA 10"
     StopSound"HTA 11"
     StopSound"HTA 12"
     StopSound"HTA 13"
     StopSound"HTA 14"
     StopSound"HTA 15"
     StopSound"HTA 16"
     StopSound"HTA 17"
     StopSound"HTA 18"
     StopSound"HTA 19"
     StopSound"HTA 20"
     StopSound"HTA 21"
     StopSound"HTA 22"
     StopSound"HTA 23"
     StopSound"HTA 24"
     StopSound"HTA 25"
     StopSound"HTA 26"
     StopSound"HTA 27"
     StopSound"HTA 28"
     StopSound"HTA 29"
     StopSound"HTA 30"
     StopSound"HTA 31"
     StopSound"HTA 32"
     StopSound"HTA 33"
     StopSound"HTA 34"
     StopSound"HTA 35"
     StopSound"HTLK 1"
     StopSound"HTLK 2"
     StopSound"HTLK 3"
     StopSound"HTLK 4"
     StopSound"HTLK 5"
     StopSound"HTLK 6"
     StopSound"HTLK 7"
     StopSound"HTLK 8"
     StopSound"HTLK 9"
     StopSound"HTLK 10"
     StopSound"HTLK 11"
     StopSound"HTLK 12"
     StopSound"HTLK 13"
     StopSound"HTLK 14"
     StopSound"HTLK 15"
     StopSound"HTLK 16"
     StopSound"HTLK 17"
     StopSound"HTLK 18"
     StopSound"HTLK 19"
     StopSound"HTLK 20"
     StopSound"HTLK 21"
     StopSound"HTLK 22"
     StopSound"HTLK 23"
     StopSound"HTRK 1"
     StopSound"HTRK 2"
     StopSound"HTRK 3"
     StopSound"HTRK 4"
     StopSound"HTRK 5"
     StopSound"HTRK 6"
     StopSound"HTRK 7"
     StopSound"HTRK 8"
     StopSound"HTRK 9"
     StopSound"HTRK 10"
     StopSound"HTRK 11"
     StopSound"HTRK 12"
     StopSound"HTRK 13"
     StopSound"HTRK 14"
     StopSound"HTRK 15"
     StopSound"HTRK 16"
     StopSound"HTRK 17"
     StopSound"HTRK 18"
     StopSound"HTRK 19"
     StopSound"HTRK 20"

End Sub

'************************
'Plunger Trigger
Dim gxx 
Dim PA, PB, PC, PD, AA, BB, CC, DD
AA=0
BB=0

Sub Trigger1_Hit()
    Dim x
    x = INT(6 * RND(1) )
    If bMultiball = False then
         EndMusic
	     If x=0  Then PlayCallout"Plunger-01": BallNumberMusic: End If
         If x=1  Then PlayCallout"Plunger-02": BallNumberMusic: End If
         If x=2  Then PlayCallout"Plunger-03": BallNumberMusic: End If
         If x=3  Then PlayCallout"Plunger-04": BallNumberMusic: End If
         If x=4  Then PlayCallout"Plunger-05": BallNumberMusic: End If
         If x=5  Then PlayCallout"Plunger-06": BallNumberMusic: End If
        Else
	     If x=0  Then PlayCallout"Plunger-01": End If
         If x=1  Then PlayCallout"Plunger-02": End If
         If x=2  Then PlayCallout"Plunger-03": End If
         If x=3  Then PlayCallout"Plunger-04": End If
         If x=4  Then PlayCallout"Plunger-05": End If
         If x=5  Then PlayCallout"Plunger-06": End If
      End If
End Sub


sub AutoPlungerDelay_timer
	If bMultiball Then
		PlungerIM.AutoFire
		SoundSaucerKick 1,swPlungerIM
	End If
	Me.enabled = False
end sub


Sub Gate2_Hit
	If Not bMultiball Then
		EndMusic
		ScoreSound=0
		PlayMusic "Halloween/PLM-01.mp3",MusicVol
		LMultiballReady.state = 0
	Else
		AutoPlungerDelay.Enabled = True
	End If
	RandomSoundBallRelease BallRelease
End Sub


'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If KeyCode = StartGameKey then ScoreSound=0:SoundStartButton: PlayCallout"Start-Sting"
    If keycode = PlungerKey Then SoundPlungerPull:Plunger.Pullback
    If keycode = RightMagnaSave Then 
		If BallNumber>0 and BallNumber<99  Then
			Controller.Switch(17) = 1
		Else
			If Not bLutActive Then ScoreCard=1 : CardTimer.enabled=True
		End If
	End If
    If keycode = RightMagnaSave and DD=0 Then
                 Playsound"0HE03"
                 If bLutActive = True Then NextLUT
                 End If
                                             
    If keycode = LeftMagnaSave Then Controller.Switch(17) = 1 : bLutActive = True
    If keycode = LeftMagnaSave and DD=0 Then Playsound"0HE03": bLutActive = True                                    
    If keycode = LeftTiltKey Then Nudge 90, 1:SoundNudgeLeft
    If keycode = RightTiltKey Then Nudge 270, 1:SoundNudgeRight
    If keycode = CenterTiltKey Then Nudge 0, 2:SoundNudgeCenter
	If keycode = LeftFlipperKey Then 
		If bFlippersEnabled Then
			FlipperActivate LeftFlipper, LFPress
			SolLFlipper True
			If GIDim = 1 Then
				For Each gxx in aGiLights
					gxx.Intensityscale = 0.94
				Next
				GIBright.enabled = 1
			End If
		End If
	End If
	If keycode = RightFlipperKey Then 
		If bFlippersEnabled Then
			FlipperActivate RightFlipper, RFPress
			SolRFlipper True
			If GIDim = 1 Then
				For Each gxx in aGiLights
					gxx.Intensityscale = 0.94
				Next
				GIBright.enabled = 1
			End If
		End If
	End If
	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then RandomCoinSound
    If keycode = 3 Then colorgi
    If vpmKeyDown(keycode)Then Exit Sub
End Sub

Sub table1_KeyUp(ByVal Keycode)
	If keycode = LeftFlipperKey Then 
		If bFlippersEnabled Then
			FlipperDeActivate LeftFlipper, LFPress
			SolLFlipper False
		End If
	End If
	If keycode = RightFlipperKey Then 
		If bFlippersEnabled Then
			FlipperDeActivate RightFlipper, RFPress
			SolRFlipper False
		End If
	End If
    If keycode = LeftMagnaSave OR keycode = RightMagnaSave Then Controller.Switch(17) = 0 : ScoreCard=0
    If keycode = LeftMagnaSave Then bLutActive = False                                               'LUT Added
    If vpmKeyUp(keycode)Then Exit Sub
    If keycode = PlungerKey Then SoundPlungerReleaseBall:Plunger.Fire
End Sub


Sub GIBright_Timer()
	For Each gxx in aGiLights
		gxx.Intensityscale = 1.1
    Next
    me.enabled = 0
End Sub

'*********
' Switches
'*********

' Slings
Dim LStep, RStep, R2Step

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotLeft Lemk
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 36
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing2.Visible = 0:Lemk.RotX = -10:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotRight Remk
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 35
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub sw34_Slingshot
	URS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotRight Remk1
    R2Sling4.Visible = 1
    Remk1.RotX = 26
    R2Step = 0
    vpmTimer.PulseSw 34
    sw34.TimerEnabled = 1
End Sub

Sub sw34_Timer
    Select Case R2Step
        Case 1:R2SLing4.Visible = 0:R2SLing3.Visible = 1:Remk1.RotX = 14
        Case 2:R2SLing3.Visible = 0:R2SLing2.Visible = 1:Remk1.RotX = 2
        Case 3:R2SLing2.Visible = 0:Remk1.RotX = -10:sw34.TimerEnabled = 0
    End Select
    R2Step = R2Step + 1
End Sub

' Rubbers
Sub sw18_Hit:vpmTimer.PulseSw 18:End Sub
Sub sw18a_Hit:vpmTimer.PulseSw 18:End Sub
Sub sw18b_Hit:vpmTimer.PulseSw 18:End Sub

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 38:RandomSoundBumperMiddle Bumper1:RandomSlice: End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 40:RandomSoundBumperMiddle Bumper2:RandomSlice: End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 39:RandomSoundBumperMiddle Bumper3:RandomSlice: End Sub
Sub Bumper4_Hit:vpmTimer.PulseSw 37:RandomSoundBumperMiddle Bumper4:RandomSlice: End Sub

' Drain holes
Sub Drain_Hit
	'debug.print "Drain_Hit: BallNumber="&BallNumber
	RandomSoundDrain Drain 
    EndMusic: StopSounds: MoveDiverter 0: DivertTimer.Enabled = False: DivT=0
    HDRSound
    If NumBallsPerGame=3 then
		If BallNumber=3 Then EndGameMusic: End If
    End If
	If NumBallsPerGame=5 Then
		If BallNumber=5 Then EndGameMusic: End If
    End If
	LMultiballReady.state = 0
	bsTrough.AddBall Me
End Sub

Sub EndGameMusic
    Dim x
    x = INT(4 * RND(1) )
      If x = prevEndMusic then
        EndGameMusic
     Else
      prevEndMusic = x
      Select Case x
				Case 0: AttMusicNum=2: AttractMusic
				Case 1: AttMusicNum=3: AttractMusic
				Case 2: AttMusicNum=4: AttractMusic
				Case 3: AttMusicNum=5: AttractMusic
	  End Select
     End If
End Sub


Sub HDRSound
    Dim x
    x = INT(17 * RND(1) )
    If x = prevHDR then
      HDRSound
      Else
       prevHDR = x
       Select Case x
		Case 0:PlayCallout"HDR 1"
		Case 1:PlayCallout"HDR 2"
		Case 2:PlayCallout"HDR 3"
		Case 3:PlayCallout"HDR 4"
		Case 4:PlayCallout"HDR 5"
		Case 5:PlayCallout"HDR 6"
		Case 6:PlayCallout"HDR 7"
		Case 7:PlayCallout"HDR 8"
		Case 8:PlayCallout"HDR 9"
		Case 9:PlayCallout"HDR 10"
		Case 10:PlayCallout"HDR 11"
        Case 11:PlayCallout"HDR 12"
        Case 12:PlayCallout"HDR 13"
        Case 13:PlayCallout"HDR 14"
        Case 14:PlayCallout"HDR 15"
        Case 15:PlayCallout"HDR 16"
        Case 16:PlayCallout"HDR 17"
       End Select
   End If
End Sub

Dim KickerBall1, KickerBall2

Sub KickBall(kball, kangle, kvel, kvelz, kzlift)
	dim rangle
	rangle = PI * (kangle - 90) / 180
    
	kball.z = kball.z + kzlift
	kball.velz = kvelz
	kball.velx = cos(rangle)*kvel
	kball.vely = sin(rangle)*kvel
End Sub

'Saucer
Sub sw19_Hit
    StopSounds
	set KickerBall1 = activeball
	Controller.Switch(19) = true
    HTLKSound
	SoundSaucerLock
End Sub

Sub HTLKSound
    Dim x
    x = INT(23 * RND(1) )
    If x = prevHTLK then
      HTLKSound
    Else
      prevHTLK = x
      Select Case x
		Case 0:PlayCallout"HTLK 1"
		Case 1:PlayCallout"HTLK 2"
		Case 2:PlayCallout"HTLK 3"
		Case 3:PlayCallout"HTLK 4"
		Case 4:PlayCallout"HTLK 5"
		Case 5:PlayCallout"HTLK 6"
		Case 6:PlayCallout"HTLK 7"
		Case 7:PlayCallout"HTLK 8"
		Case 8:PlayCallout"HTLK 9"
		Case 9:PlayCallout"HTLK 10"
		Case 10:PlayCallout"HTLK 11"
		Case 11:PlayCallout"HTLK 12"
		Case 12:PlayCallout"HTLK 13"
		Case 13:PlayCallout"HTLK 14"
		Case 14:PlayCallout"HTLK 15"
		Case 15:PlayCallout"HTLK 16"
		Case 16:PlayCallout"HTLK 17"
		Case 17:PlayCallout"HTLK 18"
		Case 18:PlayCallout"HTLK 19"
		Case 19:PlayCallout"HTLK 20"
		Case 20:PlayCallout"HTLK 21"
		Case 21:PlayCallout"HTLK 22"
		Case 22:PlayCallout"HTLK 23"
      End Select
   End If
End Sub

Sub sw19_UnHit
	Controller.Switch(19) = False
End Sub

Sub sw19_SolOut(Enabled)
	If Enabled Then
		Controller.Switch(19) = False
		sw19.TimerEnabled = True
	End If
End Sub

 sw19.TimerInterval = SaucerKickDelay
Sub sw19_Timer
	If sw19.BallCntOver > 0 Then 
        PlayCallout"Saucer_Kick2"
		KickBall KickerBall1, 0, 0, 45, 0
		DOF 301,2
	End If
	sw19.TimerEnabled = False
	SoundSaucerKick 1,sw19
	Controller.Switch(19) = False
End Sub

Sub Kicker2_Hit
    StopSounds
    HTRKSound
	set KickerBall2 = activeball
	MBDelay.Enabled = True
	SoundSaucerLock
End Sub

Sub MBDelay_Timer
	MBDelay.Enabled = False
	If LMultiballReady.state=1 Then
		bMultiball = True
		BallRelease.CreateSizedballWithMass Ballsize/2, Ballmass 
		BallRelease.Kick 0,10,0
		GIFlicker 3,120
	Else
		Kicker2.Timerenabled = True
	End If
End Sub

Sub HTRKSound
    Dim x
    x = INT(20 * RND(1) )
    If x = prevHTRK then
      HTRKSound
    Else
    prevHTRK = x
    Select Case x
		Case 0:PlayCallout"HTRK 1"
		Case 1:PlayCallout"HTRK 2"
		Case 2:PlayCallout"HTRK 3"
		Case 3:PlayCallout"HTRK 4"
		Case 4:PlayCallout"HTRK 5"
		Case 5:PlayCallout"HTRK 6"
		Case 6:PlayCallout"HTRK 7"
		Case 7:PlayCallout"HTRK 8"
		Case 8:PlayCallout"HTRK 9"
		Case 9:PlayCallout"HTRK 10"
		Case 10:PlayCallout"HTRK 11"
		Case 11:PlayCallout"HTRK 12"
		Case 12:PlayCallout"HTRK 13"
		Case 13:PlayCallout"HTRK 14"
		Case 14:PlayCallout"HTRK 15"
		Case 15:PlayCallout"HTRK 16"
		Case 16:PlayCallout"HTRK 17"
		Case 17:PlayCallout"HTRK 18"
		Case 18:PlayCallout"HTRK 19"
		Case 19:PlayCallout"HTRK 20"
    End Select
   End If
End Sub


Kicker2.TimerInterval = SaucerKickDelay
Sub Kicker2_timer
	If Kicker2.BallCntOver > 0 Then 
        PlayCallout"Saucer_Kick2"
		KickBall KickerBall2, 0, 0, 45, 0
		DOF 302,2
	End If
	LMultiballReady.state = 0
	Kicker2.Timerenabled = False
	SoundSaucerKick 1,Kicker2
End Sub

'Multiball Mod
Dim bMultiball : bMultiball = False
Dim LightAState, LightBState, Light5XState

Sub TriggerDrain_Hit
	If bMultiball Then
		bMultiball = False
		If MBS=1 Then MBLitTimer.Enabled=True
		RandomSoundDrain Drain 
		activeball.DestroyBall
	End If
End Sub

Sub Gate6_Hit
	If bMultiball Then
		Kicker2.Timerenabled = True
	Else
		If MBS=1 Then MBLitTimer.Enabled=True
	End If
End Sub


Sub Kicker1_Hit
	MoveDiverter 1
    StopSounds
    ExplsionSFX.Enabled = True
	SoundSaucerLock
    PlayCallout "sfx_99"
End Sub

' Rollovers
Sub sw32_Hit
	Controller.Switch(32) = true
    StopSounds
    HOLTDSound
End Sub

Sub sw32_UnHit:Controller.Switch(32) = false:End Sub

Sub sw32a_Hit:Controller.Switch(32) = true
    StopSounds
    HOLTDSound
End Sub

Sub HOLTDSound
   Dim x
    x = INT(13 * RND(1) )
    If x = prevHOLTD then
        HOLTDSound
    Else
      prevHOLTD = x
      Select Case x
		Case 0:PlayCallout"HOLTD 2"
		Case 1:PlayCallout"HOLTD 3"
		Case 2:PlayCallout"HOLTD 4"
		Case 3:PlayCallout"HOLTD 5"
		Case 4:PlayCallout"HOLTD 6"
		Case 5:PlayCallout"HOLTD 7"
		Case 6:PlayCallout"HOLTD 8"
		Case 7:PlayCallout"HOLTD 9"
        Case 8:PlayCallout"HOLTD 10"
        Case 9:PlayCallout"HOLTD 11"
        Case 10:PlayCallout"HOLTD 12"
        Case 11:PlayCallout"HOLTD 13"
        Case 12:PlayCallout"HOLTD 14"
      End Select
    End If
End Sub

Sub sw32a_UnHit:Controller.Switch(32) = false:End Sub
Sub sw20_Hit():controller.switch(20) = true:ScoreSound=1: End Sub
Sub sw20_unHit():controller.switch(20) = false:End Sub
Sub sw21_Hit():controller.switch(21) = true:End Sub
Sub sw21_unHit():controller.switch(21) = false:End Sub
Sub sw22_Hit():controller.switch(22) = true:End Sub
Sub sw22_unHit():controller.switch(22) = false:End Sub
Sub sw23_Hit():controller.switch(23) = true:End Sub
Sub sw23_unHit():controller.switch(23) = false:End Sub
Sub sw24_Hit():controller.switch(24) = true:MoveDiverter 1: End Sub
Sub sw24_unHit():controller.switch(24) = false:End Sub
Sub sw25_Hit():controller.switch(25) = true:End Sub
Sub sw25_unHit():sw25.TimerEnabled=True:End Sub
Sub sw25a_Hit():controller.switch(25) = true:End Sub
Sub sw25a_unHit():sw25a.TimerEnabled=True:End Sub

Sub sw25_Timer : controller.switch(25) = false : sw25.TimerEnabled=False : End Sub
Sub sw25a_Timer: controller.switch(25) = false : sw25a.TimerEnabled=False : End Sub
Sub sw26_Timer : controller.switch(26) = false : sw26.TimerEnabled=False : End Sub
Sub sw28_Timer : controller.switch(28) = false : sw28.TimerEnabled=False : End Sub

Sub sw26_Hit():controller.switch(26) = true
    HTASound1
End Sub

Sub HTASound1
    Dim x
    x = INT(5 * RND(1) )
      If x = prevHTAR then
        HTASound1
    Else
      prevHTAR = x
      Select Case x
		Case 0:PlayCallout"HTA 1"
		Case 1:PlayCallout"HTA 2"
		Case 2:PlayCallout"HTA 3"
		Case 3:PlayCallout"HTA 4"
		Case 4:PlayCallout"HTA 5"
       End Select
    End If
End Sub


Sub sw26_unHit():sw26.TimerEnabled=True:End Sub

Sub sw28_Hit():controller.switch(28) = true
    HTASound2
End Sub

Sub HTASound2
    Dim x
    x = INT(35 * RND(1) )
      If x = prevHTAL then
      HTASound2
    Else
     prevHTAL = x
      Select Case x
		Case 0:PlayCallout"HTA 1"
		Case 1:PlayCallout"HTA 2"
		Case 2:PlayCallout"HTA 3"
		Case 3:PlayCallout"HTA 4"
		Case 4:PlayCallout"HTA 5"
		Case 5:PlayCallout"HTA 6"
		Case 6:StopSounds: PlayCallout"HTA 7"
		Case 7:StopSounds: PlayCallout"HTA 8"
		Case 8:StopSounds: PlayCallout"HTA 9"
		Case 9:StopSounds: PlayCallout"HTA 10"
		Case 10:StopSounds: PlayCallout"HTA 11"
		Case 11:StopSounds: PlayCallout"HTA 12"
        Case 12:StopSounds: PlayCallout"HTA 13"
        Case 13:StopSounds: PlayCallout"HTA 14"
        Case 14:StopSounds: PlayCallout"HTA 15"
        Case 15:StopSounds: PlayCallout"HTA 16"
        Case 16:StopSounds: PlayCallout"HTA 17"
        Case 17:StopSounds: PlayCallout"HTA 18"
        Case 18:StopSounds: PlayCallout"HTA 19"
        Case 19:StopSounds: PlayCallout"HTA 20"
        Case 20:StopSounds: PlayCallout"HTA 21"
        Case 21:StopSounds: PlayCallout"HTA 22"
        Case 22:StopSounds: PlayCallout"HTA 23"
        Case 23:StopSounds: PlayCallout"HTA 24"
        Case 24:StopSounds: PlayCallout"HTA 25"
        Case 25:StopSounds: PlayCallout"HTA 26"
        Case 26:StopSounds: PlayCallout"HTA 27"
        Case 27:StopSounds: PlayCallout"HTA 28"
        Case 28:StopSounds: PlayCallout"HTA 29"
        Case 29:StopSounds: PlayCallout"HTA 30"
        Case 30:StopSounds: PlayCallout"HTA 31"
        Case 31:StopSounds: PlayCallout"HTA 32"
        Case 32:StopSounds: PlayCallout"HTA 33"
        Case 33:StopSounds: PlayCallout"HTA 34"
        Case 34:StopSounds: PlayCallout"HTA 35"
       End Select
   End If
End Sub


Sub sw28_unHit():sw28.TimerEnabled=true:End Sub

Sub sw31_Hit():SetLamp 150, 1:controller.switch(31) = true:PlaySound"0HE05":End Sub
Sub sw31_unHit():SetLamp 150, 0:controller.switch(31) = false:End Sub
Sub sw31a_Hit():SetLamp 151, 1:controller.switch(31) = true:PlaySound"0HE05":End Sub
Sub sw31a_unHit():SetLamp 151, 0:controller.switch(31) = false:End Sub

' Targets
Sub sw12_Hit: STHit 12: LightningFX: End Sub
Sub sw13_Hit: STHit 13: LightningFX: End Sub
Sub sw14_Hit: STHit 14: LightningFX: End Sub
Sub sw29_Hit: STHit 29: End Sub
Sub sw30_Hit:vpmTimer.PulseSw 30: End Sub
Sub sw27_Hit: STHit 27: End Sub

' Droptargets
Sub sw1_Hit:DTHit 1:End Sub
Sub sw2_Hit:DTHit 2:End Sub
Sub sw3_Hit:DTHit 3:End Sub
Sub sw4_Hit:DTHit 4:End Sub

Sub sw42_Hit:DTHit 42:End Sub
Sub sw43_Hit:DTHit 43:End Sub
Sub sw44_Hit:DTHit 44:End Sub
Sub sw45_Hit:DTHit 45:End Sub
Sub sw46_Hit:DTHit 46:End Sub
Sub sw47_Hit:DTHit 47:End Sub
Sub sw48_Hit:DTHit 48:End Sub


'Spinner
Sub sw33_Spin:vpmTimer.PulseSw 33:SoundSpinner sw33:End Sub

'*********
'Solenoids
'*********

Sub UpdateSolenoids
    Dim Changed, ii, solNo
    Changed = Controller.ChangedSolenoids
    If Not IsEmpty(Changed)Then
        For ii = 0 To UBound(Changed)
            solNo = Changed(ii, CHGNO)
            If Controller.Lamp(34)Then
                If SolNo = 1 Or SolNo = 2 Or SolNo = 3 Or SolNo = 4 Or Solno = 5 Or Solno = 6 Or Solno = 8 Then solNo = solNo + 24 '1->25 etc
            End If
            vpmDoSolCallback solNo, Changed(ii, CHGSTATE)
        Next
    End If
End Sub

SolCallback(1) = "vpmSolSound ""fx_knocker"","
SolCallback(2) = "dtRBankSolDropUp"
SolCallback(3) = "dtTBankSolDropUp"
SolCallback(4) = "dtTBankSolHit2"
SolCallback(5) = "dtTBankSolHit4"
SolCallback(6) = "dtTBankSolHit6"
SolCallback(7) = "SolShieldPost"
SolCallback(8) = "bsTrough.SolOut"
SolCallback(19) = "RelayAC"
SolCallback(25) = "SolZipOpen"     		'16
SolCallback(26) = "SolZipClose"			'17
SolCallback(27) = "dtTBankSolHit1"		'18
SolCallback(28) = "dtTBankSolHit3"		'19
SolCallback(29) = "dtTBankSolHit5"		'20
SolCallback(30) = "dtTBankSolHit7"		'21
SolCallback(32) = "sw19_SolOut"			'22


Sub dtRBankSolDropUp(Enabled)
	If Enabled Then
		DTRaise 1
		DTRaise 2
		DTRaise 3
		DTRaise 4
	End If
End Sub

Sub dtTBankSolDropUp(Enabled)
	If Enabled Then
		DTRaise 42
		DTRaise 43
		DTRaise 44
		DTRaise 45
		DTRaise 46
		DTRaise 47
		DTRaise 48
	End If
End Sub

Sub dtTBankSolHit1(Enabled)
	If Enabled Then
		DTDrop 48
	End If
End Sub

Sub dtTBankSolHit2(Enabled)
	If Enabled Then
		DTDrop 47
	End If
End Sub

Sub dtTBankSolHit3(Enabled)
	If Enabled Then
		DTDrop 46
	End If
End Sub

Sub dtTBankSolHit4(Enabled)
	If Enabled Then
		DTDrop 45
	End If
End Sub

Sub dtTBankSolHit5(Enabled)
	If Enabled Then
		DTDrop 44
	End If
End Sub

Sub dtTBankSolHit6(Enabled)
	If Enabled Then
		DTDrop 43
	End If
End Sub

Sub dtTBankSolHit7(Enabled)
	If Enabled Then
		DTDrop 42
	End If
End Sub



Sub SolZipOpen(enabled)
	If Enabled Then
		SoundSaucerKick 1,LeftFlipper3:CC=0
		LFlip2.Visible = True:LFlipR2.Visible = True:LeftFlipper2.Enabled = True
		RFlip2.Visible = True:RFlipR2.Visible = True:RightFlipper2.Enabled = True
		LFlip3.Visible = False:LFlipR3.Visible = False:LeftFlipper3.Enabled = False
		RFlip3.Visible = False:RFlipR3.Visible = False:RightFlipper3.Enabled = False
		Controller.Switch(41) = 1
	End If
End Sub

Sub SolZipClose(enabled)
	If Enabled Then
		SoundSaucerKick 1,LeftFlipper3:CC=1
		LFlip2.Visible = False:LFlipR2.Visible = False:LeftFlipper2.Enabled = False
		RFlip2.Visible = False:RFlipR2.Visible = False:RightFlipper2.Enabled = False
		LFlip3.Visible = True:LFlipR3.Visible = True:LeftFlipper3.Enabled = True
		RFlip3.Visible = True:RFlipR3.Visible = True:RightFlipper3.Enabled = True
		Controller.Switch(41) = 0
	End If
End Sub

Sub SolShieldPost(Enabled)
    If Enabled Then
        If MuzzleFlash.Enabled=True then
            MuzzleFlash.Enabled=False: MuzzleCnt = 2
            MuzzleFlash.Enabled=True
            DD=1
            SoundSaucerKick 1,Plunger
		    PlaySound"0HE02"
            post1.IsDropped = 1
            post1Rubber.Visible = 0
            post2.IsDropped = 0
            post2Rubber.Visible = 1
            Post.Fire
        Else
            MuzzleFlash.Enabled=True
            DD=1
            SoundSaucerKick 1,Plunger
		    PlaySound"0HE02"
            post1.IsDropped = 1
            post1Rubber.Visible = 0
            post2.IsDropped = 0
            post2Rubber.Visible = 1
            Post.Fire
        End If
    Else
        DD=0
        post1.IsDropped = 0
        post1Rubber.Visible = 1
        post2.IsDropped = 1
        post2Rubber.Visible = 0
        Post.PullBack
    End If
End Sub

Sub RelayAC(Enabled)
    vpmNudge.SolGameOn Enabled
    If Enabled Then
        GiOn
        SetLamp 152, 1
        SetLamp 153, 1
    Else
        GiOff
        SetLamp 152, 0
        SetLamp 153, 0
    End If
End Sub

'**************
' Diverter     
'**************


Sub MoveDiverter(value)
	SoundSaucerKick 1,DiverterFlipper 
	Select Case value
		Case 0: DiverterFlipper.RotateToEnd: DivertTimer.Enabled=False : L75.state = 0: L76.state = 0: DiverterFlipper.TimerEnabled = True   'Closed
		Case 1: DiverterFlipper.RotateToStart: DivertTimer.Enabled=True : DivT=0 : L75.state = 2: L76.state = 2  'Opened
	End Select
End Sub

Dim DivT
DivT=0

Sub DivertTimer_Timer
    If DivertTimer.Enabled=True then DivT=DivT+1: End If
    If DivT > 60 then MoveDiverter 0: DivertTimer.Enabled=False: DivT=0: End If
End Sub
    
Sub DiverterFlipper_Collide(parm) : RandomSoundMetal : End Sub

Sub DiverterFlipper_Timer
	DiverterFlipper.TimerEnabled = False
	if DiverterFlipper.currentangle > -40 Then MoveDiverter 1    'handle stuck ball condition
End Sub

'**************
' Flipper Subs
'**************

'SolCallback(sLRFlipper) = "SolRFlipper"
'SolCallback(sLLFlipper) = "SolLFlipper"

Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire  'leftflipper.rotatetoend
		LeftFlipper2.RotateToEnd:LeftFlipper3.RotateToEnd:SetLamp 152, 0
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If		
	Else
		LeftFlipper.RotateToStart:LeftFlipper2.RotateToStart:LeftFlipper3.RotateToStart:SetLamp 152, 1
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		RF.Fire 'rightflipper.rotatetoend
		RightFlipper2.RotateToEnd:RightFlipper3.RotateToEnd:SetLamp 153, 0
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		RightFlipper.RotateToStart:RightFlipper2.RotateToStart:RightFlipper3.RotateToStart:SetLamp 153, 1
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
    LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
    RightFlipperCollide parm
End Sub

Sub LeftFlipper2_Collide(parm)
    LeftFlipperCollide parm
End Sub

Sub RightFlipper2_Collide(parm)
    RightFlipperCollide parm
End Sub

Sub LeftFlipper3_Collide(parm)
    LeftFlipperCollide parm
End Sub

Sub RightFlipper3_Collide(parm)
    RightFlipperCollide parm
End Sub

'*****************
'   Gi Effects
'*****************

dim gilvl : gilvl = 0
Dim OldGiState
OldGiState = -1 'start witht he Gi off


Sub GiON
	gilvl = 1
	Sound_GI_Relay 1,sw33
    For each x in aGiLights
        x.State = 1
    Next
	LFLogo.blenddisablelighting = 0.1
	RFLogo.blenddisablelighting = 0.1
End Sub

Sub GiOFF
	gilvl = 0
	Sound_GI_Relay 0,sw33
    For each x in aGiLights
        x.State = 0
    Next
	LFLogo.blenddisablelighting = 0
	RFLogo.blenddisablelighting = 0
End Sub

Sub GiEffect
    For each x in aGiLights
        x.Duration 2, 2000, 1
    Next
End Sub

Sub GIUpdate
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = -1 Then
            GiOff
        Else
            GiOn
        End If
    End If
End Sub

Dim GIFlickerCount : GIFlickerCount = 0
Sub GIFlicker(num,interval)
	GIFlickerCount = int(num)*2
	GIFlickerTimer.Interval = interval
	GIFlickerTimer.Enabled = True
End Sub

Sub GIFlickerTimer_Timer
	GIFlickerCount = GIFlickerCount - 1
	if GIFlickerCount < 0 Then
		GiON
		GIFlickerTimer.Enabled = False
	Else
		If GIFlickerCount Mod 2 > 0 Then
			GiON
		Else
			GiOFF
		End If
	End If
End Sub

'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200), FlashRepeat(200)

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
LampTimer.Interval = 10 'lamp fading speed
LampTimer.Enabled = 1

' Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp)Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0)) = chgLamp(ii, 1)       'keep the real state in an array
            FadingLevel(chgLamp(ii, 0)) = chgLamp(ii, 1) + 4 'actual fading step
            'Tilt
            If chgLamp(ii, 0)=61 and chgLamp(ii, 1)=1 and BallNumber>=1 and BallNumber<=5 then 
				bFlippersEnabled=False
				SolLFlipper False
				SolRFlipper False
				PlayCallout "Tilt"
			End If
			'Extra Ball
            If chgLamp(ii, 0)=43 and BallNumber>=1 and BallNumber<=5 then 
				if L43.TimerEnabled = True Then
					L43.TimerEnabled = False
				Else 
					L43.TimerEnabled = True 
				End If
            End If
			'Multiball stuff
			Select Case MBS
				Case 2:
					If chgLamp(ii, 0)=26 or chgLamp(ii, 0)=10 Then
						Select Case chgLamp(ii, 0)
							Case 26: LightAState = chgLamp(ii, 1) 
							Case 10: LightBState = chgLamp(ii, 1) 
						End Select
						If LightAState=1 and LightBState=1 and LMultiballReady.state=0  Then
							MBLitTimer.enabled = True
						Else
							MBLitTimer.enabled = False
						End If
					End If
				Case 3:
					If chgLamp(ii, 0)=53 Then
						Light5XState = chgLamp(ii, 1)
						If Light5XState=1 and LMultiballReady.state=0 Then
							MBLitTimer.enabled = True
						Else
							MBLitTimer.enabled = False
						End If
					End If
			End Select
        Next
    End If
    UpdateLeds
    UpdateLamps
    GIUpdate
    RollingUpdate
End Sub


Sub MBLitTimer_Timer
	If Not bMultiball and LMultiballReady.state=0 Then LMultiballReady.state=1 : PlayMBCallout
	MBLitTimer.enabled = False
End Sub


Sub L43_Timer
	If L43.State=1 Then PlayCallout "extraball"  
	L43.TimerEnabled = False
End Sub


Sub PlayMBCallout
	dim y : y = INT(4 * RND(1) )
	Select Case y
		Case 0: PlayCallout "multiball_lit1"
		Case 1: PlayCallout "multiball_lit2"
		Case 2: PlayCallout "multiball_lit3"
		Case 3: PlayCallout "multiball_lit4"
	End Select
End Sub


Sub UpdateLamps()
    NFadeL 1, l1
    NFadeL 2, l2
    NFadeL 3, l3
    NFadeLm 4, l4_1
    NFadeL 4, l4
    NFadeL 5, l5
    NFadeL 6, l6
    NFadeL 7, l7
    NFadeL 8, l8
    NFadeL 9, l9
    NFadeL 10, l10

    NFadeL 12, l12

    NFadeL 14, l14
    NFadeL 15, l15
    NFadeL 17, l17
    NFadeL 18, l18
    NFadeL 19, l19
    NFadeL 20, l20
    NFadeLm 21, l21_1
    NFadeL 21, l21
    NFadeL 22, l22
    NFadeL 23, l23
    NFadeL 24, l24
    NFadeL 25, l25
    NFadeL 26, l26

    NFadeL 28, l28

    NFadeL 30, l30
    NFadeL 31, l31
    NFadeL 33, l33
    NFadeL 35, l35
    NFadeL 36, l36
    NFadeLm 37, l37_1
    NFadeL 37, l37
    NFadeL 38, l38
    NFadeL 39, l39
    NFadeL 40, l40
    NFadeL 41, l41
    NFadeL 42, l42
    NFadeL 43, l43
    NFadeL 44, l44

    NFadeL 46, l46
    NFadeLm 47, l47a
    NFadeLm 47, l47b
    NFadeLm 47, l47c
    NFadeLm 47, l47d
    NFadeLm 47, l47e
    NFadeLm 47, gi62
    NFadeLm 47, gi63
    NFadeLm 47, gi64
    NFadeLm 47, gi65
    NFadeLm 47, gi66
    NFadeLm 47, gi67
    NFadeLm 47, gi68
    NFadeLm 47, gi69
    NFadeLm 47, gi70
    NFadeLm 47, gi71
    NFadeLm 47, gi72
    NFadeLm 47, gi75
    NFadeLm 47, gi76
    NFadeLm 47, gi77
    NFadeLm 47, gi78
    NFadeLm 47, gi79
    NFadeLm 47, gi80
    NFadeLm 47, gi81
    NFadeLm 47, gi82
    NFadeLm 47, gi83
    NFadeLm 47, gi84
    NFadeLm 47, gi85
    NFadeLm 47, gi86
    NFadeLm 47, gi87
    NFadeLm 47, gi88
    NFadeLm 47, gi89
    NFadeL 47, l47
    NFadeL 49, l49
    NFadeL 51, l51
    NFadeL 52, l52
    NFadeLm 53, l53_1
    NFadeL 53, l53
    NFadeL 54, l54
    NFadeL 55, l55
    NFadeL 56, l56
    NFadeL 57, l57
    NFadeL 58, l58
    NFadeL 59, l59

    NFadeL 62, l62
    NFadeL 63, l63
    NFadeLm 70, l70_1
    NFadeL 70, l70
    NFadeLm 71, l71_1
    NFadeL 71, l71
    NFadeLm 86, l86_1
    NFadeL 86, l86
    NFadeL 87, l87
    NFadeLm 102, l102_1
    NFadeL 102, l102
    NFadeL 103, l103
    NFadeLm 118, l118_1
    NFadeL 118, l118
    NFadeL 119, l119
    NFadeL 150, lleft
    NFadeL 151, lright


    ' backdrop lights
    If VarHidden Then
        NFadeT 11, l11, "SAME PLAYER SHOOTS AGAIN"
        NFadeT 13, l13, "BALL IN PLAY"
        NFadeT 27, l27, "MATCH"
        NFadeT 29, l29, "HI SCORE TO DATE"
        NFadeT 45, l45, "GAME OVER"
        NFadeT 61, l61, "TILT"
    End If
End Sub

' div lamp subs

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0        ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4      ' used to track the fading state
        FlashSpeedUp(x) = 0.2   ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.1 ' slower speed when turning off the flasher
        FlashMax(x) = 1         ' the maximum value when on, usually 1
        FlashMin(x) = 0         ' the minimum value when off, usually 0
        FlashLevel(x) = 0       ' the intensity of the flashers, usually from 0 to 1
        FlashRepeat(x) = 20     ' how many times the flash repeats
    Next
End Sub

Sub AllLampsOff
    Dim x
    For x = 0 to 200
        SetLamp x, 0
    Next
End Sub

Sub SetLamp(nr, value)
    If value <> LampState(nr)Then
        LampState(nr) = abs(value)
        FadingLevel(nr) = abs(value) + 4
    End If
End Sub

' Lights: used for VP10 standard lights, the fading is handled by VP itself

Sub NFadeL(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.state = 0:FadingLevel(nr) = 0
        Case 5:object.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeLm(nr, object) ' used for multiple lights
    Select Case FadingLevel(nr)
        Case 4:object.state = 0
        Case 5:object.state = 1
    End Select
End Sub

'Lights, Ramps & Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.image = a:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1            'wait
        Case 9:object.image = c:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1         'wait
        Case 13:object.image = d:FadingLevel(nr) = 0                  'Off
    End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
        Case 9:object.image = c
        Case 13:object.image = d
    End Select
End Sub

Sub NFadeObj(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 0 'off
        Case 5:object.image = a:FadingLevel(nr) = 1 'on
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
    End Select
End Sub

' Flasher objects

Sub Flash(nr, object)
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr)- FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr)Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr)Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it doesn't change anything, it just follows the main flasher
    Select Case FadingLevel(nr)
        Case 4, 5
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub

Sub FlashBlink(nr, object)
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr)- FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr)Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
            If FadingLevel(nr) = 0 AND FlashRepeat(nr)Then 'repeat the flash
                FlashRepeat(nr) = FlashRepeat(nr)-1
                If FlashRepeat(nr)Then FadingLevel(nr) = 5
            End If
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr)Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
            If FadingLevel(nr) = 1 AND FlashRepeat(nr)Then FadingLevel(nr) = 4
    End Select
End Sub

' Desktop Objects: Reels & texts (you may also use lights on the desktop)

' Reels

Sub FadeR(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.SetValue 1:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.SetValue 0:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             'wait
        Case 9:object.SetValue 2:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1          'wait
        Case 13:object.SetValue 3:FadingLevel(nr) = 0                  'Off
    End Select
End Sub

Sub FadeRm(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.SetValue 1
        Case 5:object.SetValue 0
        Case 9:object.SetValue 2
        Case 3:object.SetValue 3
    End Select
End Sub

'Texts

Sub NFadeT(nr, object, message)
    Select Case FadingLevel(nr)
        Case 4:object.Text = "":FadingLevel(nr) = 0
        Case 5:object.Text = message:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeTm(nr, object, b)
    Select Case FadingLevel(nr)
        Case 4:object.Text = ""
        Case 5:object.Text = message
    End Select
End Sub



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
	Dim b, BOT
	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob - 1
		' Comment the next line if you are not implementing Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
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

		' "Static" Ball Shadows
		' Comment the next If block, if you are not implementing the Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then
			If BOT(b).Z > 30 Then
				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
			Else
				BallShadowA(b).height=0.1
			End If
			BallShadowA(b).Y = BOT(b).Y + offsetY
			BallShadowA(b).X = BOT(b).X + offsetX
			BallShadowA(b).visible = 1
		End If
	Next
End Sub


'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
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
dim RampBalls(6,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(6)	

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
					PlaySound("RampLoop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))				
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
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


'----- Ramp Triggers

Sub swRamp1_Hit
	WireRampOff
	activeball.angmomx = 0
	activeball.angmomy = 0
	activeball.angmomz = 0
End Sub

Sub swRamp1_UnHit
	activeball.velx = 0
	activeball.vely = 0
End Sub


Sub swRamp2_Hit
	WireRampOn False
	activeball.velx = activeball.velx * RampSpeedFactor
	activeball.vely = activeball.vely * RampSpeedFactor
End Sub

Sub swRamp3_Hit
	WireRampOn False
	activeball.velx = activeball.velx * RampSpeedFactor
	activeball.vely = activeball.vely * RampSpeedFactor
End Sub





'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************





'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************


'Ambient (Room light source)
Const AmbientBSFactor 		= 0.9	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const offsetX				= 0		'Offset x position under ball	(These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY				= 0		'Offset y position under ball	 (for example 5,5 if the light is in the back left corner)
'Dynamic (Table light sources)
Const DynamicBSFactor 		= 0.99	'0 to 1, higher is darker
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness is technically most accurate for lights at z ~25 hitting a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source

' *** Trim or extend these to *match* the number of balls/primitives/flashers on the table!
dim objrtx1(5), objrtx2(5)
dim objBallShadow(5)
Dim OnPF(5)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4)
Dim DSSources(30), numberofsources', DSGISide(30) 'Adapted for TZ with GI left / GI right

Dim ClearSurface:ClearSurface = True		'Variable for hiding flasher shadow on wire and clear plastic ramps
									'Intention is to set this either globally or in a similar manner to RampRolling sounds

'Initialization
DynamicBSInit

sub DynamicBSInit()
	Dim iii, source

	for iii = 0 to tnob - 1								'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = 1 + iii/1000 + 0.01			'Separate z for layering without clipping
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = 1 + iii/1000 + 0.02
		objrtx2(iii).visible = 0

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = 1 + iii/1000 + 0.04
		objBallShadow(iii).visible = 0

		BallShadowA(iii).Opacity = 100*AmbientBSFactor
		BallShadowA(iii).visible = 0
	Next

	iii = 0

	For Each Source in DynamicSources
		DSSources(iii) = Array(Source.x, Source.y)
'		If Instr(Source.name , "Left") > 0 Then DSGISide(iii) = 0 Else DSGISide(iii) = 1	'Adapted for TZ with GI left / GI right
		iii = iii + 1
	Next
	numberofsources = iii
end sub


Sub BallOnPlayfieldNow(yeh, num)		'Only update certain things once, save some cycles
	If yeh Then
		OnPF(num) = True
'		debug.print "Back on PF"
		UpdateMaterial objBallShadow(num).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(num).size_x = 5
		objBallShadow(num).size_y = 4.5
		objBallShadow(num).visible = 1
		BallShadowA(num).visible = 0
	Else
		OnPF(num) = False
'		debug.print "Leaving PF"
		If Not ClearSurface Then
			BallShadowA(num).visible = 1
			objBallShadow(num).visible = 0
		Else
			objBallShadow(num).visible = 1
		End If
	End If
End Sub

Sub DynamicBSUpdate
	Dim falloff: falloff = 150 'Max distance to light sources, can be changed dynamically if you have a reason
	Dim ShadowOpacity1, ShadowOpacity2 
	Dim s, LSd, iii
	Dim dist1, dist2, src1, src2
	Dim BOT: BOT=getballs	'Uncomment if you're deleting balls - Don't do it! #SaveTheBalls

	'Hide shadow of deleted balls
	For s = UBound(BOT) + 1 to tnob - 1
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next

	If UBound(BOT) < lob Then Exit Sub		'No balls in play, exit

'The Magic happens now
	For s = lob to UBound(BOT)

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 units and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

		If AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
			If BOT(s).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If OnPF(s) Then BallOnPlayfieldNow False, s		'One-time update

				If Not ClearSurface Then							'Don't show this shadow on plastic or wire ramps (table-wide variable, for now)
					BallShadowA(s).X = BOT(s).X + offsetX
					BallShadowA(s).Y = BOT(s).Y + BallSize/5
					BallShadowA(s).height=BOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				Else
					If BOT(s).X < tablewidth/2 Then
						objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
					Else
						objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
					End If
					objBallShadow(s).Y = BOT(s).Y + BallSize/10 + offsetY
					objBallShadow(s).size_x = 5 * ((BOT(s).Z+BallSize)/80)			'Shadow gets larger and more diffuse as it moves up
					objBallShadow(s).size_y = 4.5 * ((BOT(s).Z+BallSize)/80)
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*(30/(BOT(s).Z)),RGB(0,0,0),0,0,False,True,0,0,0,0
				End If

			Elseif BOT(s).Z <= 30 And BOT(s).Z > 20 Then	'On pf, primitive only
				If Not OnPF(s) Then BallOnPlayfieldNow True, s

				If BOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
				Else
					objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
				End If
				objBallShadow(s).Y = BOT(s).Y + offsetY
'				objBallShadow(s).Z = BOT(s).Z + s/1000 + 0.04		'Uncomment (and adjust If/Elseif height logic) if you want the primitive shadow on an upper/split pf

			Else												'Under pf, no shadows
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 0
			end if

		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
			If BOT(s).Z > 30 Then							'In a ramp
				If Not ClearSurface Then							'Don't show this shadow on plastic or wire ramps (table-wide variable, for now)
					BallShadowA(s).X = BOT(s).X + offsetX
					BallShadowA(s).Y = BOT(s).Y + BallSize/5
					BallShadowA(s).height=BOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				Else
					BallShadowA(s).X = BOT(s).X + offsetX
					BallShadowA(s).Y = BOT(s).Y + offsetY
				End If
			Elseif BOT(s).Z <= 30 And BOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				If BOT(s).X < tablewidth/2 Then
					BallShadowA(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
				Else
					BallShadowA(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
				End If
				BallShadowA(s).Y = BOT(s).Y + offsetY
				BallShadowA(s).height=0.1
			Else											'Under pf
				BallShadowA(s).visible = 0
			End If
		End If

' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If BOT(s).Z < 30 And BOT(s).X < 850 Then	'Parameters for where the shadows can show, here they are not visible above the table (no upper pf) or in the plunger lane
				dist1 = falloff:
				dist2 = falloff
				For iii = 0 to numberofsources - 1 ' Search the 2 nearest influencing lights
					LSd = Distance(BOT(s).x, BOT(s).y, DSSources(iii)(0), DSSources(iii)(1)) 'Calculating the Linear distance to the Source
					If LSd < falloff And gilvl > 0 Then
'					If LSd < dist2 And ((DSGISide(iii) = 0 And Lampz.State(100)>0) Or (DSGISide(iii) = 1 And Lampz.State(104)>0)) Then	'Adapted for TZ with GI left / GI right
						dist2 = dist1
						dist1 = LSd
						src2 = src1
						src1 = iii
					End If
				Next
				ShadowOpacity1 = 0
				If dist1 < falloff Then
					objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y
					'objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx1(s).rotz = AnglePP(DSSources(src1)(0), DSSources(src1)(1), BOT(s).X, BOT(s).Y) + 90
					ShadowOpacity1 = 1 - dist1 / falloff
					objrtx1(s).size_y = Wideness * ShadowOpacity1 + Thinness
					UpdateMaterial objrtx1(s).material,1,0,0,0,0,0,ShadowOpacity1*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx1(s).visible = 0
				End If
				ShadowOpacity2 = 0
				If dist2 < falloff Then
					objrtx2(s).visible = 1 : objrtx2(s).X = BOT(s).X : objrtx2(s).Y = BOT(s).Y + offsetY
					'objrtx2(s).Z = BOT(s).Z - 25 + s/1000 + 0.02 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx2(s).rotz = AnglePP(DSSources(src2)(0), DSSources(src2)(1), BOT(s).X, BOT(s).Y) + 90
					ShadowOpacity2 = 1 - dist2 / falloff
					objrtx2(s).size_y = Wideness * ShadowOpacity2 + Thinness
					UpdateMaterial objrtx2(s).material,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx2(s).visible = 0
				End If
				If AmbientBallShadowOn = 1 Then
					'Fades the ambient shadow (primitive only) when it's close to a light
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*(1 - max(ShadowOpacity1, ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					BallShadowA(s).Opacity = 100 * AmbientBSFactor * (1 - max(ShadowOpacity1, ShadowOpacity2))
				End If
			Else 'Hide dynamic shadows everywhere else, just in case
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
			End If
		End If
	Next
End Sub
'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************



'************************************
'          LEDs Display
'     Based on Scapino's LEDs
'************************************

Dim Digits(38)
Dim Patterns(11)
Dim Patterns2(11)

Patterns(0) = 0     'empty
Patterns(1) = 63    '0
Patterns(2) = 6     '1
Patterns(3) = 91    '2
Patterns(4) = 79    '3
Patterns(5) = 102   '4
Patterns(6) = 109   '5
Patterns(7) = 125   '6
Patterns(8) = 7     '7
Patterns(9) = 127   '8
Patterns(10) = 111  '9

Patterns2(0) = 128  'empty
Patterns2(1) = 191  '0
Patterns2(2) = 134  '1
Patterns2(3) = 219  '2
Patterns2(4) = 207  '3
Patterns2(5) = 230  '4
Patterns2(6) = 237  '5
Patterns2(7) = 253  '6
Patterns2(8) = 135  '7
Patterns2(9) = 255  '8
Patterns2(10) = 239 '9

Set Digits(0) = a0
Set Digits(1) = a1
Set Digits(2) = a2
Set Digits(3) = a3
Set Digits(4) = a4
Set Digits(5) = a5
Set Digits(6) = a6

Set Digits(7) = b0
Set Digits(8) = b1
Set Digits(9) = b2
Set Digits(10) = b3
Set Digits(11) = b4
Set Digits(12) = b5
Set Digits(13) = b6

Set Digits(14) = c0
Set Digits(15) = c1
Set Digits(16) = c2
Set Digits(17) = c3
Set Digits(18) = c4
Set Digits(19) = c5
Set Digits(20) = c6

Set Digits(21) = d0
Set Digits(22) = d1
Set Digits(23) = d2
Set Digits(24) = d3
Set Digits(25) = d4
Set Digits(26) = d5
Set Digits(27) = d6

Set Digits(28) = e0
Set Digits(29) = e1
Set Digits(30) = e2
Set Digits(31) = e3

Digits(32) = Array(a00, a02, a05, a06, a04, a01, a03)
Digits(33) = Array(a10, a12, a15, a16, a14, a11, a13)
Digits(34) = Array(a20, a22, a25, a26, a24, a21, a23)
Digits(35) = Array(a30, a32, a35, a36, a34, a31, a33)
Digits(36) = Array(a40, a42, a45, a46, a44, a41, a43)
Digits(37) = Array(a50, a52, a55, a56, a54, a51, a53)


' NEW

dim BallNumber : BallNumber = 0

Sub UpdateLeds
    On Error Resume Next
    Dim ChgLED, ii, jj, chg, stat, num, obj
    ChgLED = Controller.ChangedLEDs(&H0000003f, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
        For ii = 0 To UBound(ChgLED)
            num = chgLED(ii, 0):chg = chgLED(ii, 1):stat = chgLED(ii, 2)
            If num < 32 Then
                For jj = 0 to 10
                    If stat = Patterns(jj)OR stat = Patterns2(jj)then Digits(chgLED(ii, 0)).SetValue jj
                    ExtraSound
                Next
				If num = 31 Then
					Select Case stat
                        Case 0:    BallNumber = 99: bFlippersEnabled = False: SolLFlipper False: SolRFlipper False
						Case 6:    BallNumber = 1 : bFlippersEnabled = True
						Case 91:   BallNumber = 2 : bFlippersEnabled = True
						Case 79:   BallNumber = 3 : bFlippersEnabled = True
						Case 102:  BallNumber = 4 : bFlippersEnabled = True
						Case 109:  BallNumber = 5 : bFlippersEnabled = True
						Case 95:   'do nothing here
						Case Else: BallNumber = 0 : bFlippersEnabled = False: SolLFlipper False: SolRFlipper False
					End Select
					'debug.print "stat = " & stat & " BallNumber = " & BallNumber
				End If
            Else
                For Each obj In Digits(num)
                    If chg And 1 Then obj.State = stat And 1
                    chg = chg \ 2:stat = stat \ 2
                Next
            End If
        Next
    End If
End Sub

'END NEW

Sub editDips
    Dim vpmDips:Set vpmDips = New cvpmDips
    With vpmDips
        .AddForm 700, 400, "Halloween - DIP switches"
        .AddChk 0, 5, 120, Array("Match feature", &H08000000)                                                                                                                                                                                                                 'dip 28
        .AddChk 0, 25, 120, Array("Credits displayed", &H04000000)                                                                                                                                                                                                            'dip 27
        .AddFrame 0, 44, 190, "Extra ball match number display adjust", &H00400000, Array("any number flashing will be reset", 0, "any number flashing will for next ball", &H00400000)                                                                                       'dip 23
'        .AddFrame 0, 90, 190, "Top Shape red lites", &H00000020, Array("step back 1 when target is hit", 0, "do not step back", &H00000020)                                                                                                                                 'dip 6
'        .AddFrame 0, 136, 190, "Any advanced Shape bonus lite on", &H00000040, Array("will be reset", 0, "will come back on for next ball", &H00000040)                                                                                                                    'dip 7
'        .AddFrame 0, 182, 190, "Any lit left side 2 or 3 arrow", &H00004000, Array("will be reset", 0, "will come back on for next ball", &H00004000)                                                                                                                         'dip 15
'        .AddFrame 0, 228, 190, "Shape special lites with", 32768, Array("80K", 0, "40K and 80K", 32768)                                                                                                                                                                      'dip 16
'        .AddFrame 0, 274, 190, "Collect Shape bonus saucer lite", &H20000000, Array("will be reset", 0, "will come back on for next ball", &H20000000)                                                                                                                      'dip 30
'        .AddFrame 0, 320, 395, "Movable flipper timer adjust", &H00000080, Array("closed flippers will open after 10 seconds until next target is hit", 0, "Hitting top targets adds 10 seconds each to keep flippers closed", &H00000080)                                    'dip 8
        .AddFrame 205, 0, 190, "Maximum credits", &H03000000, Array("10 credits", 0, "15 credits", &H01000000, "25 credits", &H02000000, "40 credits", &H03000000)                                                                                                            'dip 25&26
        .AddFrame 205, 76, 190, "Balls per game", &HC0000000, Array("3 balls", 0, "5 balls", &H40000000)                                                                                                                        'dip 31&32
'        .AddFrame 205, 152, 190, "Shape bonus red lights", &H00100000, Array("1st 5-3, 2nd 5-2, 3dr 5-1, 4th 5-1", 0, "1st 5-3, 2nd 5-2, 3dr 5-2, 4th 5-2", &H00100000, "1st 5-3, 2nd 5-3, 3dr 5-2, 4th 5-2", &H00200000, "1st 5-3, 2nd 5-3, 3dr 5-3, 4th 5-3", &H00300000) 'dip 21&22
'        .AddFrame 205, 228, 190, "Halloween bonus from 1 to 19 memory", &H00800000, Array("will be reset", 0, "will come back on for next ball", &H00800000)                                                                                                                     'dip 24
'        .AddFrame 205, 274, 190, "Replay limit", &H10000000, Array("1 replay per game", 0, "unlimited replays", &H10000000)                                                                                                                                                   'dip 29
'        .AddLabel 30, 390, 350, 15, "Set selftest position 16,17,18 and 19 to 03 for the best gameplay."
        .AddLabel 50, 140, 300, 15, "After hitting OK, restart the table with the new settings."
        .ViewDips
    End With
End Sub
Set vpmShowDips = GetRef("editDips")


Sub SetDefaultDips
	SetDip &H00000020,0    	'Top Shape red lites = step back 1 when target is hit
	SetDip &H00000040,0    	'Any advanced Shape bonus lite on = will be reset
	SetDip &H00004000,0    	'Any lit left side 2 or 3 arrow = will be reset
	SetDip 32768,0    		'Shape special lites with = 80K
	SetDip &H20000000,0    	'Collect Shape bonus saucer lite = will be reset
	SetDip &H00000080,0    	'Movable flipper timer adjust = closed flippers will open after 10 seconds until next target is hit
	SetDip &H00100000,0    	'Shape bonus red lights = 1st 5-3
	SetDip &H00800000,0    	'Halloween bonus from 1 to 19 memory = will be reset
	SetDip &H10000000,0    	'Replay limit = will be reset = 1 replay per game
End Sub

Sub SetDip(pos,value)
	dim mask : mask = 255
	if value >= 0.5 then value = 1 else value = 0
	If pos >= 0 and pos <= 255 Then
		pos = pos And &H000000FF
		mask = mask And (Not pos)
		Controller.Dip(0) = Controller.Dip(0) And mask
		Controller.Dip(0) = Controller.Dip(0) + pos*value
	Elseif pos >= 256 and pos <= 65535 Then
		pos = ((pos And &H0000FF00)\&H00000100) And 255
		mask = mask And (Not pos)
		Controller.Dip(1) = Controller.Dip(1) And mask
		Controller.Dip(1) = Controller.Dip(1) + pos*value		
	Elseif pos >= 65536 and pos <= 16777215 Then
		pos = ((pos And &H00FF0000)\&H00010000) And 255
		mask = mask And (Not pos)
		Controller.Dip(2) = Controller.Dip(2) And mask
		Controller.Dip(2) = Controller.Dip(2) + pos*value
	Elseif pos >= 16777216 Then
		pos = ((pos And &HFF000000)\&H01000000) And 255
		mask = mask And (Not pos)
		Controller.Dip(3) = Controller.Dip(3) And mask
		Controller.Dip(3) = Controller.Dip(3) + pos*value
	End If
End Sub


'******************************************************
'   Number of Game Balls Checker
'******************************************************
Dim NumBallsPerGame
Sub ReadNumGameBalls
   Dim n : n = Controller.Dip(3) And &HC0
     Select Case n
        Case 192: NumBallsPerGame = 2
        Case 0:   NumBallsPerGame = 3
        Case 128: NumBallsPerGame = 4
        Case 64:  NumBallsPerGame = 5
        Case Else: NumBallsPerGame = 0
     End Select
End Sub


Sub Table1_Exit()
      Controller.Games("medusa").Settings.Value("sound")=1
      Controller.Games("medusa").Settings.Value("dmd_red") = 255
      Controller.Games("medusa").Settings.Value("dmd_green") = 69
      Controller.Games("medusa").Settings.Value("dmd_blue") = 0
      Controller.Stop
End Sub

Sub ExtraSound
     If ROMSim = 1 then
         If Controller.lamp(45) = false and ScoreSound=1 and GameSound.enabled = False then PlayCallout"10": GameSound.enabled = true: End If
     Else
       Exit Sub
     End If
End Sub

Dim GS1
GS1=0

Sub GameSound_Timer
    If GameSound.enabled=true Then GS1=GS1+1: End If
    If GS1 > 8 Then GameSound.enabled=false: GS1=0: End If
End Sub


Dim ESFX
ESFX=0

Sub ExplsionSFX_Timer
    If ExplsionSFX.enabled=true then ESFX=ESFX+1: End If
    If ESFX > 23 Then
          ExAnimation.Enabled = True
          ExplosionLight.Enabled = True
          Kicker1.Kick 0,27+rnd*6,0
		  DOF 300,2
          ExplsionSFX.enabled=false
          ESFX=0
    End If
End Sub

'*************
'   JP'S LUT
'*************

Dim bLutActive, LUTImage
bLutActive = False
Sub LoadLUT
	bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "") Then LUTImage = x Else LUTImage = 0
	UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT: LUTImage = (LUTImage +1 ) MOD 9: UpdateLUT: SaveLUT: End Sub

Sub UpdateLUT
	Select Case LutImage
		Case 0: Table1.ColorGradeImage = "LUT0"
		Case 1: Table1.ColorGradeImage = "LUT1"
		Case 2: Table1.ColorGradeImage = "LUT2"
		Case 3: Table1.ColorGradeImage = "LUT3"
		Case 4: Table1.ColorGradeImage = "LUT4"
		Case 5: Table1.ColorGradeImage = "LUT5"
		Case 6: Table1.ColorGradeImage = "LUT6"
		Case 7: Table1.ColorGradeImage = "LUT7"
		Case 8: Table1.ColorGradeImage = "LUT8"
	End Select
End Sub


Dim EXPLI
EXPLI = 0

Sub ExplosionLight_Timer
    If ExpLight.State=1 then EXPLI=EXPLI+1: ExpLight.Intensity=35 - EXPLI: End If
    If EXPLI > 35 then ExplosionLight.Enabled=False: ExpLight.Intensity=0: ExpLight.State=0: EXPLI=0: MoveDiverter 0: End If
End Sub




'******************************************************
'                  Explosion Animation
'******************************************************

Dim ExCnt
ExCnt = 0
ExAnimation.Interval=17*2  '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub ExAnimation_Timer
	'Initialize the animation
	If ExFlasher.visible = False Then 
		ExFlasher.visible = True
		ExCnt = 0
	End If

	'Select the correct frame
	If ExCnt > 99 Then
		ExFlasher.imageA = "EXP-" & ExCnt
	Elseif ExCnt > 9 Then
		ExFlasher.imageA = "EXP-0" & ExCnt
	Else
		ExFlasher.imageA = "EXP-00" & ExCnt
	End If
	ExCnt = ExCnt + 1
    ExpLight.State=1

	'Finish animation
	If ExCnt > 113 Then    
		ExCnt = 0
		ExAnimation.Enabled = False
		ExFlasher.visible = False
	End If
End Sub

'******************************************************
'                  Muzzle Flash
'******************************************************

Dim MuzzleCnt
MuzzleCnt = 2
MuzzleFlash.Interval=17*2  '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub MuzzleFlash_Timer
	'Initialize the animation
	If MuzzleFlasher.visible = False Then 
		MuzzleFlasher.visible = True
		MuzzleCnt = 2
	End If

	'Select the correct frame
	If MuzzleCnt > 99 Then
		MuzzleFlasher.imageA = "MF-" & MuzzleCnt
	Elseif MuzzleCnt > 9 Then
		MuzzleFlasher.imageA = "MF-0" & MuzzleCnt
	Else
		MuzzleFlasher.imageA = "MF-00" & MuzzleCnt
	End If
	MuzzleCnt = MuzzleCnt + 1

	'Finish animation
    If MuzzleCnt = 4 then MuzzleLight.State=1
    If MuzzleCnt > 5 then MuzzleLight.State=0
	If MuzzleCnt > 29 Then    
		MuzzleCnt = 2
		MuzzleFlash.Enabled = False
		MuzzleFlasher.visible = False
	End If
End Sub

'******************************************************
'             Callout Audio Adjustment
'******************************************************

Sub PlayCallout(sound)
    PlaySound sound, 0, CalloutVol, 0,0,0,0,0,0
End Sub

'******************************************************
'                   Lightning Calls
'******************************************************

Sub LightningFX
    Dim x
    x = INT(6 * RND(1) )
    Select Case x
		Case 0:PlayCallout"Thunder-01":LightningShow
		Case 1:PlayCallout"Thunder-02":LightningShow
		Case 2:PlayCallout"Thunder-03":LightningShow
		Case 3:PlayCallout"Thunder-04":LightningShow
		Case 4:PlayCallout"Thunder-05":LightningShow
		Case 5:PlayCallout"Thunder-06":LightningShow
    End Select
End Sub

Sub RandomSlice
    Dim x
    x = INT(6 * RND(1) )
    Select Case x
		Case 0:PlayCallout"Slice-01a"
		Case 1:PlayCallout"Slice-02a"
		Case 2:PlayCallout"Slice-03a"
		Case 3:PlayCallout"Slice-04a"
		Case 4:PlayCallout"Slice-05a"
		Case 5:PlayCallout"Slice-06a"
    End Select
End Sub 

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
Sub TargetBounce_Hit(idx)
	TargetBouncer activeball, 1
End Sub




'******************************************************
' Flippers Polarity (Select appropriate sub based on era) 
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity


Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
	Next

	AddPt "Polarity", 0, 0, 0
	AddPt "Polarity", 1, 0.05, -5
	AddPt "Polarity", 2, 0.4, -5
	AddPt "Polarity", 3, 0.6, -4.5
	AddPt "Polarity", 4, 0.65, -4.0
	AddPt "Polarity", 5, 0.7, -3.5
	AddPt "Polarity", 6, 0.75, -3.0
	AddPt "Polarity", 7, 0.8, -2.5
	AddPt "Polarity", 8, 0.85, -2.0
	AddPt "Polarity", 9, 0.9,-1.5
	AddPt "Polarity", 10, 0.95, -1.0
	AddPt "Polarity", 11, 1, -0.5
	AddPt "Polarity", 12, 1.1, 0
	AddPt "Polarity", 13, 1.3, 0

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
'  FLIPPER POLARITY. RUBBER DAMPENER, AND SLINGSHOT CORRECTION SUPPORTING FUNCTIONS 
'******************************************************


Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub


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

Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

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
Const EOSTnew = 1 'EM's to late 80's
'Const EOSTnew = 0.8 '90's and later
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
Const EOSReturn = 0.045  'late 70's to mid 80's
'Const EOSReturn = 0.035  'mid 80's to early 90's
'Const EOSReturn = 0.025  'mid 90's and later

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
End Sub



'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************





'******************************************************
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************
' To add these slingshot corrections:
' 	- On the table, add the endpoint primitives that define the two ends of the Slingshot
'	- Initialize the SlingshotCorrection objects in InitSlingCorrection
' 	- Call the .VelocityCorrect methods from the respective _Slingshot event sub


dim LS :  Set LS  = New SlingshotCorrection
dim RS :  Set RS  = New SlingshotCorrection
dim URS : Set URS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection

	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS

	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS

	URS.Object = sw34
	URS.EndPoint1 = EndPoint1URS
	URS.EndPoint2 = EndPoint2URS


	'Slingshot angle corrections (pt, BallPos in %, Angle in deg)
	' These values are best guesses. Retune them if needed based on specific table research.
	AddSlingsPt 0, 0.00,	-4
	AddSlingsPt 1, 0.45,	-7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4

End Sub


Sub AddSlingsPt(idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LS, RS)
	dim x : for each x in a
		x.addpoint idx, aX, aY
	Next
End Sub

Class SlingshotCorrection
	Public DebugOn, Enabled
	private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2

	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): Enabled = True : End Sub 

	Public Property let Object(aInput) : Set Slingshot = aInput : End Property
	Public Property Let EndPoint1(aInput) : SlingX1 = aInput.x: SlingY1 = aInput.y: End Property
	Public Property Let EndPoint2(aInput) : SlingX2 = aInput.x: SlingY2 = aInput.y: End Property

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		If gametime > 100 then Report
	End Sub

	Public Sub Report()         'debug, reports all coords in tbPL.text
		If not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub


	Public Sub VelocityCorrect(aBall)
		dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then 
			XL = SlingX1 : YL = SlingY1 : XR = SlingX2 : YR = SlingY2
		Else
			XL = SlingX2 : YL = SlingY2 : XR = SlingX1 : YR = SlingY1
		End If

		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then 
			If ABS(XR-XL) > ABS(YR-YL) Then 
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If

		'Velocity angle correction
		If not IsEmpty(ModIn(0) ) then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'debug.print " BallPos=" & BallPos &" Angle=" & Angle 
			'debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled then aBall.Velx = RotVxVy(0)
			If Enabled then aBall.Vely = RotVxVy(1)
			'debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			'debug.print " " 
		End If
	End Sub

End Class



'******************************************************
'  DROP TARGET
'  SUPPORTING FUNCTIONS 
'******************************************************


' Used for drop targets
'*** Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy)
	Dim AB, BC, CD, DA
	AB = (bx*py) - (by*px) - (ax*py) + (ay*px) + (ax*by) - (ay*bx)
	BC = (cx*py) - (cy*px) - (bx*py) + (by*px) + (bx*cy) - (by*cx)
	CD = (dx*py) - (dy*px) - (cx*py) + (cy*px) + (cx*dy) - (cy*dx)
	DA = (ax*py) - (ay*px) - (dx*py) + (dy*px) + (dx*ay) - (dy*ax)

	If (AB <= 0 AND BC <=0 AND CD <= 0 AND DA <= 0) Or (AB >= 0 AND BC >=0 AND CD >= 0 AND DA >= 0) Then
		InRect = True
	Else
		InRect = False       
	End If
End Function

Function InRotRect(ballx,bally,px,py,angle,ax,ay,bx,by,cx,cy,dx,dy)
    Dim rax,ray,rbx,rby,rcx,rcy,rdx,rdy
    Dim rotxy
    rotxy = RotPoint(ax,ay,angle)
    rax = rotxy(0)+px : ray = rotxy(1)+py
    rotxy = RotPoint(bx,by,angle)
    rbx = rotxy(0)+px : rby = rotxy(1)+py
    rotxy = RotPoint(cx,cy,angle)
    rcx = rotxy(0)+px : rcy = rotxy(1)+py
    rotxy = RotPoint(dx,dy,angle)
    rdx = rotxy(0)+px : rdy = rotxy(1)+py

    InRotRect = InRect(ballx,bally,rax,ray,rbx,rby,rcx,rcy,rdx,rdy)
End Function

Function RotPoint(x,y,angle)
    dim rx, ry
    rx = x*dCos(angle) - y*dSin(angle)
    ry = x*dSin(angle) + y*dCos(angle)
    RotPoint = Array(rx,ry)
End Function





'******************************************************
'****  DROP TARGETS by Rothbauerw
'******************************************************
' This solution improves the physics for drop targets to create more realistic behavior. It allows the ball 
' to move through the target enabling the ability to score more than one target with a well placed shot.
' It also handles full drop target animation, including deflection on hit and a slight lift when the drop 
' targets raise, switch handling, bricking, and popping the ball up if it's over the drop target when it raises.
'
'Add a Timer named DTAnim to editor to handle drop & standup target animations, or run them off an always-on 10ms timer (GameTimer)
DTAnim.interval = 10
DTAnim.enabled = True

Sub DTAnim_Timer
	DoDTAnim
	DoSTAnim
End Sub

' For each drop target, we'll use two wall objects for physics calculations and one primitive for visuals and   
' animation. We will not use target objects.  Place your drop target primitive the same as you would a VP drop target. 
' The primitive should have it's pivot point centered on the x and y axis and at or just below the playfield 
' level on the z axis. Orientation needs to be set using Rotz and bending deflection using Rotx. You'll find a hooded 
' target mesh in this table's example. It uses the same texture map as the VP drop targets.


'******************************************************
'  DROP TARGETS INITIALIZATION
'******************************************************

Class DropTarget
  Private m_primary, m_secondary, m_prim, m_sw, m_animate, m_isDropped

  Public Property Get Primary(): Set Primary = m_primary: End Property
  Public Property Let Primary(input): Set m_primary = input: End Property

  Public Property Get Secondary(): Set Secondary = m_secondary: End Property
  Public Property Let Secondary(input): Set m_secondary = input: End Property

  Public Property Get Prim(): Set Prim = m_prim: End Property
  Public Property Let Prim(input): Set m_prim = input: End Property

  Public Property Get Sw(): Sw = m_sw: End Property
  Public Property Let Sw(input): m_sw = input: End Property

  Public Property Get Animate(): Animate = m_animate: End Property
  Public Property Let Animate(input): m_animate = input: End Property

  Public Property Get IsDropped(): IsDropped = m_isDropped: End Property
  Public Property Let IsDropped(input): m_isDropped = input: End Property

  Public default Function init(primary, secondary, prim, sw, animate, isDropped)
    Set m_primary = primary
    Set m_secondary = secondary
    Set m_prim = prim
    m_sw = sw
    m_animate = animate
    m_isDropped = isDropped

    Set Init = Me
  End Function
End Class

'Define a variable for each drop target
Dim DT1, DT2, DT3, DT4, DT42, DT43, DT44, DT45, DT46, DT47, DT48

'Set array with drop target objects
'
'DropTargetvar = Array(primary, secondary, prim, swtich, animate)
' 	primary: 			primary target wall to determine drop
'	secondary:			wall used to simulate the ball striking a bent or offset target after the initial Hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							rotz must be used for orientation
'							rotx to bend the target back
'							transz to move it up and down
'							the pivot point should be in the center of the target on the x, y and at or below the playfield (0) on z
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instrucitons, set to 0
'
'	Values for annimate: 1 - bend target (hit to primary), 2 - drop target (hit to secondary), 3 - brick target (high velocity hit to secondary), -1 - raise target 

Set DT1 = (new DropTarget)(sw1, a_sw1, p_sw1, 1, 0, 0)
Set DT2 = (new DropTarget)(sw2, a_sw2, p_sw2, 2, 0, 0)
Set DT3 = (new DropTarget)(sw3, a_sw3, p_sw3, 3, 0, 0)
Set DT4 = (new DropTarget)(sw4, a_sw4, p_sw4, 4, 0, 0)
Set DT42 = (new DropTarget)(sw42, a_sw42, p_sw42, 42, 0, 0)
Set DT43 = (new DropTarget)(sw43, a_sw43, p_sw43, 43, 0, 0)
Set DT44 = (new DropTarget)(sw44, a_sw44, p_sw44, 44, 0, 0)
Set DT45 = (new DropTarget)(sw45, a_sw45, p_sw45, 45, 0, 0)
Set DT46 = (new DropTarget)(sw46, a_sw46, p_sw46, 46, 0, 0)
Set DT47 = (new DropTarget)(sw47, a_sw47, p_sw47, 47, 0, 0)
Set DT48 = (new DropTarget)(sw48, a_sw48, p_sw48, 48, 0, 0)

Dim DTArray
DTArray = Array(DT1, DT2, DT3, DT4, DT42, DT43, DT44, DT45, DT46, DT47, DT48)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 100 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 44 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 10 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30 'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0 'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "" 'Drop Target Hit sound
Const DTDropSound = "DropTarget_Down" 'Drop Target Drop sound
Const DTResetSound = "DropTarget_Up" 'Drop Target reset sound

Const DTMass = 0.2 'Mass of the Drop Target (between 0 and 1), higher values provide more resistance


'******************************************************
'  DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch)

	PlayTargetSound
	DTArray(i).isDropped = 1
	DTArray(i).animate =  DTCheckBrick(Activeball,DTArray(i).prim)
	If DTArray(i).animate = 1 or DTArray(i).animate = 3 or DTArray(i).animate = 4 Then
		DTBallPhysics Activeball, DTArray(i).prim.rotz, DTMass
	End If
	DoDTAnim
End Sub

Sub DTRaise(switch)
	Dim i
	i = DTArrayID(switch)

	DTArray(i).animate = -1
	DoDTAnim
	RandomSoundDropTargetReset DTArray(i).prim
End Sub

Sub DTDrop(switch)
	Dim i
	i = DTArrayID(switch)

	DTArray(i).animate = 1
	DTArray(i).isDropped = 0
	DoDTAnim
End Sub

Function DTArrayID(switch)
	Dim i
	For i = 0 to uBound(DTArray) 
		If DTArray(i).sw = switch Then DTArrayID = i:Exit Function 
	Next
End Function


sub DTBallPhysics(aBall, angle, mass)
	dim rangle,bangle,calc1, calc2, calc3
	rangle = (angle - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))

	calc1 = cor.BallVel(aball.id) * cos(bangle - rangle) * (aball.mass - mass) / (aball.mass + mass)
	calc2 = cor.BallVel(aball.id) * sin(bangle - rangle) * cos(rangle + 4*Atn(1)/2)
	calc3 = cor.BallVel(aball.id) * sin(bangle - rangle) * sin(rangle + 4*Atn(1)/2)

	aBall.velx = calc1 * cos(rangle) + calc2
	aBall.vely = calc1 * sin(rangle) + calc3
End Sub


'Check if target is hit on it's face or sides and whether a 'brick' occurred
Function DTCheckBrick(aBall, dtprim) 
	dim bangle, bangleafter, rangle, rangle2, Xintersect, Yintersect, cdist, perpvel, perpvelafter, paravel, paravelafter
	rangle = (dtprim.rotz - 90) * 3.1416 / 180
	rangle2 = dtprim.rotz * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)

	Xintersect = (aBall.y - dtprim.y - tan(bangle) * aball.x + tan(rangle2) * dtprim.x) / (tan(rangle2) - tan(bangle))
	Yintersect = tan(rangle2) * Xintersect + (dtprim.y - tan(rangle2) * dtprim.x)

	cdist = Distance(dtprim.x, dtprim.y, Xintersect, Yintersect)

	perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
	paravel = cor.BallVel(aball.id) * sin(bangle-rangle)

	perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle) 
	paravelafter = BallSpeed(aBall) * sin(bangleafter - rangle)

	If perpvel > 0 and  perpvelafter <= 0 Then
		If DTEnableBrick = 1 and  perpvel > DTBrickVel and DTBrickVel <> 0 and cdist < 8 Then
			DTCheckBrick = 3
		Else
			DTCheckBrick = 1
		End If
	ElseIf perpvel > 0 and ((paravel > 0 and paravelafter > 0) or (paravel < 0 and paravelafter < 0)) Then
		DTCheckBrick = 4
	Else 
		DTCheckBrick = 0
	End If
End Function

Sub DoDTAnim()
	Dim i
	For i=0 to Ubound(DTArray)
		DTArray(i).animate = DTAnimate(DTArray(i).primary,DTArray(i).secondary,DTArray(i).prim,DTArray(i).sw,DTArray(i).animate)
	Next
End Sub

Function DTAnimate(primary, secondary, prim, switch,  animate)
	dim transz, switchid
	Dim animtime, rangle

	switchid = switch

	rangle = prim.rotz * PI / 180

	DTAnimate = animate

	if animate = 0  Then
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	Elseif primary.uservalue = 0 then
		primary.uservalue = gametime
	end if

	animtime = gametime - primary.uservalue

	If (animate = 1 or animate = 4) and animtime < DTDropDelay Then
		primary.collidable = 0
	If animate = 1 then secondary.collidable = 1 else secondary.collidable= 0
		prim.rotx = DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
		DTAnimate = animate
		Exit Function
	elseif (animate = 1 or animate = 4) and animtime > DTDropDelay Then
		primary.collidable = 0
		If animate = 1 then secondary.collidable = 1 else secondary.collidable= 0
		prim.rotx = DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
		animate = 2
		SoundDropTargetDrop prim
	End If

	if animate = 2 Then
		transz = (animtime - DTDropDelay)/DTDropSpeed *  DTDropUnits * -1
		if prim.transz > -DTDropUnits  Then
			prim.transz = transz
		end if

		prim.rotx = DTMaxBend * cos(rangle)/2
		prim.roty = DTMaxBend * sin(rangle)/2

		if prim.transz <= -DTDropUnits Then 
			prim.transz = -DTDropUnits
			secondary.collidable = 0
			controller.Switch(Switchid) = 1
			DTAction(Switchid)
			primary.uservalue = 0
			DTAnimate = 0
			Exit Function
		Else
			DTAnimate = 2
			Exit Function
		end If 
	End If

	If animate = 3 and animtime < DTDropDelay Then
		primary.collidable = 0
		secondary.collidable = 1
		prim.rotx = DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
	elseif animate = 3 and animtime > DTDropDelay Then
		primary.collidable = 1
		secondary.collidable = 0
		prim.rotx = 0
		prim.roty = 0
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	End If

	if animate = -1 Then
		transz = (1 - (animtime)/DTDropUpSpeed) *  DTDropUnits * -1

		If prim.transz = -DTDropUnits Then
			Dim b, BOT
			BOT = GetBalls

			For b = 0 to UBound(BOT)
				If InRotRect(BOT(b).x,BOT(b).y,prim.x, prim.y, prim.rotz, -25,-10,25,-10,25,25,-25,25) and BOT(b).z < prim.z+DTDropUnits+25 Then
					BOT(b).velz = 20
				End If
			Next
		End If

		if prim.transz < 0 Then
			prim.transz = transz
		elseif transz > 0 then
			prim.transz = transz
		end if

		if prim.transz > DTDropUpUnits then 
			DTAnimate = -2
			prim.transz = DTDropUpUnits
			prim.rotx = 0
			prim.roty = 0
			primary.uservalue = gametime
		end if
		primary.collidable = 0
		secondary.collidable = 1
		controller.Switch(Switchid) = 0

	End If

	if animate = -2 and animtime > DTRaiseDelay Then
		prim.transz = (animtime - DTRaiseDelay)/DTDropSpeed *  DTDropUnits * -1 + DTDropUpUnits 
		if prim.transz < 0 then
			prim.transz = 0
			primary.uservalue = 0
			DTAnimate = 0

			primary.collidable = 1
			secondary.collidable = 0
		end If 
	End If
End Function

Sub DTAction(switchid)
	Dim i : i = DTArrayID(switchid)
	If switchid>=42 and switchid<=48 Then
		If DTArray(i).isDropped = 1 Then
			StopSound"0HE01"
			PlayCallout"0HE01"
		End If
	End If
	DTArray(i).isDropped = 0
End Sub


'******************************************************
'		END DROP TARGETS
'******************************************************




'******************************************************
'		STAND-UP TARGET INITIALIZATION
'******************************************************

Class StandupTarget
  Private m_primary, m_prim, m_sw, m_animate

  Public Property Get Primary(): Set Primary = m_primary: End Property
  Public Property Let Primary(input): Set m_primary = input: End Property

  Public Property Get Prim(): Set Prim = m_prim: End Property
  Public Property Let Prim(input): Set m_prim = input: End Property

  Public Property Get Sw(): Sw = m_sw: End Property
  Public Property Let Sw(input): m_sw = input: End Property

  Public Property Get Animate(): Animate = m_animate: End Property
  Public Property Let Animate(input): m_animate = input: End Property

  Public default Function init(primary, prim, sw, animate)
    Set m_primary = primary
    Set m_prim = prim
    m_sw = sw
    m_animate = animate

    Set Init = Me
  End Function
End Class

'Define a variable for each stand-up target
Dim ST12, ST13, ST14, ST27, ST29

'Set array with stand-up target objects
'
'StandupTargetvar = Array(primary, prim, swtich)
' 	primary: 			vp target to determine target hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							transy must be used to offset the target animation
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instrucitons, set to 0
' 
'You will also need to add a secondary hit object for each stand up (name sw11o, sw12o, and sw13o on the example Table1)
'these are inclined primitives to simulate hitting a bent target and should provide so z velocity on high speed impacts


set ST12 = (new StandupTarget)(sw12, psw12, 12, 0)
set ST13 = (new StandupTarget)(sw13, psw13, 13, 0)
set ST14 = (new StandupTarget)(sw14, psw14, 14, 0)
set ST27 = (new StandupTarget)(sw27, psw27, 27, 0)
set ST29 = (new StandupTarget)(sw29, psw29, 29, 0)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST12, ST13, ST14, ST27, ST29)

'Configure the behavior of Stand-up Targets
Const STAnimStep =  1.5 				'vpunits per animation step (control return to Start)
Const STMaxOffset = 9 			'max vp units target moves when hit

Const STMass = 0.2				'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				STAND-UP TARGETS FUNCTIONS
'******************************************************

Sub STHit(switch)
	Dim i
	i = STArrayID(switch)

	PlayTargetSound
	STArray(i).animate =  STCheckHit(Activeball,STArray(i).primary)

	If STArray(i).animate <> 0 Then
		DTBallPhysics Activeball, STArray(i).primary.orientation, STMass
	End If
	DoSTAnim
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 to uBound(STArray) 
		If STArray(i).sw = switch Then STArrayID = i:Exit Function 
	Next
End Function

'Check if target is hit on it's face
Function STCheckHit(aBall, target) 
	dim bangle, bangleafter, rangle, rangle2, perpvel, perpvelafter, paravel, paravelafter
	rangle = (target.orientation - 90) * 3.1416 / 180	
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)

	perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
	paravel = cor.BallVel(aball.id) * sin(bangle-rangle)

	perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle) 
	paravelafter = BallSpeed(aBall) * sin(bangleafter - rangle)

	If perpvel > 0 and  perpvelafter <= 0 Then
		STCheckHit = 1
	ElseIf perpvel > 0 and ((paravel > 0 and paravelafter > 0) or (paravel < 0 and paravelafter < 0)) Then
		STCheckHit = 1
	Else 
		STCheckHit = 0
	End If
End Function

Sub DoSTAnim()
	Dim i
	For i=0 to Ubound(STArray)
		STArray(i).animate = STAnimate(STArray(i).primary,STArray(i).prim,STArray(i).sw,STArray(i).animate)
	Next
End Sub

Function STAnimate(primary, prim, switch,  animate)
	Dim animtime

	STAnimate = animate

	if animate = 0  Then
		primary.uservalue = 0
		STAnimate = 0
		Exit Function
	Elseif primary.uservalue = 0 then 
		primary.uservalue = gametime
	end if

	animtime = gametime - primary.uservalue

	If animate = 1 Then
		primary.collidable = 0
		prim.transy = -STMaxOffset
		vpmTimer.PulseSw switch
		STAnimate = 2
		Exit Function
	elseif animate = 2 Then
		prim.transy = prim.transy + STAnimStep
		If prim.transy >= 0 Then
			prim.transy = 0
			primary.collidable = 1
			STAnimate = 0
			Exit Function
		Else 
			STAnimate = 2
		End If
	End If	
End Function

'******************************************************
'		END STAND-UP TARGETS
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
SaucerLockSoundLevel = 0.9
SaucerKickSoundLevel = 0.9

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5													'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10											'volume multiplier; must not be zero
DTSoundLevel = 0.25														'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                              					'volume level; range [0, 1]
SpinnerSoundLevel = 0.2                              					'volume level; range [0, 1]

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



'///////////////////////////  COIN SOUNDS  ///////////////////////////

Sub RandomCoinSound()
	Select Case Int(rnd*3)
		Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
		Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
		Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
	End Select
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



' 1 = Easy - Multiball is automatically lit after a 30 sec release from plunder lane
' 2 = Med - Light both A and B lights that are at top of playfield to light MB
' 3 = Hard - Light the 5X bonus multipler (three left oribit shots) to light MB
Const MBSD = 2			
SetMBSD
Sub SetMBSD
	Select Case MBSD
		Case 2: MBS=2: MBLitTimer.Interval = 800
		Case 3: MBS=3: MBLitTimer.Interval = 1000
		Case Else: MBS=1: MBLitTimer.Interval = 30000
	End Select
End Sub



'**************************
'Lightning Flashers
'**************************

Dim FlashLevelz, FlashLight
FlashLevelz = Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
FlashLight = Array(FlasherLight1,FlasherLight2,FlasherLight3,FlasherLight4,FlasherLight5,FlasherLight6,FlasherLight7,_
      FlasherLight8,FlasherLight9,FlasherLight10,FlasherLight11,FlasherLight12,FlasherLight13,FlasherLight14,FlasherLight15)

FlasherInit
Sub FlasherInit
	Dim i: For i = 0 to UBound(FlashLight)
		FlashLight(i).IntensityScale = 0
	Next
End Sub


Sub FlasherFlash(nr)
	nr = nr - 1
	If not FlashLight(nr).TimerEnabled Then 
		FlashLight(nr).TimerEnabled = True
	End If
	FlashLevelz(nr) = FlashLevelz(nr) * 0.85 - 0.01
	If FlashLevelz(nr) < 0 Then
		FlashLight(nr).TimerEnabled = False
		FlashLevelz(nr) = 0
	End If
	FlashLight(nr).IntensityScale = FlashLevelz(nr)^3
End Sub


Sub FlasherLight1_Timer:  FlasherFlash(1):  End Sub
Sub FlasherLight2_Timer:  FlasherFlash(2):  End Sub
Sub FlasherLight3_Timer:  FlasherFlash(3):  End Sub
Sub FlasherLight4_Timer:  FlasherFlash(4):  End Sub
Sub FlasherLight5_Timer:  FlasherFlash(5):  End Sub
Sub FlasherLight6_Timer:  FlasherFlash(6):  End Sub
Sub FlasherLight7_Timer:  FlasherFlash(7):  End Sub
Sub FlasherLight8_Timer:  FlasherFlash(8):  End Sub
Sub FlasherLight9_Timer:  FlasherFlash(9):  End Sub
Sub FlasherLight10_Timer: FlasherFlash(10): End Sub
Sub FlasherLight11_Timer: FlasherFlash(11): End Sub
Sub FlasherLight12_Timer: FlasherFlash(12): End Sub
Sub FlasherLight13_Timer: FlasherFlash(13): End Sub
Sub FlasherLight14_Timer: FlasherFlash(14): End Sub
Sub FlasherLight15_Timer: FlasherFlash(15): End Sub

Sub LightningLight(nr)
	FlashLevelz(nr-1) = 1
	Select Case nr
		Case 1: FlasherLight1_Timer
		Case 2: FlasherLight2_Timer
		Case 3: FlasherLight3_Timer
		Case 4: FlasherLight4_Timer
		Case 5: FlasherLight5_Timer
		Case 6: FlasherLight6_Timer
		Case 7: FlasherLight7_Timer
		Case 8: FlasherLight8_Timer
		Case 9: FlasherLight9_Timer
		Case 10: FlasherLight10_Timer
		Case 11: FlasherLight11_Timer
		Case 12: FlasherLight12_Timer
		Case 13: FlasherLight13_Timer
		Case 14: FlasherLight14_Timer
		Case 15: FlasherLight15_Timer
	End Select
End Sub


'********************************
'Random Lightning Pattern Selection

Sub LightningShow
    Dim x
    x = INT(5 * RND(1) )
    Select Case x
		Case 0:Lightning1.enabled=true:Lightning1.interval=100:Lightning1off.enabled=true:Lightning1off.interval=500
		Case 1:Lightning6.enabled=true:Lightning6.interval=100:Lightning6off.enabled=true:Lightning6off.interval=500
		Case 2:Lightning11.enabled=true:Lightning11.interval=100:Lightning11off.enabled=true:Lightning11off.interval=500
		Case 3:Lightning16.enabled=true:Lightning16.interval=100:Lightning16off.enabled=true:Lightning16off.interval=500
		Case 4:Lightning21.enabled=true:Lightning21.interval=100:Lightning21off.enabled=true:Lightning21off.interval=500
    End Select
end sub 




'**********************************
'Lightning Center

Sub Lightning1_Timer
    Dim x
    x = INT(2 * RND(1) )
    Select Case x
    Case 0:LightningLight 1:LightningLight 2
    Case 1:LightningLight 3:LightningLight 4:LightningLight 5
    End Select
end sub 


Sub Lightning1off_Timer
    Lightning1.enabled=false
    Lightning1off.enabled=false
    Lightning2.enabled=true:Lightning2.interval=100:Lightning2off.enabled=true:Lightning2off.interval=400
end sub


Sub Lightning2_Timer
    Dim x
    x = INT(4 * RND(1) )
    Select Case x
    Case 0:LightningLight 5:LightningLight 6
    Case 1:LightningLight 7:LightningLight 6
    Case 2:LightningLight 6
    Case 3:LightningLight 7
    End Select
end sub 

Sub Lightning2off_Timer
    Lightning2.enabled=false
    Lightning2off.enabled=false
    Lightning3.enabled=true:Lightning3.interval=100:Lightning3off.enabled=true:Lightning3off.interval=300
end sub

Sub Lightning3_Timer
    Dim x
    x = INT(4 * RND(1) )
    Select Case x
    Case 0:LightningLight 8:LightningLight 10
    Case 1:LightningLight 11:LightningLight 9
    Case 2:LightningLight 9
    Case 3:LightningLight 11
    End Select
end sub 

Sub Lightning3off_Timer
    Lightning3.enabled=false
    Lightning3off.enabled=false
    Lightning4.enabled=true:Lightning4.interval=100:Lightning4off.enabled=true:Lightning4off.interval=200
end sub


Sub Lightning4_Timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:LightningLight 13:LightningLight 12
    Case 1:LightningLight 12
    Case 2:LightningLight 13
    End Select
end sub 

Sub Lightning4off_Timer
    Lightning4.enabled=false
    Lightning4off.enabled=false
    Lightning5.enabled=true:Lightning5.interval=100:Lightning5off.enabled=true:Lightning5off.interval=200
end sub

Sub Lightning5_Timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:LightningLight 14
    Case 1:LightningLight 15:LightningLight 14
    Case 2:LightningLight 15
    End Select
end sub 

Sub Lightning5off_Timer
    Lightning5.enabled=false
    Lightning5off.enabled=false
end sub

'**********************************
'Lightning Left

Sub Lightning6_Timer
    Dim x
    x = INT(2 * RND(1) )
    Select Case x
    Case 0:LightningLight 1:LightningLight 2
    Case 1:LightningLight 3:LightningLight 4:LightningLight 5
    End Select
end sub 


Sub Lightning6off_Timer
    Lightning6.enabled=false
    Lightning6off.enabled=false
    Lightning7.enabled=true:Lightning7.interval=100:Lightning7off.enabled=true:Lightning7off.interval=400
end sub


Sub Lightning7_Timer
    Dim x
    x = INT(4 * RND(1) )
    Select Case x
    Case 0:LightningLight 5:LightningLight 6
    Case 1:LightningLight 7:LightningLight 6
    Case 2:LightningLight 6
    Case 3:LightningLight 7
    End Select
end sub 

Sub Lightning7off_Timer
    Lightning7.enabled=false
    Lightning7off.enabled=false
    Lightning8.enabled=true:Lightning8.interval=100:Lightning8off.enabled=true:Lightning8off.interval=300
end sub

Sub Lightning8_Timer
    Dim x
    x = INT(4 * RND(1) )
    Select Case x
    Case 0:LightningLight 8
    Case 1:LightningLight 9
    Case 2:LightningLight 9
    Case 3:LightningLight 8
    End Select
end sub 

Sub Lightning8off_Timer
    Lightning8.enabled=false
    Lightning8off.enabled=false
    Lightning9.enabled=true:Lightning9.interval=100:Lightning9off.enabled=true:Lightning9off.interval=200
end sub


Sub Lightning9_Timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:LightningLight 12
    Case 1:LightningLight 14
    Case 2:LightningLight 12
    End Select
end sub 

Sub Lightning9off_Timer
    Lightning9.enabled=false
    Lightning9off.enabled=false
    Lightning10.enabled=true:Lightning10.interval=100:Lightning10off.enabled=true:Lightning10off.interval=200
end sub

Sub Lightning10_Timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:LightningLight 14
    Case 1:LightningLight 15:LightningLight 14
    Case 2:LightningLight 14
    End Select
end sub 

Sub Lightning10off_Timer
    Lightning10.enabled=false
    Lightning10off.enabled=false
end sub


'*************************************
'Lightning Right

Sub Lightning11_Timer
    Dim x
    x = INT(2 * RND(1) )
    Select Case x
    Case 0:LightningLight 1:LightningLight 2
    Case 1:LightningLight 3:LightningLight 4:LightningLight 5
    End Select
end sub 


Sub Lightning11off_Timer
    Lightning11.enabled=false
    Lightning11off.enabled=false
    Lightning12.enabled=true:Lightning12.interval=100:Lightning12off.enabled=true:Lightning12off.interval=400
end sub


Sub Lightning12_Timer
    Dim x
    x = INT(4 * RND(1) )
    Select Case x
    Case 0:LightningLight 5:LightningLight 6
    Case 1:LightningLight 7:LightningLight 6
    Case 2:LightningLight 6
    Case 3:LightningLight 7
    End Select
end sub 

Sub Lightning12off_Timer
    Lightning12.enabled=false
    Lightning12off.enabled=false
    Lightning13.enabled=true:Lightning13.interval=100:Lightning13off.enabled=true:Lightning13off.interval=300
end sub

Sub Lightning13_Timer
    Dim x
    x = INT(4 * RND(1) )
    Select Case x
    Case 0:LightningLight 10
    Case 1:LightningLight 11
    Case 2:LightningLight 10
    Case 3:LightningLight 11
    End Select
end sub 

Sub Lightning13off_Timer
    Lightning13.enabled=false
    Lightning13off.enabled=false
    Lightning14.enabled=true:Lightning14.interval=100:Lightning14off.enabled=true:Lightning14off.interval=200
end sub


Sub Lightning14_Timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:LightningLight 13
    Case 1:LightningLight 15
    Case 2:LightningLight 13
    End Select
end sub 

Sub Lightning14off_Timer
    Lightning14.enabled=false
    Lightning14off.enabled=false
    Lightning15.enabled=true:Lightning15.interval=100:Lightning15off.enabled=true:Lightning15off.interval=200
end sub

Sub Lightning15_Timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:LightningLight 15
    Case 1:LightningLight 15:LightningLight 14
    Case 2:LightningLight 15
    End Select
end sub 

Sub Lightning15off_Timer
    Lightning15.enabled=false
    Lightning15off.enabled=false
end sub


'*********************************
'Lightning Left to Right

Sub Lightning16_Timer
    Dim x
    x = INT(2 * RND(1) )
    Select Case x
    Case 0:LightningLight 1:LightningLight 2
    Case 1:LightningLight 3:LightningLight 4:LightningLight 5
    End Select
end sub 


Sub Lightning16off_Timer
    Lightning16.enabled=false
    Lightning16off.enabled=false
    Lightning17.enabled=true:Lightning17.interval=100:Lightning17off.enabled=true:Lightning17off.interval=400
end sub


Sub Lightning17_Timer
    Dim x
    x = INT(4 * RND(1) )
    Select Case x
    Case 0:LightningLight 5:LightningLight 6
    Case 1:LightningLight 7:LightningLight 6
    Case 2:LightningLight 6
    Case 3:LightningLight 7
    End Select
end sub 

Sub Lightning17off_Timer
    Lightning17.enabled=false
    Lightning17off.enabled=false
    Lightning18.enabled=true:Lightning18.interval=100:Lightning18off.enabled=true:Lightning18off.interval=300
end sub

Sub Lightning18_Timer
    Dim x
    x = INT(4 * RND(1) )
    Select Case x
    Case 0:LightningLight 8
    Case 1:LightningLight 9
    Case 2:LightningLight 9
    Case 3:LightningLight 8
    End Select
end sub 

Sub Lightning18off_Timer
    Lightning18.enabled=false
    Lightning18off.enabled=false
    Lightning19.enabled=true:Lightning19.interval=100:Lightning19off.enabled=true:Lightning19off.interval=200
end sub


Sub Lightning19_Timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:LightningLight 13
    Case 1:LightningLight 11
    Case 2:LightningLight 13
    End Select
end sub 

Sub Lightning19off_Timer
    Lightning19.enabled=false
    Lightning19off.enabled=false
    Lightning20.enabled=true:Lightning20.interval=100:Lightning20off.enabled=true:Lightning20off.interval=200
end sub

Sub Lightning20_Timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:LightningLight 15
    Case 1:LightningLight 15:LightningLight 14
    Case 2:LightningLight 15
    End Select
end sub 

Sub Lightning20off_Timer
    Lightning20.enabled=false
    Lightning20off.enabled=false
end sub

'*************************************
'Lightning Right to Left

Sub Lightning21_Timer
    Dim x
    x = INT(2 * RND(1) )
    Select Case x
    Case 0:LightningLight 1:LightningLight 2
    Case 1:LightningLight 3:LightningLight 4:LightningLight 5
    End Select
end sub 


Sub Lightning21off_Timer
    Lightning21.enabled=false
    Lightning21off.enabled=false
    Lightning22.enabled=true:Lightning22.interval=100:Lightning22off.enabled=true:Lightning22off.interval=400
end sub


Sub Lightning22_Timer
    Dim x
    x = INT(4 * RND(1) )
    Select Case x
    Case 0:LightningLight 5:LightningLight 6
    Case 1:LightningLight 7:LightningLight 6
    Case 2:LightningLight 6
    Case 3:LightningLight 7
    End Select
end sub 

Sub Lightning22off_Timer
    Lightning22.enabled=false
    Lightning22off.enabled=false
    Lightning23.enabled=true:Lightning23.interval=100:Lightning23off.enabled=true:Lightning23off.interval=300
end sub

Sub Lightning23_Timer
    Dim x
    x = INT(4 * RND(1) )
    Select Case x
    Case 0:LightningLight 10
    Case 1:LightningLight 11
    Case 2:LightningLight 10
    Case 3:LightningLight 11
    End Select
end sub 

Sub Lightning23off_Timer
    Lightning23.enabled=false
    Lightning23off.enabled=false
    Lightning24.enabled=true:Lightning24.interval=100:Lightning24off.enabled=true:Lightning24off.interval=200
end sub


Sub Lightning24_Timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:LightningLight 12
    Case 1:LightningLight 9
    Case 2:LightningLight 12
    End Select
end sub 

Sub Lightning24off_Timer
    Lightning24.enabled=false
    Lightning24off.enabled=false
    Lightning25.enabled=true:Lightning25.interval=100:Lightning25off.enabled=true:Lightning25off.interval=200
end sub

Sub Lightning25_Timer
    Dim x
    x = INT(3 * RND(1) )
    Select Case x
    Case 0:LightningLight 14
    Case 1:LightningLight 15:LightningLight 14
    Case 2:LightningLight 14
    End Select
end sub 

Sub Lightning25off_Timer
    Lightning25.enabled=false
    Lightning25off.enabled=false
end sub



'**********************************************************************************************************
'* InstructionCard *
'**********************************************************************************************************

Dim CardCounter, ScoreCard
Sub CardTimer_Timer
	If scorecard=1 Then
		CardCounter=CardCounter+2
		If CardCounter>50 Then CardCounter=50
	Else
		CardCounter=CardCounter-4
		If CardCounter<0 Then CardCounter=0
	End If
	InstructionCard.transX = CardCounter*6
	InstructionCard.transY = CardCounter*6
	InstructionCard.transZ = -cardcounter*2
'    InstructionCard.objRotX = -cardcounter/2
	InstructionCard.size_x = 1+CardCounter/25
	InstructionCard.size_y = 1+CardCounter/25
	If CardCounter=0 Then 
		CardTimer.Enabled=False
		InstructionCard.visible=0
	Else
			InstructionCard.visible=1
	End If 
End Sub


' Change Log
' 1.0 Released
' 1.01 - apophis - Fixed issue with flippers getting stuck in up position after game ends. Fixed positions of L14 and L46.
' 1.02 - apophis - Moved Ramp003 to prevent rare double stuck ball at upper left VUK. Added logic to prevent stuck ball caught by ball save diverter.



'------------------------------------   You can't kill the boogie man! ----------------------------------------
'
'                                                                                                              
'                                                                                                              
'                                     ..                  ..;;.                                                
'                                    ...'.    ...  .;oxkkOKNWk,                                                
'                             ..    ..''''. ....',codolooxXMMMNOdl:.   .. '.                                   
'                           ...'...      .      ..';:;,;:ckWMMMMMMWXxod0xcx,                                   
'                          ...c:.      ..     .:ooxKWWWXXKXWMMMMMMMMMMMMWWO'                                   
'                         ..'od.       ;ooc... .;.'cokKMMMMMMMMMMMMMMMMMMMWXkc.                                
'                        ...:c.         .,'....d0k0X0KNMMMMMMMMMMMMMMMMMMMMMMWO,                               
'                  ...  ..      .  ...',',:lloOWMMMMWWNNXXKKKNWMMMMMMMMMMMMMMMM0:..;.     ..                   
'                   ...   .. ...oxcoxx0XNWWMWNKOxdlc:;,......';:okXWMMMMMMMMMMMMNKKK;    .cl'.                 
'                     .  'kc,do;lKMWNNWX0ko:,..                   .:xXMMMMMMMMMMMMMWk'  .oOo'                  
'                    .. .xWXKWNXXXX0xl;..                            .lOWMMMMMMMMMMMWKOOXNd.                   
'                    .. :NMMMMWKxc'.                                   .;kNMMMMMMMMMMMMMMk.                    
'                    ...kMMXxc,.                                          ;OWMMMMMMMMMMMMk.                    
'                    ...kMK;                                               .cKMMMMMMMMMMMWx.                   
'                    ...xWl                                                  'kWMMMMMMMMMMW0l,.                
'                      .xX;                                                   .oNMMMMMMMMMMMM0:.               
'                     ..o0,                               ....                  lNMWNWMMMMMWWNo.               
'                     ..l0,                     .,cc;'...',cllc;.                ;0Nd:l0NWMXdok:               
'                   ..'.;k;                    .ckOxl;,....    .                  .clc::lkNM0;':.              
'                   .. .;Oc     ...          .:ddc;;;,;::::,.                     .ll:;,;cOWMO.                
'                       'kd    .:do,.       .oWNOdx0XWMMMMMWXx;                    ;ONk,.'oXMWx.               
'                      ..;d'   .,cxOOxl'    .OMMK0WMMAPOPHISMMXo.                   .oXO..:KMWNOl,..           
'                      ...dl .:;;dKXNWMK,    ,ollOWMMMMMMMMMNkc.                    ;o0X: ;KMWOol:;.           
'                     ....co;;,.'dO0XWMX;       'x0KKXXK0Odc'                       'dko. ;XMW0c.              
'                      .''ckl.'dXWMMMMMX;          .':c,.                           .     oKOOOl'              
'                        .'l:,0MHIREZ00W0,                                           ;xl::xX0;.                 
'                        ..lddNMMMMMMXd,.                                            ;lo0MWXkc;:,.             
'                       .'..::cOXNKkl' ..      .;c:.                           ;:.      ;O0K0Od:.              
'                      ..    .. ...   .l:..:d;  .;lllc'                        oXx.     .'...                  
'                            ..        dNKXWWKkdc'  .;c;'...                  .kMX;      ..                    
'                             ..       oWMMMXxdxo'        .........           ,KMNc ..   ..                    
'                             .,.      cNMMXc.                               .dNMMKO0k'   ..                   
'                              ;l.     ,dOk;     .                         .oONMMMMMMX;    ..                  
'                              .okcc;    ..                               .dWMMMMMMMMX;    .'.. ..             
'                               .kWWl     . .'..oxdkxcccc:;,'.            lNMMMMMMMMMWOxkOOOOOkkkkxol:'..      
'                                .ONc    .''kNKKWMNKOOxc:,..   ,:cc.    .lNMMMMMMMMMMMMMMMMMMMMMMMMMMWXKOxoc:'.
'                                 .Ox.    ;0WXOkkkxxdc.        cXWW0:. .xNMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWKd
'                                  .xo. .:0Kxddk0XWMMWk;'.     :KMMOlldKMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMx
'                                   .ox'.o0ldWMMMWX0kdoc;.    .kWMMKkXMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWl
'                                    .oOl...oWWOl,..    ..    .OMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMX;
'                                      :00olONx.      .,::,..'xWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMO.
'                                       .ckNMWOc,;coxOKNWWNK0XMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMx.
'                                          ,dKWMWWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWl 
'                                            .cxKNMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMX; 
'                                               .cKMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMO. 
'                                                 ;0NMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMx. 
'                                                  ,dKWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMWl  
'                                             .;oxxO0xOWMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMX;  
'                                          .'lONMMMMMKllOXMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMO.  
'                                       ...'xNMMMMMMMMK:.lNMMMMMMWNNNNNNNNNNNNWMMMMMMMMMMMMMMMMNXWMMMMMMMMMd.  
'                                   .....  ;KMMMMMMMMMM0dcxWMMMMNo'''''lO000KKNWMMMMMMMMMMMMMKdd0WMMMMMMMMNc   
'                              .......     '0MMMMMMMMMMMM0:oNWWW0'    'OMMMMMMMMMMMMMMMMMMW0lcdXMMMMMMMMWKc.   
'                           .....          .kMMMMMMMMMMMMMOd0XNWd'cxkOKWMMMMMMMMMMMMMMMMNx::dXMMMMMMMMMXo.     
'                         ...               lWMMMMMMMMMMMMMMMMMXll00NMMMMMMMMMMMMMMMMW0o,;dXMMMMMMMMMNx'       
'                         .:cc;'.           'OWMMMMMMMMMMMMMMMWkdKxoKMMMMMMMMMMMMMMNk:.'dXMMMMMMMMMNk;         
'                          ..,cloodl;.    .cx0WMMMMMMMMMMMMMMMKd0MMMMMMMMMMMMMMMWXd,.'oXMMMMMMMMMW0:           
'                                .';loooc,..oXMMMMMMMMMMMMMMMNxxWMMMMMMMMMMMMMW0l'.,kXWMMMMMMMMMKl.            
'                                     .':looloKWMMMMMMMMMMMMWkxXMMMMMMMMMMMMWOl',coKMMMMMMMMMMXd.              
'                                        .,lOKNWMMMMNXNMMMMWOxXMMMMMMMMMMMNk;'cx0KNMMMMMMMMMNx,                
'                             ...';:codkOKXNWWMMMMMK::0MMMW0kXMMMMMMMMMMNx;,:kNNWMMMMMMMN0xc'                  
'                           ..'dXNNWWMMMMMMMMMMWOx0l ;XMMW0ONMMMMMMMMMNx:,:ONWMMMMMWXOd:'.                     
'                            .',oKWMMMMMMMMMMMKc..c. oWN0kONMMMMMMMMMWo,xKXMMMMWKko;.                          
'                             .,..,ckWMMMMMMMN:    .dXKkx0WMMMMMMMMMMNxOWMMNKxl,.                              
'                              .,,.:0WMMMMMMMk.  .cKNOkXMMMMMMMMMMWNNXK0Odc'.                                  
'                                ':dO000KKXXXl   :NNOONMMMWNXKOkxol:;'..                                       

