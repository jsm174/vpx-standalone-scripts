'
'             ___ . .  _                                                                                             
'    "T$$$P"   |  |_| |_                                                                                             
'     :$$$     |  | | |_                                                                                             
'     :$$$                                                      "T$$$$$$$b.                                          
'     :$$$     .g$VPW$p.   T$$$$b.    T$$$$$bp.                   VPW    "Tb      T$b      T$P   .g$P^^T$$  ,gP^^T$$ 
'      $$$    d^"     "^b   $$  "Tb    $$    "Tb    .s^s. :sssp   $$$     :$; T$$P $^b.     $   dP"     `T :$P    `T
'      :$$   dP         Tb  $$   :$;   $$      Tb  d'   `b $      $$$     :$;  $$  $ `Tp    $  d$           Tbp.   
'      :$$  :$;         :$; $$   :$;   $$      :$; T.   .P $$$    $$$    .dP   $$  $   ^b.  $ :$;            "T$$p.  
'      $$$  :$;         :$; $$...dP    $$      :$;  `^s^' .$.     $$$...dP"    $$  $    `Tp $ :$;     "T$$      "T$b 
'      $$$   Tb.       ,dP  $$"""Tb    $$      dP ""$""$" "$"$^^  $$$""T$b     $$  $      ^b$  T$       T$ ;      $$;
'      $$$    Tp._   _,gP   $$   `Tb.  $$    ,dP    $  $...$ $..  $$$   T$b    :$  $       `$   Tb.     :$ T.    ,dP 
'      $$$;    "^$VPW$^"   d$$     `T.d$$$$$P^"     $  $"""$ $"", $$$    T$b  d$$bd$b      d$b   "^TbsssP" 'T$bgd$P  
'      $$$b.____.dP                                 $ .$. .$.$ss,d$$$b.   T$b.                                       
'    .d$$$$$$$$$$P                                                         `T$b.
'    
'                  __ __  ___  __    __ __  __  _____  _____    _____ _____ __ _____ __  _____  __  __    
'                  \\ // ||=|| ||    || ||\\|| ((   )) ||_//    ||==  ||  ) ||  ||   || ((   )) ||\\||    
'                   \V/  || || ||__| || || \||  \\_//  || \\    ||___ ||_// ||  ||   ||  \\_//  || \||    
'
'
'***************************************************************************************************************************
'***************************************************************************************************************************
'
'****************************************************************
'    V-Pin Workshop LOTR Fellowship
'****************************************************************
'Apophis - Project Lead, Scripting, Determination. Hairy feet.
'Astronasty - All Original Artwork. Large Axe. 
'Tomate - 3D Magic and Wizardry. 
'Sixtoe - Random Stuff, VR Things, Screaming. Sword.
'iaakki - Some flashers, random stuff. Bow and Arrow.
'Skitso - Lighting tweaks.
'Dazz - 3D scans of all the figurines. 
'Wylte - Ball Shadows, Palantir Sauron eye movement.
'Fluffhead35 - Ramp and ball rolling sounds.
'HauntFreaks - Blackglass B2S.
'Bord - Playfield mesh.
'GTXJoe - Shot testing tool
'EBIsLit - EBIsLit - Baseline playfield scan, purchasing the playfield figurines.
'Flupper - General assistance with rendering issues.
'RothbauerW - Physics tutorial, general assistance.
'Rik & VPW Team - Testing.
'Everyone in VPW for their support and encouragement!
'The PinMAME and VPX Developers for the programmes we all use!

Option Explicit
Randomize

'********************
' Options
'********************
'///////////////////// ---- General Sound Options ---- //////////////////////
'// VolumeDial is the actual global volume multiplier for the mechanical sounds.
'// Values smaller than 1 will decrease mechanical sounds volume.
'// Recommended values should be no greater than 1.
Const Startaudio = 1				'0 = No Startup Audio, 1 = Startup Audio
Const VolumeDial = 0.8				'Global volume multiplier for the mechanical sounds. Set to value between 0 and 1
Const BallRollVolume = 0.5			'Ball rolling sound volume. Set to value between 0 and 1
Const RampRollVolume = 0.5			'Ramp rolling sound volume. Set to value between 0 and 1

'///////////////////// ---- Shadow Options ---- /////////////////////
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									'2 = flasher image shadow, but it moves like ninuzzu's

'///////////////////// ---- Ball Options ---- /////////////////////
Const ChooseBall = 0				'Ball Settings 
									' 0 = Normal Ball
									' 1 = Gold GlowBall
									' 2 = Green GlowBall
									' 3 = Magma GlowBall

'//////////////////// ---- LUT (Colour Look Up Table) ---- //////////////
Const LUTSelector = True  			'Enables the ability to change LUT option with magna saves in game when set to 1
Const LutToggleSound = True			'Enables or disables the LUT sound effects

'///////////////////// ---- Other Options ---- /////////////////////
Const PlayfieldToys = 1				'0 = no model toys, 1 = model toys!
Const PlungerTipColor = 0			'0 = Black, 1 = White

'///////////////////// ---- VR Mode ---- /////////////////////
Const VRRoom = 0 					'0 - Off (Desktop or Cabinet Mode), 1 - 360, 2 - Minimal Room, 2 - Ultra Minimal
Const VR360Image = 1				'0 - Forest, 1 - Castle



'**********************
' Standard definitions
'**********************

Const UseSolenoids = 2
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0
Const HandleMech = 0

Const BallSize = 50
Const BallMass = 1

' Standard Sounds
Const SSolenoidOn = ""
Const SSolenoidOff = ""
Const SCoin = ""

' Globals
Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height

Dim x, i, j, k 'used in loops
Dim mRingMagnet, plungerIM, TowerDir, TowerStep, DiverterDir, DiverterPos
Dim LOTRBall1, LOTRBall2, LOTRBall3, LOTRBall4, gBOT

'********************
' Load Stuff
'********************

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const TestMode = 0		'Set to 0 to disable.  1 to enable
'Const UseVPMModSol = True 'this does not work

Dim CabinetMode, DesktopMode: DesktopMode = Table1.ShowDT
Dim UseVPMDMD
If VRRoom <> 0 Then UseVPMDMD = True Else UseVPMDMD = DesktopMode
If Not DesktopMode and VRRoom=0 Then CabinetMode=1 Else CabinetMode=0

LoadVPM "01560000", "SEGA.VBS", 3.26

'********************
' LUT Stuff
'********************
'0 = Fleep Natural Dark 1
'1 = Fleep Natural Dark 2
'2 = Fleep Warm Dark
'3 = Fleep Warm Bright
'4 = Fleep Warm Vivid Soft
'5 = Fleep Warm Vivid Hard
'6 = Skitso Natural and Balanced
'7 = Skitso Natural High Contrast
'8 = 3rdaxis Referenced THX Standard
'9 = CalleV Punchy Brightness and Contrast
'10 = HauntFreaks Desaturated
'11 = Tomate Washed Out 
'12 = VPW Original 1 to 1
'13 = Bassgeige
'14 = Blacklight
'15 = B&W Comic Book
'16 = SWcolorGradeLUT

Dim LUTset

LoadLUT
'LUTset = 11			' Override saved LUT for debug
SetLUT

Sub SetLUT  'AXS
	Table1.ColorGradeImage = "LUT" & LUTset
end sub 

Sub LUTBox_Timer
	LUTBox.TimerEnabled = 0 
	LUTBox.Visible = 0
End Sub

Sub ShowLUT
	LUTBox.visible = 1
	Select Case LUTSet
		Case 0: LUTBox.text = "Fleep Natural Dark 1"
		Case 1: LUTBox.text = "Fleep Natural Dark 2"
		Case 2: LUTBox.text = "Fleep Warm Dark"
		Case 3: LUTBox.text = "Fleep Warm Bright"
		Case 4: LUTBox.text = "Fleep Warm Vivid Soft"
		Case 5: LUTBox.text = "Fleep Warm Vivid Hard"
		Case 6: LUTBox.text = "Skitso Natural and Balanced"
		Case 7: LUTBox.text = "Skitso Natural High Contrast"
		Case 8: LUTBox.text = "3rdaxis Referenced THX Standard"
		Case 9: LUTBox.text = "CalleV Punchy Brightness and Contrast"
		Case 10: LUTBox.text = "HauntFreaks Desaturated"
  		Case 11: LUTBox.text = "Tomate washed out"
        Case 12: LUTBox.text = "VPW original 1on1"  '<---Default
        Case 13: LUTBox.text = "bassgeige"
        Case 14: LUTBox.text = "blacklight"
        Case 15: LUTBox.text = "B&W Comic Book"
	End Select
	LUTBox.TimerEnabled = 1
End Sub

Sub SaveLUT
	Dim FileObj
	Dim ScoreFile

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if

	if LUTset = "" then LUTset = 12 'failsafe

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "LOTRLUT.txt",True)
	ScoreFile.WriteLine LUTset
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub

Sub LoadLUT
	Dim FileObj, ScoreFile, TextStr
	dim rLine

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		LUTset=12
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "LOTRLUT.txt") then
		LUTset=12
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "LOTRLUT.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
		End if
		rLine = TextStr.ReadLine
		If rLine = "" then
			LUTset=12
			Exit Sub
		End if
		LUTset = int (rLine) 
		Set ScoreFile = Nothing
	    Set FileObj = Nothing
End Sub

'************
' Table init.
'************

' choose the ROM
Const cGameName = "lotr"    'USA
'Const cGameName = "lotr_fr" 'France
'Const cGameName = "lotr_gr" 'Germany
'Const cGameName = "lotr_it" 'Italy
'Const cGameName = "lotr_sp" 'Spain

Sub Table1_Init
    vpmInit Me
    With Controller
        .GameName = cGameName
        .Games(cGameName).Settings.Value("sound") = 1 'ensure the sound is on
        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "The Lord of the Rings (Stern 2003)" & vbNewLine & "VPW Valinor Edition"
        .Games(cGameName).Settings.Value("rol") = 0
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = 0
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description

        On Error Goto 0
    End With

    ' Nudging
    vpmNudge.TiltSwitch = 56
    vpmNudge.Sensitivity = 6

	'Glowball
	ChangeBall(ChooseBall)

	'************  Trough	**************
	Set LOTRBall4 = sw11.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set LOTRBall3 = sw12.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set LOTRBall2 = sw13.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set LOTRBall1 = sw14.CreateSizedballWithMass(Ballsize/2,Ballmass)
	gBOT = Array(LOTRBall1,LOTRBall2,LOTRBall3,LOTRBall4)
	
	Controller.Switch(11) = 1
	Controller.Switch(12) = 1
	Controller.Switch(13) = 1
	Controller.Switch(14) = 1

	CheckBallLocations

    ' Impulse Plunger - used as the autoplunger
    Const IMPowerSetting = 58 ' Plunger Power
    Const IMTime = 0.7        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swPlunger, IMPowerSetting, IMTime
        .Random 0.3
        .switch 16
        .CreateEvents "plungerIM"
    End With

	Set mRingMagnet = New cvpmMagnet
 	With mRingMagnet
		.InitMagnet sw47a, 50  
		.GrabCenter = True 
        .CreateEvents "mRingMagnet"
	End With

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    ' Div Init
    OrbitPin.IsDropped = 1
    SolBalrog 0
	eyeEnable = True
	EyeLookAt drain
	clearPlastics.blenddisablelighting=2
	LUTBox.visible = 0

	If PlungerTipColor = 1 Then Plunger.Image = "CustomWhiteTip"

	If startaudio = 1 then PlaySound("start_tune_lotr")

	'Preload flasher textures
	vpmTimer.AddTimer 400, "Flash14 True'"
	vpmTimer.AddTimer 500, "Flash23 True'"
	vpmTimer.AddTimer 600, "Flash25 True'"
	vpmTimer.AddTimer 700, "Flash26 True'"
	vpmTimer.AddTimer 800, "Flash27 True'"
	vpmTimer.AddTimer 900, "Flash29 True'"
'	vpmTimer.AddTimer 1000, "Flash30 True'"

End Sub

Sub Table1_Paused:Controller.Pause = True:End Sub
Sub Table1_unPaused:Controller.Pause = False:End Sub
Sub Table1_exit():SaveLUT:Controller.Pause = False:Controller.Stop:End Sub


'******************
' RealTime Updates
'******************

Sub GameTimer_Timer
	SoundCmdListener
	RDampen
	RollingTimer
	EyeSeeYou
End Sub

Sub Frametimer_Timer
	'CheckLagtime
	CheckBallLocations
	RealTimeUpdates
	Lampz.Update
	ModLampz.Update
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 
	If Not GlowBall Then UpdateBallBrightness
	If GlowBall Then UpdateGlowball
End Sub

' Hack to return Narnia ball back in play
Sub Narnia_Timer
    Dim b
	For b = 0 to UBound(gBOT)
		if gBOT(b).z < -200 Then
			'msgbox "Ball " &b& " in Narnia X: " & gBOT(b).x &" Y: "&gBOT(b).y & " Z: "&gBOT(b).z
			'debug.print "Move narnia ball ("& gBOT(b).x &" Y: "&gBOT(b).y & " Z: "&gBOT(b).z&") to upper left vuk"
			gBOT(b).x = 253
			gBOT(b).y = 182
			gBOT(b).z = -58
		end if
	next
end sub


Sub RealTimeUpdates
	'Diverter animation and collision handling
	Diverter.objRotZ = 28/43*(DiverterFlipper.CurrentAngle+325)
	Diverter_off.objRotZ = 28/43*(DiverterFlipper.CurrentAngle+325)

	if Diverter.objRotZ=213 then
		Diverter.collidable=1
	Else
		Diverter.collidable=0
	end if

	rtower_off.rotx = rtower.rotx

	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
    LFLogo.RotZ = LeftFlipper.CurrentAngle
    RFlogo.RotZ = RightFlipper.CurrentAngle

	g01_sw10_off.rotx = g01_sw10.rotx
	g01_sw10_off.roty = g01_sw10.roty
	g01_sw23_off.rotx = g01_sw23.rotx
	g01_sw23_off.roty = g01_sw23.roty
	g01_sw29_off.rotx = g01_sw29.rotx
	g01_sw29_off.roty = g01_sw29.roty
	g01_sw53_off.rotx = g01_sw53.rotx
	g01_sw53_off.roty = g01_sw53.roty

	PinCab_Shooter.Y = -100 + (5* Plunger.Position) -20

	gate_left.objrotx = gateleft.currentangle
	gate_right.objrotx = gateright.currentangle
	gate_left_off.objrotx = gateleft.currentangle
	gate_right_off.objrotx = gateright.currentangle
End Sub

Dim lastgametime : lastgametime = 0
Sub CheckLagtime
	Dim lagtime
	lagtime = gametime - lastgametime
	If lagtime > 25 Then
		debug.print "LagTime=" & lagtime
	End If
	lastgametime = gametime
End Sub


'********
' GI
'********

Set GICallback = GetRef("GIUpdate")

dim gilvl:gilvl = 1
Sub GIUpdate(no, Enabled)
	'debug.print "GIUpdate no="&no&" Enabled="&Enabled
	If Enabled then 
		PlaySound "Relay_On"
		SetLamp 0,1
		For each x in GIBackwall:DisableLighting x, 20, 1:Next
		For each x in GI:x.state = 1:Next
		UpdateMaterial "Rubber White",0,0,0,0,0,0,1,RGB(175,175,150),0,0,False,True,0,0,0,0
		gilvl = 1
	Else
		PlaySound "Relay_Off"
		SetLamp 0,0
		For each x in GIBackwall:DisableLighting x, 0, 1:Next
		For each x in GI:x.state = 0:Next
		UpdateMaterial "Rubber White",0,0,0,0,0,0,1,RGB(10,10,6),0,0,False,True,0,0,0,0
		gilvl = 0
	End If
End Sub


'*********
' Solenoids
'*********

SolCallBack(1) = "SolRelease"
SolCallBack(2) = "AutoPlunger"
SolCallback(3) = "SolLVUK"
SolCallback(4) = "SolULVUK"
SolCallback(5) = "SolRVUK"
SolCallback(6) = "SolRingMag"
SolCallback(7) = "SolTower"
SolCallback(8) = "SolDiv"
' 9 left bumper
'10 right bumper
'11 bottom bumper
'12 not used
SolCallback(13) = "SolOrbit"
SolCallback(14) = "Flash14"
'15 left flipper
'16 right flipper
'17 left slingshot
'18 right slingshot
SolCallBack(19) = "SolURKicker"
'20 balrog motor relay
SolCallBack(21) = "SolLockRelease"
SolCallBack(22) = "SolBalrog"
SolCallBack(23) = "Flash23"
SolCallBack(24) = "SolKnocker"
SolCallBack(25) = "Flash25"
SolCallBack(26) = "Flash26"
SolCallBack(27) = "Flash27"
'28 not used
SolCallBack(29) = "Flash29"
SolCallBack(30) = "SetLamp 130,"
SolCallBack(31) = "SetLamp 131,"
SolCallBack(32) = "SetLamp 132,"

Sub AutoPlunger(Enabled)
    If Enabled Then
        PlungerIM.AutoFire
		SoundPlungerReleaseBall()
    End If
End Sub

Sub SolKnocker(Enabled)
    If enabled Then
		KnockerSolenoid 'Add knocker position object
	End If
End Sub

Sub SolOrbit(Enabled):
	OrbitPin.IsDropped = Not Enabled
End Sub



'******************************************************
' TROUGH 
'******************************************************

Sub sw14_Hit():Controller.Switch(14) = 1:UpdateTrough:End Sub
Sub sw14_UnHit():Controller.Switch(14) = 0:UpdateTrough:End Sub
Sub sw13_Hit():Controller.Switch(13) = 1:UpdateTrough:End Sub
Sub sw13_UnHit():Controller.Switch(13) = 0:UpdateTrough:End Sub
Sub sw12_Hit():Controller.Switch(12) = 1:UpdateTrough:End Sub
Sub sw12_UnHit():Controller.Switch(12) = 0:UpdateTrough:End Sub
Sub sw11_Hit():Controller.Switch(11) = 1:UpdateTrough:End Sub
Sub sw11_UnHit():Controller.Switch(11) = 0:UpdateTrough:End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If sw14.BallCntOver = 0 Then sw13.kick 56, 20
	If sw13.BallCntOver = 0 Then sw12.kick 56, 20
	If sw12.BallCntOver = 0 Then sw11.kick 56, 20
	Me.Enabled = 0
End Sub

'******************************************************
' DRAIN & RELEASE
'******************************************************

Sub Drain_Hit() 
	RandomSoundDrain drain
	EyeLookAt drain
	UpdateTrough
	vpmTimer.AddTimer 500, "Drain.kick 56, 30'"
End Sub


Sub SolRelease(enabled)
	If enabled Then 
		vpmTimer.PulseSw 15
		sw14.kick 56, 9		
		RandomSoundBallRelease sw14
	End If
End Sub



'******************************************************
' LOCK 
'******************************************************

Sub sw17_Hit():Controller.Switch(17) = 1:End Sub
Sub sw17_UnHit():Controller.Switch(17) = 0:End Sub
Sub sw18_Hit():Controller.Switch(18) = 1:End Sub
Sub sw18_UnHit():Controller.Switch(18) = 0:End Sub
Sub sw19_Hit():Controller.Switch(19) = 1 : sw19.Timerenabled = False : sw19.Timerenabled = True : End Sub
Sub sw19_UnHit():Controller.Switch(19) = 0 : sw19.Timerenabled = False : sw19.Timerenabled = True : End Sub

Sub sw19_Timer
	If Controller.Switch(19) = True Then 
		SetLamp 101,1 'sword glow
	Else
		SetLamp 101,0
	End If
	sw19.TimerEnabled = False
End Sub

Sub SolLockRelease(enabled)
	If Enabled Then
		Lockpin.Timerenabled = True
		lockpin.Isdropped = 1
		lockpost.z = -50
		lockpost_off.z = -50
	Else
		lockpin.Isdropped = 0
		lockpost.z = 0
		lockpost_off.z = 0
	End If
End Sub

'hack for fixing SolLockRelease disable delay errors
Lockpin.Timerinterval = 280
Sub LockPin_Timer
	Lockpin.Timerenabled = False
	lockpin.Isdropped = 0
	lockpost.z = 0
	lockpost_off.z = 0
End Sub


'********************
' Flippers
'********************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"


Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire  'leftflipper.rotatetoend
        
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If                
	Else
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
    End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		RF.Fire 'rightflipper.rotatetoend

		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		RightFlipper.RotateToStart
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

'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)

    If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress
    If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress
	If keycode = LeftTiltKey Then Nudge 90, 1:SoundNudgeLeft()
	If keycode = RightTiltKey Then Nudge 270, 1:SoundNudgeRight()
	If keycode = CenterTiltKey Then Nudge 0, 1:SoundNudgeCenter()
	if Keycode = StartGameKey then 
		soundStartButton
		StopSound("start_tune_lotr")
		'LUTSelector = False
	End If

	if keycode = LeftMagnaSave then
		if LUTSelector then
			StopSound("start_tune_lotr")
			LUTSet = LUTSet - 1
			if LutSet < 0 then LUTSet = 15
			If LutToggleSound then
				Playsound "click"
				If LutSet = 12 Then StopSound "edgeofknife":Playsound "edgeofknife"
			end if
			SetLUT
			ShowLUT
		end if
	End If

	if keycode = RightMagnaSave then
		if LUTSelector then
			StopSound("start_tune_lotr")
            LUTSet = LUTSet  + 1
			if LutSet > 15 then LUTSet = 0
			If LutToggleSound then
				Playsound "click"
				If LutSet = 12 Then StopSound "edgeofknife":Playsound "edgeofknife"
			end if
			SetLUT
			ShowLUT
		end if
	End If

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If
    If KeyCode = PlungerKey Then Plunger.Pullback:SoundPlungerPull()

    If KeyDownHandler(KeyCode) Then Exit Sub
End Sub


Sub table1_KeyUp(ByVal Keycode)

    If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
    If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress
	If KeyCode = PlungerKey Then
		Plunger.Fire
		SoundPlungerReleaseBall()                        'Plunger release sound when there is a ball in shooter lane
	End If
    If KeyUpHandler(KeyCode) Then Exit Sub
End Sub



'*********
' Switches
'*********

' Slings & div switches

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    RandomSoundSlingshotLeft Lemk
	EyeLookAt Lemk
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 59
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
    RandomSoundSlingshotRight Remk
	EyeLookAt Remk
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 62
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

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 49:RandomSoundBumperTop Bumper1:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 50:RandomSoundBumperMiddle Bumper2:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 51:RandomSoundBumperBottom Bumper3:End Sub

' Spinner
Sub sw52_Spin:vpmTimer.PulseSw 52:SoundSpinner sw52:End Sub

' Rollovers & Ramp Switches

Sub sw16_Hit:Controller.Switch(16) = 1:End Sub
Sub sw16_Unhit:Controller.Switch(16) = 0:End Sub

Sub sw20_Hit:Controller.Switch(20) = 1:End Sub
Sub sw20_UnHit:Controller.Switch(20) = 0:End Sub

Sub sw21_Hit:Controller.Switch(21) = 1:End Sub
Sub sw21_Unhit:Controller.Switch(21) = 0:End Sub

Sub sw22_Hit:Controller.Switch(22) = 1:End Sub
Sub sw22_Unhit:Controller.Switch(22) = 0:End Sub

Sub sw24_Hit:Controller.Switch(24) = 1:WireRampOff:End Sub
Sub sw24_Unhit:Controller.Switch(24) = 0:WireRampOn False: End Sub

Sub sw25_Hit:Controller.Switch(25) = 1
	if activeball.vely < 0 Then WireRampOn True
	if activeball.vely >= 0 Then WireRampOff
End Sub
Sub sw25_Unhit:Controller.Switch(25) = 0:End Sub

Sub sw33_Hit:Controller.Switch(33) = 1:End Sub
Sub sw33_Unhit:Controller.Switch(33) = 0:End Sub

Sub sw34_Hit:Controller.Switch(34) = 1:End Sub
Sub sw34_Unhit:Controller.Switch(34) = 0:End Sub

Sub sw35_Hit:Controller.Switch(35) = 1:End Sub
Sub sw35_Unhit:Controller.Switch(35) = 0:End Sub

Sub sw36_Hit:Controller.Switch(36) = 1:End Sub
Sub sw36_Unhit:Controller.Switch(36) = 0:End Sub

Sub sw37_Hit:Controller.Switch(37) = 1:End Sub
Sub sw37_Unhit:Controller.Switch(37) = 0:End Sub

Sub sw38_Hit:Controller.Switch(38) = 1:End Sub
Sub sw38_Unhit:Controller.Switch(38) = 0:End Sub

Sub sw39_Hit:Controller.Switch(39) = 1:End Sub
Sub sw39_Unhit:Controller.Switch(39) = 0:End Sub

Sub sw40_Hit:Controller.Switch(40) = 1:End Sub
Sub sw40_Unhit:Controller.Switch(40) = 0:End Sub

Sub sw42_Hit:Controller.Switch(42) = 1:End Sub
Sub sw42_Unhit:Controller.Switch(42) = 0:End Sub

Sub sw43_Hit:Controller.Switch(43) = 1:End Sub
Sub sw43_Unhit:Controller.Switch(43) = 0:End Sub

Sub sw44_Hit:Controller.Switch(44) = 1:End Sub
Sub sw44_Unhit:Controller.Switch(44) = 0:End Sub

Sub sw45_Hit:Controller.Switch(45) = 1:End Sub
Sub sw45_Unhit:Controller.Switch(45) = 0:End Sub


Sub sw48_Hit:Controller.Switch(48) = 1:WireRampOn False:End Sub
Sub sw48_Unhit:Controller.Switch(48) = 0:ActiveBall.VelX = 3:End Sub

Sub sw57_Hit:Controller.Switch(57) = 1:EyeLookAt sw57:End Sub
Sub sw57_UnHit:Controller.Switch(57) = 0:End Sub

Sub sw58_Hit:Controller.Switch(58) = 1:EyeLookAt sw58:End Sub
Sub sw58_UnHit:Controller.Switch(58) = 0:End Sub

Sub sw60_Hit:Controller.Switch(60) = 1:EyeLookAt sw60:End Sub
Sub sw60_Unhit:Controller.Switch(60) = 0:End Sub

Sub sw61_Hit:Controller.Switch(61) = 1:EyeLookAt sw61:End Sub
Sub sw61_Unhit:Controller.Switch(61) = 0:End Sub


' Ramp Switches
Sub swRamp1a_Hit
	if activeball.vely < 0 Then WireRampOn False 
	if activeball.vely >= 0 Then WireRampOff 
End Sub

Sub swRamp1b_Hit
	WireRampOn False
	SetLamp 100, 0
	If activeball.velz < -1 Then
		RandomSoundMetal
	End If
End Sub

Sub swRamp1c_Hit: WireRampOn False: End Sub
Sub swRamp1d_Hit:WireRampOff: End Sub
Sub swRamp1d_UnHit: End Sub
Sub swRamp2_Hit: WireRampOn False: End Sub
Sub swSwordRampExit_Hit: WireRampOff: End Sub
Sub swSwordRampExit_UnHit: End Sub

' POD ball drop
Sub podballdrop_hit
	RandomSoundBallBouncePlayfieldHard Activeball
	SetLamp 100, 0
End sub

' Targets
Sub sw10_Hit:vpmTimer.PulseSw 10:End Sub
Sub sw23_Hit:vpmTimer.PulseSw 23:End Sub
Sub sw29_Hit:vpmTimer.PulseSw 29:End Sub
Sub sw53_Hit:vpmTimer.PulseSw 53:EyeLookAt sw53:End Sub

' Upper Right Saucer
Sub sw46_Hit:SoundSaucerLock:Controller.Switch(46) = 1:End Sub
Sub sw46_UnHit:SoundSaucerKick 1, sw46:Controller.Switch(46) = 0:End Sub

Sub SolURKicker(Enable)
    If Enable then
		If Controller.Switch(46) <> 0 Then
			sw46.kick 270, 7.3+rnd
		End If
	End If
End Sub



'*************************
' VUKs
'*************************

Dim KickerBall9, KickerBall30, KickerBall41, KickerBall47

Sub KickBall(kball, kangle, kvel, kvelz, kzlift)
	dim rangle
	rangle = PI * (kangle - 90) / 180
    
	kball.z = kball.z + kzlift
	kball.velz = kvelz
	kball.velx = cos(rangle)*kvel
	kball.vely = sin(rangle)*kvel
End Sub


'Upper Left VUK
Sub sw41_Hit
    set KickerBall41 = activeball
    Controller.Switch(41) = 1
    SoundSaucerLock
	SetLamp 100, 1
End Sub

Sub SolULVUK(Enable)
    If Enable then
		If Controller.Switch(41) <> 0 Then
			KickBall KickerBall41, 270, 6.9+rnd, 0, 230
			SoundSaucerKick 1, sw41
			Controller.Switch(41) = 0
		End If
	End If
End Sub


'Lower Left VUK
Sub sw9_Hit
    set KickerBall9 = activeball
    Controller.Switch(9) = 1
    SoundSaucerLock
	EyeLookAt sw9
End Sub

Sub SolLVUK(Enable)
    If Enable then
		If Controller.Switch(9) <> 0 Then
			KickBall KickerBall9, 0, 0, 40, 50
			SoundSaucerKick 1, sw9
			Controller.Switch(9) = 0
			g01_leftpost.z=40
			g01_leftpost_off.z=40
			LVUKAnim.Enabled = True
		End If
	End If
End Sub

Sub LVUKAnim_Timer
	LVUKAnim.Enabled = False
	g01_leftpost.z=0
	g01_leftpost_off.z=0
End Sub


'Right VUK
Sub sw30_Hit
    set KickerBall30 = activeball
	Controller.Switch(30) = 1
    SoundSaucerLock
End Sub

Sub SolRVUK(Enable)
    If Enable then
		If Controller.Switch(30) <> 0 Then
			KickBall KickerBall30, 0, 0, 45, 0
			SoundSaucerKick 1, sw30
			Controller.Switch(30) = 0
			g01_rightpost.z=40
			g01_rightpost_off.z=40
			RVUKAnim.Enabled = True
		End If
	End If
End Sub

Sub RVUKAnim_Timer
	RVUKAnim.Enabled = False
	g01_rightpost.z=0
	g01_rightpost_off.z=0
End Sub


'--- Ring Magnet stuff ---
Sub sw47_Hit
	Controller.Switch(47) = 1
	'prepare to kick ball out of magnet to prevent stuck ball
    set KickerBall47 = activeball
	'reduce spin of ball in magnet
	KickerBall47.angmomx= KickerBall47.angmomx/3
	KickerBall47.angmomy= KickerBall47.angmomy/3
	KickerBall47.angmomz= KickerBall47.angmomz/3
	'set timer to check for stuck ball
	sw47.Timerenabled = True
    SoundSaucerLock
End Sub

Sub sw47_UnHit
	sw47.Timerenabled = False
	Controller.Switch(47) = 0
End Sub

Sub sw47_Timer
	'Kick ball out if it is stuck
	If Not RingMagOn Then KickBall KickerBall47, 30, 15, 0, 0
	sw47.Timerenabled = False
End Sub


Dim RingMagOn
Sub SolRingMag(Enable)
	mRingMagnet.MagnetOn = Enable
	RingMagOn = Enable
	'debug.print "SolRingMag RingMag On=" & mRingMagnet.MagnetOn 
	'Kick out ball if mag disabled
    If Not Enable then
		If Controller.Switch(47) <> 0 Then
			KickBall KickerBall47, 45, 10, 0, 0
		End If
	End If
End Sub

Sub MagDelay_Timer
	'increase magnet strength back to normal
	If RingMagOn Then mRingMagnet.MagnetOn = True
	'debug.print "MagDelay_Timer RingMag On=" & mRingMagnet.MagnetOn 
	MagDelay.Enabled = False
End Sub




'************************
' Tower animation
'************************

Sub SolTower(Enabled)
    If Enabled Then
        TowerDir = 1
    Else
        TowerDir = -1
    End If

    TowerAnim.Enabled = 0
    If TowerStep < 1 Then TowerStep = 1
    If TowerStep > 20 Then TowerStep = 20

    TowerAnim.Enabled = 1
End Sub

Sub TowerAnim_Timer()
    Select Case TowerStep
        Case 0:rtower.RotX=90:TowerAnim.Enabled = 0
        Case 1:rtower.RotX=92
        Case 2:rtower.RotX=94
        Case 3:rtower.RotX=96
        Case 4:rtower.RotX=98
        Case 5:rtower.RotX=100
        Case 6:rtower.RotX=102
        Case 7:rtower.RotX=104
        Case 8:rtower.RotX=106
		Case 9:rtower.RotX=108
		Case 10:rtower.RotX=110
		Case 11:rtower.RotX=108
		Case 12:rtower.RotX=106
		Case 13:rtower.RotX=104
		Case 14:rtower.RotX=102
		Case 15:rtower.RotX=100
		Case 16:rtower.RotX=98
		Case 17:rtower.RotX=96
		Case 18:rtower.RotX=94
		Case 19:rtower.RotX=92
		Case 20:rtower.RotX=90
        Case 21:TowerAnim.Enabled = 0
    End Select
'
    TowerStep = TowerStep + TowerDir
End Sub


'************************
' Diverter 
'************************

Sub SolDiv(Enabled)
	If Enabled Then
        DiverterFlipper.RotateToEnd
    Else
        DiverterFlipper.RotateToStart
    End If
End Sub


'************************
' Clear plastic ramo 
'************************

Sub clearPlasticsRamp_hit
	RandomSoundMetal
End Sub


'************************
' Balrog 
'************************
'
Dim BalrogPos, BalrogDir, WobbleValue
BalrogDir=0 : BalrogPos=0 : WobbleValue=0


Sub SolBalrog(Enabled)
    If Enabled Then
        If BalrogDir = 0 Then			
            Controller.Switch(32) = 0
            Controller.Switch(31) = 1
            BalrogClose.Enabled = 0
            BalrogOpen.Enabled = 1
            BalrogOpen_Timer
            BalrogDir = 1
        Else
            Controller.Switch(31) = 0
            Controller.Switch(32) = 1
            BalrogOpen.Enabled = 0
            BalrogClose.Enabled = 1
            BalrogClose_Timer
            BalrogDir = 0
        End If
    End If
End Sub

Sub BalrogOpen_Timer()
	BalrogFlipper.StartAngle= -1*BalrogPos - 90
	Balrog.ObjRotZ = BalrogFlipper.CurrentAngle + 90
	balrog_off.objrotz = balrog.objrotz

    BalrogPos = BalrogPos + 1

    If BalrogPos > 88 Then
        BalrogPos = 88
        BalrogOpen.Enabled = 0
    End If
End Sub

Sub BalrogClose_Timer()
	BalrogFlipper.StartAngle= -1*BalrogPos - 90
	Balrog.ObjRotZ = BalrogFlipper.CurrentAngle + 90
	balrog_off.objrotz = balrog.objrotz

    BalrogPos = BalrogPos - 1

    If BalrogPos < 0 Then
        BalrogPos = 0
        BalrogClose.Enabled = 0
    End If
End Sub

Sub BalrogFlipper_Collide(parm)
	'debug.print "BalrogFlipper_Collide parm=" & parm & " BalrogPos=" & BalrogPos
	If BalrogPos = 0 and parm > 3 Then
		'vpmTimer.PulseSw 28
		Controller.Switch(28) = 1
		RandomSoundMetal
		WobbleValue = parm/15
		BalrogWobble.Enabled = True
		BalrogFlash.Enabled = True
		BalrogFlashCount = 0
	End If
End Sub

BalrogWobble.interval = 34 ' Controls the speed of the wobble
Sub BalrogWobble_timer
	Balrog.ObjRotX = WobbleValue
	balrog_off.objrotx = balrog.objrotx

	if WobbleValue < 0 then
		WobbleValue = abs(WobbleValue) * 0.9 - 0.1
	Else
		WobbleValue = -abs(WobbleValue) * 0.9 + 0.1
	end if
	'debug.print WobbleValue
	if abs(WobbleValue) < 0.1 Then
		WobbleValue = 0
		Balrog.ObjRotX = WobbleValue
		balrog_off.objrotx = balrog.objrotx
		BalrogWobble.Enabled = False
	end If
	if abs(WobbleValue) < 0.5 Then Controller.Switch(28) = 0
End Sub

BalrogFlash.interval = 60
Dim BalrogFlashCount
Sub BalrogFlash_Timer
	BalrogFlashCount = BalrogFlashCount + 1
	If BalrogFlashCount < 25 Then
		BalrogFlash.interval = 40 + RndInt(0,30)
		If Lampz.state(132) = 1 Then Lampz.state(132) = 0 Else Lampz.state(132) = 1
	Else
		If BalrogPos = 88 Then
			If Lampz.state(132) = 1 Then BalrogFlash.Enabled = False
		Else
			If Lampz.state(132) = 0 Then BalrogFlash.Enabled = False
		End If
	End If
End Sub


'************************
' Palantir Eye   
'************************

dim eyeTargetLocked, eyeLockTime, currentTarget, eyeposx, eyeposy, eyeTargetAngle, eyeCurrentAngle, eyeSpeed, eyeDir, eyeEnable

eyeEnable = False
eyeCurrentAngle = 0
currentTarget = 4
eyeSpeed = 2
eyeTargetLocked = 0
eyeLockTime = 300 '3 seconds

eyeposx = g02_globeye.x
eyeposy = g02_globeye.y

sub EyeSeeYou
'	If eyeEnable Then
'		eyeTargetAngle = AnglePP(eyeposx, eyeposy, gBOT(currentTarget-1).x, gBOT(currentTarget-1).y) - 90
'	Else
'		eyeTargetAngle = 0
'	End If

	If Not eyeEnable Then
		eyeTargetAngle = 0
	End If

	If eyeTargetAngle > 360 then eyeTargetAngle = eyeTargetAngle - 360
	If eyeTargetAngle < 0 then eyeTargetAngle = eyeTargetAngle + 360
	
	If eyeCurrentAngle > 360 then eyeCurrentAngle = eyeCurrentAngle - 360
	If eyeCurrentAngle < 0 then eyeCurrentAngle = eyeCurrentAngle + 360

	dim delta
	delta = eyeTargetAngle - eyeCurrentAngle

	If delta < 0 then
		delta = delta + 360
	End If

	If delta >= 180 Then
		eyeDir = +1
	Elseif delta < 180 Then
		eyeDir = -1
	Elseif delta > 180 + 360 Then
		eyeDir = -1
	Else
		eyeDir = 0
	End If

	If abs(delta - 180) < 2 Then
'		eyeTargetLocked = eyeTargetLocked + 1
'		If eyeTargetLocked > eyeLockTime Then
'			currentTarget = currentTarget + 1
'			If currentTarget > 4 Then currentTarget = 1
'			debug.print "Ball: "&currentTarget
'			eyeTargetLocked = 0
'		End If
	Else
'		eyeTargetLocked = 0
		eyeCurrentAngle = eyeCurrentAngle + (eyeDir*eyeSpeed*min(abs(delta)/10,1)) 
		eyeAngle eyeCurrentAngle
	End If
	
End Sub

Sub EyeAngle(aAng)
	g02_globeye.roty = aAng
	g02_globeye_off.roty = aAng
End Sub

Sub EyeLookAt(loc)
	eyeTargetAngle = AnglePP(eyeposx, eyeposy, loc.x, loc.y) - 90
End Sub


Sub L22_Timer
	If Lampz.state(22) = 1 Then 
		eyeEnable = True
	Else
		eyeEnable = False
	End If
End Sub


' *************************************
' *****   Flupper Domes           *****
' *************************************

 Sub Flash25(Enabled)
	Objlevel(1) = 1 : FlasherFlash1_Timer
	Objlevel(2) = 1 : FlasherFlash2_Timer
	Objlevel(3) = 1 : FlasherFlash3_Timer
 End Sub

 Sub Flash14(Enabled)
	Objlevel(4) = 1 : FlasherFlash4_Timer
 End Sub

 Sub Flash23(Enabled)
	Objlevel(5) = 1 : FlasherFlash5_Timer
 End Sub

 Sub Flash29(Enabled)
	Objlevel(6) = 1 : FlasherFlash6_Timer
 End Sub

' Sub Flash30(Enabled)
'	Objlevel(7) = 1 : FlasherFlash7_Timer
' End Sub

 Sub Flash26(Enabled)
	Objlevel(8) = 1 : FlasherFlash8_Timer
 End Sub

 Sub Flash27(Enabled)
	Objlevel(7) = 1 : FlasherFlash7_Timer
 End Sub


Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 1		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 0.2		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherBloomIntensity = 0.3		' *** lower this, if the blooms / hazes are too bright (i.e. 0.1)	***
FlasherOffBrightness = 0.4		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objlight(20), objbloom(20)
'Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
InitFlasher 1, "red" : InitFlasher 2, "red" : InitFlasher 3, "red"
InitFlasher 4, "yellow" : InitFlasher 5, "yellow" : InitFlasher 6, "red"  
InitFlasher 7, "red" : InitFlasher 8, "white"
' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 4,17 : RotateFlasher 5,0 : RotateFlasher 6,90
'RotateFlasher 7,0 : RotateFlasher 8,0 
'RotateFlasher 9,-45 : RotateFlasher 10,90 : RotateFlasher 11,90

If CabinetMode Then
	FlasherBloom.height=345
	FlasherBloom.rotx=-188
Else
	FlasherBloom.height=240
	FlasherBloom.rotx=-186.1
End If

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
'	Set objbloom(nr) = Eval("Flasherbloom" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 50
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
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objlight(nr).intensity = 2000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59)
		Case "orange" :  objlight(nr).color = RGB(255,70,0) : objflasher(nr).color = RGB(255,70,0)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
	Flasherflash6.height = 151
End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

dim accumulatedLevel : accumulatedLevel = 0
dim MaxLevel : MaxLevel = 0

Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then 
		objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objlit(nr).visible = 1 ': objbloom(nr).visible = 0
		select case nr
			Case 1:							'1,2,3 are same event, so doing this only for 1
				F_PF_BFlash.visible = 1
				if cabinetmode = 0 then
					F_LS_BFlash.visible = 1
					F_RS_BFlash.visible = 1
				else
					F_LS_BFlashCab.visible = 1
					F_RS_BFlashCab.visible = 1
				end If
				F_BS_BFlash.visible = 1
				FlasherBloom.ImageA="bloom_center_third"
				FlasherBloom.ImageB="bloom_center_third"
				FlasherBloom.rotx = -186.1
				FlasherBloom.roty = 180
				FlasherBloom.color = RGB(202,12,2)
				FlasherBloom.amount = 750
				FlasherBloom.visible = 1
			Case 4:
				F_PF_RFlash.visible = 1
				if cabinetmode = 0 then
					F_LS_RFlash.visible = 1
					F_RS_RFlash.visible = 1
				else
					F_LS_RFlashCab.visible = 1
					F_RS_RFlashCab.visible = 1
				end if
				F_BS_RFlash.visible = 1
				FlasherBloom.ImageA="bloom_center_third"
				FlasherBloom.ImageB="bloom_center_third"
				FlasherBloom.rotx = -6.1
				FlasherBloom.roty = 180
				FlasherBloom.color = RGB(200,175,4)
				FlasherBloom.amount = 750
				FlasherBloom.visible = 1
			Case 5:
				F_PF_LFlash.visible = 1
				if cabinetmode = 0 then
					F_LS_LFlash.visible = 1
					F_RS_LFlash.visible = 1
				else
					F_LS_LFlashCab.visible = 1
					F_RS_LFlashCab.visible = 1
				end if
				F_BS_LFlash.visible = 1
				FlasherBloom.ImageA="bloom_center_third"
				FlasherBloom.ImageB="bloom_center_third"
				FlasherBloom.rotx = -6.1
				FlasherBloom.roty = 0
				FlasherBloom.color = RGB(200,175,4)
				FlasherBloom.amount = 750
				FlasherBloom.visible = 1
			Case 6:
				FlasherBloom.ImageA="bloom_center_third"
				FlasherBloom.ImageB="bloom_center_third"
				FlasherBloom.rotx = -186.1
				FlasherBloom.roty = 180
				FlasherBloom.color = RGB(202,12,2)
				FlasherBloom.amount = 500
				FlasherBloom.visible = 1
			Case 9:
				FlasherBloom.ImageA="bloom_quarter"
				FlasherBloom.ImageB="bloom_quarter"
				FlasherBloom.rotx = -6.1
				FlasherBloom.roty = 0
				FlasherBloom.color = RGB(202,12,2)
				FlasherBloom.amount = 750
				FlasherBloom.visible = 1
		end Select

	End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
'	objbloom(nr).opacity = 100 *  FlasherBloomIntensity * ObjLevel(nr)^2.5

	select case nr
		Case 1,4,5,6,7:
			MaxLevel = Max(ObjLevel(1),Max(ObjLevel(4),Max(ObjLevel(5),Max(ObjLevel(6),ObjLevel(9)))))
			'debug.print "Max: " & MaxLevel
			FlasherBloom.opacity = 100 *  FlasherBloomIntensity * MaxLevel^2.5
	end Select

	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0

	select case nr
		Case 1:		'Bumper Flash
			F_PF_BFlash.opacity = 25 * ObjLevel(nr)^1
			if cabinetmode = 0 then
				F_LS_BFlash.opacity = 25 * ObjLevel(nr)^3
				F_RS_BFlash.opacity = 25 * ObjLevel(nr)^2
			Else
				F_LS_BFlashCab.opacity = 25 * ObjLevel(nr)^3
				F_RS_BFlashCab.opacity = 25 * ObjLevel(nr)^2
			end if
			F_BS_BFlash.opacity = 15 * ObjLevel(nr)^1.5
		Case 4:		'Lower Right Flash
			F_PF_RFlash.opacity = 25 * ObjLevel(nr)^1
			if cabinetmode = 0 then
				F_LS_RFlash.opacity = 20 * ObjLevel(nr)^2.5
				F_RS_RFlash.opacity = 20 * ObjLevel(nr)^1.5
			else
				F_LS_RFlashCab.opacity = 20 * ObjLevel(nr)^2.5
				F_RS_RFlashCab.opacity = 20 * ObjLevel(nr)^1.5
			end if
			F_BS_RFlash.opacity = 15 * ObjLevel(nr)^3
			light002.IntensityScale = Flasherlight4.IntensityScale
		Case 5:		'Lower Left Flash
			F_PF_LFlash.opacity = 25 * ObjLevel(nr)^1
			if cabinetmode = 0 then
				F_LS_LFlash.opacity = 20 * ObjLevel(nr)^1.5
				F_RS_LFlash.opacity = 20 * ObjLevel(nr)^2.5
			else
				F_LS_LFlashCab.opacity = 20 * ObjLevel(nr)^1.5
				F_RS_LFlashCab.opacity = 20 * ObjLevel(nr)^2.5
			end if
			F_BS_LFlash.opacity = 15 * ObjLevel(nr)^3
			light001.IntensityScale = Flasherlight5.IntensityScale
	end Select
 
	ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then 
		ObjLevel(nr) = 0
		objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objlit(nr).visible = 0 ': objbloom(nr).visible = 0
		select case nr
			Case 1:
				F_PF_BFlash.visible = 0
				if cabinetmode = 0 then
					F_LS_BFlash.visible = 0
					F_RS_BFlash.visible = 0
				Else
					F_LS_BFlashCab.visible = 0
					F_RS_BFlashCab.visible = 0
				end if
				F_BS_BFlash.visible = 0
			Case 4:
				F_PF_RFlash.visible = 0
				if cabinetmode = 0 then
					F_LS_RFlash.visible = 0
					F_RS_RFlash.visible = 0
				Else
					F_LS_RFlashCab.visible = 0
					F_RS_RFlashCab.visible = 0
				end if
				F_BS_RFlash.visible = 0
			Case 5:
				F_PF_LFlash.visible = 0
				if cabinetmode = 0 then
					F_LS_LFlash.visible = 0
					F_RS_LFlash.visible = 0
				Else
					F_LS_LFlashCab.visible = 0
					F_RS_LFlashCab.visible = 0
				end if
				F_BS_LFlash.visible = 0
		end Select
		'if all the flashers have faded to 0, make FlasherBloom invisible
		accumulatedLevel = 0
		for x = 1 to 8:accumulatedLevel = accumulatedLevel + ObjLevel(x):next
		'debug.print "acc level: " & accumulatedLevel
		if accumulatedLevel = 0 then FlasherBloom.visible = 0':debug.print "bloom invisible"

	End If
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub 
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub 
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub 
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub 
Sub FlasherFlash5_Timer() : FlashFlasher(5) : End Sub 
Sub FlasherFlash6_Timer() : FlashFlasher(6) : End Sub 
Sub FlasherFlash7_Timer() : FlashFlasher(7) : End Sub
Sub FlasherFlash8_Timer() : FlashFlasher(8) : End Sub

' *************************************
' ******  End Flupper Domes       *****
' *************************************


'***************************************
'* Shot light blinking hack            *
'***************************************
' Lights 19, 25, 33, 37, 41, 45, 49 need to blink after on for some short period of time
Const BlinkHackInterval = 60
Const BlinkHackDelay = 1000

Sub L19_timer
	L19.timerinterval = BlinkHackInterval
	If Lampz.state(19) = 1 Then Lampz.state(19) = 0 Else Lampz.state(19) = 1 
End Sub

Sub L25_timer
	L25.timerinterval = BlinkHackInterval
	If Lampz.state(25) = 1 Then Lampz.state(25) = 0 Else Lampz.state(25) = 1 
End Sub

Sub L33_timer
	L33.timerinterval = BlinkHackInterval
	If Lampz.state(33) = 1 Then Lampz.state(33) = 0 Else Lampz.state(33) = 1 
End Sub

Sub L37_timer
	L37.timerinterval = BlinkHackInterval
	If Lampz.state(37) = 1 Then Lampz.state(37) = 0 Else Lampz.state(37) = 1 
End Sub

Sub L41_timer
	L41.timerinterval = BlinkHackInterval
	If Lampz.state(41) = 1 Then Lampz.state(41) = 0 Else Lampz.state(41) = 1 
End Sub

Sub L45_timer
	L45.timerinterval = BlinkHackInterval
	If Lampz.state(45) = 1 Then Lampz.state(45) = 0 Else Lampz.state(45) = 1 
End Sub

Sub L49_timer
	L49.timerinterval = BlinkHackInterval
	If Not Snd_Wraithes Then
		If Lampz.state(49) = 1 Then Lampz.state(49) = 0 Else Lampz.state(49) = 1 
	End If
End Sub




'***************************************
'* Light callback subs                 *
'***************************************

Dim DLintensity

Sub DisableLighting(pri, DLintensity, ByVal aLvl)
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	
	pri.blenddisablelighting = aLvl * DLintensity
End Sub

Sub DisableLightingGrnBulbs(pri, DLintensity, ByVal aLvl)	
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	
	pri.blenddisablelighting = aLvl * DLintensity
	UpdateMaterial pri.material,1,0,0,0,0,0,aLvl*0.25,RGB(24,240,0),0,0,False,True,0,0,0,0
End Sub

Sub DisableLightingSword(pri, DLintensity, ByVal aLvl)	
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	
	pri.blenddisablelighting = aLvl * DLintensity
	'UpdateMaterial clearPlasticsSword.material,1,0,0,0,0,0,aLvl,RGB(74,197,255),0,0,False,True,0,0,0,0
	if aLvl > 0 then clearPlasticsSword.visible=1 else clearPlasticsSword.visible=0
End Sub

Sub DisableLightingVial(pri, DLintensity, ByVal aLvl)	
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	
	pri.blenddisablelighting = aLvl * DLintensity
	UpdateMaterial pri.material,1,0,0,0,0,0,aLvl,RGB(255,255,255),0,0,False,True,0,0,0,0
End Sub

Dim BalrogLit : BalrogLit=False
Sub BalrogImageSwap(ByVal aLvl)	
	If Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)
    If aLvl > 0.01 and Not BalrogLit Then
		balrog.Image = "GION_balrog_lit"
		balrog_off.Image = "GION_balrog_lit"
		BalrogLit = True

	ElseIf aLvl <= 0.01 and BalrogLit Then
		balrog.Image = "GION_balrog_unlit"
		balrog_off.Image = "GIOFF_balrog_unlit"
		BalrogLit = False
    End If
	balrog.blenddisablelighting = aLvl * 4 + 1
	balrog_off.blenddisablelighting = aLvl * 4 + 1
	'If aLvl > 0.01 then debug.print "balrog aLvl=" & aLvl & " Lampz.state(132)=" & Lampz.state(132)
End Sub


'***************************************
'*** Begin nFozzy lamp handling      ***
'***************************************

Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
Dim ModLampz : Set ModLampz = New DynamicLamps
InitLampsNF              ' Setup lamp assignments
LampTimer.Interval = 12
LampTimer.Enabled = 1

Sub LampTimer_timer()
	dim x, chglamp
	chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
			'blinking lamp hack
			Select Case chglamp(x, 0)
				Case 19: L19.timerenabled = False : If chglamp(x, 1)=1 Then : L19.timerinterval = BlinkHackDelay : L19.timerenabled = True : End If
				Case 25: L25.timerenabled = False : If chglamp(x, 1)=1 Then : L25.timerinterval = BlinkHackDelay : L25.timerenabled = True : End If
				Case 33: L33.timerenabled = False : If chglamp(x, 1)=1 Then : L33.timerinterval = BlinkHackDelay : L33.timerenabled = True : End If
				Case 37: L37.timerenabled = False : If chglamp(x, 1)=1 Then : L37.timerinterval = BlinkHackDelay : L37.timerenabled = True : End If
				Case 41: L41.timerenabled = False : If chglamp(x, 1)=1 Then : L41.timerinterval = BlinkHackDelay : L41.timerenabled = True : End If
				Case 45: L45.timerenabled = False : If chglamp(x, 1)=1 Then : L45.timerinterval = BlinkHackDelay : L45.timerenabled = True : End If
				Case 49: L49.timerenabled = False : If chglamp(x, 1)=1 Then : L49.timerinterval = BlinkHackDelay : L49.timerenabled = True : End If
			End Select
		next
	End If
	Lampz.Update1	'update (fading logic only)
	ModLampz.Update1
'	lampz.Update2
'	ModLampz.Update2
End Sub

Function FlashLevelToIndex(Input, MaxSize)
	FlashLevelToIndex = cInt(MaxSize * Input)
End Function

Sub InitLampsNF()
	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating
	ModLampz.Filter = "LampFilter"

	'Adjust fading speeds (1 / full MS fading time)
    dim x : for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/3 : Lampz.FadeSpeedDown(x) = 1/17 : next
    for x = 0 to 28 : ModLampz.FadeSpeedUp(x) = 1/3 : ModLampz.FadeSpeedDown(x) = 1/20 : Next
    Lampz.FadeSpeedUp(100) = 1/50 : Lampz.FadeSpeedDown(100) = 1/50 'POTD
    Lampz.FadeSpeedUp(101) = 1/125 : Lampz.FadeSpeedDown(101) = 1/125 'Sword
    Lampz.FadeSpeedUp(102) = 1/750 : Lampz.FadeSpeedDown(102) = 1/750 'Vial
    Lampz.FadeSpeedUp(132) = 1/4 : Lampz.FadeSpeedDown(132) = 1/8 'Balrog

	'Lamp Assignments
	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays

	Lampz.MassAssign(1)= l1
	Lampz.MassAssign(1)= l1f
	Lampz.MassAssign(1)= l1h
	Lampz.Callback(1) = "DisableLighting p1, 70,"
	Lampz.MassAssign(2)= l2
	Lampz.MassAssign(2)= l2h
	Lampz.Callback(2) = "DisableLighting p2, 60,"
	Lampz.MassAssign(3)= l3
	Lampz.MassAssign(3)= l3h
	Lampz.Callback(3) = "DisableLighting p3, 70,"
	Lampz.MassAssign(4)= l4
	Lampz.MassAssign(4)= l4h
	Lampz.Callback(4) = "DisableLighting p4, 80,"
	Lampz.MassAssign(5)= l5
	Lampz.MassAssign(5)= l5h
	Lampz.Callback(5) = "DisableLighting p5, 80,"
	Lampz.MassAssign(6)= l6
	Lampz.MassAssign(6)= l6h
	Lampz.Callback(6) = "DisableLighting p6, 100,"
	Lampz.MassAssign(7)= l7
	Lampz.MassAssign(7)= l7h
	Lampz.Callback(7) = "DisableLighting p7, 90,"
	Lampz.MassAssign(8)= l8
	Lampz.MassAssign(8)= l8h
	Lampz.Callback(8) = "DisableLighting p8, 250,"
	Lampz.MassAssign(9)= l9
	Lampz.MassAssign(9)= l9h
	Lampz.Callback(9) = "DisableLighting p9, 250,"
	Lampz.MassAssign(10)= l10
	Lampz.Callback(10) = "DisableLighting p10, 200,"
	Lampz.MassAssign(11)= l11
	Lampz.MassAssign(11)= l11h
	Lampz.Callback(11) = "DisableLighting p11, 150,"
	Lampz.MassAssign(12)= l12
	Lampz.MassAssign(12)= l12h
	Lampz.Callback(12) = "DisableLighting p12, 70,"
	Lampz.MassAssign(13)= l13
	Lampz.MassAssign(13)= l13h
	Lampz.Callback(13) = "DisableLighting p13, 60,"
	Lampz.MassAssign(14)= l14
	Lampz.MassAssign(14)= l14h
	Lampz.Callback(14) = "DisableLighting p14, 70,"
	Lampz.MassAssign(15)= l15
	Lampz.MassAssign(15)= l15h
	Lampz.Callback(15) = "DisableLighting p15, 90,"
	Lampz.MassAssign(16)= l16
	Lampz.MassAssign(16)= l16h
	Lampz.Callback(16) = "DisableLighting p16, 150,"
	Lampz.MassAssign(17)= l17
	Lampz.MassAssign(17)= l17h
	Lampz.Callback(17) = "DisableLighting p17, 150,"
	Lampz.MassAssign(18)= l18
	Lampz.MassAssign(18)= l18h
	Lampz.Callback(18) = "DisableLighting p18, 70,"
	Lampz.MassAssign(19)= l19
	Lampz.MassAssign(19)= l19h
	Lampz.Callback(19) = "DisableLighting p19, 80,"
	Lampz.MassAssign(20)= l20
	Lampz.MassAssign(20)= l20h
	Lampz.Callback(20) = "DisableLighting p20, 200,"
	'Lampz.Callback(20) = "DisableLighting theoneringtext, 350,"
	Lampz.MassAssign(21)= l21
	Lampz.MassAssign(21)= l21h
	Lampz.Callback(21) = "DisableLighting p21, 250,"
	Lampz.MassAssign(22)= l22
	Lampz.MassAssign(22)= l22h
	Lampz.Callback(22) = "DisableLighting p22, 150,"
	Lampz.MassAssign(23)= l23	'globe
	Lampz.MassAssign(23)= l23h	'globe
	Lampz.Callback(23) = "DisableLighting g02_globeye, 3,"
	Lampz.Callback(23) = "DisableLighting g02_globeye_off, 50,"

	Lampz.MassAssign(24)= l24
	Lampz.MassAssign(24)= l24h
	Lampz.Callback(24) = "DisableLighting p24, 70,"
	Lampz.MassAssign(25)= l25
	Lampz.MassAssign(25)= l25h
	Lampz.Callback(25) = "DisableLighting p25, 30,"
	Lampz.MassAssign(26)= l26
	Lampz.MassAssign(26)= l26h
	Lampz.Callback(26) = "DisableLighting p26, 30,"
	Lampz.MassAssign(27)= l27
	Lampz.MassAssign(27)= l27h
	Lampz.Callback(27) = "DisableLighting p27, 250,"
	Lampz.MassAssign(28)= l28
	Lampz.MassAssign(28)= l28h
	Lampz.Callback(28) = "DisableLighting p28, 250,"
	Lampz.MassAssign(29)= l29
	Lampz.MassAssign(29)= l29h
	Lampz.Callback(29) = "DisableLighting p29, 250,"
	Lampz.MassAssign(30)= l30
	Lampz.MassAssign(30)= l30h
	Lampz.Callback(30) = "DisableLighting p30, 250,"
	Lampz.MassAssign(31)= l31
	Lampz.MassAssign(31)= l31h
	Lampz.Callback(31) = "DisableLighting p31, 250,"
	Lampz.MassAssign(32)= l32
	Lampz.MassAssign(32)= l32h
	Lampz.Callback(32) = "DisableLighting p32, 250,"
	Lampz.MassAssign(33)= l33
	Lampz.MassAssign(33)= l33h
	Lampz.Callback(33) = "DisableLighting p33, 20,"
	Lampz.MassAssign(34)= l34
	Lampz.MassAssign(34)= l34h
	Lampz.Callback(34) = "DisableLighting p34, 20,"
	Lampz.MassAssign(35)= l35
	Lampz.MassAssign(35)= l35h
	Lampz.Callback(35) = "DisableLighting p35, 30,"
	Lampz.MassAssign(36)= l36
	Lampz.MassAssign(36)= l36h
	Lampz.Callback(36) = "DisableLighting p36, 50,"


	Lampz.MassAssign(37)= l37
	Lampz.MassAssign(37)= l37h
	Lampz.Callback(37) = "DisableLighting p37, 30,"



	Lampz.MassAssign(38)= l38
	Lampz.MassAssign(38)= l38h
	Lampz.Callback(38) = "DisableLighting p38, 40,"
	Lampz.MassAssign(39)= l39
	Lampz.MassAssign(39)= l39h
	Lampz.Callback(39) = "DisableLighting p39, 50,"
	Lampz.MassAssign(40)= l40
	Lampz.MassAssign(40)= l40h
	Lampz.Callback(40) = "DisableLighting p40, 80,"
	Lampz.MassAssign(41)= l41
	Lampz.MassAssign(41)= l41h
	Lampz.Callback(41) = "DisableLighting p41, 70,"
	Lampz.MassAssign(42)= l42
	Lampz.MassAssign(42)= l42h
	Lampz.Callback(42) = "DisableLighting p42, 90,"
	Lampz.MassAssign(43)= l43
	Lampz.MassAssign(43)= l43h
	Lampz.Callback(43) = "DisableLighting p43, 110,"
	Lampz.MassAssign(44)= l44
	Lampz.MassAssign(44)= l44h
	Lampz.Callback(44) = "DisableLighting p44, 80,"
	Lampz.MassAssign(45)= l45
	Lampz.MassAssign(45)= l45h
	Lampz.Callback(45) = "DisableLighting p45, 120,"
	Lampz.MassAssign(46)= l46
	Lampz.MassAssign(46)= l46h
	Lampz.Callback(46) = "DisableLighting p46, 60,"
	Lampz.MassAssign(47)= l47
	Lampz.MassAssign(47)= l47h
	Lampz.Callback(47) = "DisableLighting p47, 90,"
	Lampz.MassAssign(48)= l48
	Lampz.MassAssign(48)= l48h
	Lampz.Callback(48) = "DisableLighting p48, 150,"
	Lampz.MassAssign(49)= l49
	Lampz.MassAssign(49)= l49h
	Lampz.Callback(49) = "DisableLighting p49, 60,"
	Lampz.MassAssign(50)= l50
	Lampz.MassAssign(50)= l50h
	Lampz.Callback(50) = "DisableLighting p50, 80,"
	Lampz.MassAssign(51)= l51
	Lampz.MassAssign(51)= l51h
	Lampz.Callback(51) = "DisableLighting p51, 100,"
	Lampz.MassAssign(52)= l52
	Lampz.MassAssign(52)= l52h
	Lampz.Callback(52) = "DisableLighting p52, 250,"
	Lampz.MassAssign(53)= l53
	Lampz.MassAssign(53)= l53h
	Lampz.Callback(53) = "DisableLighting p53, 40,"
	Lampz.MassAssign(54)= l54
	Lampz.MassAssign(54)= l54h
	Lampz.Callback(54) = "DisableLighting p54, 200,"
	Lampz.MassAssign(55)= l55
	Lampz.MassAssign(55)= l55h
	Lampz.Callback(55) = "DisableLighting p55, 200,"
	Lampz.MassAssign(56)= l56
	Lampz.MassAssign(56)= l56h
	Lampz.Callback(56) = "DisableLighting p56, 200,"
	Lampz.MassAssign(57)= l57
	Lampz.MassAssign(57)= l57h
	Lampz.Callback(57) = "DisableLighting p57, 150,"
	Lampz.MassAssign(58)= l58
	Lampz.MassAssign(58)= l58h
	Lampz.Callback(58) = "DisableLighting p58, 150,"
	Lampz.MassAssign(59)= l59
	Lampz.MassAssign(59)= l59h
	Lampz.Callback(59) = "DisableLighting p59, 150,"

	Lampz.MassAssign(60)= l60
	Lampz.Callback(60) = "DisableLightingGrnBulbs g01_p60, 3,"
	Lampz.MassAssign(61)= l61
	Lampz.Callback(61) = "DisableLightingGrnBulbs g01_p61, 3,"
	Lampz.MassAssign(62)= l62
	Lampz.MassAssign(62)= l62b
	Lampz.Callback(62) = "DisableLightingGrnBulbs g01_p62, 3,"
	Lampz.MassAssign(63)= l63
	Lampz.MassAssign(63)= l63b
	Lampz.Callback(63) = "DisableLightingGrnBulbs g01_p63, 3,"

	Lampz.MassAssign(64)= l64
	'Lampz.MassAssign(64)= l64a
	Lampz.Callback(64) = "DisableLighting g01_p64, 2,"
	Lampz.Callback(64) = "DisableLighting g01_p64_off, 2,"
	Lampz.MassAssign(65)= l65
	Lampz.MassAssign(65)= l65a
	Lampz.Callback(65) = "DisableLighting g01_p65, 2,"
	Lampz.Callback(65) = "DisableLighting g01_p65_off, 2,"
	Lampz.MassAssign(66)= l66
	Lampz.MassAssign(66)= l66a
	Lampz.Callback(66) = "DisableLighting g01_p66, 2,"
	Lampz.Callback(66) = "DisableLighting g01_p66_off, 2,"
	Lampz.MassAssign(67)= l67
	Lampz.MassAssign(67)= l67a
	Lampz.Callback(67) = "DisableLighting g01_p67, 2,"
	Lampz.Callback(67) = "DisableLighting g01_p67_off, 2,"
	Lampz.MassAssign(68)= l68
	Lampz.MassAssign(68)= l68a
	Lampz.MassAssign(68)= l68b
	Lampz.Callback(68) = "DisableLighting g01_p68, 2,"
	Lampz.Callback(68) = "DisableLighting g01_p68_off, 2,"
	Lampz.MassAssign(69)= l69
	Lampz.MassAssign(69)= l69a
	Lampz.Callback(69) = "DisableLighting g01_p69, 2,"
	Lampz.Callback(69) = "DisableLighting g01_p69_off, 2,"
	Lampz.MassAssign(70)= l70
	Lampz.MassAssign(70)= l70a
	Lampz.Callback(70) = "DisableLighting g01_p70, 2,"
	Lampz.Callback(70) = "DisableLighting g01_p70_off, 2,"
	Lampz.MassAssign(71)= l71
	Lampz.MassAssign(71)= l71a
	Lampz.Callback(71) = "DisableLighting g01_p71, 2,"
	Lampz.Callback(71) = "DisableLighting g01_p71_off, 2,"
	Lampz.MassAssign(72)= l72
	Lampz.MassAssign(72)= l72a
	Lampz.Callback(72) = "DisableLighting g01_p72, 2,"
	Lampz.Callback(72) = "DisableLighting g01_p72_off, 2,"

	'Lampz.MassAssign(73)= l73
	Lampz.Callback(73) = "DisableLighting p73, 20,"
	'Lampz.MassAssign(74)= l74
	Lampz.Callback(74) = "DisableLighting p74, 20,"
	'Lampz.MassAssign(75)= l75
	Lampz.Callback(75) = "DisableLighting p75, 20,"
	'Lampz.MassAssign(76)= l76
	Lampz.Callback(76) = "DisableLighting p76, 20,"
	'Lampz.MassAssign(77)= l77
	Lampz.Callback(77) = "DisableLighting p77, 20,"
	'Lampz.MassAssign(78)= l78
	Lampz.Callback(78) = "DisableLighting p78, 20,"
	'Lampz.MassAssign(79)= l79	'Tournament Button
	'Lampz.MassAssign(80)= l80  'Start Button
	Lampz.MassAssign(81)= l81
	Lampz.MassAssign(81)= l81a
	Lampz.MassAssign(82)= l82
	Lampz.MassAssign(82)= l82a
	Lampz.MassAssign(83)= l83
	Lampz.MassAssign(83)= l83a
	Lampz.MassAssign(84)= l84
	Lampz.MassAssign(84)= l84a
	Lampz.MassAssign(85)= l85
	Lampz.MassAssign(85)= l85a
	Lampz.MassAssign(86)= l86
	Lampz.MassAssign(86)= l86a
	Lampz.MassAssign(87)= l87
	Lampz.MassAssign(87)= l87a
	Lampz.MassAssign(88)= l88
	Lampz.MassAssign(88)= l88a
	Lampz.MassAssign(89)= l89
	Lampz.MassAssign(89)= l89a
	Lampz.MassAssign(90)= l90
	Lampz.MassAssign(90)= l90a
	Lampz.MassAssign(91)= l91
	Lampz.MassAssign(91)= l91a
	Lampz.MassAssign(92)= l92
	Lampz.MassAssign(92)= l92a
	Lampz.MassAssign(93)= l93
	Lampz.MassAssign(93)= l93a
	Lampz.MassAssign(94)= l94
	Lampz.MassAssign(94)= l94a
	Lampz.MassAssign(95)= l95
	Lampz.MassAssign(95)= l95a
	Lampz.MassAssign(96)= l96
	Lampz.MassAssign(96)= l96a
	Lampz.MassAssign(97)= l97
	Lampz.MassAssign(97)= l97a
	Lampz.MassAssign(98)= l98
	Lampz.MassAssign(98)= l98a
	Lampz.MassAssign(99)= l99
	Lampz.MassAssign(99)= l99a

    'potd Lights
	Lampz.MassAssign(100)=l_potd_1
	Lampz.MassAssign(100)=l_potd_2
	Lampz.MassAssign(100)=l_potd_3
	Lampz.MassAssign(100)=l_potd_4
	Lampz.MassAssign(100)=l_potd_5
	
	'sword glow
	Lampz.Callback(101) = "DisableLightingSword clearPlasticsSword, 0.3,"
	Lampz.Callback(101) = "DisableLightingSword swordHandletext, 20,"
'	Lampz.Callback(101) = "DisableLightingSword swordHandletext_off, 20,"
	Lampz.MassAssign(101) = swordflash
	Lampz.MassAssign(101) = swordflashs

	'vial glow
	'Lampz.Callback(102) = "DisableLightingVial vial, 5,"
	Lampz.MassAssign(102) = Lvial
	Lampz.MassAssign(102) = Lvialhalo


	'flashers

'	Lampz.Callback(114)= "Flash14"
'	Lampz.Callback(123)= "Flash23"
'	Lampz.Callback(125)= "Flash25"
'	Lampz.Callback(126)= "Flash26"
'	Lampz.Callback(127)= "Flash27"
'	Lampz.Callback(129)= "Flash29"
'	Lampz.Callback(130)= "Flash30"
	'Lampz.Callback(131)= "Flash31"  'This would be an additional lamp under the Frodo insert. Not needed?

	Lampz.MassAssign(130)= f130
	Lampz.MassAssign(130)= f130l
'	Lampz.Callback(130) = "DisableLighting f130bulb, 1,"

	Lampz.MassAssign(132) = Lbalrogbloom
	Lampz.Callback(132) = "BalrogImageSwap"


	Lampz.obj(0) = ColtoArray(GI)
	Lampz.Callback(0) = "GIUpdates"
	Lampz.state(0) = 1

	'Turn off all lamps on startup
	lampz.Init	'This just turns state of any lamps to 1
	ModLampz.Init

	'Immediate update to turn on GI, turn off lamps
	lampz.update
	ModLampz.Update

End Sub

'Lamp Filter
Function LampFilter(aLvl)
	LampFilter = aLvl^1.6	'exponential curve?
End Function


'GI related subs

dim kk, giprevalvl, ballbrightness
giprevalvl = 0

sub OnPrimsVisible(aValue)
	If aValue then
		If PlayfieldToys = 1 then
			For each kk in ON_Prims:kk.visible = 1:next
		Else
			For each kk in ON_Prims:kk.visible = 1:next
			For each kk in ToyModels:kk.visible = 0:next
		End If
	Else
		For each kk in ON_Prims:kk.visible = 0:next
	end If
end Sub

sub OffPrimsVisible(aValue)
	If aValue then
		If PlayfieldToys = 1 then
			For each kk in OFF_Prims:kk.visible = 1:next
		Else
			For each kk in OFF_Prims:kk.visible = 1:next
			For each kk in ToyModels:kk.visible = 0:next
		End If
	Else
		For each kk in OFF_Prims:kk.visible = 0:next
	end If
end Sub

'GI callback
Const PFGIOFFOpacity = 100

Sub GIUpdates(ByVal aLvl)	'argument is unused
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	'debug.print "aLvl=" & aLvl & " giprevalvl=" & giprevalvl

	if aLvl = 0 then										'GI OFF, let's hide ON prims
		'debug.print "aLvl = 0. OnPrimsVisible False"
		OnPrimsVisible False
		Flasherlight1.intensity = 7
		If giprevalvl = 1 Then OffPrimsVisible true
		if ballbrightness <> -1 then ballbrightness = ballbrightMin
	Elseif aLvl = 1 then									'GI ON, let's hide OFF prims
		'debug.print "aLvl = 1. OffPrimsVisible False"
		OffPrimsVisible False
		Flasherlight1.intensity = 2
		If giprevalvl = 0 Then OnPrimsVisible True
		if ballbrightness <> -1 then ballbrightness = ballbrightMax
	Else
		if giprevalvl = 0 Then								'GI has just changed from OFF to fading, let's show ON
			'debug.print "giprevalvl = 0. OnPrimsVisible True"
			OnPrimsVisible True
			ballbrightness = ballbrightMin + 1
		elseif giprevalvl = 1 Then							'GI has just changed from ON to fading, let's show OFF
			'debug.print "giprevalvl = 1. OffPrimsVisible true"
			OffPrimsVisible true
			ballbrightness = ballbrightMax - 1
		Else
			'no change
		end if
	end if

	UpdateMaterial "OpaqueON",			0,0,0,0,0,0,((aLvl*0.75)+0.25)^1,RGB(255,255,255),0,0,False,True,0,0,0,0		'let transparency be only 0.25 and 1.
	'UpdateMaterial "PlasticTransON",	0,0,0,0,0,0,aLvl^1,RGB(255,255,255),0,0,False,True,0,0,0,0

	Playfield_OFF.opacity = PFGIOFFOpacity - (PFGIOFFOpacity * alvl^3)
	clearPlastics.blenddisablelighting = 1.5 * alvl + 0.5

	'ball
	if ballbrightness <> ballbrightMax Or ballbrightness <> ballbrightMin Or ballbrightness <> -1 then ballbrightness = INT(alvl * (ballbrightMax - ballbrightMin) + ballbrightMin)

	giprevalvl = alvl
End Sub


'Helper functions

Function ColtoArray(aDict)	'converts a collection to an indexed array. Indexes will come out random probably.
	redim a(999)
	dim count : count = 0
	dim x  : for each x in aDict : set a(Count) = x : count = count + 1 : Next
	redim preserve a(count-1) : ColtoArray = a
End Function

Sub SetLamp(aNr, aOn)
	Lampz.state(aNr) = abs(aOn)
End Sub


'***************************************
'***End nFozzy lamp handling***
'***************************************



'******************************************************
'                BALL ROLLING AND DROP SOUNDS
'******************************************************

Const tnob = 4 ' total number of balls
Const lob = 0		'number of locked balls
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

Sub RollingTimer()
	Dim b
	
	' stop the sound of deleted balls
	For b = UBound(gBOT) + 1 to tnob
		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
		rolling(b) = False
		StopSound("BallRoll_" & b & "_amp9")
	Next


	' exit the sub if no balls on the table
	If UBound(gBOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(gBOT)
		If BallVel(gBOT(b)) > 1 AND gBOT(b).z < 30 AND NOT bBallInTrough(b) Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b & "_amp9"), -1, VolPlayfieldRoll(gBOT(b)) * BallRollVolume * VolumeDial, AudioPan(gBOT(b)), 0, PitchPlayfieldRoll(gBOT(b)), 1, 0, AudioFade(gBOT(b))
		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b & "_amp9")
				rolling(b) = False
			End If
		End If

		'***Ball Drop Sounds***
		If gBOT(b).VelZ < -1 and gBOT(b).z < 55 and gBOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If gBOT(b).velz > -7 Then
					RandomSoundBallBouncePlayfieldSoft gBOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard gBOT(b)
				End If                                
			End If
		End If
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If

		' "Static" Ball Shadows
		If AmbientBallShadowOn = 0 Then
			If gBOT(b).Z > 30 Then
				BallShadowA(b).height=gBOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
			Else
				BallShadowA(b).height=gBOT(b).z - BallSize/2 + 5
			End If
			BallShadowA(b).Y = gBOT(b).Y + Ballsize/5 + fovY
			BallShadowA(b).X = gBOT(b).X
			BallShadowA(b).visible = 1
		End If
	Next
End Sub


const BallBrightMax = 255			'Brightness setting when GI is on (max of 255). Only applies for Normal ball.
const BallBrightMin = 100			'Brightness setting when GI is off (don't set above the max). Only applies for Normal ball.
Sub UpdateBallBrightness
	Dim b, brightness
	For b = 0 to UBound(gBOT)
		If NOT bBallInTrough(b) then
			If gBOT(b).z < 0 Then 'make ball darker when uner the PF
				brightness = INT(min(ballbrightMax * max(1 + 0.9/55*gBOT(b).z, 0.1),255) )
				gBOT(b).color = brightness + (brightness * 256) + (brightness * 256 * 256)
			Else
				gBOT(b).color = ballbrightness + (ballbrightness * 256) + (ballbrightness * 256 * 256)
			End If
			'gBOT(b).color = ballbrightness + (ballbrightness * 256) + (ballbrightness * 256 * 256)
			if b = UBound(gBOT) then 'until last ball brightness is set, then reset to -1
				if ballbrightness = ballbrightMax Or ballbrightness = ballbrightMin then ballbrightness = -1
			end if
		End If
	Next
End Sub


'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************


Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 0.99	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.8	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 15	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source


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

Dim sourcenames, currentShadowCount, DSSources(30), numberofsources, numberofsources_hold
sourcenames = Array ("","","","","","","","","","","","")
currentShadowCount = Array (0,0,0,0,0,0,0,0,0,0,0,0)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!
dim objrtx1(4), objrtx2(4)
dim objBallShadow(4)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4)

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
	Dim s, Source, LSd, currentMat, AnotherSource, iii

'	'Hide shadow of deleted balls
'	For s = UBound(gBOT) + 1 to tnob
'		objrtx1(s).visible = 0
'		objrtx2(s).visible = 0
'		objBallShadow(s).visible = 0
'		BallShadowA(s).visible = 0
'	Next
'
'	If UBound(gBOT) < lob Then Exit Sub		'No balls in play, exit

	'The Magic happens now
	For s = lob to UBound(gBOT)

		' *** Normal "ambient light" ball shadow
		'Layered from top to bottom. If you had an upper pf at for example 80 and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

		If AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
			If gBOT(s).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If gBOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = gBOT(s).Y + BallSize/10 + fovY
				objBallShadow(s).visible = 1

				BallShadowA(s).X = gBOT(s).X
				BallShadowA(s).Y = gBOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf, primitive only
				objBallShadow(s).visible = 1
				If gBOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = gBOT(s).Y + fovY
				BallShadowA(s).visible = 0
			Else											'Under pf, no shadows
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 0
			end if

		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
			If gBOT(s).Z > 30 Then							'In a ramp
				BallShadowA(s).X = gBOT(s).X
				BallShadowA(s).Y = gBOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				If gBOT(s).X < tablewidth/2 Then
					BallShadowA(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					BallShadowA(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				BallShadowA(s).Y = gBOT(s).Y + Ballsize/10 + fovY
				BallShadowA(s).height=gBOT(s).z - BallSize/2 + 5
			Else											'Under pf
				BallShadowA(s).visible = 0
			End If
		End If

		' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If gBOT(s).Z < 30 Then 'And BOT(s).Y < (TableHeight - 200) Then 'Or BOT(s).Z > 105 Then		'Defining when and where (on the table) you can have dynamic shadows
				For iii = 0 to numberofsources - 1 
					LSd=DistanceFast((gBOT(s).x-DSSources(iii)(0)),(gBOT(s).y-DSSources(iii)(1)))	'Calculating the Linear distance to the Source
					If LSd < falloff And gilvl > 0 Then	  'If the ball is within the falloff range of a light and light is on
						currentShadowCount(s) = currentShadowCount(s) + 1		'Within range of 1 or 2
						if currentShadowCount(s) = 1 Then						'1 dynamic shadow source
							sourcenames(s) = iii
							currentMat = objrtx1(s).material
							objrtx2(s).visible = 0 : objrtx1(s).visible = 1 : objrtx1(s).X = gBOT(s).X : objrtx1(s).Y = gBOT(s).Y + fovY
	'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01						'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), gBOT(s).X, gBOT(s).Y) + 90
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
							objrtx1(s).visible = 1 : objrtx1(s).X = gBOT(s).X : objrtx1(s).Y = gBOT(s).Y + fovY
	'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01							'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(DSSources(AnotherSource)(0),DSSources(AnotherSource)(1), gBOT(s).X, gBOT(s).Y) + 90
							ShadowOpacity = (falloff-DistanceFast((gBOT(s).x-DSSources(AnotherSource)(0)),(gBOT(s).y-DSSources(AnotherSource)(1))))/falloff
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0

							currentMat = objrtx2(s).material
							objrtx2(s).visible = 1 : objrtx2(s).X = gBOT(s).X : objrtx2(s).Y = gBOT(s).Y + fovY
	'						objrtx2(s).Z = BOT(s).Z - 25 + s/1000 + 0.02							'Uncomment if you want to add shadows to an upper/lower pf
							objrtx2(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), gBOT(s).X, gBOT(s).Y) + 90
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





'********************
'     FlippersPol
'********************


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

	Addpt "Velocity", 0, 0,         1
	Addpt "Velocity", 1, 0.16, 1.06
	Addpt "Velocity", 2, 0.41,         1.05
	Addpt "Velocity", 3, 0.53,         1'0.982
	Addpt "Velocity", 4, 0.702, 0.968
	Addpt "Velocity", 5, 0.95,  0.968
	Addpt "Velocity", 6, 1.03,         0.945

	LF.Object = LeftFlipper        
	LF.EndPoint = EndPointLp
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
End Sub

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub


'******************************************************
'           FLIPPER CORRECTION FUNCTIONS
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
'                FLIPPER POLARITY AND RUBBER DAMPENER
'                        SUPPORTING FUNCTIONS 
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
'                        FLIPPER TRICKS
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
	Dim b

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
			For b = 0 to Ubound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					exit Sub
				end If
			Next
			For b = 0 to Ubound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper2) Then
					gBOT(b).velx = gBOT(b).velx / 1.3
					gBOT(b).vely = gBOT(b).vely - 0.5
				end If
			Next
		End If
	Else 
		If Flipper1.currentangle <> EndAngle1 then 
			EOSNudge1 = 0
		end if
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

Function min(a,b)
	if a > b then 
		min = b
	Else
		min = a
	end if
end Function


'*************************************************
' Check ball distance from Flipper for Rem
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
' End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 0.8 
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
SOSRampup = 2.5

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.025

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
		Dim b
                        
		For b = 0 to UBound(gBOT)
			If Distance(gBOT(b).x, gBOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If gBOT(b).vely >= -0.4 Then gBOT(b).vely = -0.4
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

'######################### Add new dampener to CheckLiveCatch 
'#########################    Note the updated flipper angle check to register if the flipper gets knocked slightly off the end angle

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

'****************************************************************************
'PHYSICS DAMPENERS
'****************************************************************************

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR


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
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7 	'Level of bounces. 0.2 - 1.2 are probably usable values.

sub TargetBouncer(aBall,defvalue)
    dim zMultiplier, vel, vratio
    if TargetBouncerEnabled <> 0 and aball.z < 30 then
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


'******************************************************
'                TRACK ALL BALL VELOCITIES
'                 FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
	public ballvel, ballvelx, ballvely

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update()        'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs

		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)        'set bounds
		if uBound(ballvelx) < highestID then redim ballvelx(highestID)        'set bounds
		if uBound(ballvely) < highestID then redim ballvely(highestID)        'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

Sub RDampen()
	Cor.Update
End Sub


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


Dim bBallInTrough(3)
Sub CheckBallLocations
	Dim b
	For b = 0 to UBound(gBOT)
		'Check if ball is in the trough
		If InRect(gBOT(b).X, gBOT(b).Y, 847,1840,425,2117,494,2196,847,1960) Then
			bBallInTrough(b) = True
		Else
			bBallInTrough(b) = False
		End If
	Next
End Sub


'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1														'volume level; range [0, 1]
NudgeLeftSoundLevel = 1													'volume level; range [0, 1]
NudgeRightSoundLevel = 1												'volume level; range [0, 1]
NudgeCenterSoundLevel = 1												'volume level; range [0, 1]
StartButtonSoundLevel = 0.1												'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr									'volume level; range [0, 1]
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
RubberStrongSoundFactor = 0.055/5										'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5											'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5										'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025									'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025									'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8								'volume level; range [0, 1]
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
SpinnerSoundLevel = 0.1                              					'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8													'volume level; range [0, 1]
BallReleaseSoundLevel = 1												'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2									'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015										'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5												'volume multiplier; must not be zero


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
	PlaySoundAtLevelStatic ("Flipper_Attack-L01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic ("Flipper_Attack-R01"), FlipperUpAttackLeftSoundLevel, flipper
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
	TargetBouncer Activeball, 1
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

	'briefly turn off magnet already on. This should allow 2 ball DTR to work
	If RingMagOn and Snd_DTRstart Then
		mRingMagnet.MagnetOn = False
		KickBall KickerBall47, 0, 10, 0, 0
		MagDelay.Enabled = True	
		'debug.print "ball ball collision, ringmag off"
	End If

End Sub


'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************


'====================
'Class jungle nf
'=============

'No-op object instead of adding more conditionals to the main loop
'It also prevents errors if empty lamp numbers are called, and it's only one object
'should be g2g?

Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class

'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
' Note: if using multiple 'LampFader' objects, set the 'name' variable to avoid conflicts with callbacks

Class LampFader
	Public FadeSpeedDown(140), FadeSpeedUp(140)
	Private Lock(140), Loaded(140), OnOff(140)
	Public UseFunction
	Private cFilter
	Public UseCallback(140), cCallback(140)
	Public Lvl(140), Obj(140)
	Private Mult(140)
	Public FrameTime
	Private InitFrame
	Public Name

	Sub Class_Initialize()
		InitFrame = 0
		dim x : for x = 0 to uBound(OnOff) 	'Set up fade speeds
			FadeSpeedDown(x) = 1/100	'fade speed down
			FadeSpeedUp(x) = 1/80		'Fade speed up
			UseFunction = False
			lvl(x) = 0
			OnOff(x) = False
			Lock(x) = True : Loaded(x) = False
			Mult(x) = 1
		Next
		Name = "LampFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OF THESE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
		for x = 0 to uBound(OnOff) 		'clear out empty obj
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		Next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property		'debug.print Lampz.Locked(100)	'debug
	Public Property Get state(idx) : state = OnOff(idx) : end Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function
	'Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property
	Public Property Let Callback(idx, String)
		UseCallBack(idx) = True
		'cCallback(idx) = String 'old execute method
		'New method: build wrapper subs using ExecuteGlobal, then call them
		cCallback(idx) = cCallback(idx) & "___" & String	'multiple strings dilineated by 3x _

		dim tmp : tmp = Split(cCallback(idx), "___")

		dim str, x : for x = 0 to uBound(tmp)	'build proc contents
			'If Not tmp(x)="" then str = str & "	" & tmp(x) & " aLVL" & "	'" & x & vbnewline	'more verbose
			If Not tmp(x)="" then str = str & tmp(x) & " aLVL:"
		Next

		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		'if idx = 132 then msgbox out	'debug
		ExecuteGlobal Out

	End Property

	Public Property Let state(ByVal idx, input) 'Major update path
		if Input <> OnOff(idx) then  'discard redundant updates
			OnOff(idx) = input
			Lock(idx) = False
			Loaded(idx) = False
		End If
	End Property

	'Mass assign, Builds arrays where necessary
	'Sub MassAssign(aIdx, aInput)
	Public Property Let MassAssign(aIdx, aInput)
		If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
			if IsArray(aInput) then
				obj(aIdx) = aInput
			Else
				Set obj(aIdx) = aInput
			end if
		Else
			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
		end if
	end Property

	Sub SetLamp(aIdx, aOn) : state(aIdx) = aOn : End Sub	'Solenoid Handler

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x)' : debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.001 ' this can prevent init stuttering
				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx)' : debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.001 ' this can prevent init stuttering
			end if
		Next
		'debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 0.2 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'Just runs TurnOnStates right now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1()	 'Handle all boolean numeric fading. If done fading, Lock(x) = True. Update on a '1' interval Timer!
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx : for x = 0 to uBound(OnOff)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(Lvl(x)*Mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*Mult(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(Lvl(x)*Mult(x))
					Else
						obj(x).Intensityscale = Lvl(x)
					End If
				end if
				if TypeName(lvl(x)) <> "Double" and typename(lvl(x)) <> "Integer" then msgbox "uhh " & 2 & " = " & lvl(x)
				'If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x))	'Callback
				If UseCallBack(x) then Proc name & x,Lvl(x)*mult(x)	'Proc
				If Lock(x) Then
					if Lvl(x) = 1 or Lvl(x) = 0 then Loaded(x) = True	'finished fading
				end if
			end if
		Next
	End Sub
End Class




'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be publicly accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
'Version 0.13a - fixed DynamicLamps hopefully
' Note: if using multiple 'DynamicLamps' objects, change the 'name' variable to avoid conflicts with callbacks

Class DynamicLamps 'Lamps that fade up and down. GI and Flasher handling
	Public Loaded(50), FadeSpeedDown(50), FadeSpeedUp(50)
	Private Lock(50), SolModValue(50)
	Private UseCallback(50), cCallback(50)
	Public Lvl(50)
	Public Obj(50)
	Private UseFunction, cFilter
	private Mult(50)
	Public Name

	Public FrameTime
	Private InitFrame

	Private Sub Class_Initialize()
		InitFrame = 0
		dim x : for x = 0 to uBound(Obj)
			FadeSpeedup(x) = 0.01
			FadeSpeedDown(x) = 0.01
			lvl(x) = 0.0001 : SolModValue(x) = 0
			Lock(x) = True : Loaded(x) = False
			mult(x) = 1
			Name = "DynamicFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property
	'Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function

	Public Property Let Callback(idx, String)
		UseCallBack(idx) = True
		'cCallback(idx) = String 'old execute method
		'New method: build wrapper subs using ExecuteGlobal, then call them
		cCallback(idx) = cCallback(idx) & "___" & String	'multiple strings dilineated by 3x _

		dim tmp : tmp = Split(cCallback(idx), "___")

		dim str, x : for x = 0 to uBound(tmp)	'build proc contents
			'debugstr = debugstr & x & "=" & tmp(x) & vbnewline
			'If Not tmp(x)="" then str = str & "	" & tmp(x) & " aLVL" & "	'" & x & vbnewline	'more verbose
			If Not tmp(x)="" then str = str & tmp(x) & " aLVL:"
		Next

		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		'if idx = 132 then msgbox out	'debug
		ExecuteGlobal Out

	End Property


	Public Property Let State(idx,Value)
		'If Value = SolModValue(idx) Then Exit Property ' Discard redundant updates
		If Value <> SolModValue(idx) Then ' Discard redundant updates
			SolModValue(idx) = Value
			Lock(idx) = False : Loaded(idx) = False
		End If
	End Property
	Public Property Get state(idx) : state = SolModValue(idx) : end Property

	'Mass assign, Builds arrays where necessary
	'Sub MassAssign(aIdx, aInput)
	Public Property Let MassAssign(aIdx, aInput)
		If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
			if IsArray(aInput) then
				obj(aIdx) = aInput
			Else
				Set obj(aIdx) = aInput
			end if
		Else
			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
		end if
	end Property

	'solcallback (solmodcallback) handler
	Sub SetLamp(aIdx, aInput) : state(aIdx) = aInput : End Sub	'0->1 Input
	Sub SetModLamp(aIdx, aInput) : state(aIdx) = aInput/255 : End Sub	'0->255 Input
	Sub SetGI(aIdx, ByVal aInput) : if aInput = 8 then aInput = 7 end if : state(aIdx) = aInput/7 : End Sub	'0->8 WPC GI input

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x) ': debugstr = debugstr & tmp(x).name & " state'd" & vbnewline

				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx) ': debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline

			end if
		Next
		'debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'just call turnonstates for now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1()	 'Handle all numeric fading. If done fading, Lock(x) = True
		'dim stringer
		dim x : for x = 0 to uBound(Lvl)
			'stringer = "Locked @ " & SolModValue(x)
			if not Lock(x) then 'and not Loaded(x) then
				If lvl(x) < SolModValue(x) then '+
					'stringer = "Fading Up " & lvl(x) & " + " & FadeSpeedUp(x)
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				ElseIf Lvl(x) > SolModValue(x) Then '-
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					'stringer = "Fading Down " & lvl(x) & " - " & FadeSpeedDown(x)
					if Lvl(x) <= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				End If
			end if
		Next
		'tbF.text = stringer
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(Lvl)
			if not Lock(x) then 'and not Loaded(x) then
				If lvl(x) < SolModValue(x) then '+
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				ElseIf Lvl(x) > SolModValue(x) Then '-
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				End If
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx
		for x = 0 to uBound(Lvl)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(abs(Lvl(x))*mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*mult(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(abs(Lvl(x))*mult(x))
					Else
						obj(x).Intensityscale = Lvl(x)*mult(x)
					End If
				end if
				'If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x)*mult(x))	'Callback
				If UseCallBack(x) then Proc name & x,Lvl(x)*mult(x)	'Proc
				If Lock(x) Then
					Loaded(x) = True
				end if
			end if
		Next
	End Sub
End Class

'Helper functions
Sub Proc(string, Callback)	'proc using a string and one argument
	'On Error Resume Next
	dim p : Set P = GetRef(String)
	P Callback
	If err.number = 13 then  msgbox "Proc error! No such procedure: " & vbnewline & string
	if err.number = 424 then msgbox "Proc error! No such Object"
End Sub

Function AppendArray(ByVal aArray, aInput)	'append one value, object, or Array onto the end of a 1 dimensional array
	if IsArray(aInput) then 'Input is an array...
		dim tmp : tmp = aArray
		If not IsArray(aArray) Then	'if not array, create an array
			tmp = aInput
		Else						'Append existing array with aInput array
			Redim Preserve tmp(uBound(aArray) + uBound(aInput)+1)	'If existing array, increase bounds by uBound of incoming array
			dim x : for x = 0 to uBound(aInput)
				if isObject(aInput(x)) then
					Set tmp(x+uBound(aArray)+1 ) = aInput(x)
				Else
					tmp(x+uBound(aArray)+1 ) = aInput(x)
				End If
			Next
		AppendArray = tmp	 'return new array
		End If
	Else 'Input is NOT an array...
		If not IsArray(aArray) Then	'if not array, create an array
			aArray = Array(aArray, aInput)
		Else
			Redim Preserve aArray(uBound(aArray)+1)	'If array, increase bounds by 1
			if isObject(aInput) then
				Set aArray(uBound(aArray)) = aInput
			Else
				aArray(uBound(aArray)) = aInput
			End If
		End If
		AppendArray = aArray 'return new array
	End If
End Function

'***********************class jungle**************


'=====================================
'		Ramp Rolling SFX updates nf
'=====================================
'Ball tracking ramp SFX 1.0
'	Usage:
'- Setup hit events with WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'- To stop tracking ball, use WireRampoff
'--	Otherwise, the ball will auto remove if it's below 30 vp units

'Example, from Space Station:
'Sub RampSoundPlunge1_hit() : WireRampOn  False : End Sub						'Enter metal habitrail
'Sub RampSoundPlunge2_hit() : WireRampOff : WireRampOn True : End Sub			'Exit Habitrail, enter onto Mini PF 
'Sub RampEntry_Hit() : If activeball.vely < -10 then WireRampOn True : End Sub 	'Ramp enterance
dim RampMinLoops : RampMinLoops = 4

dim RampBalls(12,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

dim RampType(12)	'Slapped together support for multiple ramp types... False = Wire Ramp, True = Plastic Ramp

Sub WireRampOn(input)  : Waddball ActiveBall, input : RampRollUpdate: End Sub
Sub WireRampOff() : WRemoveBall ActiveBall.ID	: End Sub

Sub Waddball(input, RampInput)	'Add ball
    'Debug.Print "In Waddball() + add ball to loop array"	
	dim x : for x = 1 to uBound(RampBalls)	'Check, don't add balls twice
		if RampBalls(x, 1) = input.id then 
			if Not IsEmpty(RampBalls(x,1) ) then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next
	
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
			'Debug.print "WireRampOn error, ball queue is full: " & vbnewline & RampBalls(0, 0) & vbnewline & _
				'Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbnewline & _
				'Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbnewline & _
				'Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbnewline & _
				'Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbnewline & _
				'Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbnewline & _
				'" "
		End If
	next
End Sub

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
					PlaySound("RampLoop" & x & "_amp9"), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))				
					StopSound("wireloop" & x & "_amp9")
				Else
					StopSound("RampLoop" & x & "_amp9")
					PlaySound("wireloop" & x & "_amp9"), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2)	= RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x & "_amp9")
				StopSound("wireloop" & x & "_amp9")
			end if
			if RampBalls(x,0).Z < 30 and RampBalls(x, 2) > RampMinLoops then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x & "_amp9")
				StopSound("wireloop" & x & "_amp9")
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x & "_amp9")
			StopSound("wireloop" & x & "_amp9")
		end if
	next
	if not RampBalls(0,0) then RampRoll.enabled = 0

End Sub


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



'****************************************************************
' The One Ring glowing text animation
'****************************************************************

Dim TORtest: TORtest = False
Dim TORcnt: TORcnt = 0

Dim TORbeta1 : TORbeta1 = 0
Dim TORbeta2 : TORbeta2 = 0
Dim TORbeta3 : TORbeta3 = 0

Const TORcntmax1 = 600
Const TORcntdead1 = 350
Const TORcntmax2 = 350
Const TORcntdead2 = 120
Const TORcntmax3 = 200
Const TORcntdead3 = 0

TORInit

Sub TORInit
	UpdateMaterial TheOneRingText1.material,0,0,0,0,0,0,0,RGB(0,0,0),0,0,False,True,0,0,0,0
	UpdateMaterial TheOneRingText2.material,0,0,0,0,0,0,0,RGB(0,0,0),0,0,False,True,0,0,0,0
	UpdateMaterial TheOneRingText3.material,0,0,0,0,0,0,0,RGB(0,0,0),0,0,False,True,0,0,0,0
	TheOneRingText1.blenddisablelighting = 40
	TheOneRingText2.blenddisablelighting = 40
	TheOneRingText3.blenddisablelighting = 40
End Sub


Sub TheOneRingUpdates_Timer
	' start to fade out if mode is over
	If Snd_DTRfinal or Snd_EOB or Snd_Main Then
		Snd_DTRstart = False
		Snd_DTRfinal = False
		Snd_EOB = False
		Snd_Main = False
	End If
	' manage fading counter
	If Snd_DTRstart or TORtest Then
		TORcnt = TORcnt + 1
		TORcnt = min(TORcntmax1,TORcnt)
	Else
		TORcnt = TORcnt - 1
		TORcnt = max(0,TORcnt)
	End If
	'update the text opacity based on the fading counter value
	TORbeta1 = max(0,(TORcnt-TORcntdead1)/(TORcntmax1-TORcntdead1))
	TORbeta2 = 0.6*(max(0,(TORcnt-TORcntdead2)/(TORcntmax2-TORcntdead2) - 2*max(0,(TORcnt-TORcntmax2)/(TORcntmax2-TORcntdead2))))
	TORbeta3 = 0.4*(max(0,(TORcnt-TORcntdead3)/(TORcntmax3-TORcntdead3) - 2*max(0,(TORcnt-TORcntmax3)/(TORcntmax3-TORcntdead3))))

	UpdateMaterial TheOneRingText1.material,0,0,0,0,0,0,0.3*TORbeta1,RGB(255*TORbeta1,83*TORbeta1,83*TORbeta1),0,0,False,True,0,0,0,0
	UpdateMaterial TheOneRingText2.material,0,0,0,0,0,0,0.3*TORbeta2,RGB(255*TORbeta2,83*TORbeta2,83*TORbeta2),0,0,False,True,0,0,0,0
	UpdateMaterial TheOneRingText3.material,0,0,0,0,0,0,0.3*TORbeta3,RGB(255*TORbeta3,83*TORbeta3,83*TORbeta3),0,0,False,True,0,0,0,0

	'debug.print "TORcnt=" & TORcnt & " TORbeta1=" & TORbeta1 & " TORbeta2=" & TORbeta2 & " TORbeta3=" & TORbeta3

	' disable this timer if we are done fading out
	If TORcnt<= 0 Then TheOneRingUpdates.Enabled=False ': debug.print "TheOneRingUpdates.Enabled=False"
End Sub




'********************************************************
'  ROM SoundCommand Listener
'********************************************************

' Sound commands
' FD1D = start of destroy the ring mode
' FD2C = successfully completed DTR
' FD0B = Ring wraithes mission 1
' FD0C = Ring wraithes mission 2
' FD0D = Ring wraithes mission 3
' FD09 = shelob mission starts
' FD04 = end of ball
' FD03 = main theme 2 (not in a mode)
' FD02 = main theme 1 (not in a mode)

Const hexFD = 253
Const hex2C = 44
Const hex1D = 29
Const hex0D = 13
Const hex0C = 12
Const hex0B = 11
Const hex09 = 9
Const hex08 = 8
Const hex04 = 4
Const hex03 = 3
Const hex02 = 2

Dim Snd_DTRstart
Dim Snd_DTRfinal
Dim Snd_Wraithes
Dim Snd_Shelob
Dim Snd_EOB
Dim Snd_Main 

Dim LastSnd : LastSnd = 0
Dim SndCmdStr

ClearSndFlags

Sub ClearSndFlags
	'debug.print "ClearSndFlags"
	Snd_DTRstart = False
	Snd_DTRfinal = False
	Snd_Shelob = False
	Snd_Wraithes = False
	Snd_EOB = False
	Snd_Main = False

	Setlamp 102,0
End Sub

Sub SoundCmdListener
    Dim NewSounds,ii,Snd
    NewSounds=Controller.NewSoundCommands
    If Not IsEmpty(NewSounds) Then
		' Listen for specific commands
        For ii=0 To UBound(NewSounds)
            Snd=NewSounds(ii,0)
            If LastSnd = hexFD Then
				Select Case Snd
					Case hex1D: ClearSndFlags : Snd_DTRstart = True : TORcnt = 1 : TheOneRingUpdates.Enabled=True ': debug.print "Snd_DTRstart = True"
					Case hex2C: ClearSndFlags : Snd_DTRfinal = True ': debug.print "Snd_DTRfinal = True"
					Case hex0D: ClearSndFlags : Snd_Wraithes = True ': debug.print "hex0D: Snd_Wraithes = True"
					Case hex0C: ClearSndFlags : Snd_Wraithes = True ': debug.print "hex0C: Snd_Wraithes = True"
					Case hex0B: ClearSndFlags : Snd_Wraithes = True ': debug.print "hex0B: Snd_Wraithes = True"
					Case hex08: ClearSndFlags ': debug.print "hex08: Snd_Wraithes = False"
					Case hex09: ClearSndFlags : Snd_Shelob = True : Setlamp 102,1 ': debug.print "Snd_Shelob = True"
					Case hex04: ClearSndFlags : Snd_EOB = True ': debug.print "Snd_EOB = True"
					Case hex03: ClearSndFlags : Snd_Main = True ': debug.print "Snd_Main = True"
					Case hex02: ClearSndFlags : Snd_Main = True ': debug.print "Snd_Main = True"
				End Select
            End If
			LastSnd = Snd
        Next
    End If
End Sub



Dim PFToys
If PlayfieldToys = 1 Then
	for each PFToys in ToyModels:PFToys.visible = 1:Next
	Else
	for each PFToys in ToyModels:PFToys.visible = 0:Next
End If



'****************************************************************
' Glowball code
'****************************************************************
 
Dim GlowBall, CustomBulbIntensity(3)
Dim  GBred(3)
Dim GBgreen(3), GBblue(3)
Dim CustomBallImage(3), CustomBallLogoMode(3), CustomBallDecal(3), CustomBallGlow(3)
Const anglecompensate = 15

' default Ball
CustomBallGlow(0) = 		False
CustomBallImage(0) = 		"ball_pinball4"
CustomBallLogoMode(0) = 	False
CustomBallDecal(0) = 		"balldecal_scratches2"
CustomBulbIntensity(0) = 	0.7
GBred(0) = 0 : GBgreen(0)	= 0 : GBblue(0) = 0

' Gold GlowBall
CustomBallGlow(1) = 		True
CustomBallImage(1) = 		"ball_old_ass_eyes"
CustomBallLogoMode(1) = 	True
CustomBallDecal(1) = 		"balldecal_scratches2"
CustomBulbIntensity(1) = 	0
GBred(1) = 255 : GBgreen(1)	= 128 : GBblue(1) = 0

' Green GlowBall
CustomBallGlow(2) = 		True
CustomBallImage(2) = 		"ball_glowgreen"
CustomBallLogoMode(2) = 	True
CustomBallDecal(2) = 		""
CustomBulbIntensity(2) = 	0
GBred(2) = 20 : GBgreen(2)	= 200 : GBblue(2) = 0

' Magma GlowBall
CustomBallGlow(3) = 		True
CustomBallImage(3) = 		"ball_black"
CustomBallLogoMode(3) = 	True
CustomBallDecal(3) = 		"balldecal_magma"
CustomBulbIntensity(3) = 	0
GBred(3) = 150 : GBgreen(3)	= 0 : GBblue(3) = 0
'GBred(3) = 255 : GBgreen(3)	= 255 : GBblue(3) = 100  'orange


' *** prepare the variable with references to three lights for glow ball ***
Dim Glowing(10)
Set Glowing(0) = Glowball1 : Set Glowing(1) = Glowball2 : Set Glowing(2) = Glowball3 : Set Glowing(3) = Glowball4


'*** change ball appearance ***

Sub ChangeBall(ballnr)
	Dim ii, col
	table1.BallDecalMode = CustomBallLogoMode(ballnr)
	table1.BallFrontDecal = CustomBallDecal(ballnr)
	table1.DefaultBulbIntensityScale = CustomBulbIntensity(ballnr)
	table1.BallImage = CustomBallImage(ballnr)
	GlowBall = CustomBallGlow(ballnr)
	For ii = 0 to 3
		col = RGB(GBred(ballnr), GBgreen(ballnr), GBblue(ballnr))
		Glowing(ii).color = col : Glowing(ii).colorfull = col 
	Next
End Sub

'*** This sub runs in the FrameTimer ***

Sub UpdateGlowball
	Dim b
    For b = 0 to UBound(gBOT) 
		' move glow light if ball not in trough, otherwise turn off the light
		If NOT bBallInTrough(b) Then
			If Glowing(b).state = 0 Then Glowing(b).state = 1 
			Glowing(b).BulbHaloHeight = gBOT(b).z + 51
			Glowing(b).x = gBOT(b).x : Glowing(b).y = gBOT(b).y + anglecompensate
		Else
			Glowing(b).state = 0
		End If
	Next
End Sub

'****************************************************************
'		Cabinet Mode
'****************************************************************

If CabinetMode Then
	PinCab_Rails.visible = 0
	PinCab_RailsVR.visible = 0
	Cab_Sides.size_y = 1500
	Cab_Sides_Off.size_y = 1500
Else
	PinCab_Rails.visible = 1
	Cab_Sides.size_y = 1000
	Cab_Sides_Off.size_y = 1000
End If

'****************************************************************
'		VR Mode
'****************************************************************
DIM VRThings
If VRRoom > 0 Then
	ScoreText.visible = 0
	PinCab_Rails.visible = 0
	PinCab_RailsVR.visible = 1
	swordflash.height = 160
	If VRRoom = 1 Then
		for each VRThings in VR360:VRThings.visible = 1:Next
		for each VRThings in VRMin:VRThings.visible = 0:Next
		for each VRThings in VRStuff:VRThings.visible = 1:Next
	End If
	If VRRoom = 2 Then
		for each VRThings in VR360:VRThings.visible = 0:Next
		for each VRThings in VRMin:VRThings.visible = 1:Next
		for each VRThings in VRStuff:VRThings.visible = 1:Next
	End If
	If VRRoom = 3 Then
		for each VRThings in VR360:VRThings.visible = 0:Next
		for each VRThings in VRMin:VRThings.visible = 0:Next
		for each VRThings in VRStuff:VRThings.visible = 0:Next
		PinCab_Backbox.visible = 1
		PinCab_Backglass.visible = 1
		DMD.visible = 1
	End If
Else
	PinCab_RailsVR.visible = 0
	swordflash.height = 200
	If desktopmode And CabinetMode = 0 then PinCab_Rails.visible = 1
	for each VRThings in VRStuff:VRThings.visible = 0:Next
'		for each VRThings in VR_Cab:VRThings.visible = 0:Next
'		for each VRThings in VR_Min:VRThings.visible = 0:Next
End if

If VR360Image = 1 then VR360_Sphere.image = "360_Castle" else VR360_Sphere.image = "360_Forest"




'****************************************************************
'		VPW change log
'****************************************************************
'
'000 - AstroNasty - added nFozzy physics and Fleep sounds
'001 - iaakki - added EB's PF and checked few values
'002 - iaakki - PF image top border added and VPX dimensions adjusted
'003 - iaakki - new PF cutout and Insert text image
'004 - apophis - Realigned objects on all layers to conform with PF image. Some primitive models still need to be rebuilt to align (like sword, wire ramps, etc). Also fixed issues with nFozzy and Fleep implmentation. Ramp sounds still need work.
'005 - iaakki - left sandwich rubber adjusted, Reworked to NF lampz (flashers later). Layer 7 now dedicated to insert lamps
'006 - apophis - installed and wired up 3D inserts. Ball can now fall from path of dead to left wire ramp. 
'007 - bord - Added playfield mesh
'008 - fluffhead35 - Added RampRoll Looping Logic for WireRamps.  Added new sounds for wireramps.  Added new ramp triggers to turn on and off wire ramp sounds and wireramp stop sounds.
'009 - apophis - Fixed left VUK behavior. Added Roth saucer code and applied to gollum VUK. Added iaakki's flipper Rubberizer option.
'010 - apophis - Removed Gate001. Added dynamic RTX ball shadow option (having strange behavior on insert Lights)
'011 - iaakki - changed RTB bs update material parameters and but DB to 100 on those small inserts between flips
'012 - apophis - Added more GI lights to the RtxBS collection. Modified Rtx bs code to only cast shadows when the GI light is on.
'013 - iaakki - swapped to flupper flip logos, added FlipperCoilRampupMode script option
'014 - apophis - Updated RTX BS to latest version. Adjusted physics properties on flipper rubbers and rubber posts next to flippers. Adjusted sword ramp position.
'015 - gtxjoe - Added Debug outlane/drain block (Press 2 key).  Added Shot tester, Use W E R Y U I P A S F G to test shots.  Press key and use flippers to adjust Angle.  Fixed RVUK ball search error.
'016 - apophis - made curved diverter collidable when not open to Ent hole
'017 - astronasty - moved inserts to match new layout
'018 - apophis - Made BOT array global, Rdampen timer at 1 ms. Fixed ball stuck issue during multiballs (need more test to confirm). Set RTX ball shadow prims 1 & 2 DB to different values
'019 - astronasty - Moved playfield elements to whitewood in prep for redraw.
'020 - apophis - Reverted to local BOT arrays, rdampen at 10 ms. Fixed BS DB issues.
'021 - apophis - Added Flupper flashers. Flashers 26, 27, and 29 still need work.
'022 - apophis - Flupperized flashers 26, 27, 29, and 30
'023 - apophis - Updated the back wall lights
'024 - apophis - Updated RTX BS image and equations
'025 - apophis - Updated RTX BS image and shadow primitives 
'026 - apophis - Added iaakki's target bouncer code
'027 - apophis - Included early draft of AstroNasty's PF artwork. Added flipper shadows. Ambient shadows respond to RTX GI lights.
'028 - apophis - Cut hole in playfield mesh and added trough ramp and walls for Ent hole. Fixed targetbouncer bug. Fixed right ramp bounce back issue. Fixed some lighting.
'029 - apophis - Included updated draft of AstroNasty's PF artwork. Added "the one ring" glowing text during Destroy the Ring mode.
'030 - apophis - Updated "the one ring" text settings and behavior. Added a timer. See new code at bottom of script.
'031 - apophis - Added SoundCmdListener functionality so now TOR animation only happens during DTR. Updated TOR animation. Created GameTimer to get rid of Motorcallback. Updated rubberizer and flipper nudge settings.
'032 - Sixtoe - changed loads of materials, added a collection and switch for toys, tried to fix the left wire ramp entrance.
'033 - astronasty - Put the fellowship inserts in the right order.
'034 - apophis - Included final draft of AstroNasty's PF artwork. Added wall on right side of orthanc tower.
'035 - apophis - Added draft of AstroNasty's apron and plastics art. Fleep wall sound fix. Potential divide by zero fix. Added Shelob mode SoundCmds to Listener routine and Vial glow animation.
'036 - apophis - Inceased brightness of glowing vial. Added BallBrightness option in the script (for experimenting). Disabled reflections from backwall lights. Changed table slope from 6.5 to 5.8.
'037 - astronasty - PF/plastics/apron drawing updates.  Changed the plastics to 'plastic with an image' per Hauntfreaks suggestion.
'038 - apophis - Added brighter ball hdri, changed sling physics parameters, changed hit thresh on posts from 2 to 0.5, changed ball rolling sound effect
'039 - tomate - new wireRamps prims added at layer 7
'040 - apophis - Reorganized layers. Adjust VPX ramps to match new wire ramps. Set ball roll sound back to standard Fleep. 
'041 - tomate - Vuks/wireRamps separeted, vuks turned as collidable objects, new metalRamps added, new trigger ramps added, erased old prims at layer 11 and set them as non collidables, VPX ramps fixed
'042 - tomate - all the new prims added at layer4, GION textures added
'043 - Sixtoe - So much stuff... Split new prims, duplicated for off gi on layer5, hooked up new individual prims to lights, changed origins of movable prims and hooked them up (balrog, rtower, lockpost, diverter), rebuilt left doubleback ramp and surrounding area and walls (including new low friction material), rebuilt ring ramp (behind backboard), now works after ball captured by magnet. Aligned playfield triggers and holes. Locked unlocked devices. Removed collidable status on certain things, removed half the GI and dropped it below the playfield, redid some walls, added some blocker walls, added mroe collidable objects, almost certainly more stuff
'044 - Sixtoe - Hooked up movable "_off" primitive trackers, got diverter working properly, significantly updated playfield layout and removed a lot of redundant assets.
'045 - Sixtoe - Timers unified, trimmed flashers and gi, separated orbitpin, seperated globe and hooked it up to lighting system, seperated realigned & duplicated targets (readied for roths targets), added toy collection back to switch, added and hooked up playfield insert blooms, hooked up insert sidewall flashers, replaced sidewall primitive image.
'046 - apophis - Updated sword ramp: floor is stepped, deleted SwordGate. Installed physical trough. Installed physical sw9 VUK. Installed physical ball lock. Improved ball reflections and insert blooms. 
'047 - apophis - Completely reworked Balrog animation. Made him collidable while opening and closing (using a flipper).
'048 - Sixtoe - Split primitives for bumper flashers, altered ltower prim, added ltower collidable vuk, added / tweaked potd walls, this will need setting up when kicker added, might need to move and tweak objects around ltower vuk.
'049 - apophis - Upper left VUK and upper right saucer do not destroy balls now. #savetheballs. Tuned lower left VUK to be more reliable. 
'050 - fluffhead35 - Updating ramprolling code and sounds.  Implementing ramprolling amplification settings as well as ball rolling amplificaiton settings.
'051 - apophis - Some script cleanup. Wire vuk sound effects. Tuned ring magnet behavior. Reworked some ramp rolling sound triggers.
'052 - apophis - Eliminated all Getballs calls. Using global gBOT. Added Narnia checker (returns ball to UL VUK). Updated ramprolling volume code. Deleted unused ampfactor sound files.
'053 - apophis - Added Palantir Eye code (Thanks Wylte).  Eye currently always follows a ball only when palantir is lit, switching which ball after 3 seconds without motion.
'054 - tomate - Tons of prims and textures remplaced, some tweaks here and there, updated On/Off collections
'055 - Sixtoe - Added playfield ring lights & blooms, removed lotr-pf, disabled flashers & textures, adjusted flashers and light heights (now light metals on inlane)
'056 - Sixtoe - Added POTD playfield glow.
'057 - apophis - Updated playfield and insert overlay images. POTD bulbs are green now. Mode bulbs are flat now.
'058 - apophis - Connected POTD lights to Lampz. Sound file cleanup. Updated to latest Fleep code. Updated to latest ballshadow code.
'059 - apophis - Added a prototype code to distort ball image when on the sword ramp.
'060 - apophis - Reduced screen space reflections. Updated balrog prim and texture (as a test), thanks Tomate. Increased inert bloom object areas, tuned red inserts a little. 
'061 - apophis - Updated Sauron Eye look logic. Balrog light functionality added. Figurine prim and texture updates. Added ball height GI around light clusters. Fixed issue with upper left vuk when more than one ball in the kicker trough. Updated Targetbouncer. Assigned some physics materials. 
'062 - tomate - added new toy's textures, updated all the toys prims in layer4 and layer 5 and added to collections
'063 - tomate - added clear plastics prims and textures 
'064 - apophis - Made Balrog animation smoother. Fixed issues with drain and trough. Orthanc wall tweak. Updated Rubberizer. Updated flipper physics parameters. Made ULVUK scoop not collidable and updated how it kicks. Updated clear platics texture and material. Removed ball distortion test.
'065 - Sixtoe - Added sword glowing effect, currently hooked up to lamp 20 in the lamps sub to test. Split transparent plastic primitives so plastic diverter for if you miss the ring shot works and is now collidable. Trimming bloom lamps & adjusted falloff so they don't clip
'066 - apophis - Hooked up GI prims to Lampz. Imported some OFF textures. Now sword glows only when balls are locked on ramp. Removed cutouts from TOR text. Tweaked TOR animation. Added ball brightness updates
'067 - Sixtoe - Added collidable primitive ring and back ramp, adjusted ring entrance ramp to centre it to the ring and new height, adjusted materials, adjusted clear plastic protector.
'068 - tomate - Lanes, metals walls and screws prims fixed and replaced in layer 4 and 5, new GION_metals and GION_screws textures added
'069 - apophis - Added all the GI OFF textures. Thanks a million Tomate. Set ON_Prims DB to -100. Set all GI OFF prims to visible initially. Set GI ON prims DB. l64a thru l74a DB set to -200000. Deleted image "BackGround".
'070 - apophis - Added glow balls. Tweaked flipper physics parameters. Removed GIOFF_balrog_lit. Consolodated GI callbacks. Made ball brightness a function of GI (when not Glowball). Added off PF image. Added Playfield_OFF flasher, but it's not working yet.
'071 - apophis - Optimized PNG textures. Insert prims z set to -0.1. Fixed Playfield_OFF flasher visibility. Changed playfield overlay from a ramp to a flasher. Made CheckBallLocations sub to reduce InRect calls. Added LUT selector.
'072 - apophis - Updated GION_metals03new.png. Adjusted position of Flasher 29. Changed primitive for Flasher 27. Fixed flasher material assignments. Attempted to tuned Legolas backhand issue: Reduced flipper strength to 3100 (from 3200), Increased ramp friction to 0.12 (from 0.1), Changed Wall028 geometry. Changed Flip end angle to 70 (from 69) and updated flip triggers.
'073 - iaakki - added clearplastics to giupdates, modified lampz fading speeds to look more natural
'074 - iaakki - Flasher side and pf reflections added, Inserts made a bit faster, but still not seeing them flashing for some modes
'075 - iaakki - Domes fixed, side wall flashers updated and tuned, globe lights adjusted
'076 - Sixtoe - Enabled UseVPMModSol, messed around with sword and flashers overhauled inc. blooms.
'077 - apophis - Added balrog light bloom. Tweaked ring magnet position and left VUK Wall3 to avoid needing to nudge ball off stuck positions. Fixed the cracks in the hobbit figurines. Tuned and animated locking mechanism. Tuned g02_globeye_off DL. Slowed down sword glow fade.
'078 - Skitso - Added glow to visile bulbs, made area lighting for GI, tweaked insert lighting, tweaked ball brightness to fit new area ligthing
'079 - apophis - Fixed ball reflection issue. Tweaked some flasher dome intensities. Tweaked green POTD bulb lit opacity. Tweaked red mode light lit DL.
'080 - apophis - Solved the GI ON and OFF prim visibility issue. Preloaded flashers at table initialization.
'081 - apophis - Fixed GI off sword text issue. Made animated sling rubbers white to match baked rubbers. Adjusted position of POTD green lights.
'082 - Skitso - Fixed small ring inserts with skitso style inserts to look correct. Further tweak to inserts to reduce excessive blooming
'082 - iaakki - Flasher blooms optimizations
'083 - apophis - Added low poly collidable VUKs. Added some saucer wall geometry on lower left VUK. Adjusted position of swRamp2. Fixed all the figurine cracks. Tweaked POTD bulb color. Updating "Rubber White" material color with GI state. Reduced ball angmoms when ring magnet hit. Added some kick to ball coming out of ring magnet. Disabled reflections for most prims.
'084 - apophis - Added Cabinet Mode. Set ball trail to 10. Updated PF flasher images. Corrected ring magnet kick angle. Updated POTD lights again. Lowered the opacity of the yellow flasher sideblade images. Upper right kicker power reduced and randomized a bit. Fixed sword lock mechanism timing. Changed normal pinball image.
'085 - Skitso - Small POTD bulb improvemets. Disabled ball reflection from green lamps below POTD
'086 - apophis - Added blink hack for lamps 25, 33, 37, 41, 49, 45. Fixed some LUT code. Added desktop DMD. Updated ball decal to scratches2. Fixed sling rubber shapes. Increase ball reflections from GI. Removed extra wired sounds at end of ramps. Added metal sound when ball falls into left wire ramp from POTD. Made POTD bulb textures more greenish. Increased ring magnet strength and size. Disabled reflection from ring flasher. 
'087 - apophis - Added Balrog hit blinking. Patched ball stuck issue in the ring. Adjusted lampz fade speeds. Adjust some flashers light intesities and heights. Adjusted POTD floor lights. Reduce magnet strength after capture to allow for 2 ball DTR to work (doesnt work yet). 
'088 - Sixtoe - Tuned 2x bottom yellow flashers, flasher7 (table level under sword) and POTD rear panel lights.
'089 - EBisLit - removed artifacts from insert holes on pf
'090 - iaakki - backramp and sw47a position minor change
'091 - apophis - Added light 19 to blink hack. Updated PF and insert overlay images. Updated balrog ON Lit texture and DL (will need to be redone because UV map is a little wrong). Added texture to mode lights. 
'092 - apophis - Got 2 ball DTR working using OnBallBallCollision as trigger. Good idea tomate.
'093 - Wylte - Wall042 added to match skulls plastic below PotD, podballdrop trigger moved onto it
'094 - Sixtoe - Fixed ring LED panel layout and assignments (jeez what a mess), added preliminary VR room and cabinet artwork (backbox art to be replaced), added creamrubbers to slings so they're more muted, removed flasher30 from flupperflash and rebuilt it so it behaves a bit more realistically, no more glare but might need some more tuning but it's 1am.
'095 - Skitso - Fixed broken gi bulb bloom effect, improved flasher130 under the sword, removed ball reflection from all green and red insert lights.
'096 - Sixtoe - Set up VR cabinet and room properly including code (still needs final backglass, backbox artwork & start button), added specific lockdown / rails for VR, added switch & code to remove playfield toys so it actually works (*massive* vr performance improvement), added pincab_bottom.
'097 - iaakki - Rothbauerw updates to rubberizer, 3 different versions available. Set now to same as Indi, but feel free to experiment
'098 - apophis - Tied vial glow to Lampz.
'099 - Skitso - Remade vial insert in skitso style, altered insert primitives' DL values to even out those that are too bright with new GI and other recent lighting changes etc.
'100 - Sixtoe - Fixed texture issue causing white line in plunger lane (GION_metals/GION_metals), changed flipper buttons to yellow.
'101 - Skitso - Fixed right side blade reflection flasher positions, remade plungerlane launch lamps, adjusted insert primitive DL values, adjusted back wall ring flasher.
'102 - tomate - Balrog prims and textures Updated 
'103 - apophis - Tuned left VUK strength. Tried to fix stuck ball issue on Legolas ramp. Tuned balrog DL. Adjusted vertical positions of the mode lights and increased DL. 
'104 - apophis - Fiddled with the legolas ramp as outside wall was in wrong position. Added clear plastic ramp hit sound. Fixed issue with balrog light flicker.
'105 - apophis - Flipper strength at 3150. Adjusted flash27's flasher object. Fixed ball stuck issues on Legolas ramp and Sword ramp.
'106 - Sixtoe - Finished (?) VR cabinet, added 360 room with a couple of images, scripted everything up, added trapped ball resiliance in several places (shouldn't have broken anything)
'107 - apophis - Updated Lvial light. Replaced apron textures. Optimized some images. Cleaned up script. Updated POV.
'108 - Sixtoe - Split cab and sides prim, hooked side prim up to cabinet switch so they're bigger in that mode, tweaked area around legolas ramp to provide more resiliance to stuck balls, raised swordflash height to 200 for cabinet mode to stop it cutting toys in half, looks too floaty in VR at 200 so left at 160 (choose your poison etc.), tidied up some table level stuff, removed duplicates, cut holes in playfield on/off/flashers for drop holes, tweaked apron area for VR
'RC1 - apophis - Fixed artifacts around some inserts. Removed a couple useless gates. Flipper strength 3200. Adjusted shape of Wall10. 
'RC2 - iaakki - startup improvement
'RC3 - apophis - Updated LUT7. Updated Flasherflash7 settings and image. Adjusted desktop POV. Fixed L49 behavior during Ring Wraith mode.
'RC4 - Sixtoe - Rebuilt ramp behind ring, split rampgates primtive, added vpx gates, hooked up primitive tracker so they move, adjusted and turned off bracket from ring spinner and replaced with one that actually fits, tweaked metalwalls prim so it's not visible under tower.
'RC5 - apophis - Updated GIOFF_metals04 and GION_metals04 textures. Swordflash height a function of VRRoom setting. objflash heights changed. sw47 hit height set to 100. Adjusted physics parameters of materials at top of sword ramp (less bouncy). Wall14 top height changed to 200 (revent POTD stuck ball). 
'RC6 - apophis - Fixed visual problem with ramp below Orthanc tower. Now ball gets darker when under the PF. 
'RC7 - apophis - Updated Lampz timer interval and fading speed numbers. Desaturated texture of tower subway. Reduced swordflash amount to 900. 
'RC8 - Sixtoe - Hooked up missing pop-bumper light (oops!), fixed ball trap behind POTD, added switch to turn off the startup audio, fixed metal textures to remove white line from plunger lane, add backwall_roof to hide the ring flasher under certain POV's, moved rear flasher flush to back wall, adjust flasherflare positions slightly, added latest on/off playfield textures.
'RC9 - apophis - Fixed ball stuck issue under ring.
'RC10 - iaakki - Sideblade flashers for cabinetmode
'RC11 - Sixtoe - Added blocker wall so ball can't go through orthanc tower, plunger pull speed increased
'v1.0 Release
'v1.01 - Sixtoe - Adjusted colliable posts and sleeves to match prims. They were slightly off. 
'v1.02 - apophis - Fixed the sideblade flashes in CabinetMode. Also increased the FlasherBloom height in CabinetMode. 
'v1.03 - apophis - Tuned sword ramp return sleeve.
'v1.1 Release
'v1.11 - apophis - Fixed bug with sw47 not always sending off command. Caused intermittent issues with ring shot behavior.
'v1.12 - apophis - Made balrog switch (28) register more consistently. Fortified top of sword ramp. Made SwordRoof bottom collidable. Change physics settings for Wall051. Made upper right kicker always feed pop bumpers.
'v1.13 - apophis - More tweaks to Wall051 and OrbitPin physics settings. Plunger strength reduced to 70. Reduced nudge strengths reduced 1, inc sensitivity to 3. Added PlungerTipColor option. Added Wall043 to fortify trough ceiling. 
'v1.14 - apophis - Updated dynamic shadow code. Added VUK solenoid animations.
'v1.2 Release
'v1.2.1 - apophis - Reduced the texture size for all figurines. Update ball rolling and ramp rolling sounds to latest code (removed extra sound files). Update rubberizer to latest. Removed all physics related player options. Automated cabinetmode.
'v1.3 Release
