' Indiana Jones: The Pinball Adventure - IPDB No. 1267
' © Williams 1993
' Widebody "Superpin" Line
' https://www.ipdb.org/machine.cgi?id=1267
'
'**************************************
'	V-Pin Workshop Pinball Explorers
'**************************************
' Playfield Redraw - Brad1X
' Graphics / 3D Work - Benji, iaakki, Tomate, Sixtoe
' Scripting - iaakki, Benji, Apophis, Sixtoe
' Sound - Benji, Apophis, Fluffhead, iaakki
' Physics - iaakki, Benji, Sixtoe, rothbauerw
' Lighting - iaakki, Benji, Skitso, Sixtoe
' Shadows - Apophis, Wylte
' VR - Sixtoe, Leojreimroc
' Testing - Rik, PinStratsDan, CalleV, VPW team.
'
' VPW Mod based on Ninuzzua / Tom Tower v1.2
' Thanks to destruk for original code, knorr and clark kent for the art resources, flupper for bumper caps & flasher domes models and VP Dev team for VPX!

Option Explicit
Randomize

'************************************************************************
'							Table options
'************************************************************************
Const VRRoom = 0					'VR Room (0 = Off, 1 = Minimal, 2 = Ultra Minimal)
Const VRFlashingBackglass = 0		'VR Flashing Backglass (0 = Off, 1 = On)
Const CabinetMode = 0				'Makes sides taller and turns off siderails
Const PropellerMod = 0				'Animate Bi-Plane Propeller when making left ramp (0= no, 1= yes)
Const FlipperType = 0				'Flippers Type (0= White/Red, 1= White/Orange, 2 = White/Black, 3 = White/Blue, 4 = Orange/Red)
Const OutlaneDifficulty = 1			'0 = Very Easy, 1 = Medium (Default), 2 = Hard
Const BlimpToy = 0					'0 = No Blimp, 1 = Blimp!

'///////////////////////-----General Sound Options-----///////////////////////
'// VolumeDial is the actual global volume multiplier for the mechanical sounds.
'// Values smaller than 1 will decrease mechanical sounds volume.
'// Recommended values should be no greater than 1.
Const VolumeDial = 0.8

'//////////////---- LUT (Colour Look Up Table) ----//////////////
Const DisableLUTSelector = False  	'Disables the ability to change LUT option with magna saves in game when set to 1
Const LutToggleSound = True			'Enables or disables the LUT sound effects

'Live catch window size. Higher value will make live catching easier. 8-32
Const LiveCatch = 16
Const Rubberizer = 2				'Enhance micro bounces on flippers, 0 - disable, 1 - rothbauerw version, 2 - iaakki version
Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 1.1 	'Level of bounces. 0.2 - 1.5 are probably usable values.

'Shadow Options
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's)
									'2 = flasher image shadow, but it moves like ninuzzu's
Const BallSize = 50
Const BallMass = 1

Const Testmode = 0

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim DesktopMode:DesktopMode = Table1.ShowDT
Dim UseVPMDMD
If VRRoom <> 0 Then UseVPMDMD = True Else UseVPMDMD = DesktopMode End If
Const UseVPMModSol = 1

'************************************************************************
'						End of table options
'************************************************************************

LoadVPM "02800000", "WPC.VBS", 3.55

'************************************
'******* Standard definitions *******
'************************************
' Rom Name
Const cGameName = "ij_l7"

' Standard Options
Const UseSolenoids = 2
Const UseLamps = 0
Const UseSync = 1
Const HandleMech = 0

' IJ Specific Option
Const cSingleLFlip = 0
Const cSingleRFlip = 0

'keyStagedFlipperL=""
'keyStagedFlipperR=""

' Standard Sounds
Const SSolenoidOn = "fx_Solenoid"
Const SSolenoidOff = ""
Const SCoin = ""

'************************************************************************
'						 Solenoids map
'************************************************************************

SolCallback(1)="bsPopper.SolOut" 												'BallPopper
SolCallback(2)="AutoPlunger" 													'BallLaunch
SolCallback(3)="TotemDropUP" '"dtTotem.SolDropUp" 								'TotemDropUp
SolCallback(4)="SolBallRelease" 												'BallRelease
SolCallback(5)="ResetDrops"   '"dtBank.SolDropUp"								'CenterDropUp
SolCallback(6)="SolIdol"														'IdolRelease
SolCallback(7)="vpmSolSound SoundFX(""fx_Knocker"",DOFKnocker),"				'Knocker
SolCallback(8)="bsLEject.SolOut"												'Left Eject
'SolCallback(9)="vpmSolSound SoundFX(""LeftJetNOTUSED"",DOFContactors),"		'Left Bumper
'SolCallback(10)="vpmSolSound SoundFX(""RightJetNOTUSED"",DOFContactors),"		'Right Bumper
'SolCallback(11)="vpmSolSound SoundFX(""BottomJetNOTUSED"",DOFContactors),"		'Bottom Bumper
'SolCallback(12)="RandomSoundSlingshotRight"										'Right Sling
'SolCallback(13)="RandomSoundSlingshotLeft"										'Left Sling	
SolCallback(14)="vpmSolGate LeftGate,SoundFX(""DiverterOn"",DOFContactors),"	'Left ControlGate
SolCallback(15)="vpmSolGate RightGate,SoundFX(""DiverterOn"",DOFContactors),"	'Right ControlGate
SolCallback(16)="TotemDropDOWN" '"dtTotem.SolDropDown"							'TotemDropDown
SolCallback(17)="SolFlash17"													'Insert:Eternal Life        	'BG Center
SolModCallback(18)="SolFlash18"		'"SetModLamp 18, "							'Flasher:Light JackPot
SolCallback(19)="SetLamp 119, "													'Insert:Super Jackpot	
SolModCallback(20)="SetModLamp 20, "											'Flasher:JackPot				'BG Ark
SolModCallback(21)="SetModLamp 21, "											'Flasher:Path of Adventure		'BG Letters
SolCallback(22)="PoAMoveLeft"													'L_PoA (*)
SolCallback(23)="PoAMoveRight"													'R_PoA (*)
SolModCallback(24)="SetModLamp 24, "											'Flasher:Plane Gun LEDS
SolCallback(25)="SetLamp 125, "													'Insert:Dogfight Hurry up
SolModCallback(26)="SolFlash26"		'"solflashRRamp"	'"SetModLamp 26, "		'Flasher:Right Ramp (x3)		'BG Right Fire
SolModCallback(27)="SolFlash27"		'"solflashLRamp"	'SetModLamp 27, "		'Flasher:Left Ramp				'BG Left Sky
SolCallback(28)="bsSubway.SolOut"												'SubwayRelease

SolCallback(33)="SolDivPower"													'DivPower
SolCallback(34)="SolDivHold"													'DivHold
SolCallback(35)="SolTopPostPower"												'TopPostPower
SolCallback(36)="SolTopPostHold"												'TopPostHold

SolModCallback(51)="solflash51"  	'"SetModLamp 31, "							'Flasher:Left Side (x2)			'BG Left Horse
SolModCallback(52)="SolFlash52"		'"SetModLamp 32, "							'Flasher:Right Side (x2)		'BG Right Cave
SolCallback(53)="SetLamp 116, "													'53 -> 116 -> l153l, L153R
SolCallback(54)="SetLamp 115, "													'Insert:Totem Multi
SolModCallback(55)= "solflash55"	'"SetModLamp 35, "							'Flasher:Jackpot Multi
SolCallback(56)="SolMoveIdol"													'Idol Motor

SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sLRFlipper) = "SolRFlipper"

Sub ResetDrops(enabled)
     if enabled then
          PlaySoundAt SoundFX(DTResetSound,DOFContactors), p_sw115
          DTRaise 115
          DTRaise 116
          DTRaise 117
     end if
End Sub

Sub TotemDropUP(enabled)
     if enabled then
		PlaySoundAt SoundFX(DTResetSound,DOFContactors), p_sw11
        DTRaise 11
     end if
End Sub

Sub TotemDropDOWN(enabled)
     if enabled then
          DTDrop 11
     end if
End Sub

'************************************************************************
'						 Table Init
'************************************************************************

Dim bsTrough, bsLEject, bsSubway, bsPopper, bsIdol, PoAMech

Sub Table1_Init
	vpmInit Me 
	With Controller      
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "Indiana Jones - The Pinball Adventure (Williams 1993)" & vbnewline & "VPW"
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
		.DIP(0)=&H00	'set dipswitch to USA
		.Switch(22) = 1 'close coin door
		.Switch(24) = 0 'always closed
	End With

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    ' Nudging
    vpmNudge.TiltSwitch = 14
    vpmNudge.Sensitivity = 3
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)

	'Trough
    Set bsTrough = New cvpmTrough
    With bsTrough
		.Size = 6
		.InitSwitches Array(86, 85, 84, 83, 82, 81)
		.InitExit BallRelease, 70, 15
		'.InitEntrySounds "fx_drain", SoundFX(SSolenoidOn,DOFContactors), SoundFX(SSolenoidOn,DOFContactors)
		.InitExitSounds  SoundFX(SSolenoidOn,DOFContactors), SoundFX("BallRelease",DOFContactors)
		.Balls = 6
    End With

	'Left Eject
    Set bsLEject = new cvpmSaucer
    With bsLEject
        .InitKicker Sw31, 31, 160, 17, 0
        .InitSounds "fx_saucerHit", SoundFX("LeftEject",DOFContactors), SoundFX("LeftEject",DOFContactors)
        .CreateEvents "bsLEject", Sw31
    End With

	'Subway Eject
    Set bsSubway = new cvpmSaucer
    With bsSubway
        .InitKicker sw47, 47, 90, 10, 0
        .InitSounds "", SoundFX("SubWayRelease",DOFContactors), SoundFX("SubWayRelease",DOFContactors)
        .CreateEvents "bsSubway", Sw47
    End With

	'Subway Popper Eject
    Set bsPopper = new cvpmSaucer
    With bsPopper
        .InitKicker Sw44, 44, 0, 32, 1.56
        .InitSounds "", SoundFX("Ball Popper",DOFContactors), SoundFX("Ball Popper",DOFContactors)
        .CreateEvents "bsPopper", Sw44
    End With

	'Idol Eject
    Set bsIdol = New cvpmTrough
    With bsIdol
		.Size = 3
		.InitSwitches Array(0, 0, 0)
		.InitExit IdolExit, 180, 0
		.Balls = 0
		.CreateEvents "bsIdol", IdolEnter
    End With

	'Path of Adventure
	Set POAMech=New cvpmMech
	With POAMech
	 .MType = vpmMechTwoDirSol + vpmMechStopEnd + vpmMechLinear
	 .Sol1=23
	 .Sol2=22
	 .Length=9
	 .Steps=9
	 .AddSw 124,0,0
	 .AddSw 125,8,8
	 .CallBack=GetRef("UpdatePoA")
	 .ACC=1
	 .RET=1
	 .Start
	End With

	'Init Captive Ball
	CapKicker.CreateSizedBallWithMass Ballsize/2, BallMass:CapKicker.kick 0,0 :CapKicker.enabled=0


	select case OutlaneDifficulty
		case 0:
			p_outlanepostON.x = 545.3
			p_outlanepostON.y = 961.8
			p_outlanepostOFF.x = p_outlanepostON.x
			p_outlanepostOFF.y = p_outlanepostON.y
			R_easy.collidable = true
			R_medium.collidable = false
			R_hard.collidable = false
		Case 1:
			p_outlanepostON.x = 547.3
			p_outlanepostON.y = 960.7
			p_outlanepostOFF.x = p_outlanepostON.x
			p_outlanepostOFF.y = p_outlanepostON.y
			R_easy.collidable = false
			R_medium.collidable = true
			R_hard.collidable = false
		Case 2:
			p_outlanepostON.x = 549.5
			p_outlanepostON.y = 959.6
			p_outlanepostOFF.x = p_outlanepostON.x
			p_outlanepostOFF.y = p_outlanepostON.y
			R_easy.collidable = false
			R_medium.collidable = false
			R_hard.collidable = true
	end select

	'Other Suff
	InitPoA:InitIdol
	'InitLamps
	InitLampsNF
	InitRolling:InitOptions
	DiverterOn.IsDropped=1

	LUTBox.visible = 0
	LoadLUT
	SetLUT
End Sub

'************************************************************************
'							Keys
'************************************************************************

Sub Table1_KeyDown(ByVal Keycode)
	If Keycode= keyFront Then Controller.Switch(12)=1		'buy-in
	If keycode = PlungerKey Then Controller.Switch(34) = 1
	If keycode = LeftTiltKey Then Nudge 90, 5:SoundNudgeLeft()
	If keycode = RightTiltKey Then Nudge 270, 5:SoundNudgeRight()
	If keycode = CenterTiltKey Then Nudge 0, 3:SoundNudgeCenter()

	If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then 'Use this for ROM based games
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If

	if keycode = LeftMagnaSave then
		if DisableLUTSelector = False then
			LUTSet = LUTSet - 1
			if LutSet < 0 then LUTSet = 17
			If LutToggleSound then
				If LutSet = 17 Then 
					Playsound "Knocker_1"
				Else
					Playsound "click"
				End If
			end if
			SetLUT
			ShowLUT
		end if
	End If

	if keycode = RightMagnaSave then
		if DisableLUTSelector = False then
            LUTSet = LUTSet  + 1
			if LutSet > 17 then LUTSet = 0
			If LutToggleSound then
				If LutSet = 17 Then 
					Playsound "Knocker_1"
				Else
					Playsound "click"
				End If
			end if
			SetLUT
			ShowLUT
		end if
	End If

	If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal Keycode)
	If Keycode = keyFront Then Controller.Switch(12)=0		'buy-in
    If keycode = PlungerKey Then Controller.Switch(34) = 0

	If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress

    If vpmKeyUp(keycode) Then Exit Sub
End Sub

Sub Table1_Paused:Controller.Pause = True:End Sub
Sub Table1_unPaused:Controller.Pause = False:End Sub
Sub Table1_exit():SaveLUT:Controller.Pause = False:Controller.Stop:End Sub


' #####################################
' ###### flupper domes start      #####
' #####################################

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherOffBrightness

FL_LRampF.opacity = 0
FL_RRampF.opacity = 0
								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 1		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 1		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherOffBrightness = 0.3		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objlight(20)
Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
InitFlasher 1, "yellow"
InitFlasher 2, "green"
InitFlasher 3, "green"
RotateFlasher 2,55
RotateFlasher 3,22

'InitFlasher 2, "red" : InitFlasher 3, "white"
'InitFlasher 4, "green" : InitFlasher 5, "red" : InitFlasher 6, "white"
'InitFlasher 7, "green" : InitFlasher 8, "red"
'InitFlasher 9, "green" : InitFlasher 10, "red" : InitFlasher 11, "white" 
' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 4,17 : RotateFlasher 5,0 : RotateFlasher 6,90
'RotateFlasher 7,0 : RotateFlasher 8,0 
'RotateFlasher 9,-45 : RotateFlasher 10,90 : RotateFlasher 11,90

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr):	Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr):	Set objlight(nr) = Eval("Flasherlight" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 55
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


sub OnPrimsVisible(aValue)
	If aValue then
		For each kk in ON_Prims
			if BlimpToy = 0 then
				if Not kk.name = "p_blimp" then
					kk.visible = 1
				End If
			Else
				kk.visible = 1
			End if
		next
	Else
		For each kk in ON_Prims:kk.visible = 0:next
		if not p_plasticsOFF.visible then OffPrimsVisible true:Debug.print "why onprims not visible"
	end If
end Sub

sub OffPrimsVisible(aValue)
	If aValue then
		For each kk in OFF_Prims
			if BlimpToy = 0 then
				if Not kk.name = "p_blimpOFF" then
					kk.visible = 1
				End if
			Else
				kk.visible = 1
			End if
		next
	Else
		For each kk in OFF_Prims:kk.visible = 0:next
		if not p_plastics.visible then OnPrimsVisible true:Debug.print "why offprims not visible"
	end If
end Sub

sub BothPrimsVisible
	For each kk in OFF_Prims
		if BlimpToy = 0 then
			if Not kk.name = "p_blimpOFF" then
				kk.visible = 1
			End if
		Else
			kk.visible = 1
		End if
	next
	For each kk in ON_Prims 
		if BlimpToy = 0 then
			if Not kk.name = "p_blimp" then
				kk.visible = 1
			End if
		Else
			kk.visible = 1
		End if
	next
end sub

sub OffPrimSwap(aFlashNro, aReturn)'1 = LF, 2 = RF
	if aReturn Then
		For each ii in p_toysplastics_off:ii.image	="p_col_toysplastics_gi_off":Next
		For each ii in p_cab_off:ii.image			="p_col_cab_gi_off0000":Next
		For each ii in p_metalsposts_off:ii.image	="p_col_metalsposts_gi_off0000":Next
	Else
		Select Case aFlashNro
			Case 1:
				For each ii in p_toysplastics_off:ii.image	="p_col_toysplastics_RuinsFlash":Next
				For each ii in p_cab_off:ii.image			="p_col_cab_RuinsRamp_Flash":Next
				For each ii in p_metalsposts_off:ii.image	="p_col_metalsposts_RuinsFlash":Next
			Case 2:
				For each ii in p_toysplastics_off:ii.image	="p_col_toysplastics_RRampFlash":Next
				For each ii in p_cab_off:ii.image			="p_col_cab_RRamp_Flash":Next
				For each ii in p_metalsposts_off:ii.image	="p_col_metalsposts_RRampFlash":Next
			Case 3:
				For each ii in p_toysplastics_off:ii.image	="p_col_toysplastics_LRampFlash":Next
				For each ii in p_cab_off:ii.image			="p_col_cab_LRamp_Flash":Next
				For each ii in p_metalsposts_off:ii.image	="p_col_metalsposts_LRampFlash":Next
		End Select
	end If
end sub

sub OnPrimsTransparency(aValue)
	UpdateMaterial "ToyPlasticON",0,0,0,0,0,0,aValue,RGB(255,255,255),0,0,False,True,0,0,0,0 
	UpdateMaterial "MetalspostsON",0,0,0,0,0,0,aValue,RGB(255,255,255),0,0,False,True,0,0,0,0
	UpdateMaterial "CabMaterialON",0,0,0,0,0,0,aValue,RGB(255,255,255),0,0,False,True,0,0,0,0
end sub

dim ii
Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then
		BothPrimsVisible

		OffPrimSwap nr, false

		objflasher(nr).TimerEnabled = True
		objflasher(nr).visible = 1
		objlit(nr).visible = 1
	End If

	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0

	Select Case Nr
		Case 1:
			if ObjLevel(nr) > ObjLevel(2) And ObjLevel(nr) > ObjLevel(3) then
				UpdateMaterial "ToyPlasticON",0,0,0,0,0,0,1-ObjLevel(nr)^2,RGB(255,255,255),0,0,False,True,0,0,0,0 
				UpdateMaterial "MetalspostsON",0,0,0,0,0,0,1-ObjLevel(nr)^2.5,RGB(255,255,255),0,0,False,True,0,0,0,0
				UpdateMaterial "CabMaterialON",0,0,0,0,0,0,1-ObjLevel(nr)^1.5,RGB(255,255,255),0,0,False,True,0,0,0,0
			end if
			PLAYFIELD_ruins.opacity = 700 * ObjLevel(nr)^2
			RightSideFlasher.opacity = 1000 * ObjLevel(nr)^1 '1500
			If VRRoom > 0 and VRFlashingBackglass = 1 Then
				BGFL38_1.visible = 1
				BGFL38_2.visible = 1
				BGFL38_3.visible = 1
				BGFL38_4.visible = 1
				BGFL38_1.opacity = 50 * ObjLevel(nr)^2
				BGFL38_2.opacity = 50 * ObjLevel(nr)^2
				BGFL38_3.opacity = 50 * ObjLevel(nr)^2
				BGFL38_4.opacity = 50 * ObjLevel(nr)^2
			End If
		Case 2:
			if ObjLevel(nr) > ObjLevel(1) And ObjLevel(nr) > ObjLevel(3) then
				UpdateMaterial "ToyPlasticON",0,0,0,0,0,0,1-ObjLevel(nr)^2,RGB(255,255,255),0,0,False,True,0,0,0,0 
				UpdateMaterial "MetalspostsON",0,0,0,0,0,0,1-ObjLevel(nr)^2.5,RGB(255,255,255),0,0,False,True,0,0,0,0
				UpdateMaterial "CabMaterialON",0,0,0,0,0,0,1-ObjLevel(nr)^1.5,RGB(255,255,255),0,0,False,True,0,0,0,0
			end if
			FL_RRampF.opacity = 1000 * ObjLevel(nr)^1 '1500
			PLAYFIELD_RRamp.opacity = 2000 * ObjLevel(nr)^2
			If VRRoom > 0 and VRFlashingBackglass = 1 Then
				BGFL26_1.visible = 1
				BGFL26_2.visible = 1
				BGFL26_3.visible = 1
				BGFL26_4.visible = 1
				BGFL26_1.opacity = 50 * ObjLevel(nr)^1
				BGFL26_2.opacity = 50 * ObjLevel(nr)^1
				BGFL26_3.opacity = 50 * ObjLevel(nr)^1
				BGFL26_4.opacity = 50 * ObjLevel(nr)^1
			End If
		Case 3:
			if ObjLevel(nr) > ObjLevel(1) And ObjLevel(nr) > ObjLevel(2) then
				UpdateMaterial "ToyPlasticON",0,0,0,0,0,0,1-ObjLevel(nr)^2,RGB(255,255,255),0,0,False,True,0,0,0,0 
				UpdateMaterial "MetalspostsON",0,0,0,0,0,0,1-ObjLevel(nr)^2.5,RGB(255,255,255),0,0,False,True,0,0,0,0
				UpdateMaterial "CabMaterialON",0,0,0,0,0,0,1-ObjLevel(nr)^1.5,RGB(255,255,255),0,0,False,True,0,0,0,0
			end if
			FL_LRampF.opacity = 1000 * ObjLevel(nr)^1 '1500
			PLAYFIELD_LRamp.opacity = 1000 * ObjLevel(nr)^2
			If VRRoom > 0 and VRFlashingBackglass = 1 Then
				BGFL27_1.visible = 1
				BGFL27_2.visible = 1
				BGFL27_3.visible = 1
				BGFL27_4.visible = 1
				BGFL27_1.opacity = 30 * ObjLevel(nr)^1
				BGFL27_2.opacity = 30 * ObjLevel(nr)^1
				BGFL27_3.opacity = 30 * ObjLevel(nr)^1
				BGFL27_4.opacity = 30 * ObjLevel(nr)^1
			End If
	end Select
 
	ObjLevel(nr) = ObjLevel(nr) * 0.90 - 0.01

	If ObjLevel(nr) <= 0 Then
		if ObjLevel(1) <= 0 And ObjLevel(2) <= 0 And ObjLevel(3) <= 0 then 'if all flashers done, then swap off prims back, invisible and make ON prims opaque

			p_col_metalspostsOFF.blenddisablelighting = -0.05
			ruinsoff.blenddisablelighting = -0.1
			p_planesOFF.blenddisablelighting = -0.1
			p_blimpOFF.blenddisablelighting = -0.1
			p_col_wireRampsOFF.blenddisablelighting = 0
			UpdateMaterial "MetalspostsOFF",0,0,0,0,0,0,1,RGB(255,255,255),0,0,False,True,0,0,0,0

			offPrimSwap nr, true
			if modlampz.state(1) = 1 then 'just to check if GI state has changed while flasher fading was still ongoing
				OffPrimsVisible false
				OnPrimsTransparency 1
			Else
				debug.print "GIOFF while fading?"
				OffPrimsVisible true
				OnPrimsTransparency 0
			end if
			flash52prev = 0
			flash26prev = 0
			flash27prev = 0
			If VRRoom > 0 and VRFlashingBackglass = 1 Then
				BGFL26_1.visible = 0
				BGFL26_2.visible = 0
				BGFL26_3.visible = 0
				BGFL26_4.visible = 0
				BGFL27_1.visible = 0
				BGFL27_2.visible = 0
				BGFL27_3.visible = 0
				BGFL27_4.visible = 0
				BGFL38_1.visible = 0
				BGFL38_2.visible = 0
				BGFL38_3.visible = 0
				BGFL38_4.visible = 0
			End If
		end If

		objflasher(nr).TimerEnabled = False
		objflasher(nr).visible = 0
		objlit(nr).visible = 0
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
Sub FlasherFlash9_Timer() : FlashFlasher(9) : End Sub
Sub FlasherFlash10_Timer() : FlashFlasher(10) : End Sub
Sub FlasherFlash11_Timer() : FlashFlasher(11) : End Sub

' ###################################
' ###### flupper domes end		#####
' ###################################


'************************************************************************
'						 Solenoids
'************************************************************************
dim flash52prev : flash52prev = 0
dim flash26prev : flash26prev = 0
dim flash27prev : flash27prev = 0
dim flash51prev : flash51prev = 0
dim flash18prev : flash18prev = 0

Sub SolFlash17(Enabled)
	If Enabled Then
		SetLamp 117, 1
		If VRRoom > 0 and VRFlashingBackglass = 1 Then
			BGFL17_1.visible = 1
			BGFL17_2.visible = 1
			BGFL17_3.visible = 1
			BGFL17_4.visible = 1
			BGFL17_5.visible = 1
			BGFL17_6.visible = 1
			BGFL17_7.visible = 1
		End If
	Else
		SetLamp 117, 0
		If VRRoom > 0 and VRFlashingBackglass = 1 Then
			BGFL17_1.visible = 0
			BGFL17_2.visible = 0
			BGFL17_3.visible = 0
			BGFL17_4.visible = 0
			BGFL17_5.visible = 0
			BGFL17_6.visible = 0
			BGFL17_7.visible = 0
		End If
	End If
End Sub

Sub SolFlash52(level)
	if level >= flash52prev then
		p_col_metalspostsOFF.blenddisablelighting = 0.4
		ruinsoff.blenddisablelighting = 0
		p_planesOFF.blenddisablelighting = 0
		p_blimpOFF.blenddisablelighting = 0
		p_planes.blenddisablelighting = 0
		p_col_wireRampsOFF.blenddisablelighting = 0.5

		'making metals yellow
		UpdateMaterial "MetalspostsOFF",0,0,0,0,0,0,1,RGB(255,215,30),0,0,False,True,0,0,0,0

		if level > 180 then level = 180
		Objlevel(1) = level/180 'making it brighter
		FlasherFlash1_Timer
	Else
		Objlevel(1) = (level + flash52prev) / 510 'average of current and previous value
	end If
	flash52prev = level
End Sub

Sub SolFlash26(level)
	if level >= flash26prev then
		p_col_metalspostsOFF.blenddisablelighting = 0.4
		ruinsoff.blenddisablelighting = 0
		p_planesOFF.blenddisablelighting = 0
		p_blimpOFF.blenddisablelighting = 0
		p_planes.blenddisablelighting = 0
		p_col_wireRampsOFF.blenddisablelighting = 0.5
		'making metals green
		UpdateMaterial "MetalspostsOFF",0,0,0,0,0,0,1,RGB(80,215,30),0,0,False,True,0,0,0,0
		if level > 180 then level = 180
		Objlevel(2) = level/180
		FlasherFlash2_Timer
	Else
		Objlevel(2) = (level + flash26prev) / 510 'average of current and previous value
	end If
	flash26prev = level
End Sub

Sub SolFlash27(level)
	if level >= flash27prev then
		p_col_metalspostsOFF.blenddisablelighting = 0.4
		ruinsoff.blenddisablelighting = 0
		p_planesOFF.blenddisablelighting = 0
		p_blimpOFF.blenddisablelighting = 0
		p_planes.blenddisablelighting = 0
		p_col_wireRampsOFF.blenddisablelighting = 0.5
		'making metals green
		UpdateMaterial "MetalspostsOFF",0,0,0,0,0,0,1,RGB(80,215,30),0,0,False,True,0,0,0,0
		if level > 180 then level = 180
		Objlevel(3) = level/180
		FlasherFlash3_Timer
	Else
		Objlevel(3) = (level + flash27prev) / 510 'average of current and previous value
	end If
	flash27prev = level
End Sub

dim FlashLevel51
PLAYFIELD_leftside.visible = 0
PLAYFIELD_leftside.opacity = 0
LeftSideFlashA.IntensityScale = 0
LeftSideFlashB.IntensityScale = 0

sub solflash51(aLevel)
	'debug.print "51 -> " & aLevel
	if aLevel >= flash51prev then
		if aLevel > 180 then aLevel = 180
		FlashLevel51 = aLevel / 180	'153 was highest value I got from modulated solenoid
		PLAYFIELD_leftside_Timer
	Else
		FlashLevel51 = (aLevel + flash51prev) / 510 'average of current and previous value
	end if
	flash51prev = aLevel
End Sub

sub PLAYFIELD_leftside_Timer()
	If not PLAYFIELD_leftside.TimerEnabled Then 
		PLAYFIELD_leftside.TimerEnabled = True
		PLAYFIELD_leftside.visible = 1
		If VRRoom > 0 and VRFlashingBackglass = 1 Then
			BGFL37_1.visible = 1
			BGFL37_2.visible = 1
			BGFL37_3.visible = 1
			BGFL37_4.visible = 1
		End If
	End If

	PLAYFIELD_leftside.opacity = 200 * FlashLevel51^2
	LeftSideFlashA.IntensityScale = 1 * FlashLevel51^1.2
	LeftSideFlashB.IntensityScale = 1 * FlashLevel51 
	If VRRoom > 0 and VRFlashingBackglass = 1 Then
		BGFL37_1.opacity = 50 * FlashLevel51^2
		BGFL37_2.opacity = 50 * FlashLevel51^2
		BGFL37_3.opacity = 50 * FlashLevel51^2
		BGFL37_4.opacity = 50 * FlashLevel51^2
	End If

	FlashLevel51 = FlashLevel51 * 0.90 - 0.01
	If FlashLevel51 < 0 Then
		PLAYFIELD_leftside.TimerEnabled = False
		PLAYFIELD_leftside.visible = 0
		If VRRoom > 0 and VRFlashingBackglass = 1 Then
			BGFL37_1.visible = 0
			BGFL37_2.visible = 0
			BGFL37_3.visible = 0
			BGFL37_4.visible = 0
		End If
	End If

end Sub

'	ModLampz.MassAssign(18)= FL_LJackpotA
'	ModLampz.MassAssign(18)= FL_LJackpotB

dim FlashLevel18
FL_LJackpotA.visible = 0
FL_LJackpotA.opacity = 0
FL_LJackpotB.visible = 0
FL_LJackpotB.opacity = 0
p_litejackpot.blenddisablelighting = 0.4
p_litejackpotOFF.blenddisablelighting = 0.4
p_litejackpot.visible = 1
p_litejackpotOFF.visible = 0

sub SolFlash18(aLevel)
	'debug.print "18 -> " & aLevel
	if aLevel >= flash18prev then
		if aLevel > 180 then aLevel = 180
		FlashLevel18 = aLevel / 180	'153 was highest value I got from modulated solenoid
		FL_LJackpotA_Timer
	Else
		FlashLevel18 = (aLevel + flash18prev) / 510 'average of current and previous value
	end if
	flash18prev = aLevel
End Sub

sub FL_LJackpotA_Timer()
	If not FL_LJackpotA.TimerEnabled Then 
		FL_LJackpotA.TimerEnabled = True
		FL_LJackpotA.visible = 1
		p_litejackpotOFF.visible = 1
	End If


	FL_LJackpotA.opacity = 1000 * FlashLevel18^2
	FL_LJackpotB.opacity = 1000 * FlashLevel18^2

	UpdateMaterial "LiteJackpotON",0,0,0,0,0,0,1-FlashLevel18^1,RGB(255,255,255),0,0,False,True,0,0,0,0 

	FlashLevel18 = FlashLevel18 * 0.90 - 0.01
	If FlashLevel18 < 0 Then
		FL_LJackpotA.TimerEnabled = False
		FL_LJackpotA.visible = 0
		p_litejackpotOFF.visible = 0
	End If
end Sub

FL_JackpotMultia.IntensityScale = 0
FL_JackpotMultib.IntensityScale = 0
dim flash55prev, FlashLevel55
FlashLevel55 = 0
flash55prev = 0

sub SolFlash55(aLevel)
	'debug.print "55 -> " & aLevel
	if aLevel >= flash55prev then
		if aLevel > 180 then aLevel = 180
		FlashLevel55 = aLevel / 180	'153 was highest value I got from modulated solenoid
		FL_JackpotMultia_Timer
	Else
		FlashLevel55 = (aLevel + flash55prev) / 510 'average of current and previous value
	end if
	flash55prev = aLevel
End Sub

sub FL_JackpotMultia_Timer()
	If not FL_JackpotMultia.TimerEnabled Then 
		FL_JackpotMultia.TimerEnabled = True
		FL_JackpotMultia.visible = 1
	End If
	'debug.print "55 lvl -> " & FlashLevel55
	FL_JackpotMultia.IntensityScale = 1 * FlashLevel55^1
	FL_JackpotMultib.IntensityScale = 1 * FlashLevel55^1.2

	FlashLevel55 = FlashLevel55 * 0.90 - 0.01
	If FlashLevel55 < 0 Then
		FL_JackpotMultia.TimerEnabled = False
		FL_JackpotMultia.visible = 0
	End If
end Sub


'******************************************************
'				NFOZZY'S FLIPPERS
'******************************************************

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


'******************************************************
'				FLIPPER AND RUBBER CORRECTION
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

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub


'******************************************************
'                        FLIPPER CORRECTION FUNCTIONS
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
         :                         VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

                                if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

                                if Enabled then aBall.Velx = aBall.Velx*VelCoef
                                if Enabled then aBall.Vely = aBall.Vely*VelCoef
                        End If

                        'Polarity Correction (optional now)
                        if not IsEmpty(PolarityIn(0) ) then
                                If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
                                dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
        
                                if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
                                'playsound "fx_knocker"
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
        Dim BOT, b

        If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
                EOSNudge1 = 1
                'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
                If Flipper2.currentangle = EndAngle2 Then 
                        BOT = GetBalls
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
                If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 15 then 
                        EOSNudge1 = 0
                end if
        End If
End Sub

'*****************
' Maths
'*****************
'Const PI = 3.1415927

Function dSin(degrees)
        dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
        dcos = cos(degrees * Pi/180)
End Function

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

' Used for drop targets and stand up targets
Function Atn2(dy, dx)
'        dim pi
'        pi = 4*Atn(1)

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
'Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.018

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
                Dim BOT, b
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
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm, Rubberizer
    End If
End Sub


'*********** AutoPlunger
Sub AutoPlunger(Enabled)
    If Enabled Then
       Plunger.Fire
	   SoundPlungerReleaseBall
	End If
End Sub

'*********** BallRelease
Sub SolBallRelease(Enabled)
    If Enabled Then
        bsTrough.ExitSol_On
        If bsTrough.Balls Then vpmTimer.PulseSw 87
    End If
End Sub

'*********** Diverter
Dim DiverterDir

Sub SolDivPower(Enabled)

	'debug.print SystemTime & ": SolDivPower: " & enabled
	If Enabled Then
		DiverterOff.IsDropped=1
		DiverterOn.IsDropped=0
		DiverterDir = 1
		Diverter.Interval = 5:Diverter.Enabled = 1
		PlaySoundAt SoundFX("DiverterOn",DOFContactors),DiverterP
	End If
End Sub

Sub SolDivHold(Enabled)

	'debug.print SystemTime & ": SolDivHold: " & enabled
	If NOT Enabled AND DiverterDir = 1 Then
		DiverterOff.IsDropped=0
		DiverterOn.IsDropped=1
		DiverterDir = -1
		Diverter.Interval = 5:Diverter.Enabled = 1
		PlaySoundAt SoundFX("DiverterOff",DOFContactors),DiverterP
    End If
End Sub

Sub Diverter_Timer()

	'debug.print SystemTime & ": Diverter_Timer: " & DiverterP.RotZ & " : " & DiverterDir
	DiverterP.RotZ=DiverterP.RotZ+DiverterDir
	If DiverterP.RotZ<-2 AND DiverterDir=-1 Then Me.Enabled=0:DiverterP.RotZ=-2
	If DiverterP.RotZ>30 AND DiverterDir=1 Then Me.Enabled=0:DiverterP.RotZ=30

	DiverterPOff.RotZ = DiverterP.RotZ
End Sub

'*********** Idol Motorized Toy
Dim IdolPos, Ipos, IW

Sub InitIdol
	Controller.Switch(121)=1:Controller.Switch(122)=0:Controller.Switch(123)=0
End Sub

Sub SolMoveIdol(enabled)
	If Enabled Then
		UpdateIdol.enabled = True
		DOF 102, DOFOn
		PlaySoundAt SoundFX("IdolMotor",DOFGear),IdolEnter
	Else
		UpdateIdol.enabled = False
		ResetIdol.enabled = True
	End If
End Sub

Sub UpdateIdol_timer
	ResetIdol.Enabled = False
	IdolPos=(IdolPos+1)Mod 360
	totem.rotz= -IdolPos:totem1.rotz = totem.rotz
	'OFF primitives
	totemOFF.rotz=totem.rotz:totem1OFF.rotz=totem.rotz
	Select Case IdolPos											'91		'92		'93
		Case 0:Controller.Switch(122)=0:IPos=0			'Pos 1	  1      0       0
		Case 60:Controller.Switch(123)=1:IPos=1			'Pos 2	  1      0       1    
		Case 120:Controller.Switch(121)=0:IPos=2		'Pos 3    0      0       1
		Case 180:Controller.Switch(122)=1:IPos=3		'Pos 4    0      1       1
		Case 240:Controller.Switch(123)=0:IPos=4		'Pos 5    0      1       0    
		Case 300:Controller.Switch(121)=1:IPos=5		'Pos 6    1      1       0
	End Select
End Sub

Sub ResetIdol_timer
	If totem.rotz< -60 * Ipos Then totem.rotz = totem.rotz + 1:totem1.rotz = totem.rotz:totemOFF.rotz = totem.rotz:totem1OFF.rotz = totem.rotz
	If totem.rotz = -60 * Ipos Then Me.Enabled=0:IdolPos=-totem.rotz:StopSound "IdolMotor":DOF 102, DOFOff
End Sub

'*********** Idol Kickout
Sub SolIdol(Enabled)
	If Enabled Then
		IdolStop.IsDropped=1
		LockDoor1.Z=30
		LockDoor1OFF.Z=30
		LockDoor2.Z=-55
		LockDoor3.Z=-55
		PlaysoundAt SoundFX("IdolReleaseOn",DOFContactors),IdolExit
		bsIdol.ExitSol_On
	Else
		IdolStop.IsDropped=0
		LockDoor1.Z=85
		LockDoor1OFF.Z=85
		LockDoor2.Z=0
		LockDoor3.Z=0
		PlaysoundAt SoundFX("IdolReleaseOff",DOFContactors),IdolExit
	End If
End Sub

'*********** Path Of Adventure
Dim movePoA,PoAPos,PoADropTrack,MyBall,BallspeedPath

Sub InitPoA
	PoADropTrack=0:PoaPos=0:BallspeedPath=0:TopPost.Z=0
	Controller.Switch (124) = 0 : Controller.Switch (125) = 0
End Sub

Sub PoaMoveLeft(enabled)
	If enabled then
		movePoA=0
	Else
		ResetPoA.Enabled = 1
	End If
End Sub

Sub PoAMoveRight(enabled)
	If Enabled Then
		movePoA=0
	Else
		ResetPoA.Enabled = 1
	End If
End Sub

Sub ResetPoA_timer
	Dim ii
	movePoA=movePoA+1
	If NOT Controller.Switch (124) AND NOT Controller.Switch (125) AND movePoA > 50 AND NOT PoAPos=0 Then
		PoAPos = 0: Me.Enabled=0
		minipf.roty=PoAPos:minipf1.roty=PoAPos:minipf2.roty=PoAPos:minipf3.roty=PoAPos:minipf4.roty=PoAPos:minipf5.roty=PoAPos:minipf_screws.roty=-PoAPos
		'OFF primitives
		minipfOFF.roty=PoAPos:minipf1OFF.roty=PoAPos:minipf2OFF.roty=PoAPos:minipf3OFF.roty=PoAPos:minipf4OFF.roty=PoAPos:minipf5OFF.roty=PoAPos:minipf_screwsOFF.roty=-PoAPos
		'li71.rotY=PoAPos:li72.rotY=PoAPos:li73.rotY=PoAPos:li74.rotY=PoAPos:li75.rotY=PoAPos
		'li81.rotY=PoAPos:li82.rotY=PoAPos:li83.rotY=PoAPos:li85.rotY=PoAPos
		'li84on.rotZ=-PoAPos:li84off.rotZ=-PoAPos
		'li75.rotY=PoAPos
		li71on.rotZ=-PoAPos:li71off.rotZ=-PoAPos:li72on.rotZ=-PoAPos:li72off.rotZ=-PoAPos:li73on.rotZ=-PoAPos:li73off.rotZ=-PoAPos:li74on.rotZ=-PoAPos:li74off.rotZ=-PoAPos:li75on.rotZ=PoAPos::li75off.rotZ=PoAPos
		li81on.rotZ=-PoAPos:li81off.rotZ=-PoAPos:li82on.rotZ=-PoAPos:li82off.rotZ=-PoAPos:li83on.rotZ=-PoAPos:li83off.rotZ=-PoAPos:li84on.rotZ=-PoAPos:li84off.rotZ=-PoAPos:li85on.rotZ=PoAPos::li85off.rotZ=PoAPos
		sw65p.roty=PoAPos:sw66p.rotY=PoAPos:sw67p.rotY=PoAPos:sw68p.rotY=PoAPos
		sw75p.roty=PoAPos:sw76p.rotY=PoAPos:sw77p.rotY=PoAPos:sw78p.rotY=PoAPos
		For each ii in GIPOA:ii.rotY=PoAPos:Next
	End If
End Sub

Sub UpdatePOA(oldPos,newPos,aspeed)
	Dim ii
	PoAPos=2*(POAMech.Position-4)
	minipf.roty=PoAPos:minipf1.roty=PoAPos:minipf2.roty=PoAPos:minipf3.roty=PoAPos:minipf4.roty=PoAPos:minipf5.roty=PoAPos:minipf_screws.roty=PoAPos
	'OFF primitives
	minipfOFF.roty=PoAPos:minipf1OFF.roty=PoAPos:minipf2OFF.roty=PoAPos:minipf3OFF.roty=PoAPos:minipf4OFF.roty=PoAPos:minipf5OFF.roty=PoAPos:minipf_screwsOFF.roty=PoAPos
	'POASh.transX=PoaPos
	'li71.rotY=PoAPos:li72.rotY=PoAPos:li73.rotY=PoAPos:li74.rotY=PoAPos:li75.rotY=PoAPos
	'li81.rotY=PoAPos:li82.rotY=PoAPos:li83.rotY=PoAPos:li85.rotY=PoAPos
	'li84on.rotZ=-PoAPos:li84off.rotZ=-PoAPos
	'li75.rotY=PoAPos
	li71on.rotZ=-PoAPos:li71off.rotZ=-PoAPos:li72on.rotZ=-PoAPos:li72off.rotZ=-PoAPos:li73on.rotZ=-PoAPos:li73off.rotZ=-PoAPos:li74on.rotZ=-PoAPos:li74off.rotZ=-PoAPos:li75on.rotZ=PoAPos::li75off.rotZ=PoAPos
	li81on.rotZ=-PoAPos:li81off.rotZ=-PoAPos:li82on.rotZ=-PoAPos:li82off.rotZ=-PoAPos:li83on.rotZ=-PoAPos:li83off.rotZ=-PoAPos:li84on.rotZ=-PoAPos:li84off.rotZ=-PoAPos:li85on.rotZ=PoAPos::li85off.rotZ=PoAPos
	sw65p.roty=PoAPos:sw66p.rotY=PoAPos:sw67p.rotY=PoAPos:sw68p.rotY=PoAPos
	sw75p.roty=PoAPos:sw76p.rotY=PoAPos:sw77p.rotY=PoAPos:sw78p.rotY=PoAPos
	For each ii in GIPOA:ii.rotY=PoAPos:Next
End Sub

Sub EnterPoA_hit:Me.TimerInterval=10:Me.TimerEnabled=1:Playsound "Ball_Bounce_Playfield_Soft_1",0,1,-.2,0,0,1,0,-.8:POABallShadow.visible=1:End Sub
Sub EnterPoA_timer
	If NOT IsEmpty (myball) Then
		myball.velx = myball.velx + 0.5 * Sgn(PoAPos)
		If myball.VelY<0 Then myball.VelY=1
		POABallShadow.X = (myball.X - (Ballsize/6) + ((myball.X - (Table1.Width/2))/7)) + 10
		POABallShadow.Y = myball.Y + 20
		POABallShadow.Z = 156
	End If
End Sub

Sub ExitPoA_hit:myball=empty:EnterPOA.TimerEnabled=0:Me.TimerInterval=100:Me.TimerEnabled=1:End Sub
Sub ExitPoA_timer:Me.TimerEnabled=0:Playsound "fx_ramp_metal",0,1,-.2,0,0,1,0,-.8:POABallShadow.visible=0:POABallShadow.X=182:POABallShadow.Y=130:End Sub

Sub ExitBridge_hit:myball=empty:EnterPOA.TimerEnabled=0:PlaysoundAt "fx_ramp_turn",ExitBridge:End Sub

'*********** Top Post
 Sub SolTopPostPower(Enabled)
	If Enabled Then
		If POADropTrack=1 Then Enabled=0
		DropPoA.IsDropped=1
		sw46.Kick 270,5
		TopPost.Z=-30
		If POADropTrack=0 Then PlaysoundAt SoundFX("TopPostDown",DOFContactors),TopPost
	Else
		DropPoA.IsDropped=0
		TopPost.Z=-30
		If POADropTrack=0 Then DivHelp.TimerInterval=400:DivHelp.TimerEnabled=1
	End If
End Sub

Sub SolTopPostHold(Enabled)
	If Enabled Then 
		POADropTrack=1	
		DivHelp.IsDropped=1
		DropPoA.IsDropped=0
		TopPost.Z=-30:PlaysoundAt SoundFX("TopPostDown",DOFContactors),TopPost
	Else
		DivHelp.TimerInterval=400
		DivHelp.TimerEnabled=1
	End If
End Sub

Sub DivHelp_timer
	Me.TimerEnabled=0
	POADropTrack=0
	DivHelp.IsDropped=0
	TopPost.Z=0:PlaysoundAt SoundFX("TopPostUp",DOFContactors),TopPost
End Sub

Sub sw46_Hit()
	Set myball=ActiveBall
	BallspeedPath=myBall.VelX
	If POADropTrack = 0 Then StopSound "fx_ramp_enter3"
	If POADropTrack = 1 Then sw46.Kick 270,ABS(BallspeedPath)
	Controller.Switch(46)=1
End Sub

Sub sw46_UnHit():Controller.Switch(46) = 0:End Sub

'************************************************************************
'						Switches
'************************************************************************

Sub Drain_hit
	bsTrough.AddBall Me
	Dim BOT:BOT=GetBalls
	If Ubound(BOT)=0 Then LeftFlipper.RotateToStart:RightFlipper.RotateToStart
	RandomSoundDrain Drain
End Sub

'*********** Drop Targets
'Sub sw11_dropped:dtTotem.Hit 1:End Sub

Sub sw11a_Hit : DTHit 11 : End Sub
Sub sw115a_Hit : DTHit 115 : End Sub
Sub sw116a_Hit : DTHit 116 : End Sub
Sub sw117a_Hit : DTHit 117 : End Sub

'*********** Rollovers
Sub Sw15_Hit:Controller.Switch(15)=1: End Sub
Sub Sw15_UnHit:Controller.Switch(15)=0: End Sub
Sub Sw16_Hit:Controller.Switch(16)=1: End Sub
Sub Sw16_UnHit:Controller.Switch(16)=0: End Sub
Sub Sw17_Hit:Controller.Switch(17)=1: End Sub
Sub Sw17_UnHit:Controller.Switch(17)=0: End Sub
Sub Sw18_Hit:Controller.Switch(18)=1: End Sub
Sub Sw18_UnHit:Controller.Switch(18)=0: End Sub

Sub Sw25_Hit:Controller.Switch(25)=1: End Sub
Sub Sw25_UnHit:Controller.Switch(25)=0: End Sub
Sub Sw26_Hit:Controller.Switch(26)=1: End Sub
Sub Sw26_UnHit:Controller.Switch(26)=0: End Sub
Sub Sw27_Hit:Controller.Switch(27)=1: End Sub
Sub Sw27_UnHit:Controller.Switch(27)=0: End Sub
Sub Sw28_Hit:Controller.Switch(28)=1: End Sub
Sub Sw28_UnHit:Controller.Switch(28)=0: End Sub

Sub Sw32_Hit:Controller.Switch(32)=1:Activeball.VelY=1: End Sub
Sub Sw32_UnHit:Controller.Switch(32)=0: End Sub

Sub Sw54_Hit:Controller.Switch(54)=1: End Sub
Sub Sw54_UnHit:Controller.Switch(54)=0: End Sub
Sub Sw55_Hit:Controller.Switch(55)=1: End Sub
Sub Sw55_UnHit:Controller.Switch(55)=0: End Sub
Sub Sw56_Hit:Controller.Switch(56)=1: End Sub
Sub Sw56_UnHit:Controller.Switch(56)=0: End Sub
Sub Sw57_Hit:Controller.Switch(57)=1: End Sub
Sub Sw57_UnHit:Controller.Switch(57)=0: End Sub
Sub Sw58_Hit:Controller.Switch(58)=1: End Sub
Sub Sw58_UnHit:Controller.Switch(58)=0: End Sub

Sub Sw88_Hit:Controller.Switch(88)=1: End Sub
Sub Sw88_UnHit:Controller.Switch(88)=0: End Sub

'*********** Slingshots
Dim LStep, RStep

Sub LeftSlingShot_Slingshot()
    LSling1.Visible = 1
    Lemk.TransZ = -20
    LStep = 0
    vpmTimer.PulseSw 33
	RandomSoundSlingshotLeft Lemk
    Me.TimerInterval = 20:Me.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer()
    Select Case LStep
        Case 1:LSLing1.Visible = 0:LSLing2.Visible = 1:Lemk.TransZ = -10
        Case 2:LSLing2.Visible = 0:Lemk.TransZ = 0:Me.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot()
    RSling1.Visible = 1
    Remk.TransZ = -20
    RStep = 0
    vpmTimer.PulseSw 48
	RandomSoundSlingshotRight Remk
    Me.TimerInterval = 20:Me.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer()
    Select Case RStep
        Case 1:RSLing1.Visible = 0:RSLing2.Visible = 1:Remk.TransZ = -10
        Case 2:RSLing2.Visible = 0:Remk.TransZ = 0:Me.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'*********** Bumpers
Sub Bumper1_Hit():vpmTimer.PulseSw 35:RandomSoundBumperTop(Bumper1):End Sub
Sub Bumper2_Hit():vpmTimer.PulseSw 36:RandomSoundBumperMiddle(Bumper2):End Sub
Sub Bumper3_Hit():vpmTimer.PulseSw 37:RandomSoundBumperBottom(Bumper3):End Sub

'*********** Center Standup Target
Sub Sw38_Hit():vpmTimer.PulseSw 38: End Sub

'*********** Left Ramp
Sub Sw41_Hit():vpmtimer.pulseSw 41:End Sub 
Sub Sw118_Hit()
vpmtimer.pulseSw 118:sw118p.Rotz=-50: Me.TimerEnabled=1
If PropellerMod=1 Then RotatePropeller
End Sub
Sub Sw118_timer:Me.TimerEnabled=0:sw118p.Rotz=-20:End Sub

'******	 Propeller MOD
Dim stepangle

Sub RotatePropeller()
	PlaysoundAt SoundFXDOF("fx_motor",101,DOFOn,DOFGear),Bumper3
	PropellerMove.Enabled = 0
	PropellerMove.Interval = 10
	PropellerMove.Enabled = 1
	stepAngle=10
End Sub

Sub PropellerMove_Timer()
	Propeller.roty = Propeller.roty + stepAngle
	If Propeller.roty >= 6*360 Then stepAngle = stepAngle - 0.2
	If stepAngle <= 0 Then Me.Enabled = 0 : Propeller.roty = Propeller.roty -6*360 : StopSound "fx_motor" : DOF 101, DOFOff
End Sub

'*********** Right Ramp
Sub Sw42_Hit():vpmtimer.pulseSw 42:End Sub 
Sub Sw74_Hit():vpmtimer.pulseSw 74:sw74p.Rotz=-30:Me.TimerEnabled=1: End Sub
Sub Sw74_timer:Me.TimerEnabled=0:sw74p.Rotz=0:End Sub

'*********** Idol Enter
Sub sw43_hit():vpmtimer.pulseSw 43:End Sub

'*********** Subway Enter
'Sub sw45_hit():vpmtimer.pulseSw 45:SoundHole45:End Sub
Sub sw45_Hit:vpmtimer.pulseSw 45:PlaysoundAt "fx_plasticrolling",sw45:SoundHole45:End Sub

'*********** Captive Ball Target
Sub Sw64_Hit():vpmTimer.PulseSw 64: End Sub

'*********** Captive Ball Opto
Sub Sw71_Hit:Controller.Switch(71) = 1:End Sub 
Sub Sw71_UnHit:Controller.Switch(71) = 0:End Sub

'*********** Adventure Targets
Sub Sw51_Hit():vpmTimer.PulseSw 51: End Sub	'(U)
Sub Sw52_Hit():vpmTimer.PulseSw 52: End Sub	'(R)
Sub Sw53_Hit():vpmTimer.PulseSw 53: End Sub	'(E)

Sub Sw61_Hit():vpmTimer.PulseSw 61: End Sub	'(A)
Sub Sw62_Hit():vpmTimer.PulseSw 62: End Sub	'(D)
Sub Sw63_Hit():vpmTimer.PulseSw 63: End Sub	'(V)

'*********** Path of Adventure
Sub Sw65_Hit:vpmTimer.PulseSw 65:sw65p.rotx=-20:Me.TimerEnabled=1:End Sub
Sub Sw65_timer:Me.TimerEnabled=0:sw65p.rotx=0:End Sub
Sub Sw66_Hit:vpmTimer.PulseSw 66:sw66p.rotx=-20:Me.TimerEnabled=1:End Sub
Sub Sw66_timer:Me.TimerEnabled=0:sw66p.rotx=0:End Sub
Sub Sw67_Hit:vpmTimer.PulseSw 67:sw67p.rotx=-20:Me.TimerEnabled=1:End Sub
Sub Sw67_timer:Me.TimerEnabled=0:sw67p.rotx=0:End Sub
Sub Sw68_Hit:vpmTimer.PulseSw 68:sw68p.rotx=-20:Me.TimerEnabled=1:End Sub
Sub Sw68_timer:Me.TimerEnabled=0:sw68p.rotx=0:End Sub

Sub sw72_hit():vpmtimer.pulseSw 72:Me.TimerInterval=200:Me.TimerEnabled=1:End Sub
Sub sw72_timer():Me.TimerEnabled=0:SoundHole72:myball=empty:EnterPOA.TimerEnabled=0:End Sub
Sub sw73_hit():vpmtimer.pulseSw 73:Me.TimerInterval=200:Me.TimerEnabled=1:End Sub
Sub sw73_timer():Me.TimerEnabled=0:SoundHole73:myball=empty:EnterPOA.TimerEnabled=0:End Sub

Sub Sw75_Hit:vpmTimer.PulseSw 75:sw75p.rotx=-20:Me.TimerEnabled=1:End Sub
Sub Sw75_timer:Me.TimerEnabled=0:sw75p.rotx=0:End Sub
Sub Sw76_Hit:vpmTimer.PulseSw 76:sw76p.rotx=-20:Me.TimerEnabled=1:End Sub
Sub Sw76_timer:Me.TimerEnabled=0:sw76p.rotx=0:End Sub
Sub Sw77_Hit:vpmTimer.PulseSw 77:sw77p.rotx=-20:Me.TimerEnabled=1:End Sub
Sub Sw77_timer:Me.TimerEnabled=0:sw77p.rotx=0:End Sub
Sub Sw78_Hit:vpmTimer.PulseSw 78:sw78p.rotx=-20:Me.TimerEnabled=1:End Sub
Sub Sw78_timer:Me.TimerEnabled=0:sw78p.rotx=0:End Sub


'***************************************
'***Begin nFozzy lamp handling***
'***************************************

Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
Dim ModLampz : Set ModLampz = New DynamicLamps
InitLampsNF              ' Setup lamp assignments
'LampTimer.Interval = -1
'LampTimer.Enabled = 1

Sub LampTimer()
	dim x, chglamp
	chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		next
	End If
	Lampz.Update1	'update (fading logic only)
	ModLampz.Update1
	if LeftFlipper.Y > 1824 Then 
		LeftFlipper.Y = LeftFlipper.Y - 3
	Else
		LeftFlipper.Y = 1821
	end If
	if RightFlipper.Y > 1824 Then 
		RightFlipper.Y = RightFlipper.Y - 3
	Else
		RightFlipper.Y = 1821
	end If
End Sub

dim FrameTime, InitFrameTime : InitFrameTime = 0
Wall97.TimerInterval = -1
Wall97.TimerEnabled = True
Sub Wall97_Timer()	'Stealing this random wall's timer for -1 updates
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime. Unused atm?
	Lampz.Update 'updates on frametime (Object updates only)
	ModLampz.Update
End Sub

Function FlashLevelToIndex(Input, MaxSize)
	FlashLevelToIndex = cInt(MaxSize * Input)
End Function

'Material swap arrays.
Dim TextureArray1: TextureArray1 = Array("Plastic with an image trans", "Plastic with an image trans","Plastic with an image trans","Plastic with an image")


Dim DLintensity
'***************************************
'***Prim Image Swaps***
'***************************************
Sub ImageSwap(pri, group, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 3)
		Case 1:pri.Image = group(0) 'Full
		Case 2:pri.Image = group(1) 'Fading...
		Case 3:pri.Image = group(2) 'Fading...
        Case 4:pri.Image = group(3) 'Off
    End Select
pri.blenddisablelighting = aLvl * DLintensity
End Sub


'***************************************
'***Prim Material Swaps***
'***************************************
Sub MatSwap(pri, group, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 3)
		Case 1:pri.Material = group(0) 'Full
		Case 2:pri.Material = group(1) 'Fading...
		Case 3:pri.Material = group(2) 'Fading...
              Case 4:pri.Material = group(3) 'Off
    End Select
pri.blenddisablelighting = aLvl * DLintensity
End Sub


Sub FadeMaterialToys(pri, group, ByVal aLvl)	'cp's script
'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
        Case 3:pri.Material = group(3) 'Full
    End Select
	'if tb.text <> pri.image then tb.text = pri.image : debug.print pri.image end If	'debug
pri.blenddisablelighting = aLvl * 1 'Intensity Adjustment
End Sub

Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity * 0.4
End Sub

Sub DisableLightingBulb(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl^0.6 * DLintensity * 0.4
End Sub

Sub InitLampsNF()
	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating
	ModLampz.Filter = "LampFilter"

	'Adjust fading speeds (1 / full MS fading time)
	dim x
	for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/5 : Lampz.FadeSpeedDown(x) = 1/20 : next
	for x = 0 to 5 : ModLampz.FadeSpeedUp(x) = 1/5 : ModLampz.FadeSpeedDown(x) = 1/25 : Next
	for x = 6 to 28 : ModLampz.FadeSpeedUp(x) = 1/5 : ModLampz.FadeSpeedDown(x) = 1/35 : Next

	'Lamp Assignments
	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays

	Lampz.MassAssign(11)= Light11
	Lampz.MassAssign(11)= Light11a
	Lampz.Callback(11) = " DisableLighting p11, 50,"
	Lampz.Callback(11) = "DisableLightingBulb p11bulb, 50,"
	Lampz.MassAssign(12)= Light12
	Lampz.MassAssign(12)= Light12a
	Lampz.Callback(12) = " DisableLighting p12, 50,"
	Lampz.Callback(12) = "DisableLightingBulb p12bulb, 50,"
	Lampz.MassAssign(13)= Light13
	Lampz.MassAssign(13)= Light13a
	Lampz.Callback(13) = " DisableLighting p13, 50,"
	Lampz.Callback(13) = "DisableLightingBulb p13bulb, 50,"
	Lampz.MassAssign(14)= Light14
	Lampz.MassAssign(14)= Light14a
	Lampz.MassAssign(14)= Light14b
	Lampz.Callback(14) = " DisableLighting p14, 60,"
	Lampz.Callback(14) = "DisableLightingBulb p14bulb, 40,"
	Lampz.MassAssign(15)= Light15
	Lampz.MassAssign(15)= Light15a
	Lampz.MassAssign(15)= Light15b
	Lampz.Callback(15) = " DisableLighting p15, 60,"
	Lampz.Callback(15) = "DisableLightingBulb p15bulb, 40,"
	Lampz.MassAssign(16)= Light16
	Lampz.MassAssign(16)= Light16a
	Lampz.MassAssign(15)= Light16b
	Lampz.Callback(16) = " DisableLighting p16, 60,"
	Lampz.Callback(16) = "DisableLightingBulb p16bulb, 40,"
	Lampz.MassAssign(17)= Light17
	Lampz.MassAssign(17)= Light17a
	Lampz.Callback(17) = " DisableLighting p17, 50,"
	Lampz.Callback(17) = "DisableLightingBulb p17bulb, 50,"
	Lampz.MassAssign(18)= Light18
	Lampz.MassAssign(18)= Light18a
	Lampz.MassAssign(18)= Light18b
	Lampz.Callback(18) = " DisableLighting p18, 10,"
	Lampz.MassAssign(21)= Light21
	Lampz.MassAssign(21)= Light21a
	Lampz.MassAssign(21)= Light21b
	Lampz.Callback(21) = " DisableLighting p21, 10,"
	Lampz.MassAssign(22)= Light22
	Lampz.MassAssign(22)= Light22a
	Lampz.MassAssign(22)= Light22b
	Lampz.MassAssign(22)= Light22f
	Lampz.Callback(22) = " DisableLighting p22, 60,"
	Lampz.Callback(22) = "DisableLightingBulb p22bulb, 40,"
	Lampz.MassAssign(23)= Light23
	Lampz.MassAssign(23)= Light23a
	Lampz.MassAssign(23)= Light23b
	Lampz.MassAssign(23)= Light23f
	Lampz.Callback(23) = " DisableLighting p23, 60,"
	Lampz.Callback(23) = "DisableLightingBulb p23bulb, 40,"
	Lampz.MassAssign(24)= Light24
	Lampz.MassAssign(24)= Light24a
	Lampz.MassAssign(24)= Light24b
	Lampz.MassAssign(24)= Light24f
	Lampz.Callback(24) = " DisableLighting p24, 60,"
	Lampz.Callback(24) = "DisableLightingBulb p24bulb, 40,"
	Lampz.MassAssign(25)= Light25
	Lampz.MassAssign(25)= Light25a
	Lampz.MassAssign(25)= Light25b
	Lampz.Callback(25) = " DisableLighting p25, 10,"
	Lampz.MassAssign(26)= Light26
	Lampz.MassAssign(26)= Light26a
	Lampz.MassAssign(26)= Light26b
	If VRRoom > 0 and VRFlashingBackglass = 1 Then
		Lampz.MassAssign(26)= BGFLGrail
	End If
	Lampz.Callback(26) = " DisableLighting p26, 40,"
	Lampz.MassAssign(27)= Light27
	Lampz.MassAssign(27)= Light27a
	Lampz.MassAssign(27)= Light27b
	Lampz.Callback(27) = " DisableLighting p27, 10,"
	Lampz.MassAssign(28)= Light28
	Lampz.MassAssign(28)= Light28a
	Lampz.MassAssign(28)= Light28b
	If VRRoom > 0 and VRFlashingBackglass = 1 Then
		Lampz.MassAssign(28)= BGFLStones
	End If
	Lampz.Callback(28) = " DisableLighting p28, 40,"
	Lampz.MassAssign(31)= Light31
	Lampz.MassAssign(31)= Light31a
	Lampz.Callback(31) = " DisableLighting p31, 40,"
	Lampz.Callback(31) = " DisableLightingBulb p31bulb, 25,"
	Lampz.MassAssign(32)= Light32
	Lampz.MassAssign(32)= Light32a
	Lampz.MassAssign(32)= Light32b
	Lampz.Callback(32) = " DisableLighting p32, 10,"
	Lampz.MassAssign(33)= Light33
	Lampz.MassAssign(33)= Light33a
	Lampz.Callback(33) = "DisableLighting p33, 60,"
	Lampz.Callback(33) = "DisableLightingBulb p33bulb, 40,"
	Lampz.MassAssign(34)= Light34
	Lampz.MassAssign(34)= Light34a
	Lampz.MassAssign(34)= Light34b
	Lampz.Callback(34) = " DisableLighting p34, 10,"
	Lampz.MassAssign(35)= Light35
	Lampz.MassAssign(35)= Light35a
	Lampz.Callback(35) = "DisableLighting p35, 60,"
	Lampz.Callback(35) = "DisableLightingBulb p35bulb, 40,"
	Lampz.MassAssign(36)= Light36
	Lampz.MassAssign(36)= Light36a
	Lampz.MassAssign(36)= Light36b
	Lampz.Callback(36) = " DisableLighting p36, 20,"
	Lampz.MassAssign(37)= Light37
	Lampz.MassAssign(37)= Light37a
	Lampz.Callback(37) = " DisableLighting p37, 60,"
	Lampz.Callback(37) = " DisableLightingBulb p37bulb, 45,"
	Lampz.MassAssign(38)= Light38
	Lampz.MassAssign(38)= Light38a
	Lampz.Callback(38) = "DisableLighting p38, 60,"
	Lampz.Callback(38) = "DisableLightingBulb p38bulb, 40,"
	Lampz.MassAssign(41)= Light41
	Lampz.MassAssign(41)= Light41a
	Lampz.MassAssign(41)= Light41b
	Lampz.Callback(41) = " DisableLighting p41, 10,"
	Lampz.MassAssign(42)= Light42
	Lampz.MassAssign(42)= Light42a
	Lampz.MassAssign(42)= Light42b
	If VRRoom > 0 and VRFlashingBackglass = 1 Then
		Lampz.MassAssign(42)= BGFLArk
	End If
	Lampz.Callback(42) = " DisableLighting p42, 10,"
	Lampz.MassAssign(43)= Light43
	Lampz.MassAssign(43)= Light43a
	Lampz.MassAssign(43)= Light43b
	Lampz.Callback(43) = " DisableLighting p43, 10,"
	Lampz.MassAssign(44)= Light44
	Lampz.MassAssign(44)= Light44a
	Lampz.Callback(44) = " DisableLighting p44, 60,"
	Lampz.Callback(44) = "DisableLightingBulb p44bulb, 40,"
	Lampz.MassAssign(45)= Light45
	Lampz.MassAssign(45)= Light45a
	Lampz.Callback(45) = " DisableLighting p45, 60,"
	Lampz.Callback(45) = " DisableLightingBulb p45bulb, 45,"
	Lampz.MassAssign(46)= Light46
	Lampz.MassAssign(46)= Light46a
	Lampz.Callback(46) = " DisableLighting p46, 60,"
	Lampz.Callback(46) = "DisableLightingBulb p46bulb, 40,"
	Lampz.MassAssign(47)= Light47
	Lampz.MassAssign(47)= Light47a
	Lampz.MassAssign(47)= Light47b
	Lampz.Callback(47) = " DisableLighting p47, 10,"
	Lampz.MassAssign(48)= Light48
	Lampz.MassAssign(48)= Light48a
	Lampz.Callback(48) = " DisableLighting p48, 80,"
	Lampz.Callback(48) = " DisableLightingBulb p48bulb, 35,"
	Lampz.MassAssign(51)= Light51
	Lampz.MassAssign(51)= Light51a
	Lampz.Callback(51) = " DisableLighting p51, 30,"
	Lampz.MassAssign(52)= Light52
	Lampz.MassAssign(52)= Light52a
	Lampz.Callback(52) = " DisableLighting p52, 60,"
	Lampz.Callback(52) = "DisableLightingBulb p52bulb, 40,"
	Lampz.MassAssign(53)= Light53
	Lampz.MassAssign(53)= Light53a
	Lampz.MassAssign(53)= Light53b
	Lampz.Callback(53) = " DisableLighting p53, 10,"
	Lampz.MassAssign(54)= Light54
	Lampz.MassAssign(54)= Light54a
	Lampz.MassAssign(54)= Light54b
	Lampz.Callback(54) = " DisableLighting p54, 60,"
	Lampz.Callback(54) = "DisableLightingBulb p54bulb, 40,"
	Lampz.MassAssign(55)= Light55
	Lampz.MassAssign(55)= Light55a
	Lampz.MassAssign(55)= Light55b
	Lampz.Callback(55) = " DisableLighting p55, 60,"
	Lampz.Callback(55) = "DisableLightingBulb p55bulb, 40,"
	Lampz.MassAssign(56)= Light56
	Lampz.MassAssign(56)= Light56a
	Lampz.MassAssign(56)= Light56b
	Lampz.Callback(56) = " DisableLighting p56, 60,"
	Lampz.Callback(56) = "DisableLightingBulb p56bulb, 40,"
	Lampz.MassAssign(57)= Light57
	Lampz.MassAssign(57)= Light57a
	Lampz.MassAssign(57)= Light57b
	Lampz.Callback(57) = " DisableLighting p57, 10,"
	Lampz.MassAssign(58)= Light58
	Lampz.MassAssign(58)= Light58a
	Lampz.Callback(58) = " DisableLighting p58, 120,"
	Lampz.Callback(58) = " DisableLightingBulb p58bulb, 35,"
	Lampz.MassAssign(61)= Light61
	Lampz.MassAssign(61)= Light61a
	Lampz.MassAssign(61)= Light61b
	Lampz.MassAssign(61)= l61r
	Lampz.MassAssign(61)= l61r2
	Lampz.Callback(61) = " DisableLighting p61, 60,"
	Lampz.Callback(61) = "DisableLightingBulb p61bulb, 10,"
	Lampz.MassAssign(62)= Light62
	Lampz.MassAssign(62)= Light62a
	Lampz.MassAssign(62)= Light62b
	Lampz.MassAssign(62)= l62r
	Lampz.MassAssign(62)= l62r2
	Lampz.Callback(62) = " DisableLighting p62, 60,"
	Lampz.Callback(62) = "DisableLightingBulb p62bulb, 40,"
	Lampz.MassAssign(63)= Light63
	Lampz.MassAssign(63)= Light63a
	Lampz.MassAssign(63)= Light63b
	Lampz.MassAssign(63)= l63r
	Lampz.MassAssign(63)= l63r2
	Lampz.Callback(63) = " DisableLighting p63, 60,"
	Lampz.Callback(63) = "DisableLightingBulb p63bulb, 40,"
	Lampz.MassAssign(64)= Light64
	Lampz.MassAssign(64)= Light64a
	Lampz.MassAssign(64)= Light64b
	Lampz.MassAssign(64)= l64r
	Lampz.MassAssign(64)= l64r2
	Lampz.Callback(64) = " DisableLighting p64, 60,"
	Lampz.Callback(64) = "DisableLightingBulb p64bulb, 40,"
	Lampz.MassAssign(65)= Light65
	Lampz.MassAssign(65)= Light65a
	Lampz.MassAssign(65)= Light65b
	Lampz.Callback(65) = " DisableLighting p65, 20,"
	Lampz.MassAssign(66)= Light66
	Lampz.MassAssign(66)= Light66a
	Lampz.Callback(66) = " DisableLighting p66, 60,"
	Lampz.Callback(66) = " DisableLightingBulb p66bulb, 45,"
	Lampz.MassAssign(67)= Light67
	Lampz.MassAssign(67)= Light67a
	Lampz.MassAssign(67)= Light67b
	Lampz.Callback(67) = " DisableLighting p67, 20,"
	Lampz.MassAssign(68)= Light68
	Lampz.MassAssign(68)= Light68a
	Lampz.Callback(68) = " DisableLighting p68, 40,"
	Lampz.Callback(68) = " DisableLightingBulb p68bulb, 25,"
	Lampz.MassAssign(76)= Light76
	Lampz.MassAssign(76)= Light76a
	Lampz.MassAssign(76)= Light76b
	Lampz.Callback(76) = " DisableLighting p76, 20,"
	Lampz.MassAssign(77)= Light77
	Lampz.MassAssign(77)= Light77a
	Lampz.Callback(77) = " DisableLighting p77, 60,"
	Lampz.Callback(77) = " DisableLightingBulb p77bulb, 45,"
	Lampz.MassAssign(78)= Light78
	Lampz.MassAssign(78)= Light78a
	Lampz.MassAssign(78)= Light78b
	Lampz.Callback(78) = " DisableLighting p78, 20,"
	Lampz.MassAssign(86)= Light86
	Lampz.MassAssign(86)= Light86a
	Lampz.Callback(86) = " DisableLighting p86, 40,"
	Lampz.Callback(86) = " DisableLightingBulb p86bulb, 25,"
	Lampz.MassAssign(87)= Light87
	Lampz.MassAssign(87)= Light87a
	Lampz.Callback(87) = " DisableLighting p87, 80,"
	Lampz.Callback(87) = " DisableLightingBulb p87bulb, 45,"
	Lampz.MassAssign(115)= f54
	Lampz.MassAssign(115)= f54a
	Lampz.Callback(115) = " DisableLighting pf54, 40,"
	Lampz.Callback(115) = " DisableLightingBulb pf54bulb, 25,"
	Lampz.MassAssign(116)= L153L
	Lampz.MassAssign(116)= L153La
	Lampz.Callback(116) = " DisableLighting p153l, 60,"
	Lampz.Callback(116) = " DisableLightingBulb p153lBulb, 40,"
	Lampz.MassAssign(116)= L153R
	Lampz.MassAssign(116)= L153Ra
	Lampz.Callback(116) = " DisableLighting p153r, 60,"
	Lampz.Callback(116) = " DisableLightingBulb p153rBulb, 40,"
	Lampz.MassAssign(117)= Light117
	Lampz.MassAssign(117)= Light117a
	Lampz.Callback(117) = " DisableLighting p117, 70,"
	Lampz.Callback(117) = " DisableLightingBulb p117bulb, 45,"
	Lampz.MassAssign(119)= FL_SJackpot
	Lampz.MassAssign(119)= FL_SJackpota
	Lampz.MassAssign(119)= FL_SJackpotb
	Lampz.Callback(119) = " DisableLighting pSJackpotOn, 40,"
	Lampz.Callback(119) = " DisableLightingBulb pSJackpotBulb, 20,"
	Lampz.MassAssign(125)= l25
	Lampz.MassAssign(125)= l25a
	Lampz.Callback(125) = " DisableLighting p125, 60,"
	Lampz.Callback(125) = " DisableLightingBulb p125bulb, 45,"

	ModLampz.MassAssign(20)= FL_Jackpot
	ModLampz.MassAssign(21)= FL_POA
	ModLampz.MassAssign(24)= FL_PlaneGunsA
	ModLampz.MassAssign(24)= FL_PlaneGunsB
	If VRRoom > 0 and VRFlashingBackglass = 1 Then
		ModLampz.MassAssign(20)= BGFL20_1
		ModLampz.MassAssign(20)= BGFL20_2
		ModLampz.MassAssign(20)= BGFL20_3
		ModLampz.MassAssign(20)= BGFL20_4
		ModLampz.MassAssign(20)= BGFL20_5
		ModLampz.MassAssign(20)= BGFL20_6
		ModLampz.MassAssign(20)= BGFL20_7
		ModLampz.MassAssign(21)= BGFL21IN_1
		ModLampz.MassAssign(21)= BGFL21IN_2
		ModLampz.MassAssign(21)= BGFL21IN_3
		ModLampz.MassAssign(21)= BGFL21DI_1
		ModLampz.MassAssign(21)= BGFL21DI_2
		ModLampz.MassAssign(21)= BGFL21DI_3
		ModLampz.MassAssign(21)= BGFL21JO_1
		ModLampz.MassAssign(21)= BGFL21JO_2
		ModLampz.MassAssign(21)= BGFL21JO_3
		ModLampz.MassAssign(21)= BGFL21ES_1
		ModLampz.MassAssign(21)= BGFL21ES_2
		ModLampz.MassAssign(21)= BGFL21ES_3
	End If

'Path of Adventure inserts
	Lampz.Callback(71) = " DisableLighting Li71on, 600,"
	Lampz.Callback(72) = " DisableLighting Li72on, 600,"
	Lampz.Callback(73) = " DisableLighting Li73on, 600,"
	Lampz.Callback(74) = " DisableLighting Li74on, 600,"
	Lampz.Callback(75) = " DisableLighting Li75on, 600,"
	Lampz.Callback(81) = " DisableLighting Li81on, 600,"
	Lampz.Callback(82) = " DisableLighting Li82on, 600,"
	Lampz.Callback(83) = " DisableLighting Li83on, 600,"
	Lampz.Callback(84) = " DisableLighting Li84on, 600,"
	Lampz.Callback(85) = " DisableLighting Li85on, 600,"


'**************************************************
'			GI assignments
'**************************************************

	ModLampz.Callback(0) = "GIUpdates"
	ModLampz.Callback(1) = "GIUpdates"
	'ModLampz.Callback(2) = "GIUpdates"
	'ModLampz.Callback(3) = "GIUpdates"
	ModLampz.Callback(4) = "GIupdates"

	ModLampz.MassAssign(0)= ColToArray(GiTop)                  '2 GI Top PF
	ModLampz.MassAssign(0)= ColToArray(GiBumpers)                  '2 GI Top PF
	'ModLampz.MassAssign(0)= ColToArray(GiPOA)
	'ModLampz.MassAssign(0)= ColToArray(GITopSides)

	ModLampz.MassAssign(1)= ColToArray(GIBot)               '1 GI Bottom PF
	If VRRoom > 0 and VRFlashingBackglass = 1 Then
		ModLampz.MassAssign(1)= ColToArray(VRBGGI)
	End If
	'ModLampz.MassAssign(1)= ColToArray(GIBotSides)
	'ModLampz.MassAssign(2)= ColToArray(GIInsertTop)            '3 GI Insert Top
	'ModLampz.MassAssign(3)= ColToArray(GIInsertBottom)         '4 GI Insert Bottom

	ModLampz.MassAssign(4)= ColToArray(GIRLaneCoin)            '5 GI Return Lane/Coin
	ModLampz.MassAssign(4)= LiteHOF_La
	ModLampz.MassAssign(4)= LiteHOF_Ra
	ModLampz.Callback(4) = " DisableLighting pLiteHOF_Lon, 30,"
	ModLampz.Callback(4) = " DisableLighting pLiteHOF_Lbulb, 30,"
	ModLampz.Callback(4) = " DisableLighting pLiteHOF_Ron, 30,"
	ModLampz.Callback(4) = " DisableLighting pLiteHOF_Rbulb, 30,"

	dim ii
	'set gi flasher visible
	For each ii in GIPOA:ii.visible=1:Next
	For each ii in GITopSides:ii.visible=1:Next
	For each ii in GIBotSides:ii.visible=1:Next

	'Turn off all lamps on startup
	lampz.Init	'This just turns state of any lamps to 1
	ModLampz.Init

	'Immediate update to turn on GI, turn off lamps
	lampz.update
	ModLampz.Update

End Sub

dim giprevalvl, kk, PFGIOFFOpacity
PFGIOFFOpacity = 90

'adjusting the toys. Do not touch these unless you set them properly in flasher code too
p_planes.blenddisablelighting = 0
p_planesOFF.blenddisablelighting = -0.1
p_blimp.blenddisablelighting = 0
p_blimpOFF.blenddisablelighting = -0.1
p_col_metalsposts.blenddisablelighting = 0.4
p_col_metalspostsOFF.blenddisablelighting = 0
minipf.blenddisablelighting = 0.1
minipfoff.blenddisablelighting = 0.1
minipf1.blenddisablelighting = 0.1
minipf1off.blenddisablelighting = 0.1
minipf2.blenddisablelighting = 0.1
minipf2off.blenddisablelighting = 0.1
minipf3.blenddisablelighting = 0.4
minipf3off.blenddisablelighting = 0.4
minipf4.blenddisablelighting = 0.4
minipf4off.blenddisablelighting = 0.4
minipf5.blenddisablelighting = 0.4
minipf5off.blenddisablelighting = 0.4
minipf_screws.blenddisablelighting = 0.4
minipf_screwsOFF.blenddisablelighting = 0.4
p_plastics.blenddisablelighting = 0.3
p_plasticsoff.blenddisablelighting = 0.3
Primitive9.blenddisablelighting = 0.3
Primitive9off.blenddisablelighting = 0.3
p_leftmetalramp.blenddisablelighting = 0.1
p_leftmetalrampOFF.blenddisablelighting = 0.1
p_sw38.blenddisablelighting = 0.3
p_sw38OFF.blenddisablelighting = 0.3

Sub GIupdates(ByVal aLvl)	'GI update odds and ends go here
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically

	dim gi0lvl,gi1lvl
	if Lampz.UseFunction then 	'Callbacks don't get this filter automatically
		gi0lvl = LampFilter(ModLampz.Lvl(0))
		gi1lvl = LampFilter(ModLampz.Lvl(1))
	Else
		gi0lvl = ModLampz.Lvl(0)
		gi1lvl = ModLampz.Lvl(1)
	end if


	'DOF
	if gi0lvl = 0 Then
		DOF 103, DOFOff
	else
		DOF 103, DOFOn
	end If

	if ObjLevel(1) <= 0 and ObjLevel(2) <= 0 and ObjLevel(3) <= 0 then 'And ObjLevel(2) <= 0 Then
		p_col_metalspostsOFF.blenddisablelighting = 0
		ruinsoff.blenddisablelighting = -0.1
		p_planesOFF.blenddisablelighting = -0.1
		p_blimpOFF.blenddisablelighting = -0.1
		p_col_wireRampsOFF.blenddisablelighting = 0

		'commenting this out for now, as it has issues with flashers
		if gi1lvl = 0 then										'GI OFF, let's hide ON prims
			OnPrimsVisible False
		Elseif gi1lvl = 1 then									'GI ON, let's hide OFF prims
			OffPrimsVisible False
		Else
			if giprevalvl = 0 Then								'GI has just changed from OFF to fading, let's show ON
				OnPrimsVisible True
			elseif giprevalvl = 1 Then							'GI has just changed from ON to fading, let's show OFF
				OffPrimsVisible true
			Else
				'no change
			end if
		end if

		UpdateMaterial "ToyPlasticON",0,0,0,0,0,0,gi1lvl^1.5,RGB(255,255,255),0,0,False,True,0,0,0,0 
		UpdateMaterial "MetalspostsON",0,0,0,0,0,0,gi1lvl^1,RGB(255,255,255),0,0,False,True,0,0,0,0
		UpdateMaterial "CabMaterialON",0,0,0,0,0,0,gi1lvl^2,RGB(255,255,255),0,0,False,True,0,0,0,0

		p_litejackpot.blenddisablelighting = 0.4 * gi1lvl
		p_litejackpotOFF.blenddisablelighting = 0.4 * gi1lvl

'Not needed as not having flashers kicking when GI is off
'	Elseif ObjLevel(1) > 0 Or ObjLevel(2) > 0 Or ObjLevel(3) > 0 then 
'		if gi1lvl = 0 Or gi1lvl = 1 then
'			'nothing, flashers just fading and no real change to gi
'		Elseif giprevalvl = 0 then 'gi went ON while some flasher was fading
'			debug.print "##on prims to on image"
'			OnPrimSwap "ON"
'		elseif giprevalvl = 1 Then 'gi went OFF while some flasher was fading
'			debug.print "##on prims to OFF images"
'			OnPrimSwap "OFF"
'		end if
		
	end If

	'PLAYFIELD_GI.IntensityScale = 2 * gi1lvl
	PLAYFIELD_GI.opacity = PFGIOFFOpacity - (PFGIOFFOpacity * gi1lvl)
	
	'modlampz.state(1) = 0
	p_plastics_RCover.blenddisablelighting = 0.2 * gi1lvl
	Plastic_ramp.blenddisablelighting = 0.7 * gi1lvl - 0.2
	Lockdoor2.blenddisablelighting = 2 * gi1lvl
	RFLogo.blenddisablelighting = 0.3 * gi1lvl - 0.1
	LFLogo.blenddisablelighting = 0.3 * gi1lvl - 0.1
	'debug.print "GI1 level: " & gi1lvl

	giprevalvl = gi1lvl

End Sub

'Lamp Filter
Function LampFilter(aLvl)

	LampFilter = aLvl^1.6	'exponential curve?
End Function

Dim GIoffMult : GIoffMult = 2 'adjust how bright the inserts get when the GI is off
Dim GIoffMultFlashers : GIoffMultFlashers = 2	'adjust how bright the Flashers get when the GI is off

'Helper functions

Function ColtoArray(aDict)	'converts a collection to an indexed array. Indexes will come out random probably.
	redim a(999)
	dim count : count = 0
	dim x  : for each x in aDict : set a(Count) = x : count = count + 1 : Next
	redim preserve a(count-1) : ColtoArray = a
End Function


'***********************************************
'Intermediate Solenoid Procedures (Setlamp, etc)
'***********************************************
'Solenoid pipeline looks like this:
'Pinmame Controller -> UseSolenoids -> Solcallback -> intermediate subs (here) -> ModLampz dynamiclamps object -> object updates / more callbacks

'GI
'Pinmame Controller -> core.vbs PinMameTimer Loop -> GIcallback2 ->  ModLampz dynamiclamps object -> object updates / more callbacks
'(Can't even disable core.vbs's GI handling unless you deliberately set GIcallback & GIcallback2 to Empty)

'Lamps, for reference:
'Pinmame Controller -> LampTimer -> Lampz Fading Object -> Object Updates / callbacks

Set GICallback2 = GetRef("SetGI")

Sub SetGI(aNr, aValue)
	ModLampz.SetGI aNr, aValue 'Redundant. Could reassign GI indexes here
End Sub

'***************************************
'	*** End nFozzy lamp handling ***
'***************************************

' *********************************************************************
'						Lighting
' *********************************************************************

Sub SetLamp(aNr, aOn)
	Lampz.state(aNr) = abs(aOn)
End Sub

Sub SetModLamp(aNr, aInput)
	ModLampz.state(aNr) = abs(aInput)/255
End Sub

' *********************************************************************
'					Supporting Ball & Sound Functions
' *********************************************************************

Function RndNum(min,max)
 RndNum = Int(Rnd()*(max-min+1))+min     ' Sets a random number between min and max
End Function

' *********************************************************************
' 						Other Sound FX
' *********************************************************************

Sub SoundHole45()
	PlaySoundAtVol "fx_hole3",sw45,.5
End Sub 

Sub SoundHole72()
	PlaySoundAtVol "fx_hole3",sw72,.2
End Sub 

Sub SoundHole73()
	PlaySoundAtVol "fx_hole3",sw73,.2
End Sub 

' *********************************************************************
' 					Ball Drop & Ramp Sounds
' *********************************************************************

Sub SubwayExit_hit:StopSound "fx_plasticrolling":PlaysoundAt "fx_kickerstop",SubwayExit:End Sub

Sub ShooterStart_Hit():StopSound "fx_launchball":If ActiveBall.VelY < 0 Then PlaySoundAt "fx_launchball",ShooterStart:End If:End Sub	'ball is going up
Sub ShooterEnd_Hit:If ActiveBall.Z > 30  Then Me.TimerInterval=100:Me.TimerEnabled=1:End If:End Sub						'ball is flying
Sub ShooterEnd_Timer(): Me.TimerEnabled=0 : PlaySound "Ball_Bounce_Playfield_Soft_1",0,2,.2,0,0,0,1,-.6 : End Sub

Sub LREnter_Hit():If ActiveBall.VelY < 0 Then PlaySoundAtVol "fx_lrenter",LREnter,.2:End If:End Sub			'ball is going up
Sub LREnter_UnHit():If ActiveBall.VelY > 0 Then StopSound "fx_lrenter":End If:End Sub		'ball is going down
Sub LREnter1_Hit():StopSound "fx_lrenter":PlaySoundAtVol "fx_ramp_turn",LREnter1,.2:End Sub
Sub LREnter2_Hit():StopSound "fx_ramp_turn":End Sub
Sub LRExit_Hit():ActiveBall.VelY=1:PlaySoundAtVol SoundFX("WireRamp_Stop", DOFFlippers), LRExit, 1.5:End Sub

Sub RREnter_Hit():If ActiveBall.VelY < 0 Then PlaySoundAtVol "fx_ramp_enter1",RREnter,.2:End If:End Sub			'ball is going up
Sub RREnter_UnHit():If ActiveBall.VelY > 0 Then StopSound "fx_ramp_enter1":End If:End Sub		'ball is going down
Sub RREnter1_Hit():PlaySoundAtVol "fx_ramp_enter2",RREnter1,.2:End Sub
Sub RREnter2_Hit()
	PlaySoundAtVol "fx_ramp_enter2",RREnter2,.2
'	debug.print "ball vely: " & activeball.vely
End Sub
Sub RREnter3_Hit():StopSound "fx_ramp_enter2":End Sub
Sub RRExit_Hit():ActiveBall.VelY=1:PlaySoundAtVol SoundFX("WireRamp_Stop", DOFFlippers), RRExit, 3:End Sub

Sub BREnter_Hit():StopSound "fx_ramp_enter2":PlaySoundAtVol "fx_ramp_enter3",BREnter,.2:End Sub
Sub BRExit_Hit():ActiveBall.VelY=1:PlaySoundAtVol SoundFX("WireRamp_Stop", DOFFlippers), BRExit, 3:End Sub

' *********************************************************************
' 			Left and Right Orbits Hack
' *********************************************************************

Sub LoopHelpL_Unhit():If ActiveBall.VelY > 20 Then ActiveBall.VelY = RndNum(12,14):End If:End Sub
Sub LoopHelpR_Unhit():If ActiveBall.VelY > 20 Then ActiveBall.VelY = RndNum(12,14):End If:End Sub

' *********************************************************************
' 						RealTime Updates
' *********************************************************************

Sub FrameTimer_Timer()
	RollingTimer
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
	MechsUpdate
	LampTimer
	if p_sw117.transz < -2 then light22f.visible = 0
	if p_sw115.transz < -2 then light23f.visible = 0
	if p_sw116.transz < -2 then light24f.visible = 0
End Sub

'******************************************************
'		BALL ROLLING AND DROP SOUNDS
'******************************************************

Const tnob = 7 ' total number of balls
Const lob = 0
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
	Dim BOT, b
	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
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
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If

		'***Ball Drop Sounds***
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
		If AmbientBallShadowOn = 0 Then
			If BOT(b).Z > 30 Then
				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
			Else
				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
			End If
			BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + fovY
			BallShadowA(b).X = BOT(b).X
			BallShadowA(b).visible = 1
		End If
	Next
End Sub

'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
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

Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

Dim sourcenames, currentShadowCount
sourcenames = Array ("","","","","","","","","","","","")
currentShadowCount = Array (0,0,0,0,0,0,0,0,0,0,0,0)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!
dim objrtx1(8), objrtx2(8)
dim objBallShadow(8)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadowA5,BallShadowA6,BallShadowA7)

DynamicBSInit

sub DynamicBSInit()
	Dim iii

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
end sub


Sub DynamicBSUpdate
	Dim falloff:	falloff = 150			'Max distance to light sources, can be changed if you have a reason
	Dim ShadowOpacity, ShadowOpacity2 
	Dim s, Source, LSd, currentMat, AnotherSource, BOT
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
			If BOT(s).Z < 30 And UBound(BOT) <= 4 Then 'And BOT(s).Y < (TableHeight - 200) Then 'Or BOT(s).Z > 105 Then		'Defining when and where (on the table) you can have dynamic shadows
				For Each Source in DynamicSources
					LSd=DistanceFast((BOT(s).x-Source.x),(BOT(s).y-Source.y))	'Calculating the Linear distance to the Source
					If LSd < falloff and Source.state=1 Then	    			'If the ball is within the falloff range of a light and light is on
						currentShadowCount(s) = currentShadowCount(s) + 1		'Within range of 1 or 2
						if currentShadowCount(s) = 1 Then						'1 dynamic shadow source
							sourcenames(s) = source.name
							currentMat = objrtx1(s).material
							objrtx2(s).visible = 0 : objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
	'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01						'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(Source.x, Source.y, BOT(s).X, BOT(s).Y) + 90
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

'*********** Gates Primitives Sync *********************************
Sub MechsUpdate()
	TopGateP.RotX = TopGate.currentangle
	BottomGateP.RotX = BottomGate.currentangle + 10
	LeftGateP.RotY = -LeftGate.currentangle
	RightGateP.RotY = -RightGate.currentangle
	LeftFlipperSh.RotZ = LeftFlipper.currentangle
	RightFlipperSh.RotZ = RightFlipper.currentangle
	LFLogo.RotZ = LeftFlipper.currentangle
	RFLogo.RotZ = RightFlipper.currentangle
	
	'droptarget off prims
	p_sw11OFF.transz = p_sw11.transz
	p_sw115OFF.transz = p_sw115.transz
	p_sw116OFF.transz = p_sw116.transz
	p_sw117OFF.transz = p_sw117.transz
End Sub

' *********************************************************************
'					Table Options 
' *********************************************************************
Dim InstrChoice, FlipperChoice

Sub InitOptions

	Select Case FlipperType
		Case 0
			LFLogo.image= "williamsbatwhitered" : RFLogo.image= "williamsbatwhitered"
		Case 1
			LFLogo.image= "williamsbatwhiteorange" : RFLogo.image= "williamsbatwhiteorange"
		Case 2
			LFLogo.image= "williamsbatwhiteblack" : RFLogo.image= "williamsbatwhiteblack"
		Case 3
			LFLogo.image= "williamsbatwhiteblue" : RFLogo.image= "williamsbatwhiteblue"
		Case 4
			LFLogo.image= "williamsbatorange" : RFLogo.image= "williamsbatorange"
	End Select

	Select Case PropellerMod
		Case 0:Propeller.visible=0:Propeller1.visible=1
		Case 1:Propeller.visible=1:Propeller1.visible=0
	End Select

End Sub

'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.

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


' *********************************************************************
'                     Fleep  Supporting Ball & Sound Functions
' *********************************************************************

'Dim tablewidth, tableheight : tablewidth = table1.width : tableheight = table1.height


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
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd*10)+1,DOFContactors), SlingshotSoundLevel, LEMK
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd*8)+1,DOFContactors), SlingshotSoundLevel, REMK
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

Sub LeftFlipper_Collide(parm)
	FlipperLeftHitParm = parm/10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipper_Collide(parm)
	FlipperRightHitParm = parm/10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
 	RandomSoundRubberFlipper(parm)
End Sub

'' iaakki Rubberizer
'sub Rubberizer(parm)
'	if parm < 10 And parm > 2 And Abs(activeball.angmomz) < 10 then
'		'debug.print "parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
'		activeball.angmomz = activeball.angmomz * 1.2
'		activeball.vely = activeball.vely * 1.2
'		'debug.print ">> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
'	Elseif parm <= 2 and parm > 0.2 Then
'		'debug.print "* parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
'		activeball.angmomz = activeball.angmomz * -1.2
'		activeball.vely = activeball.vely * 1.4
'		'debug.print "**** >> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
'	end if
'end sub
'
'' apophis Rubberizer
'sub Rubberizer2(parm)
'	if parm < 10 And parm > 2 And Abs(activeball.angmomz) < 10 then
'		'debug.print "parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
'		activeball.angmomz = -activeball.angmomz * 2
'		activeball.vely = activeball.vely * 1.2
'		'debug.print ">> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
'	Elseif parm <= 2 and parm > 0.2 Then
'		'debug.print "* parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
'		activeball.angmomz = -activeball.angmomz * 0.5
'		activeball.vely = activeball.vely * (1.2 + rnd(1)/3 )
'		'debug.print "**** >> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
'	end if
'end sub

' iaakki Rubberizer
'sub Rubberizer(parm)
'	dim origAngMomz, OrigVelY, OrigVelX
'
'	origAngMomz = activeball.angmomz
'	OrigVelY = activeball.vely
'	OrigVelX = activeball.velx
'
'    if parm < 10 And parm > 2 And Abs(origAngMomz) < 15 And OrigVelY < 0 then
'        'debug.print "parm: " & parm & " momz: " & activeball.angmomz &" velx: "& activeball.velx
'        activeball.angmomz = origAngMomz * 1.2
'        activeball.vely = OrigVelY * (1.1 + (parm/50))
'        'debug.print ">> newmomz: " & activeball.angmomz&" newvelx: "& activeball.velx
'    Elseif parm <= 2 and parm > 0.2 And OrigVelY < 0 Then
'        'debug.print "**** parm: " & parm & " momz: " & activeball.angmomz &" velx: "& activeball.velx
'        if (OrigVelX > 0 And origAngMomz > 0) Or (OrigVelX < 0 And origAngMomz < 0) then
'            activeball.angmomz = origAngMomz * -0.7
'            'debug.print "reverse spin!"
'        Else
'            activeball.angmomz = origAngMomz * 1.2
'        end if
'        activeball.vely = OrigVelY * (1.2 + (parm/10))
'        'debug.print "**** >> newmomz: " & activeball.angmomz&" newvelx: "& activeball.velx
'    end if
'end sub

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
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 5 then
 		RandomSoundRubberStrong 1 
	End if
	If finalspeed <= 5 then
 		RandomSoundRubberWeak()
 	End If	
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

'/////////////////// Wire Ramp Bump Sounds///////////////'

Dim NextOrbitHit:NextOrbitHit = 0 

Sub WireRampBumps_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump3 .3, Pitch(ActiveBall)+5
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .2 + (Rnd * .2)
	end if 
End Sub

Sub PlasticRampBumps_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump 2, -20000
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .1 + (Rnd * .2)
	end if 
End Sub

'' Requires rampbump1 to 7 in Sound Manager
Sub RandomBump(voladj, freq)
	dim BumpSnd:BumpSnd= "rampbump" & CStr(Int(Rnd*7)+1)
		PlaySoundAtBall(BumpSnd)
End Sub

' Requires WireRampBump1 to 5 in Sound Manager
Sub RandomBump3(voladj, freq)
	dim BumpSnd:BumpSnd= "WireRampBump" & CStr(Int(Rnd*5)+1)
		PlaySoundAtBall(BumpSnd)
End Sub

' Stop Bump Sounds
Sub BumpSTOP1_Hit ()
dim i:for i=1 to 4:StopSound "WireRampBump" & i:next
NextOrbitHit = Timer + 1
End Sub

Sub BumpSTOP2_Hit ()
dim i:for i=1 to 4:StopSound "WireRampBump" & i:next
NextOrbitHit = Timer + 1
End Sub

Sub BumpSTOP3_Hit ()
dim i:for i=1 to 4:StopSound "WireRampBump" & i:next
NextOrbitHit = Timer + 1
End Sub


Sub Targets_Hit (idx)
	PlayTargetSound	
	TargetBouncer Activeball, 0.7
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'********************************************************
'				FLIPPER AND RUBBER CORRECTION
'********************************************************

'****************************************************************************
'nFozzy PHYSICS DAMPENERS

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx)
	RubbersD.dampen Activeball 
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
	TargetBouncer Activeball, 0.7
End Sub

dim RubbersD : Set RubbersD = new Dampener	'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 0.96 'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.96 
RubbersD.addpoint 2, 5.76, 0.967 'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener	'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	'debug, reports in debugger (in vel, out cor)
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

'######################### Add Dampenf to Dampener Class 
'#########################    Only applies dampener when abs(velx) < 2 and vely < 0 and vely > -3.75  


Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold 	'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
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
		if cor.ballvel(aBall.id) = 0 then
			RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.001) 'hack
		Else
			RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
		end If
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	public sub Dampenf(aBall, parm, ver)
		If ver = 1 Then
			dim RealCOR, DesiredCOR, str, coef
			DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
			if cor.ballvel(aBall.id) = 0 then
                RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.001) 'hack
            Else
                RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
            end If
			coef = desiredcor / realcor 
			If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
				aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
			End If
		Elseif ver = 2 Then
			If parm < 10 And parm > 2 And Abs(aball.angmomz) < 15 And aball.vely < 0 then
				aball.angmomz = aball.angmomz * 1.2
				aball.vely = aball.vely * (1.1 + (parm/50))
			Elseif parm <= 2 and parm > 0.2 And aball.vely < 0 Then
				if (aball.velx > 0 And aball.angmomz > 0) Or (aball.velx < 0 And aball.angmomz < 0) then
			        	aball.angmomz = aball.angmomz * -0.7
				Else
					aball.angmomz = aball.angmomz * 1.2
				end if
				aball.vely = aball.vely * (1.2 + (parm/10))
			End if
		End If
	End Sub

	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub


	Public Sub Report() 	'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub
	

End Class

'******************************************************
'		TRACK ALL BALL VELOCITIES
' 		FOR RUBBER DAMPENER AND DROP TARGETS
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

Sub RDampen_Timer()
	Cor.Update
End Sub

Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	'Clamp if on the boundry lines
	'if L=1 and Y < yLvl(LBound(yLvl) ) then Y = yLvl(lBound(yLvl) )
	'if L=uBound(xKeyFrame) and Y > yLvl(uBound(yLvl) ) then Y = yLvl(uBound(yLvl) )
	'clamp 2.0
	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
End Function

'*******************************************************
'	End nFozzy Dampening'
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


'Helper function
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

If CabinetMode = 1 Then
	Pincab_Rails.visible  = 0
	SideBlades.size_z = 2
	SideBladesoff.size_z = 2
End If

If BlimpToy = 0 Then
	p_blimp.visible = 0
	p_blimpoff.visible = 0
End If

DIM VRThings
If VRRoom > 0 Then
	'Plastic_ramp.blenddisablelighting=0.1
	Scoretext.visible = 0
	If VRRoom = 1 Then
		for each VRThings in VRCab:VRThings.visible = 1:Next
		for each VRThings in VRStuff:VRThings.visible = 1:Next
	End If
	If VRRoom = 2 Then
		for each VRThings in VRCab:VRThings.visible = 0:Next
		for each VRThings in VRStuff:VRThings.visible = 0:Next
		PinCab_Backglass.visible = 1
		PinCab_Backbox.visible = 1
		PinCab_Backbox.image = "Pincab_Backbox_Min"
		DMD1.visible = 1
	End If
	If VRFlashingBackglass = 1 Then
		SetBackglass
		For each vrthings in VRBGModLampFlasher:vrthings.visible = 1:Next
		For each vrthings in VRBackglassSpeaker:vrthings.visible = 1:Next
		For each vrthings in VRBGGI:vrthings.visible = 1:Next
		BGDark.visible = 1
		BGSpeaker.visible = 1
		PinCab_Backglass.visible = 0
	End If	
Else
		for each VRThings in VRCab:VRThings.visible = 0:Next
		for each VRThings in VRStuff:VRThings.visible = 0:Next
		'Plastic_ramp.blenddisablelighting=2
End if

Sub SetBackglass()
	Dim obj
	For Each obj In VRBackglass
		obj.x = obj.x + 3
		obj.height = - obj.y + 375
		obj.y = -120 'adjusts the distance from the backglass towards the user
		obj.rotx=-89
	Next
	BGSpeaker.height = BGspeaker.y + 753
	BGSpeaker.y = -84
	BGSpeaker.rotx = -86
	For Each obj In VRBackglassSpeaker
		obj.x = obj.x + 3
		obj.height = - obj.y + 400
		obj.y = -70 'adjusts the distance from the backglass towards the user
		obj.rotx=-86
	Next
End Sub

'******************************************************
'				DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch)

'	PlaySoundAtVol  DTHitSound, Activeball, Vol(Activeball)*22.5
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
End Sub

Sub DTDrop(switch)
	Dim i
	i = DTArrayID(switch)

	DTArray(i).animate = 1
	DoDTAnim
End Sub

Function DTArrayID(switch)
	Dim i
	For i = 0 to uBound(DTArray) 
		If DTArray(i).sw = switch Then DTArrayID = i:Exit Function 
	Next
End Function


'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

sub TargetBouncer(aBall,defvalue)
    dim zMultiplier, vel, vratio
    if TargetBouncerEnabled = 1 and aball.z < 30 and aBall.vely > 0 then
        'debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz

		'debug.print "velz: " & round(aBall.velz,2) & "   horizontal vel: " & round(sqr(abs(aBall.velx^2 + aBall.vely^2)),2) & "  angle: " & Atn(aBall.velz/sqr(abs(aBall.velx^2 + aBall.vely^2)))*180/pi

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
		'debug.print " ---> velz: " & round(aBall.velz,2) & "   horizontal vel: " & round(sqr(abs(aBall.velx^2 + aBall.vely^2)),2) & "  angle: " & Atn(aBall.velz/sqr(abs(aBall.velx^2 + aBall.vely^2)))*180/pi
        'debug.print "conservation check: " & BallSpeed(aBall)/vel
    elseif TargetBouncerEnabled = 2 and aball.z < 30 and aBall.vely > 0 then
		'debug.print "velz: " & activeball.velz
		Select Case Int(Rnd * 4) + 1
			Case 1: zMultiplier = defvalue+1.1
			Case 2: zMultiplier = defvalue+1.05
			Case 3: zMultiplier = defvalue+0.7
			Case 4: zMultiplier = defvalue+0.3
		End Select
		aBall.velz = aBall.velz * zMultiplier * TargetBouncerFactor
		'debug.print "----> velz: " & activeball.velz
		'debug.print "conservation check: " & BallSpeed(aBall)/vel
	end if
end sub

sub DTBallPhysics(aBall, angle, mass)
	dim rangle,bangle,calc1, calc2, calc3
	rangle = (angle - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))

	calc1 = cor.BallVel(aball.id) * cos(bangle - rangle) * (aball.mass - mass) / (aball.mass + mass)
	calc2 = cor.BallVel(aball.id) * sin(bangle - rangle) * cos(rangle + 4*Atn(1)/2)
	calc3 = cor.BallVel(aball.id) * sin(bangle - rangle) * sin(rangle + 4*Atn(1)/2)

	'debug.print "drop target hit, ball velz: " & aball.velz & " BAL VEL: " & cor.BallVel(aball.id) & " BAL VELy: " & aball.vely
	if aball.vely > 10 then
		TargetBouncer(aBall), 1.4
		'debug.print "--> new ball velz: " & aball.velz
	end if

	aBall.velx = calc1 * cos(rangle) + calc2
	aBall.vely = calc1 * sin(rangle) + calc3
End Sub

'Add Timer name DTAnim to editor to handle drop target animations
DTAnim.interval = 10
DTAnim.enabled = True

Sub DTAnim_Timer()
	DoDTAnim
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
	dim transz
	Dim animtime, rangle

	rangle = prim.rotz * 3.1416 / 180

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
		PlaySoundAt SoundFX(DTDropSound,DOFDropTargets),prim
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
			controller.Switch(Switch) = 1
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
			Dim BOT, b
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
			prim.rotx = 0
			prim.roty = 0
			primary.uservalue = gametime
		end if
		primary.collidable = 0
		secondary.collidable = 1
		controller.Switch(Switch) = 0

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

'******************************************************
'		DROP TARGET
'		SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction, rubber dampeners, and drop targets
'Function BallSpeed(ball) 'Calculates the ball speed	'double definition
'    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
'End Function

Dim PI: PI = 4*Atn(1)

' Used for drop targets
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

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

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

' Used for drop targets
Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

'******************************************************
'		DROP TARGETS INITIALIZATION
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
Dim DT115, DT116, DT117, DT11

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
'       isDropped:  Boolean which determines whether a drop target is dropped. Set to false if they are initially raised, true if initially dropped.

Set DT11 = (new DropTarget)(sw11a,sw11b, p_sw11, 11, 0, false)
Set DT115 = (new DropTarget)(sw115a,sw115b, p_sw115, 115, 0, false)
Set DT116 = (new DropTarget)(sw116a,sw116b, p_sw116, 116, 0, false)
Set DT117 = (new DropTarget)(sw117a,sw117b, p_sw117, 117, 0, false)

'Add all the Drop Target Arrays to Drop Target Animation Array
' DTAnimationArray = Array(DT1, DT2, ....)
Dim DTArray
DTArray = Array(DT11, DT115, DT116, DT117)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110 			'in milliseconds
Const DTDropUpSpeed = 40 			'in milliseconds
Const DTDropUnits = 44 			'VP units primitive drops
Const DTDropUpUnits = 10 			'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 				'max degrees primitive rotates when hit
Const DTDropDelay = 20	 		'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40	 		'time in milliseconds before target drops back to normal up position after the solendoid fires to raise the target
Const DTBrickVel = 0				'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0			'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "DropTargetHit"	'Drop Target Hit sound
Const DTDropSound = "DropTarget_Down"		'Drop Target Drop sound
Const DTResetSound = "DropTarget_Up"	'Drop Target reset sound

Const DTMass = 0.2				'Mass of the Drop Target (between 0 and 1), higher values provide more resistance


dim preloadCounter
sub preloader_timer
	preloadCounter = preloadCounter + 1
	If BlimpToy = 0 Then
		p_blimp.visible = 0
		p_blimpoff.visible = 0
	End If
	if preloadCounter = 1 then
        For each ii in p_toysplastics_off:ii.image="p_col_toysplastics_RuinsFlash":ii.visible=0:Next
        For each ii in p_cab_off:ii.image="p_col_cab_RuinsRamp_Flash":ii.visible=1:Next
        For each ii in p_metalsposts_off:ii.image="p_col_metalsposts_RuinsFlash":ii.visible=1:Next
	Elseif preloadCounter = 2 then
        For each ii in p_toysplastics_off:ii.image="p_col_toysplastics_RRampFlash":ii.visible=0:Next
        For each ii in p_cab_off:ii.image="p_col_cab_RRamp_Flash":ii.visible=1:Next
        For each ii in p_metalsposts_off:ii.image="p_col_metalsposts_RRampFlash":ii.visible=1:Next
	Elseif preloadCounter = 3 then
        For each ii in p_toysplastics_off:ii.image="p_col_toysplastics_LRampFlash":ii.visible=0:Next
        For each ii in p_cab_off:ii.image="p_col_cab_LRamp_Flash":ii.visible=1:Next
        For each ii in p_metalsposts_off:ii.image="p_col_metalsposts_LRampFlash":ii.visible=1:Next
	Elseif preloadCounter = 4 then
		For each ii in p_toysplastics_off:ii.image="p_col_toysplastics_gi_off":ii.visible=0:Next
		For each ii in p_cab_off:ii.image="p_col_cab_gi_off0000":ii.visible=1:Next
		For each ii in p_metalsposts_off:ii.image="p_col_metalsposts_gi_off0000":ii.visible=1:Next
	Elseif preloadCounter = 7 then
        solflash26 5
	Elseif preloadCounter = 8 then
        solflash27 5
	Elseif preloadCounter = 9 then
        solflash52 5
	Elseif preloadCounter = 12 then
		me.enabled = false
	end if
	'msgbox preloadCounter
end sub





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
'16 = Indiana Jones Cool
'17 = Indiana Jones Original

Dim LUTset

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
        Case 16: LUTBox.text = "Indiana Jones Cool"
        Case 17: LUTBox.text = "Indiana Jones Original"
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

	if LUTset = "" then LUTset = 17 'failsafe

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "IJPALUT.txt",True)
	ScoreFile.WriteLine LUTset
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub

Sub LoadLUT
	Dim FileObj, ScoreFile, TextStr
	dim rLine

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		LUTset=17
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "IJPALUT.txt") then
		LUTset=17
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "IJPALUT.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
		End if
		rLine = TextStr.ReadLine
		If rLine = "" then
			LUTset=17
			Exit Sub
		End if
		LUTset = int (rLine) 
		Set ScoreFile = Nothing
	    Set FileObj = Nothing
End Sub





'************************************************************************************************************************
'	VPW CHANGE LOG
'************************************************************************************************************************
' 001 - iaakki - initial version with nfozzy lights script. Only 2 lamps connected for now. All the inserts needs renaming.
' 002 - iaakki - Adding triangle inserts, Playfield ambient occlusion flasher disabled for now
' 003 - iaakki - round inserts
' 012 - iaakki - inserts...
' 013 - iaakki/wrd - NF GI light and LUT updates, adjusted some inserts, AO image now has holes on inserts
' 014 - iaakki - Solenoid Mod lamps set, all inserts set, tons of bugs fixed.. 
' 015 - iaakki - removed one debug print that paused game play
' 016 - iaakki - insert adjust
' 017 - benji  - updated nfozzy physics to "1.6"
' 018 - Sixtoe - Added and lined up vr cabinet, including gun launcher, more work to do with this, added VR switch to turn all VR stuff on and off, added pincab_base to occlude carpet, 'dropped rubber pole walls (e.g. Wall3) 1 pixel to stop z clashing (black flickering on outlane plastics), rotated wire triggers 180 degrees (triggers upside down), aligned screw and outlane narrow escape peg (screw was floating off to the side), changed material of yellow pegs and set to non-static for VR, test on normal to see if it's the same (stops swimming lights), might have to have a switch if they don't look as good in VR, raised POASh upper playfield shadow to stop z flight (black flickering) with insert text layers, redid gi11 as an occluded light, dropped it and Gi13, 3, 4, 7, 8  to stop light cutting targets in half, edited IJ-plastics-sh to stop shadow overhanging outlane on left hand side, raised flipper shadows to 0.11 to stop z flight (black flickering) with insert text layers, fixed lighting for l54 and f54 being swapped, fixed a few messed up flasher assignments, fixed some depth bias issues with lights and flashers, replaced flasher glow lighting with flashers, messed around with all of them, added dome primitives to insert lighting system.
' 019 - iaakki - NF livecatch script modified to make partialf catch only when catch happens too late. Audio issues fixed. Cleanup
' 020 - iaakki - flipper length 118 -> 114, ball_drop sounds added, fixed few insert prints and added shadows
' 021 - iaakki - experimental jerk of flipper after high collision
' 022 - Benji - Updated to latest physics 1.7.1
' 023 - tomate - add new plastic ramp mesh and new textures for Cabinet and VR modes (plastic_ramp / VR_plastic_ramp)
' 024 - Benji
' 025 - Sixtoe - VR and Cabinet mode completed(?), fixed shadows, adjusted rightflasher, fixed timers and flipper shadow, removed collision status from things that don't need it, fixed right ramp, adjusted walls.
' 029 - iaakki - Brad1x's new PF and insert text layers added. Inserts 38 and 45 reworked with bulb primitive and new materials. "Hack" added to cor div by zero. Sling audios tied to LEMK and REMK
' 030 - Benji - Reimported all primitives with new baked textures
' 032 - iaakki - tied collections to giupdates
' 033 - iaakki - rest of minipf stuff and all target prims added to fading, Cards has issues. SideBlades bug fixed
' 034 - iaakki - PF GI Flasher added. Amount is 200 for now. Probably needs adjusting.
' 035 - iaakki - repacked some textures, reworking inserts
' 036 - iaakki - reworking more inserts
' 038 - iaakki - inserts done, edges needs work
' 039 - iaakki - new textures recompressed and imported
' 040 - Benji - Removed duplicate objects, turned off visibility of collidable walls. Reimported missing rubber band. Reimported deleted dome and bumper textures. Separated ramp covers from p_plastics primitive and re-imported them as p_plastics_RCovers, and p_plastics_RCoversOFF, put them in their respective collections. 
' 041 - iaakki - solflashRRamp and solflashLRamp implemented, some collections created to handle primitive fading for RRamp flasher, old RRamp flasher lamps removed except FL_RRampF.
' 042 - Sixtoe - Went through table and removed most / all duplicates, cleaned up some z fighting, added some rubbers and adjusted them, messed with the GI and dropped most to -3, adjusted sideblades, reverted to old propeller prims so they rotate
' 043 - Benji - Fixed various stuffs
' 044 - iaakki - fixed fading for flashers, fixed some nf scrips, flipper physics parameters fixed, ball mass 1.5 -> 1. Right flasher converted to flupper dome and all primitive fadings included in that code. "Flasherlight1" is commented out is we might not need it.
' 045 - Benji - Imported updated/repacked cab collection geometry and textures. Imported updated toysplastics gi on/off images
' 046 - iaakki - metalposts rramp image reimported, gi transmit values set to 0, GI collections have own materials for ON and OFF states, all fadings updated to use updatematerial method, Williams bats added
' 048 - iaakki - normals removed from all ON primitives as they don't work with UpdateMaterial method in VR. PincabBottom removed, as it made my PF grey for some reason, Painted inserts are way too bright in VR and we must tune 
' 049 - Benji - Optimized main prim image sets reducing size. fixed misassigned minipf prims. fixed rcover alpha (needs some tuning)
' 050 - Benji - Separate rubber posts and rubber bands
' 051 - Benji - 051 - Moved all ON prims to layer 3, all OFF prims to layer 4. Moved unbaked objects from layer 3 and layer 4 to layer 1. Double checked proper materials assigned.Changed slope from 6-7 to 5.5-6Changed ball size/mass from 52/1 to 50/1.3 to match physics on Roth's update to Twilight ZoneMade Sure flipper strengh is 3250 per TZKnown Bugs: sometimes ball bounces back into trough before launch. Right ramp does not seem makeable maybe increase flip strength?
' 052 - Benji - fixed plastic ramp and adjusted texture. reimported plastics with 'lite jacpot' plastic detached. new prim p_LiteJackpot and p_LiteJackpotOFF added and new materials for it for fading.
' 053 - Benji - Adjusted flip angles to match TZ. Updated flip polarity. Changes flipper elasticity from .88 to .86
' 054 - Benji - Changed polarity to polarity from TZ 
' 055 - Benji - Ball mass 1. Flipper meshes set to non-collidable(!). All On prims set to 0 DB, all OFF prims at 0.
' 058 - iaakki - flasher codes updated. fading multiplier set to 0.99 so it is really slow. Mod solenoid values are averaged, so it won't shut the flasher off immediately.
' 059 - iaakki - instructions cards, shadows and z-order fixed. NF Drop targets added and fixed some incorrectly set prims
' 060 - iaakki - solflash51 done
' 061 - benji - swapped out temp debugging solid images for renders
' 062 - iaakki - Lite Jackpot plastic lighting done, preloader added
' 063 - iaakki - Imported Brad1x's Lite Jackbot images and improved some insert edges. Also made insert "Paint" image a bit darker.
' 064 - Benji - Full new render set imported. Holes fixed in POA. Glitchy images fixed
' 065 - Benji - Fixed instruction cards...don't touch them or breath on them and keep fingers crossed at all times
' 066 - Benji - New Renders with updated lighting
' 067 - Benji - Adjusted various problem areas in renders and re-rendered 'one more time'. MetalsPosts and 'cab' objects now use JPEGS, toys/plastics use 24bit png
' 072 - iaakki - Diverter rest at -2, idol lockdoor1 fix, targetbouncer with some values to test it out. Some gi fade tune
' 073 - iaakki - solflash51 lamps fixed, GI flasher style changed
' 074 - iaakki - Plastic Ramp DB fixed, Ramp covers separated from prim fading and added to DL fading, Diverted collection issues solved, GI fading fixed, Flip trigger areas redone, Flasher Domes fixed.
' 075 - iaakki - Right outlane post can be moved with script options, rubberizer added, flipnudge values updated
' 076 - iaakki - right sling rubbers tuned to make inlane ball not bounce from them, sw38 made invisible.
' 077 - iaakki - GI ball reflections done
' 078 - iaakki - inserts redone differenly
' 079 - Sixtoe - Realigned a lot of physical objects, replaced a lot of rubbers, deleted old assets, realigned sling rubbers, modified old collidable walls to fill ball trap gaps, aligned narrow escape gate, changed physics materials on objects, trimmed lights, put lamptimer on the frametimer, hooked up flippers to GI, fixed flipper selection script, moved testmode switch to the top, fixed depth bias issues and adjusted position of flasherflash2/3, adjusted flippers
' 080 - apophis - Updated Drop Target code. Changed the drop target sound effects. Added Rubber_4 sound. Updated AudioFade and AudioPan functions to prevent overflow error. Removed duplicate OnBallBallCollision sub. Added Targets collection (Fleep). Added some Fleep stuff. Removed some unused sounds calls. Reduced slingshot strength from 5.5 to 4.5. Reduced bumper force from 12 to 10. Reduced plunger speed from 150 to 130.
' 081 - iaakki - insert adjusted and bulbs fade slower, main ramp added to GI DL, outlane post tuned, targetbouncer redone, dampening fixed, mainramp tune for easier POA shots, cabinetmode fixed
' 082 - iaakki - Flasher max mod value limit, ruins and cols gi off state tuned, flipper tips and start angles tuned
' 083 - Sixtoe - Replaced cab textures, made sw31 kicker visible, added missing sleeve, adding texture to the sling rubbers so they match the pre-rendered rubbers better, changed ball size to 50, added one way gate to ball trough exit, added texture to path of adventure stop pole
' 084 - iaakki - random lampz bug fixed. Flipper Nudge fixed with tip from Apophis
' 085 - Sixtoe - Rebuilt right ramp (still iffy?), changed some more physics materials, changed and added some blocker walls
' 086 - apophis - Added dynamic shadows. Made Ramp7 invisible.
' 087 - iaakki - totem lockdoor visuals fixed, PF friction to 0.15 -> 0.2. Slope setting to middle 6 (was 5.5-6.5), main ramp testing with friction 0, rampbump sounds added, some prim DLFB set to 1
' 088 - Sixtoe - Replaced metalposts textures (8bit jpg), upped flipper strength to 3900, changed gravity constant to 1, changed difficulty to 50 (6 degrees), removed old redundant light primitives, changed main ramp friction back.
' 089 - tomate - tweaked wireRamps textures, separated wireRamps prims and changed DL value for ON  and OFF prims
' 090 - apophis - Optimized textures.
' 091 - Skitso - New LUT, brightness tweaks to PF and apron texture, tweaks to GI lamps
' 092 - iaakki - "start mode" exit angle change, toy, flip and wireramp brightness adjusted, flip params changed, plunger scatter added, I and jackpot inserts adjusted, p_plastics & p_col_metalsposts DLFB set to 1, Lite&multi Jackpot flashers fixed
' 093 - fluffhead35 - Updated wire ramp exit sounds.  Removed unused table options from script.
' 094 - fluffhead35 - Based on AstroNasty's suggestions moved bottom slings posts over so it is in line with slings.  Moved wall84 over to the left to make post stick out less. Changed sw31 to 160
' 095 - Skitso - Redone bumper lighting, removed modulation from insert halos, fixed one insert halo not working, changed ball and scratches textures, toned pf ball reflections down a bit. Still hate the wire ramps.
' 096 - iaakki - updated flips code, removed shoot tester thing, PinCab_Bottom set visible, R_medium moved slightly in right outlane, POA DivHelp adjusted
' 097 - Skitso - Removed two odd square flashers from above the top diverters (looked like boxes in the top Wall001). Reduced SSR to 0.2, Tweaked Super Jackpot, Jackpot, Path of Adventure and Jackpot multiplier flashers for bit more ooomph.
' 098 - iaakki - Top DT reset sound added, all DT's repositioned, limit dynamic shadows to max 4 balls, red inserts fiddled 
' 099 - tomate - tweaked wireRamps texture, ON/OFF prims DL changed from 0 --> 0.7
' 100 - Sixtoe - Captive ball lane split and rebuilt (taller protection), inlanes tweaked, rollover drop hole textures added, triggerlf/rf adjusted, flippers changed to 3500, delete duplicate sling, removed collidable from numerous objects, changed some primitives to toys, made pincab_bottom visible, added pincab blades to fixholes in sideblades in VR, updated vr fixtures prim, dropped DL on cabinet and backbox, plunger tweaked (needs more work).
' 101 - rothbauerw - Adjusted position, elasticity, and friction for left and right gate (and prims) and strength and scatter of plunger for better ball launch randomness. Disabled "Render backfacing transparent" for right ramp and p_plastic_rcover as it was doing some wondky stuff in VR. Updated USEVPMDMD code,  Adjusted dPosts height to 50 from 25. Adjusted idol eject (position and elasticity) for smooth feed to the right flipper. Added a check to make sure a flipper drops more than 15 degrees before flippernudge will work again.     
' 102 - Leojreimroc - VR Backglass Flashers/GI implemented.
' 103 - iaakki - Targetbouncer code updated, sleeve bounces reduced, some off prims had incorrect settings, wireramps tuned one more time, gi fade speed change
' 104 - Sixtoe - Fixed sideblades, realigned some gi edges to cabinet size, split left difficulty post and made it higher, tweaked the I insert to fix it in VR.
' RC1 - iaakki - removed subway helper, added pf mesh, revised default options, ruins fixed, updated wireramp images from Tomate
' RC1.1 - iaakki - changed how images are named, one not used image removed
' RC2 - apophis - Adjusted subway geometry. Cut alpha holes for POA EB and Pit inserts. Corrected some ambient ball shadow code.
' RC3 - iaakki - minor adjustment to RubberPost_Prim001, right outlane difficulty reworked, "start mode" kicker angle fixed, p_col_metalspostsOFF DL adjusted
' RC4 - Sixtoe - Split planes and blimp and added option to turn off blimp (currently doesn't work as flashers turn it back On), lightened p_sw38 as it was a bit too dark, added glow flashers to l22/23/24 DT's, modified totemramp and fixed broken sw71, tweaked the left orbit fractionally wider to stop catches, tweaked ramp switches, fixed minipf textures not having light cutsouts for flashontextures, 
' RC5 - iaakki - ruinsoff was not set properly in collections, adjusted various default DL values and they are now all in script, added code that alters metalsposts material color for flashers, blimptoy disabled by default
' RC6 - Leojreimroc - VR backglass flasher adjustments
' RC7 - Sixtoe - Dropped wall100 to 50 to avoid ramp, added sw31_prot above sw31 and dropped hit height to 16, added DL to quite a few things, set DLFB to 1 for metal ramps
' RC8 - iaakki - RCover brightness tweak, dome off state tweak, POA arrow inserts transparency fix with wall005 and wall005
' RC9 - Sixtoe - Added desktop scoretext and basic desktop/fss backdrop, script cleanup, made VR min room non-static, change flipper return strength from 0.055 to 0.048 and EOSReturn in the script from 0.025 to 0.018, added proper drop through for ball lock hole and rebuilt whole area and including subway, changed sw45 from kicker to switch and merged subwayenter, added collidable prim scoop to catch ball, reprofiled entrance as it was the wrong shape, added sw31_floor
'v1.0 Release.
'1.01 - iaakki - Rubberizer updated, Sling SSF sounds fixed, environment emis. scale increased, top INDY insert reflections reworked
'1.02 - apophis - LUT changer and new ramp textures from Tomate
'1.03 - iaakki - Rubberizer rework to make bounce dynamix to collision force
'1.04 - Wylte - Fixed left triangle insert bloom light, added _SuperBallD and Calle_MRBALL ball images for testing/comparison, deleted excess shadow objects, changed default visual settings to just SMAA
'1.05 - iaakki - ball adjusted, Rothbauerw updates to rubberizer
'v1.1 Release
