'***********************
'* Scared Stiff for VPX
'* By Shoopity
'* With HEAVY borrowing from Dozer's Mod of JPSalas' table
'***********************
' Shoopity, Hauntfreaks, ICPJuggla, nFozzy, Arngrim, Clark Kent, Mikcab, leojreimroc, TastyWasps
' Ramp Textures by Flupper1
' Flare flasher image by LoadedWeapon
' New DT Backdrop by Batch
' EOStimer script based on LFHM by WRD1972 and Rothbauer
' nFozzy Physics, Fleep sounds and modern updates by Mikcab - 02/23
' VR Backglass added by leojreimroc - 03/23
' VR Room(s) added to make hybrid by TastyWasps - 03/23 (Thanks to Senseless for Pool Hall VR models)

Option Explicit
Randomize

'------ OPTIONS -------
'=======================

'To change GI - Press right magnsave with left magnasave held down
'Change Scorecard - Press Left magnasave with right magnasave held down
'LED skull mod - Press right Flipper with left magnasave held down

'---- Show Alternate Sidewalls ----
Dim sidewalls
sidewalls = 0	'(0 = default, 1 = custom 1, 2 = custom 2, 3 = custom Scared Stiff) 

'----- General Sound Options -----
Const VolumeDial = 0.8				'Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Const BallRollVolume = 0.5 			'Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 0.5 			'Level of ramp rolling volume. Value between 0 and 1

'----- VR Room Auto-Detect -----
Dim VR_Obj, VR_Room

VR_Room = 1	' 1 = Pool Hall 2 = Minimal Room					  

If RenderingMode = 2 Then

	For Each VR_Obj in Cabinet : VR_Obj.Visible = 0 : Next
	For Each VR_Obj in VRCabinet : VR_Obj.Visible = 1 : Next
	Table1.PlayfieldReflectionStrength = 10

	If VR_Room = 1 Then
		For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 0 : Next
		For Each VR_Obj in VRPoolHall : VR_Obj.Visible = 1 : Next
		If sidewalls <> 0 Then ' sidewalls option recommended to be 0 for correct cab sizing.
			PinCab_Blades.Visible = 0
		End If
		For Each VR_Obj in VRSpiderFlashers : VR_Obj.Visible = 1 : Next
		For Each VR_Obj in DTSpiderFlashers : VR_Obj.Visible = 0 : Next
	Else
		For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 1 : Next
		For Each VR_Obj in VRPoolHall : VR_Obj.Visible = 0 : Next
		If sidewalls <> 0 Then ' sidewalls option recommended to be 0 for correct cab sizing.
			PinCab_Blades.Visible = 0
		End If
		For Each VR_Obj in VRSpiderFlashers : VR_Obj.Visible = 1 : Next
		For Each VR_Obj in DTSpiderFlashers : VR_Obj.Visible = 0 : Next
	End If

Else
	For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 0 : Next
	For Each VR_Obj in VRPoolHall : VR_Obj.Visible = 0 : Next
	For Each VR_Obj in VRSpiderFlashers : VR_Obj.Visible = 0 : Next
	For Each VR_Obj in DTSpiderFlashers : VR_Obj.Visible = 1 : Next
End If



dim UseVPMColoredDMD : UseVPMColoredDMD = 1
Dim  SoundLevelMult, SoundLevelMultCoils, LutFading

'*******************************************
'  Constants and Global Variables
'*******************************************
Const BallSize = 50					'Ball size must be 50
Const BallMass = 1					'Ball mass must be 1
Const tnob = 5 ' total number of balls

Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height

Sub InitSideWalls
	Select Case sidewalls
		Case 0
			Primary_Sidewalls.image= "Cab_sidewalls"
		Case 1
			Primary_Sidewalls.image= "Cab_sidewalls_custom_1"
		Case 2
			Primary_Sidewalls.image= "Cab_sidewalls_custom_2"
		Case 3
			Primary_Sidewalls.image= "Cab_sidewalls_custom_3"
			' Backwall.image="Cab_background_custom" 03/22/23 - TastyWasps - Image does not exist in Image Manager
	End Select
End Sub

InitSideWalls()
'===========================

'Rom select - uncomment one
'Const cGameName = "SS_01" 'prototype rom with kickback 'works, missing a few lamps though
Const cGameName = "SS_15" 'latest rom

'Table SFX multiplier - may cause some normalization
'make sure Table Sound Effect Volume (under Table Properties) is already at 100 before increasing this
SoundLevelMult = 1		'Physical collision volume
SoundLevelMultCoils = 1	'Coil Volume (Max safe value 1.11)

'Single-Screen FS support (Puts spider on the playfield) Default 0
const SingleScreenFS = 0

'LUT shifting when GI is off (default False)
LutFading = False 

'Floating Text Mod (May not work with B2S. Default False)
Const UseVPMNVRAM = 0

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0
const UseVPMModSol=true
LoadVPM "01530000", "WPC.VBS", 3.56

Const UseSolenoids = 1
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0
Const HandleMech = 1
Const SCoin = "fx_Coin"

If RenderingMode <> 2 Then
	Lockdownbar18.Visible = Table1.ShowDT
End If

Lbolt1.visible = 0 : Lbolt2.Visible = 0 : L35r.visible = 0 : Lcandle1.visible = 0 : Lcandle2.visible = 0
dim Proto : Proto = False
If cGameName = "SS_01" then 	'preproduction aux lamp board is not fully supported by pinmame
	'tb.text = "prototype running..."
	MetalProto.Visible = True
	MetalProto.Collidable = True
	Proto = True

	'get rid of left post
	Col_Rubber_LeftAdjust.Collidable = False
	Post_Adjustable1_Rubber.Visible = False
	Post_Adjustable1.visible = False

	'Disable Skull Flashers
	F21f.visible = 0
	f25f.visible = 0
	F20n.Visible = 0	'and Bolt flasher
	'Enable Prototype Lamps
	Lbolt1.visible = 1 : Lbolt2.Visible = 1 : L35r.visible = 1	'bolt lamps, and new metal wall reflection
	Lcandle1.visible = 1 : Lcandle2.visible = 1 'backglass???
End If

'****************
'	Key inputs
'****************

dim CardTex : CardTex = True  'False = Default paper (Custom ScoreCards)
Sub ToggleCard() 
	CardTex = Not CardTex : ScoreCardDT.Image = abs(cInt(CardTex))+1 & "ScoreCard" :ScoreCardFS.Image = ScoreCardDT.Image: PriceCard.Visible = CardTex 
	if Not CardTex then plunger.image = "custom_ss_plunger" else plunger.image = "CustomWhiteTip" end If
End Sub
dim SkullLEDsequence 'stops rom control briefly
Sub ToggleLED() : if Proto then exit sub : end if : if not cBool(FlSkull4_1.State) then ToggleLEDsOn 1 else ToggleLEDsOn 0 end If :End Sub
Sub ToggleLEDsOn(aOn) 
	dim x: If aOn Then
		for each x in SkullLEDs : x.IntensityScale = 12 : x.State = 2: Next : SkullLEDsequence = True
		Playsound "BSDLaser", 0, LVL(0.1), PanX(800),0, 0,0,0,FadeY(81) : VPMtimer.Addtimer 560, "ColStatesOn SkullLEDs, 1'"
	Else
		for each x in SkullLEDs : x.IntensityScale = 0.5 : x.State = 1: Next : SkullLEDsequence = True
		 : VPMtimer.Addtimer 80, "ColStatesOn SkullLEDs, 0'"
		Playsound "BSDdown", 0, LVL(0.1), PanX(800),0, 0,0,0,FadeY(81)
	end If
End Sub
'For skull mod toggle
Sub ColStatesOn(aCol, aOn) :SkullLEDsequence=False: dim x : for each x in aCol : x.State = aOn :x.IntensityScale = Lampz.OnOff(x.UserValue) :Next : End Sub

dim catchinput(1)

Dim BIPL : BIPL=0
Sub Table1_KeyDown(ByVal keycode)
	If Keycode = StartGameKey then Controller.Switch(13) = 1

    If keycode = PlungerKey Then 
		Plunger.Pullback
		SoundPlungerPull()
		TimerPlunger.Enabled = True
		TimerPlunger2.Enabled = False
	End If

	'if keycode = 200 then TestAnims : modlampz.state(35) = abs(Not cBool(ModLampz.state(35)))	'test uparrow
	if KeyCode = KeyRules then ShowCard True

	If keycode = LeftFlipperKey Then
		FlipperActivate LeftFlipper, LFPress
        vpmFlipsSam.FlipL True
		PinCab_LeftFlipperButton.X = PinCab_LeftFlipperButton.X + 10
		'SolLFlipper True						'This would be called by the solenoid callbacks if using a ROM
	End If

	If keycode = RightFlipperKey Then
		FlipperActivate RightFlipper, RFPress
        vpmFlipsSam.FlipR True: if CatchInput(0) then ToggleLED
		PinCab_RightFlipperButton.X = PinCab_RightFlipperButton.X - 10
		'SolRFlipper True						'This would be called by the solenoid callbacks if using a ROM
	End If

'	if KeyCode = LeftFlipperKey then vpmFlipsSam.FlipL True
'	if KeyCode = RightFlipperKey then vpmFlipsSam.FlipR True: if CatchInput(0) then ToggleLED

	If keycode = LeftTiltKey Then nfNudge -1, 10	'only integers are region safe (halves so min=2, use even numbers)
	If keycode = CenterTiltKey Then nfNudge 0,10
	If keycode = RightTiltKey Then nfNudge 1, 10
	if keycode = LeftMagnaSave then catchinput(0) = True : If catchinput(1) = True then ToggleCard : SFXt 51
	if keycode = RightMagnaSave then CatchInput(1) = True : if catchinput(0) and modlampz.LVL(2) > 0 then 	GIc0.ChangeGI : GIc1.ChangeGI : GIc2.ChangeGI : SFXt 52
	If vpmKeyDown(keycode) Then Exit Sub
End Sub


'-----nudge script-----
dim LastNudge 'Overwrite overlapping nudges
'SetLocale(1033) 'for nudge script (only necessary if decimals are used here)
Sub nfNudge(aDir,Strength)
	LastNudge = gametime 'debug 
	dim steps: steps = 3
	nfDoNudge aDir, Strength, 1, steps, GameTime
	dim str : str = "nfDoNudge " & aDir & ", "& strength/2 & ", " & "-1," & steps & "," & Gametime &"'"
	vpmtimer.addtimer 250, str '	'Step 2: reverse direction with half strength
End Sub

sub nfDoNudge(aDir, Strength, Invert, aStep, NudgeID)
	if LastNudge <> NudgeId then 
		'debug.print "Cancelled nudge " & nudgeid
		exit Sub
	end If
	dim b: if aDir = 0 then
		for each b in getballs : if not NudgeOutOfBounds(b) then b.Vely = b.Vely + strength/100 * Invert end if : next
	else
		for each b in getballs : if not NudgeOutOfBounds(b) then b.Velx = b.VelX + strength/100 * aDir * Invert end if : next
	end if
	dim str : str = "nfDoNudge " & aDir & ", "& strength & ", " & invert & "," & aStep-1 & "," & NudgeID & "'"
	'debug.print LastNudge &": " & str
	if aStep > 0 then vpmtimer.addtimer 40, str
End Sub

Function NudgeOutOfBounds(byval aBall) 'Balls in kickers will accumulate fake velocity and start rolling sounds.
	if aBall.x > 730 and aball.y > 2100 or aball.ForceReflection then NudgeOutOfBounds = True
End Function
'---------------------

Sub Table1_KeyUp(ByVal keycode)
	If Keycode = StartGameKey then Controller.Switch(13) = 0
	if KeyCode = KeyRules then ShowCard False

    If keycode = PlungerKey Then 
		Plunger.Fire
		TimerPlunger.Enabled = False
		TimerPlunger2.Enabled = True
		PinCab_Shooter.Y = -215.78

                If BIPL = 1 Then
                        SoundPlungerReleaseBall()                        'Plunger release sound when there is a ball in shooter lane
                Else
                        SoundPlungerReleaseNoBall()                        'Plunger release sound when there is no ball in shooter lane
                End If
		if BallInPlunger then SFXt 22 else SFXt 23 end If
	End If

'	if KeyCode = LeftFlipperKey then vpmFlipsSam.FlipL False' : vpmFlipsSam.FlipUL False
'	if KeyCode = RightFlipperKey then vpmFlipsSam.FlipR False' : vpmFlipsSam.FlipUR False
	 
	if keycode = LeftMagnaSave then catchinput(0) = False
	if Keycode = RightMagnaSave then catchinput(1) = False


	If keycode = LeftFlipperKey Then
        vpmFlipsSam.FlipL False' : vpmFlipsSam.FlipUL False
		FlipperDeActivate LeftFlipper, LFPress
		PinCab_LeftFlipperButton.X = PinCab_LeftFlipperButton.X - 10
		'SolLFlipper False						'This would be called by the solenoid callbacks if using a ROM
	End If

	If keycode = RightFlipperKey Then
        vpmFlipsSam.FlipR False' : vpmFlipsSam.FlipUR False
		FlipperDeActivate RightFlipper, RFPress
		PinCab_RightFlipperButton.X = PinCab_RightFlipperButton.X + 10
		'SolRFlipper False						'This would be called by the solenoid callbacks if using a ROM
	End If

	If vpmKeyUp(keycode) Then Exit Sub
End Sub



'****************
'	Table Init
'****************

Dim bsTrough, bsCoffin, bsLeftKick, bsSpider, WheelMech, IMAutoPlunger
Dim UseMech, FSSpiderenabled

' Init table
Sub table1_Init()
	vpmInit Me
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game: " & cGameName & vbNewLine & Err.Description:Exit Sub
		.Games(cGameName).Settings.Value("rol") = 0 'rotated to the left
		.HandleMechanics = UseMech	'Empty value
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.ShowTitle = 0
		.Hidden = 0
		'.SetDisplayPosition 0, 0, GetPlayerHWnd   'uncomment this line If you don't see the vpm window
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0
	End With
	Controller.Switch(22) = 1 ' coin door closed...
	Controller.Switch(24) = 1 ' and keep it closed
	Controller.Switch(48) = 1 ' turn on the coffin diode

    vpmNudge.TiltSwitch = 14
    vpmNudge.Sensitivity = 1'0.25
    vpmNudge.TiltObj = Array(bumper1, bumper2, bumper3, LeftSlingshot, RightSlingshot)

	RandomBoogieColors	'Initialize Boogieman colors 
	InitAnimations	'Initialize Keyframe Animations
	InitLampsNF		'Initialize Lamp Assignments
	InitFloatingText'Initialize Floating Score Objects
	FlasherPlacement 'some dirty script based flasher placements
	DetectOldScripts 'Check for outdated core.vbs
	
	set bsTrough = new cvpmBallStack	' Main Ball Trough
	With bsTrough
		.InitSw 0, 32, 33, 34, 35, 0, 0, 0
		.InitKick BallRelease, 90, 6
		.Balls = 4
	End With

	set bsCoffin = new cvpmBallStack	'Coffin
	With bsCoffin
		.InitSw 0, 41, 42, 43, 0, 0, 0, 0
		.InitKick CoffinKicker, 170, 5
	End With

	Set bsSpider = New cvpmBallStack	'Spider
	bsSpider.InitSw 0, 36, 0, 0, 0, 0, 0, 0
	bsSpider.InitKick sw36, 202, 40		'202, 35
	bsSpider.KickZ = 95

	Set bsLeftKick = New cvpmBallStack	'Left Kickout
	bsLeftKick.InitSw 0, 37, 0, 0, 0, 0, 0, 0
	bsLeftKick.InitKick sw37, 91, 63	'91,60
	bsLeftKick.KickZ = 60	'80
	bsLeftKick.KickForceVar = 8	'5
	bsLeftKick.KickAngleVar = 0.1	'0

	sw37_dropwall.isdropped = 1	'init crate

	Set IMAutoPlunger = New cvpmImpulseP	'Autoplunger
	With IMAutoPlunger
		.InitImpulseP Sw18, 38, 0.4
		.Random 0.6
		.CreateEvents "IMAutoPlunger"
		.Switch 18
	End With

	'Wheel mech
	Set WheelMech = New cvpmmech'cvpmMech
	With WheelMech
		.MType = vpmMechStepSol + vpmMechCircle + vpmMechLinear + vpmMechFast
		.Sol1 = 39
		.Sol2 = 40
		.Length = 200
		.Steps = 48
		.CallBack = GetRef("UpdateWheel")
		.AddSw 12, 0, 0
		.Start
	End With

	'Wheel Placement (DT / SSFS)
	dim x, a, a2, str
	if Table1.ShowDT then
		If Renderingmode = 0 Then
	'		flspiderback.visible = 0
			WheelPlacer l82, -90.1
			WheelPlacer l83, -67.5
			WheelPlacer l64, -45
			WheelPlacer l65, -22.5
			WheelPlacer l66, 0
			WheelPlacer l67, 22.5
			WheelPlacer l68, 45
			WheelPlacer l71, 67.5
			WheelPlacer l72, 90
			WheelPlacer l73, 112.5
			WheelPlacer l74, 135
			WheelPlacer l75, 157.5
			WheelPlacer l76, 179.9
			WheelPlacer l77, -157.5
			WheelPlacer l78, -135
			WheelPlacer l81, -112.5
			FSSpiderenabled = False
		End If
	Elseif SingleScreenFS Then
	'		FSSpiderenabled = True
	'		if not table1.showdt then ShowSpider 0
	'		FLspiderback.visible = 1
	'		FLspider.visible = 1
	'		a= Array(l82,l83,l64,l65,l66,l67,l68, l71,l72,l73,l74,l75,l76,l77,l78,l81)
	'		a2= Array(fs1,fs2,fs3,fs4,fs5,fs6,fs7, fs8,fs9,fs10,fs11,fs12,fs13,fs14,fs15,fs16)
	'		for x = 0 to uBound(a)
	'			str = a2(x).x & "->" & a(x).x
	'			MatchObj a(x), a2(x)	'move the lamps to the center of the playfield
	'		Next
	'		For each x in a
	'			x.RotX = 0 : x.RotZ = 0 : x.RotY = 0
	'		Next
	'
	'		FlSpider.x = FlSpider1.x : FlSpider.y = FlSpider1.y
	'		FlSpider.Height = 166
	'		FlSpider.RotX = 0 : FlSpider.RotZ = 0 : FlSpider.RotY = 0
	'		'showspider 1 'debug turns on spider to start
	End If

	'Init Backglass 
	dim aBackglass : aBackglass = Array(Backglass1, Backglass2, SpeakerPanel)
	for each x in aBackglass : x.x = ItV(10.125) : x.y = ITV(-2.57194188) : x.z = ITV(20.95034488) : x.rotx = x.rotx + 5 : next 
	dmd.height = 582 : dmd.y=itv(-1.45) : dmd.rotx = -85 : dmd.x=ItV(10.125)
	
	If RenderingMode = 2 Then
		SpiderFS.x = ItV(10.2)  : SpiderFS.y = ItV(-3) : SpiderFS.z = ItV(29.5) : SpiderFS.Rotx = 85
	Else
		SpiderFS.x = ItV(10.2)  : SpiderFS.y = ItV(-3) : SpiderFS.z = ItV(28.5) : SpiderFS.Rotx = 85
	End If

	'-----------VPReg.stg saving ---------
	'String input. Sets Name of game in VPReg.stg. IE 'SpaceStationNF'
	gic0.name = "ScaredStiffVPX" : gic1.name = gic0.name : gic2.name = gic0.name	'GI colors
	gic0.Value = "GIcolor0" 		'Value (Public) 'String input. Sets Key for color in VPReg.stg.IE 'GIcolor'
	gic1.Value = "GIcolor1" 		'Value (Public) 'String input. Sets Key for color in VPReg.stg.IE 'GIcolor'
	gic2.Value = "GIcolor2" 		'Value (Public) 'String input. Sets Key for color in VPReg.stg.IE 'GIcolor'
	gic0.LoadColorsIDX:	gic1.LoadColorsIDX	: gic2.LoadColorsIDX

	'Init Instructions / Price Card Type
	dim tmp  : tmp = LoadValue(gic0.name, "CustomCards")	'1 = custom cards, 0 = regular
	If IsNumeric(tmp) then 
		if cInt(tmp) then ToggleCard : end If
	end If

	'Init Skull LEDs
	tmp = LoadValue(gic0.name, "SkullLEDMod")	'1 = LED skull mod. 0 = No LEDS in skulls
	If IsNumeric(tmp) then 
		if cInt(tmp) and Not Proto then 
			for each x in SkullLEDs : x.state = 1 : Next
		end If
	end If
	'-------------------------

End Sub

Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_UnPaused:Controller.Pause = 0:End Sub
Sub Table1_exit():SaveLED:SaveCards: gic0.SaveColorsIDX :gic1.SaveColorsIDX :gic2.SaveColorsIDX : Controller.Pause = False:Controller.Stop:End Sub 'Save color to VPReg.stg

Sub SaveCards() : dim tmp : tmp = abs(Not PriceCard.Visible) : SaveValue gic0.name, "CustomCards", tmp : End Sub
Sub SaveLED(): If Proto then Exit Sub : End If: dim tmp : tmp = abs(FlSkull6_2.State) : SaveValue gic0.name, "SkullLEDMod", tmp : End Sub 

function ItV(aInch) : ItV = aInch*(800/17) : End Function 'inch to vpu
Sub MatchObj(aObjSrc, aObjDest) : aObjSrc.X = aObjDest.x : aObjSrc.y = aObjDest.y : aObjSrc.height = aObjDest.height : end Sub

dim nullvar	'empty is probably not region safe
Sub DetectOldScripts
	On Error Resume Next
	err.clear
	dim tmp: set tmp = GetRef("NullFunction")
	If Err then 
		tb.text = "Your VPX scripts are out of date"
		vpmtimer.addtimer 5000, "tb.text=nullvar'" 'clear textbox and continue
	end If
	On Error Goto 0
End Sub

Sub RandomBoogieColors()	'Init boogey colors
	dim a, x, x2
	a = Array("boogin_Green", "Boogin_Red", "Boogin_Purple", "Boogin_Blue", "Boogin_Yellow")
	x = cInt(rndnum(0, uBound(a))	)
	x2 = x

	x = x + rndnum(1, uBound(a)	)
	if x > uBound(a) then x = x - uBound(a)
	if x = x2 then	'try again if the boogies are the same
		RandomBoogieColors 'bad idea
		Exit Sub
	End If

	Boogie1.Image = a(x2) : BoogieArms1.Image = a(x2)
	Boogie2.Image = a(x) : BoogieArms2.Image = a(x)
'	tb.text = boogie1.image & vbnewline & boogie2.image
End Sub

Sub WheelPlacer(object, angle)
	Dim a, b, rad
	rad = (angle/180)*Pi												'Converts radians into degrees
	a=220'a = FlSpider.SizeX/2+75												'Set the width of the ellipse/circle
	b=220'b = FlSpider.SizeX/4+75												'Set the height of the ellipse
	object.RotX = FlSpider.RotX											'First, rotate to face the player
	object.X = FlSpider.X + (a)*dCos(angle)								'The icon's X coordinate is based off the spider's center and the angle around it (3 o'clock being 0 degrees, noon being -90, 6 o'clock +90, etc.)
	object.Y = FlSpider.Y + ((a)*(dCos(FlSpider.RotX)))*dSin(angle)		'The Y coord is based off both the angle of the clock as well as the angle of table inclination
	object.Height = FlSpider.Height + a*-dSin(angle)					'The Z coord is based off just the angle
End Sub

Dim lednr, np
Sub UpdateWheel(aNewPos, aSpeed, aLastPos)
	DOF 101, DOFPulse
	np=aNewPos+12:If np>47 Then np=np-48
	lednr=int(np/4.8)
	if lednr>4 then lednr=lednr-5
	DOF 201+lednr, DOFPulse

	if FSSpiderenabled then
		if bsSpider.Balls = 1 and uBound(getballs) = -1 then ShowSpider 1
	end if
'	dim temp : if IsObject(bsSpider) then temp = bsspider.balls
'	tb.text = aNewPos & " " & aspeed & " " & aLastPos & vbnewline & _
'			"bip " & bip & " bsSpider.Balls:" & temp
End Sub

Sub FlasherPlacement() 'dirty script based flasher placement
	L35r.x = 22.75	'prototype kickback only
	L35r.y = 1584.5
	L45r.x = 864
	L45r.y = 1533.3037335

	FlSkull2_5.bulbhaloheight =34.7'28
	FlSkull2_6.bulbhaloheight =35.1'28
	FlSkull2_5.x = 807'808.1973
	FlSkull2_6.x = 836	'838.3223

	FlSkull2_3.bulbhaloheight =33'29
	FlSkull2_4.bulbhaloheight =29'29
	FlSkull2_3.x = 837'838.39
	FlSkull2_4.x = 864	'860.765

	FlSkull2_1.bulbhaloheight =32'28
	FlSkull2_2.bulbhaloheight =33	'30
	FlSkull2_1.x = 780'782.3
	FlSkull2_2.x = 804	'806.785

	FlSkull5_1.bulbhaloheight =	31.5'28
	FlSkull5_2.bulbhaloheight =	31.5'28
	FlSkull5_1.x =	805.5'807.7266

	FlSkull4_1.bulbhaloheight =	29'28
	FlSkull4_2.bulbhaloheight =	29'28
	FlSkull4_1.x =	743'743.7
	FlSkull4_2.x =	772.2'774.3
	FlSkull4_1.y =	81'86.609

	f19f.x = 854'853.3457
	f19f.y = 697'696.2
	f18f.x = 703'695
	f18f.y = 555'550.6

	f17.x = 858.2477164
	f17.y = 355.0953586
	f18.x = 712.9653396
	f18.y = 502.0359103
	f19.x = 858.3208126
	f19.y = 648.2506583

	f17f.x = 848'850.5333
	f17f.y = 420'424.1581

	f23.x = 215'195
	f23.y = 600'527

	f27side.x = -17.6	'sidewalls
	f24side.x = f27side.x : f35side.x = f27side.x
	f22side.x = 970.5
	f28side.x = f22side.x : f36side.x = f22side.x

	Bumperw.x = 970.5
	Bumperw.y = 525
	Bumperw.Height = 155

	f27side.y	   = 164.7058	'sidewalls L
	f27side.height = 198.2352
	f24side.y	   = 905.1276
	f24side.height = 155.195
	f35side.y	   = 1438.183
	f35side.height = 155.651

	f22side.y	   = 235.294	'Sidewalls R
	f22side.height = 234.294
	f28side.y	   = 815.1855836
	f28side.height = 152.135
	f36side.y	   = 1461.3333459
	f36side.height = 155.647

	'ambient flashers
	f27a.x = 96.19271199	'left
	f27a.y = 300.65188432

	f24a.x = 59.41704729
	f24a.y = 995.53468832

	f35a.x = 43.68561293
	f35a.y = 1451.0359464

	f22a.x = 635.50467781	'right
	f22a.y = 318.84008661

	f28a.x = 888.72165828
	f28a.y = 932.63012561

	f36a.x = 836.31251062
	f36a.y = 1479.67256102

End Sub


'******************************************************************************
'     _______.  ______    __    __  .__   __.  _______      _______ ___   ___ 
'    /       | /  __  \  |  |  |  | |  \ |  | |       \    |   ____|\  \ /  / 
'   |   (----`|  |  |  | |  |  |  | |   \|  | |  .--.  |   |  |__    \  v  /  
'    \   \    |  |  |  | |  |  |  | |  . `  | |  |  |  |   |   __|    >   <   
'.----)   |   |  `--'  | |  `--'  | |  |\   | |  '--'  |   |  |      /  .  \  
'|_______/     \______/   \______/  |__| \__| |_______/    |__|     /__/ \__\ 
'                                                                             
'				All SFX stems in one big table
'******************************************************************************
Class SFXtester : public x,y,velx,vely,velz : Private Sub Class_Initialize : x=0:y=0:velx=0:vely=0:velz=0 : end Sub : End Class
dim NullBall : set NullBall = new SFXtester : NullBall.x = 429 : nullball.y = 1400

'SFX Testing environment
dim s : set s = new SFXtester
Sub SpeedHigh() :  NullBall.velx = 58 : End Sub
Sub SpeedMed() :  NullBall.velx = 27 : End Sub
Sub SpeedLow() :  NullBall.velx = 5 : End Sub
Sub sPos(ax,yy) :  NullBall.x=ax : s.y=ay: End Sub

Sub SFXt(aNR)
	select case aNr 'PlaySound- name,loopcount,volume,pan,randompitch,pitch,UseExisting,Restart,Fade
		'-Rolling (Just for testing! Handled by object 'Roll')-
'		Case 0:	PlaySound "tablerolling1",1,(VolV(NullBall)),Pan(NullBall),0,PSlope(ballspeed(NullBall),1,-1000,60,10000),1,0,Fade(NullBall)'Rolling (Table): 
		Case 1:	PlaySound "RampLoop1", 1,Vol(NullBall)^0.9,Pan(NullBall),0,PSlope(ballspeed(NullBall),1,-4000,60,7000),1,0,Fade(NullBall)	'Rolling (Ramp)
		Case 2:	PlaySound "BallDropTall", 0, 0.1, Pan(NullBall), 0, 0, 1, 0,Fade(NullBall)	'Ramp Drop (High)
		'------Simple Collisions---------------------------------------------------------------------------------------------
		Case 3:	PlaySound RandomFlipper, 0, LVL(Vol(Activeball) ), Pan(Activeball), 0, Pitch(Activeball), 1, 0,Fade(Activeball)			'Flipper Rubber Hit
		Case 4:	PlaySound RandomBand, 0, LVL(Vol(Activeball) ), Pan(Activeball)*5, 0, Pitch(Activeball),1,0,Fade(Activeball)			'Bands Hit 
'		Case 5:	PlaySound RandomPost, 0, LVL(Vol(Activeball) ), Pan(Activeball)*5, 0, Pitch(Activeball),1,0,Fade(Activeball)			'Posts /Sleeves Hit 
		Case 6:	PlaySound SoundFX("target",DOFTargets),0,LVL(Vol(Activeball)),Pan(Activeball),0,Pitch(Activeball),0,0,Fade(Activeball)	'Targets Hit
		Case 7: PlaySound SoundFX("LockupPin",DOFTargets),0, LVL(Vol(Activeball)*0.5), Pan(Activeball), 0,Pitch(Activeball),0,0,Fade(Activeball)	'Frogs Hit (Light, see 54)
		Case 8:	PlaySound "woodhitaluminium",0,LVL(Vol(Activeball)),Pan(Activeball),0,pSlope(BallSpeed(Activeball),12,12500, 23,19000),1,0,Fade(Activeball)	'Crate Door hit (Hard)
		Case 9: PlaySound "WoodHitAluminium",0,LVL(Vol(Activeball)),Pan(Activeball),0,Pitch(Activeball),1,0,Fade(Activeball)		'Apron hit
		Case 10:PlaySound "metalhit_medium",0,LVL(Vol(Activeball)),Pan(Activeball),0,Pitch(Activeball),1,0,Fade(Activeball)			'Metal hit, Light 
		Case 11:PlaySound "metalhit2", 0, LVL(Vol(Activeball)), Pan(Activeball), 0, Pitch(Activeball), 1,0,Fade(Activeball)			'Metal hit, Heavy (Inlanes and soft Crate Door Hit)
		Case 12:PlaySound "fx_collide", 0, LVL(0.05), PanX(290), 0, 0,1,0,FadeY(515)												'Collide against ball (in crate trough)
		Case 13:PlaySound "ramp_hit2", 0, LVL(Vol(Activeball)), Pan(Activeball), 0,Pitch(Activeball),1,0,Fade(Activeball)			'Ramp Entry Hit
		Case 14:PlaySound "ramp_hit1",0,LVL(Vol(Activeball)/2), Pan(Activeball),0, Pitch(Activeball), 1,0,Fade(Activeball)			'Ramp Side Hit
		Case 15:PlaySound "PlayfieldHit",0,LVL(Vol(Activeball)), Pan(Activeball), 0,Pitch(Activeball),1,0,Fade(Activeball)			'Ramp Reject PF hit
		Case 16:PlaySound "Saucer_Enter_2", 0, LVL(0.10), Pan(CoffinEntrance), 0.1,0,1,0,Fade(CoffinEntrance)		'Hole - Left Lock
		Case 17:PlaySound "Saucer_Enter_1", 0, LVL(0.10), PanX(300), 0.1,0,1,0,FadeY(515)							'Hole - Crate Kickout Hole
		Case 18:PlaySound "Trough3", 0, LVL(Vol(Activeball)	), Pan(Activeball), 0, Pitch(Activeball),1,0,Fade(Activeball)		'Hole - Crate 
		Case 19:PlaySound "Trough1", 0, LVL(0.10), PanX(790), 0.1, 0,1,0,FadeY(938)					'Hole - Right Spider Hole (Heavy)
		Case 20:PlaySound "Trough2", 0, LVL(0.10), PanX(790), 0.1, 0,1,0,FadeY(930)					'Hole - Right Spider Hole (Light)
'		Case 21:PlaySound "ball_trough", 0, LVL(0.06), Pan(Activeball), 0.1, 0,1,0,FadeY(2073)		'Hole - Outhole fall-through
		Case 22:PlaySound SoundFX("Plunger3",Empty) ,0, LVL(0.3),PanX(907),0.05, 0,1,0,FadeY(1892)			'Plunger Fire - Ball in Plunger
		Case 23:PlaySound SoundFX("plunger",Empty),0, LVL(0.3),PanX(907),0.05, 0,1,0,FadeY(1892)				'Plunger Fire - Empty
'		Case 24:PlaySound "gate4",0,LVL(Vol(Activeball)),Pan(Activeball),0,Pitch(Activeball),1,0,Fade(Activeball) 				'Gate passthrough
		'---------Coils------------------------------------------------------------------------------------------------------
		Case 25:PlaySound SoundFX("DiverterLeft_Open",DOFcontactors),0,LvLC(0.7),PanX(130),0.1, 0,0,0,FadeY(532)	'Left Diverter Up
		Case 26:PlaySound SoundFX("DiverterLeft_Close",DOFcontactors),0,LvLC(0.21),PanX(130),0.1, 0,0,0,FadeY(532)	'Left Diverter Down
		Case 27:PlaySound SoundFX("Kicker_Release",DOFcontactors),0,LvLC(0.7),PanX(85),0.1, 0,0,0,FadeY(1156)	'Cadaver Popper
		Case 28:PlaySound SoundFX("Saucer_Kick",DOFcontactors),0,LvLC(0.7),PanX(88),0.1,0,0,0,FadeY(1156)		'Cadaver Popper Empty
		Case 29:PlaySound SoundFX("TedLids_Up",DOFcontactors),0,LvLC(0.7),PanX(85), 0,0,0,0,FadeY(1150)	'Cadaver Door Up
		Case 30:PlaySound SoundFX("TedLids_Down",DOFcontactors),0,LvLC(0.21),PanX(85),0,0,0,0,FadeY(1150)	'Cadaver Door Down
		Case 31:PlaySound SoundFX("Kicker_Release",DOFcontactors),0,LvLC(0.5),PanX(293),0.1,0,0,0,FadeY(510)	'Crate Kickout 
		Case 32:PlaySound SoundFX("LeftEject",DOFcontactors),0,LvLC(0.51),PanX(294),0.1,0,0,0,FadeY(510)		'Crate Kickout Empty
		Case 33:PlaySound SoundFX("",DOFcontactors),0,LvLC(0.41), Panx(Bumper1.x), 0.1,0,0,0,Fadey(Bumper1.y)		'Bumper1
		Case 34:PlaySound SoundFX("",DOFcontactors),0,LvLC(0.41), Panx(Bumper2.x),0.1,0,0,0,Fadey(Bumper2.y)		'Bumper2 
		Case 35:PlaySound SoundFX("",DOFcontactors),0,LvLC(0.41),Panx(Bumper3.x),0.1,0,0,0,Fadey(Bumper3.y)		'Bumper3 
		Case 36:PlaySound SoundFX("Kicker_Release",DOFcontactors),0,LvLC(0.7), Panx(sw36.x), 0.1,0,0,0,FadeY(931)	'Spider Hole Popper
		Case 37:PlaySound SoundFX("LeftEject",DOFcontactors),0,LvLC(0.51),PanX(790),0.1,0,0,0,FadeY(931)		'Spider Hole Popper Empty
		Case 38:PlaySound SoundFX("Ball Launch",DOFcontactors), 0, LvLC(0.81),PanX(50),0.05,0,0,0,FadeY(1832)	'Kickback Fire (Prototype)
'		Case 39:PlaySound SoundFX("LeftSlingShotTrimmed",DOFcontactors),0,LvLC(0.28),PanX(358),0.2,0,0,0,FadeY(661)	'Top Slingshot
'		Case 40:PlaySound SoundFX("LeftSlingShotTrimmed",DOFcontactors),0,LvLC(0.61),PanX(283),0.2,0,0,0,FadeY(1620)		'Left Slingshot 
'		Case 41:PlaySound SoundFX("RightSlingShot",DOFcontactors), 0, LvLC(0.61), PanX(616), 0.2,0,0,0,FadeY(1620)			'Right Slingshot 
'		Case 42:PlaySound SoundFX("FlipperUpLeft",DOFFlippers), 0, LvLC(0.9), -0.0375, 0.1,0,0,0,FadeY(1620)	'Left Flipper Up
'		Case 43:PlaySound SoundFX("FlipperDown",DOFFlippers), 0, LvLC(0.01), -0.0375, 0.1,0,0,0,FadeY(1810)	'Left Flipper Down
'		Case 44:PlaySound SoundFX("FlipperUpLeft",DOFFlippers), 0, LvLC(0.9), 0.0375, 0.1,0,0,0,FadeY(1810)		'Right Flipper Up
'		Case 45:PlaySound SoundFX("FlipperDown",DOFFlippers), 0, LvLC(0.01), 0.0375, 0.1,0,0,0,FadeY(1810)			'Right Flipper Down
		Case 46:PlaySound SoundFX("BallReleaseRS",DOFcontactors), 0, LvLC(0.71), PanX(800), 0.1,0,0,0,FadeY(1876)		'Trough Popper
		Case 47:PlaySound SoundFX("LeftEject",DOFcontactors),0,LvLC(0.51),PanX(837),0.1,0,0,0,FadeY(1876)			'Trough Popper Empty
		Case 48:PlaySound SoundFX("Kicker_Release",DOFcontactors), 0, LvLC(0.61), PanX(900),0.05,0,0,0,FadeY(1837)	'Autoplunger
		Case 50:'N/A - plays Plunger3 or Plunger depending									'Autoplunger Empty

		'------------Misc SFX-----------
		Case 51:PlaySound "BSDwhop", 0, LvLC(0.08) 			'Special - Toggle Table Options
		Case 52:PlaySound "fx_relay_on", 0, LvLC(0.7),0,0.05	'Special - Toggle Table GI type
'		Case 53:PlaySound SoundFx("PlungerPull",Empty),0,LVL(0.05),PanX(900),0.05, 0,1,0,FadeY(1892)	'Plunger pullback (woops)

		Case 54: PlaySound SoundFX("LockupPin",DOFTargets),0, LVL(Vol(Activeball)*2), Pan(Activeball), 0,Pitch(Activeball),0,0,Fade(Activeball)	'Frogs Hit (Center)
		'Special - Knocker		vpmSolSound SoundFX(""Knocker"",DOFKnocker)
		'Special - Insert Coin	Const SCoin = "fx_Coin"

		Case Else:Msgbox "unassigned SFX number " & aNr
	End Select
End Sub


'Special SFX procedures (RNG sounds and such)
Function RandomPost() : RandomPost = "Post" & rndnum(1,5) : End Function
Function RandomBand() : dim x : x = rndnum(1,4) : if BallVel(activeball) > 30 then RandomBand = "Rubber" & x & x else RandomBand = "Rubber" & x end If : End Function
Function RandomFlipper() : dim x : x = RndNum(1,3) : RandomFlipper = "flip_hit_" & x : End Function

'-------------------------Volume / Pitch / Pan / Fade functions-------------------------
'Function RndNum(min, max) : RndNum = Int(Rnd() * (max-min + 1) ) + min : End Function
'Function BallVel(ball) : BallVel = INT(SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2) ) : End Function
Function LVL(input) : LVL = Input * SoundLevelMult : End Function	'Collisions
Function LVLC(input) : LVLC = Input * SoundLevelMultCoils : End Function	'Coils 
'Function LVL(input) : LVL = Lvla : End Function	'debug
'Function LVLc(input) : LVLc = Lvla : End Function	'debug

'Function Vol(ball) : Vol = Csng(BallVel(ball) ^2 / 3000) : End Function
'Function Vol(ball) : Vol = (BallVel(ball) / 45)*0.1 : End Function
'Function Vol2(ball1, ball2) : Vol2 = (Vol(ball1) + Vol(ball2) ) / 2 : End Function

'Function Pitch(ball) : Pitch = BallVel(ball) * 20 : End Function
Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. Object input.
    Dim tmp : tmp = ball.x * 2 / Table1.width-1
    If tmp> 0 Then Pan = Csng(tmp ^10) Else Pan = Csng(-((- tmp) ^10) ) End If
End Function
Function PanX(aX) ' Calculates the pan for a ball based on the X position on the table. X coord input
    Dim tmp : tmp = aX * 2 / Table1.width-1
    If tmp> 0 Then PanX = Csng(tmp ^10) Else PanX = Csng(-((- tmp) ^10) ) End If
End Function
Function Fade(tableobj)'Fades between front and back of the table (for surround systems or 2x2 speakers, etc).  Object input.
	Dim tmp : tmp = tableobj.y * 2 / table1.height-1
    If tmp > 0 Then Fade = Csng(tmp ^10) else Fade = Csng(-((- tmp) ^10) ) end If
End Function
Function FadeY(Y)	   'Fades between front and back of the table (for surround systems or 2x2 speakers, etc).  X-Coord input.
	Dim tmp : tmp = y * 2 / table1.height-1
    If tmp > 0 Then FadeY = Csng(tmp ^10) Else FadeY = Csng(-((- tmp) ^10) ) End If
End Function

'-------------------------Collection Hit Event SFX-------------------------

'Sub Rubbers_Hit(idx): SFXt 4: End Sub
'Sub Posts_Hit(idx) : SFXt 5 : End Sub
'Sub zCol_PostSleeves_Hit() : SFXt 5 : End Sub
'Sub Targets_Hit (idx):SFXt 6 : LswitchBall: End Sub
Sub Frogs_Hit(idx): SFXt 7 : LswitchBall: End Sub
Sub FrogsCenter_Hit(idx): SFXt 54 : LswitchBall: End Sub

'Sub ApronWall_Hit():SFXt 9 :end sub
'Sub Metals_Medium_Hit (idx) : SFXt 10 : End Sub
'Sub Metals2_Hit (idx):SFXt 11 : End Sub


Sub RampSounds_Hit(idx):SFXt 14 : end sub 'ramp hit sounds

'Sub Gates_Hit (idx) : SFXt 24 :End Sub

'--------------------------------------------------

'Debug command, test the drop sfx from weak right ramp shot 
Sub FeedReject() : drain.createball:drain.lastcapturedball.x=600:drain.lastcapturedball.y=1150:drain.kick 16, 40:End Sub


'****************
'Solendoid Callbacks
'****************

SolCallback(1) = "AutoPlunge"
SolCallback(2) = "Sol2"
Sub Sol2(enabled)	'Kickback / SolLoopGate
	if Proto then
		if enabled then 
			kickback.Fire : SFXt 38 
		else 
			Kickback.Pullback 
		End If
	Else
		SolLoopGate enabled
	end If
End Sub
kickback.pullback

SolCallback(3) = "SolSpiderPopper"
SolCallback(4) = "SolCoffinPopper"
SolCallback(5) = "SolCoffinDoor"
SolCallback(6) = "SolCrateKickout"
SolCallback(7) = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
'SolCallback(8) = "CratePostPower"	'crate flip coil. not necessary?
SolCallback(9) = "SolBallRelease"
SolCallback(10) = "SolLeftSling"
SolCallback(11) = "SolRightSling"
SolCallBack(12) = "SolBumper2"
SolCallBack(13) = "SolBumper1"
SolCallBack(14) = "SolBumper3"
SolCallBack(15) = "SolUpperSling"
SolCallback(16) = "CratePostHold"

SolCallback(33) = "LDiverterPower"
SolCallback(34) = "LDiverterHold"


'Flashers

SolModCallback(17) = "modlampz.setmodlamp 17,"	'Top Bumper Flash
SolModCallback(18) = "modlampz.setmodlamp 18,"	'Mid Bumper Flash
SolModCallback(19) = "modlampz.setmodlamp 19,"	'Bottom Bumper Flash
if not Proto then 
	SolModCallback(20) = "modlampz.setmodlamp 20," 'Bolts Flasher / Aux Board Enabled 
	SolModCallback(21) = "modlampz.setmodlamp 21," 'Bone Pile Flasher Blue / Backbox Spider (is this the motor? TODO)
End If

SolModCallback(22) = "modlampz.setmodlamp 22,"		'Upper Right Flasher
SolModCallback(23) = "modlampz.setmodlamp 23,"		'Skull Flasher
SolModCallback(24) = "modlampz.setmodlamp 24,"		'Mid Left Flasher
SolModCallback(25) = "Sol25"	'Bone Pile Flsaher #2 (White) \ SolLoopGate
Sub Sol25(value)
	if Proto then
		SolLoopGate cBool(value)
	Else
		modlampz.setmodlamp 25, value	'Bone Pile Flasher #2 (White)
	end If
End Sub

SolModCallback(26) = "modlampz.setmodlamp 26,"	'TVFlasher
SolModCallback(27) = "modlampz.setmodlamp 27,"	'Up Left Flasher
SolModCallback(28) = "modlampz.setmodlamp 28,"	'Mid Right Flasher
SolModCallback(35) = "modlampz.setmodlamp 35,"	'Bottom Left Flasher
SolModCallback(36) = "modlampz.setmodlamp 36,"	'Bottom Right Flasher

'Intermediate GI sub
Set GICallback2 = GetRef("SetGI")	'upper \ middle  \ lower \ 1 \ 2
Sub SetGI(aNr, aValue) : modlampz.SetGI aNr, aValue : End Sub

'****************
'Solenoid Callback Scripts
'****************

'Crate Door
'Trigger in front of the crate turns on/off the prim update timer
sub TiCratesw_Hit():CrateState 1:Me.Timerinterval=2600:Me.timerenabled = 1:end sub
Sub TiCratesw_Timer():CrateState 0:Me.timerenabled = 0:end sub	'disables update after an interval

sub CoffinKicker_Timer()	'starts with bscoffin.exitsol. 1500ms shut off Prcadaver tracking
	me.enabled = 0
'	TiCadaver.enabled = 0
	CadaverState 0 'FadingLevel(cCadaver) = 0
end sub


Dim LockPower, LockHold
Sub LDiverterPower(enabled)	'Lock Diverter
	LockPower = enabled
	If enabled Then
		LockFlipper.RotateToEnd
		If LockFlipper.CurrentAngle < 207 then SFXt 25
	End If
	If Not enabled AND Not LockHold Then
		LockFlipper.RotateToStart
		SFXt 26
	End If
End Sub

Sub LDiverterHold(enabled)
	LockHold = enabled
	If Not enabled AND Not LockPower Then LockFlipper.RotateToStart
End Sub

'Slingshots
Sub SolUpperSling(enabled) :If Enabled and vpmflipsSam.romcontrol Then PlayTopSlingShot : end if : End Sub
Sub SolLeftSling(enabled) : If Enabled then PlayLeftSlingShot : end If: End Sub
Sub SolRightSling(enabled) :If Enabled then PlayRightSlingShot : end If: End Sub

Sub SolCoffinPopper(Enabled)
	If Enabled Then
		If bsCoffin.Balls Then
			bsCoffin.ExitSol_On
			CadaverState 1
			CoffinKicker.TimerEnabled = 1
			CoffinKicker.TimerInterval = 1500
			SFXt 27
		Else
			SFXt 28
		End If
	End If
End Sub


Sub SolLoopGate(Enabled) : LoopGate.Open = Enabled : End Sub	'Bumper access loop gate

Sub AutoPlunge(enabled)
	If Enabled Then 
		IMAutoPlunger.Autofire : SFXt 48
		if BallInPlunger then
           SoundPlungerReleaseBall()                        'Plunger release sound when there is a ball in shooter lane
		Else
           SoundPlungerReleaseNoBall()                        'Plunger release sound when there is no ball in shooter lane
		end if
	End if
End Sub

Sub CratePostHold(Enabled)
	sw57.Collidable = Not Enabled
	CrateOpen Enabled	'modlampz(16) interpolates between gate animations
End Sub

Sub SolBallRelease(Enabled)	'trough release. Ballrelease
	If Not Enabled Then Exit Sub
	If bsTrough.Balls Then : RandomSoundBallRelease BallRelease : vpmTimer.PulseSw 31 : bsTrough.ExitSol_On : else : SFXt 47 : end If
End Sub


Dim CoffinDir
Sub SolCoffinDoor(Enabled)
	If Enabled Then
		CoffinDir = -1 : SFXt 29
	Else
		CoffinDir = 1 : SFXt 30
	End If
	CoffinState 1
End Sub

'****************
'Keyframe Animations
'****************

Dim aLeftSlingArm, aRightSlingArm, aTopSlingArm
Dim aLeftSlingShot, aRightSlingShot, aTopSlingShot, aBoogie1, aBoogie2, aBoogieRot1, aBoogieRot2

Set aLeftSlingArm = New cAnimation : Set aRightSlingArm = New cAnimation : Set aTopSlingArm = New cAnimation 
Set aLeftslingshot = New cAnimation  : Set aRightslingshot = New cAnimation  : Set aTopSlingShot = New cAnimation 
Set aBoogie1 = New cAnimation  : Set aBoogie2 = New cAnimation 
Set aBoogieRot1 = New cAnimation  : Set aBoogieRot2 = New cAnimation 

Sub InitAnimations
	dim x,a
	a = Array(aLeftSlingShot, aRightSlingShot, aTopSlingShot)	'sling rubbers
	For each x in a 
		x.AddPoint 0, 0, 0
		x.AddPoint 1, 10, 0	'wait for kicker
		x.AddPoint 2, 31, 1		'5 down
		x.AddPoint 3, 133, 1	'11 hold
		x.AddPoint 4, 233, 0	'8 Up
	Next
	aLeftSlingShot.Callback = "animLeftSlingShot"
	aRightSlingShot.Callback= "animRightSlingShot"
	aTopSlingShot.Callback	= "animTopSlingShot"
	a = Array(aLeftSlingArm, aRightSlingArm, aTopSlingArm)	'Sling Arms
	For each x in a 
		x.AddPoint 0, 0, -1
		x.AddPoint 1, 10, 0			'hit sling
		x.AddPoint 2, 31, 16		'5 down
		x.AddPoint 3, 133, 16		'11 hold
		x.AddPoint 4, 241, -1		'8 Up
	Next
	aLeftSlingArm.Callback = "animLeftSlingArm"
	aRightSlingArm.Callback= "animRightSlingArm"
	aTopSlingArm.Callback= "animTopSlingArm"
	a = Array(aBoogie1, aBoogie2)				'boogiemen
	for each x in a 
		x.AddPoint 0, 0, 0		'Syntax: Keyframe#, MS, Output value
		x.AddPoint 1, 15, 1
		x.AddPoint 2, 38, 2
		x.AddPoint 3, 220, 3
		x.AddPoint 4, 221, 0
		x.AddPoint 5, 250, 0.55
		x.AddPoint 6, 260, 0
		x.AddPoint 7, 270, 0.2
		x.AddPoint 8, 280, 0
		x.AddPoint 9, 290, 0.1
		x.AddPoint 10,300, 0
	Next
	aBoogie1.Callback = "animBoogie1"
	aBoogie2.Callback = "animBoogie2"
	a = Array(aBoogieRot1, aBoogieRot2)				'boogiemen
	for each x in a 
		x.AddPoint 0, 0, -1
		x.AddPoint 1, 10, -1	'wait for kicker
		x.AddPoint 2, 31, 16		'5 down
		x.AddPoint 3, 133, 16		'11 hold
		x.AddPoint 4, 233, -1	'8 Up
	Next
	aBoogieRot1.Callback = "animBoogieRot1"
	aBoogieRot2.Callback = "animBoogieRot2"
End Sub

'-----Wrapper Subs---------
Sub TestAnims() : PlayLeftSlingShot : vpmtimer.addtimer 250, "PlayRightSlingShot'" :  vpmtimer.addtimer 450, "PlayTopSlingShot'" : End Sub	'debug
Sub PlayLeftSlingShot() : dim x,a: a=Array(aLeftSlingArm, aLeftSlingShot, aBoogie1,aBoogieRot1): for each x in a : x.play : next : end Sub
Sub PlayRightSlingShot(): dim x,a: a=Array(aRightSlingArm, aRightSlingShot, aBoogie2,aBoogieRot2): for each x in a : x.play : next : end Sub
Sub PlayTopSlingShot() : dim x,a : a=Array(aTopSlingArm, aTopSlingShot): for each x in a : x.play : next : end Sub

'-----Keyframe Animation Callbacks ------
Sub animBoogie1(aLVL) : Boogiearms1.Showframe aLvl : End Sub
Sub animBoogie2(aLVL) : Boogiearms2.Showframe aLvl : End Sub

Sub animBoogieRot1(aLVL) : Boogie1.RotX= aLvl : BoogieArms1.RotX= aLvl : End Sub
Sub animBoogieRot2(aLVL) : Boogie2.RotX= aLvl : Boogiearms2.RotX= aLvl : End Sub

Sub animLeftSlingShot(aLVL) :Sling1.ShowFrame aLvl : End Sub
Sub animRightSlingShot(aLVL) :Sling2.ShowFrame aLvl : End Sub
Sub animTopSlingShot(aLVL) :Sling3.ShowFrame aLvl : End Sub

Sub animLeftSlingArm(aLVL) :SlingK1.rotx = aLvl : End Sub
Sub animRightSlingArm(aLVL) :SlingK2.rotx = aLvl : End Sub
Sub animTopSlingArm(aLVL) :SlingK3.rotx = aLvl : End Sub

'****************
' Lamps & Timers
'****************
dim NullFader : set NullFader = new NullFadingObject
dim modlampz : set modlampz = New DynamicLamps
dim Lampz : Set Lampz = New LampFader
Dim GIc0 : set GIc0 = New GIcolorswapper
Dim GIc1 : set GIc1 = New GIcolorswapper
Dim GIc2 : set GIc2 = New GIcolorswapper

dim FrameTime, InitFrameTime

Sub FrameTimer_Timer()
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime
	FlipperVisualUpdate				'update flipper shadows and primitives
	UpdateFlippers
	UpdateBallShadow
'	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
End Sub

Sub GameTimer_Timer()	'major script update loop
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime
	dim a, x, chglamp
	chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		next
	End If
'	Roll.Update
	Cor.Update 						'update ball tracking (this sometimes goes in the RDampen_Timer sub)
	RollingUpdate					'update rolling sounds
	FastFlipsUpdate
	modlampz.Update2
	Lampz.Update2

'	UpdateFlippers
'	UpdateBallShadow

	UpdateCoffin	'	previously cCoffin 205
	UpdateCadaver	'	previously cCadaver 206
	UpdateCrate		'	previously cCrate 207

	a = Array(aLeftSlingArm, aRightSlingArm, aTopSlingArm, aLeftslingshot, aRightslingshot, aTopSlingShot, aBoogie1, aBoogie2, aBoogieRot1, aBoogieRot2)
	for each x in a : x.update2 : Next	'update all keyframe animations

	if UseVPMNVRAM then FloatingTextUpdates

End Sub

dim CoffinUpdateState, CadaverUpdateState, CrateUpdateState '---
Sub CoffinState(aBool) : CoffinUpdateState = abs(aBool) : End Sub
Sub CadaverState(aBool) : CadaverUpdateState = abs(aBool) : End Sub
Sub CrateState(aBool) : CrateUpdateState = abs(aBool) : End Sub

Sub UpdateCoffin()	'gametimer	previously cCoffin 205
	if CoffinUpdateState then 
			PrCoffinLid.RotY = PrCoffinLid.RotY + ((1.5*FrameTime	)*CoffinDir)	'adjust speed here
			If PrCoffinLid.RotY <= -110 Then
				CoffinUpdateState = 0
				PrCoffinLid.RotY = -110
			ElseIf PrCoffinLid.RotY > 0 Then
				CoffinUpdateState = 0
				PrCoffinLid.RotY = 0
			End If
	End if
End Sub

sub UpdateCadaver()	'previously cCadaver 206
	If CadaverUpdateState then 
		PrCadaver.RotX = CadaverSpinner.CurrentAngle - 30 
	end if
End Sub

Sub UpdateCrate()		'previously cCrate 207
	If CrateUpdateState Then
		dim D
		D = PSlope(ModLampz.LVL(16), 0, -CrateSpinner_Closed.CurrentAngle, 1, -CrateSpinner_Open.CurrentAngle)
		PrCrateDoor.RotX = D
		if PrCrateDoor.RotX < -5 then controller.switch(57) = 1 else controller.Switch(57) = 0
	End If
End Sub

Sub UpdateFlippers()
	PrLeftFlipper.RotZ = LeftFlipper.CurrentAngle
	PrRightFlipper.RotZ = RightFlipper.CurrentAngle
	FlSpider.RotZ = WheelMech.position * 7.5 + 18
	FlSpiderVR.RotZ = WheelMech.position * 7.5 + 18
	SpiderFS.RotZ = FLspider.Rotz*-1
End Sub

'Ballshadow routine by Ninuzzu

Dim BallShadow
BallShadow = Array(BallShadow1, BallShadow2, BallShadow3, BallShadow4)

Sub UpdateBallShadow()	'called by -1 lamptimer
	On Error Resume Next
    Dim BOT, b : BOT = GetBalls
	dim CenterPoint : CenterPoint = 425
	dim m(3)	'mask visible
    For b = 0 to UBound(BOT) 	' render the shadow for each ball
		m(b)=True
		If BOT(b).X < CenterPoint Then
			BallShadow(b).X = ((BOT(b).X) - (50/6) + ((BOT(b).X - (CenterPoint))/7)) + 10
		Else
			BallShadow(b).X = ((BOT(b).X) + (50/6) + ((BOT(b).X - (CenterPoint))/7)) - 10
		End If
		BallShadow(b).Y = BOT(b).Y + 20
		BallShadow(b).Z = 1
		If BOT(b).Z > 20 Then
			BallShadow(b).visible = 1
		Else
			BallShadow(b).visible = 0
		End If
	Next
	for b = 0 to 3 : if Not m(b) then BallShadow(b).visible = 0 end if : Next
	'dim str : str = gametime &vbnewline
	'for b = 0 to 3 : str = str&b&": "&m(b)&" "&BallShadow(b).visible&vbnewline : if Not m(b) then BallShadow(b).visible = 0 end if : Next
	'if tb.text<>str then tb.text=str
End Sub

'dim ilstr	'debug
Sub InitLampsNF()	'All lamp / flasher / GI assignments go here
	dim x, obj, tmp, w
	dim str 'debug
	for each x in GetElements	'quick maplamps / Flashers
		if TypeName(x) = "Light" or TypeName(x) = "Flasher" then 
			if Mid(x.Name,1,1) = "l" or Mid(x.Name,1,1) = "L" Then set obj = Lampz	: w="L:"	end if
			if Mid(x.Name,1,1) = "f" or Mid(x.Name,1,1) = "F" Then set obj = ModLampz: w="F:" 	end if
				err.clear
				on error resume Next
				tmp = mid(x.name,2,2)
				x.UserValue = cInt(tmp)
				if err then 
					str = str & "err "&x.name & vbnewline 
				Else
					obj.visible = 0
					obj.MassAssign(x.UserValue) = x
					str = str & "+("&w&x.uservalue&")" & x.name & vbnewline	'Debug
					x.UserValue = Empty		
				end if
				On Error Goto 0
		end If
	Next
	'ilstr = str	'Log of the maplamps loop. debug

	lampz.filter = "LampFilter"

	for x = 64 to 83 
		select case x
			case 69,70,79,80 : 
			case else
				set lampz.obj(x) = NullFader
				lampz.Callback(x) = "WheelLamp " &"l"&x&","
		end Select
	Next
		for x = 64 to 83 
			select case x
				case 69,70,79,80 : 
				case else
					set lampz.obj(x) = NullFader
					lampz.Callback(x) = "WheelLamp " &"VRl"&x&","
			end Select
		Next


	'flasher cap locations:
	'f27	f22
	'f24	f28
	'f35 	f36
'	modlampz.Callback(27) = "FlashCapBulb f27f,"
'	modlampz.Callback(22) = "FlashCapBulb f22f,"
'	modlampz.Callback(24) = "FlashCapBulb f24f,"
'	modlampz.Callback(28) = "FlashCapBulb f28f,"
'	modlampz.Callback(35) = "FlashCapBulb f35f,"
'	modlampz.Callback(36) = "FlashCapBulb f36f,"

	for x = 17 to 19 : ModLampz.Callback(x) = "BumperWall" : Next


	'**********'prototype rom alternate lamp numbers*****
	If Proto then 
		for x = 11 to 98
			Select Case x
				Case 18,31,32,33,34,44 : Set Lampz.Obj(x) = NullFader	'clear out these before adding prototype assignments
			end Select
		Next

		Lampz.MassAssign(18)= Lbolt1 'Left Bolt
		Lampz.MassAssign(31)= L18 'ramp Left eye
		Lampz.Callback(31) = "PrimEyes p18,"
		Lampz.Callback(32) = "PrimEyes p44,"
		Lampz.MassAssign(32)= L44 'ramp Right eye	
		Lampz.MassAssign(33)= Lbolt2 'Right Bolt
		Lampz.MassAssign(34)= LCandle1'Left Candle (backglass?)
		Lampz.MassAssign(44)= LCandle2'Right Candle (backglass?)


				'These lamps don't work right now
				'---------------------------------
		'87 - Buy In Button (Pre-production rom only)
		Lampz.MassAssign(91)= l34	'crate eyes, right to left
		Lampz.MassAssign(91)= L34a
		Lampz.MassAssign(92)= l33
		Lampz.MassAssign(92)= L33a
		Lampz.MassAssign(93)= l32
		Lampz.MassAssign(93)= L32a
		Lampz.MassAssign(94)= l31
		Lampz.MassAssign(94)= L31a
		for x = 91 to 94 : lampz.fadespeedup(x) = 1/20 : lampz.fadespeeddown(x) = 1/24: Next

				'Skull LEDs 
				'bottom to top / left to right... (not sure the correct order)
		'Lampz.MassAssign(95)= FlSkull2_5'#11
		'Lampz.MassAssign(96)= FlSkull2_6'#12		
		'Lampz.MassAssign(97)= FlSkull2_4'#10
		'Lampz.MassAssign(98)= FlSkull2_3'#9
		'Lampz.MassAssign(101)= FlSkull6_2'#6
		'Lampz.MassAssign(102)= FlSkull6_1'#5
		'Lampz.MassAssign(103)= FlSkull5_2	'#4
		'Lampz.MassAssign(104)= FlSkull5_1	'#3
		'Lampz.MassAssign(105)= FlSkull4_2	'#2
		'Lampz.MassAssign(106)= FlSkull4_1	'#1
		'Lampz.MassAssign(107)= FlSkull2_1'#7
		'Lampz.MassAssign(108)= FlSkull2_2'#8
				'Top to bottom, left to right...
		Lampz.MassAssign(95)= 	FlSkull6_1'#11
		Lampz.MassAssign(96)= 	FlSkull6_2'#12		
		Lampz.MassAssign(97)= 	FlSkull5_2'#10
		Lampz.MassAssign(98)= 	FlSkull5_1'#9
		Lampz.MassAssign(1)= FlSkull2_4'#6
		Lampz.MassAssign(2)= FlSkull2_3'#5
		Lampz.MassAssign(3)= FlSkull2_2'#4
		Lampz.MassAssign(4)=	FlSkull2_1'#3
		Lampz.MassAssign(5)=	FlSkull2_6'#2
		Lampz.MassAssign(6)= FlSkull2_5'#1
		Lampz.MassAssign(7)= FlSkull4_1'#7
		Lampz.MassAssign(8)= FlSkull4_2'#8
		'--------------------------------
	Else
		'normal eyes
		Lampz.Callback(18) = "PrimEyes p18,"
		Lampz.Callback(44) = "PrimEyes p44,"
		'crate eyes
		for x = 31 to 33 : lampz.fadespeedup(x) = 1/20 : lampz.fadespeeddown(x) = 1/24: Next
		
	End If
	'******************************************************
	If Not Proto Then		'Skull Mod
'		Lampz.MassAssign(51)= FlSkull6_1 : FlSkull6_1.UserValue = 51
'		Lampz.MassAssign(51)= FlSkull6_2 : FlSkull6_2.UserValue = 51
'		Lampz.MassAssign(52)= FlSkull5_1 : FlSkull5_1.UserValue = 52
'		Lampz.MassAssign(52)= FlSkull5_2 : FlSkull5_2.UserValue = 52
'		Lampz.MassAssign(53)= FlSkull4_1 : FlSkull4_1.UserValue = 53 
'		Lampz.MassAssign(53)= FlSkull4_2 : FlSkull4_2.UserValue = 53
'		Lampz.MassAssign(61)= FlSkull2_3 : FlSkull2_3.UserValue = 61
'		Lampz.MassAssign(61)= FlSkull2_4 : FlSkull2_4.UserValue = 61
'		Lampz.MassAssign(62)= FlSkull2_5 : FlSkull2_5.UserValue = 62 
'		Lampz.MassAssign(62)= FlSkull2_6 : FlSkull2_6.UserValue = 62
'		Lampz.MassAssign(63)= FlSkull2_1 : FlSkull2_1.UserValue = 63 
'		Lampz.MassAssign(63)= FlSkull2_2 : FlSkull2_2.UserValue = 63 
		Lampz.Callback(51)= "TwoLEDs 51,FlSkull6_1, FlSkull6_2,": FlSkull6_1.UserValue = 51: FlSkull6_2.UserValue = 51
		Lampz.Callback(52)= "TwoLEDs 52,FlSkull5_1, FlSkull5_2,": FlSkull5_1.UserValue = 52: FlSkull5_2.UserValue = 52
		Lampz.Callback(53)= "TwoLEDs 53,FlSkull4_1, FlSkull4_2,": FlSkull4_1.UserValue = 53: FlSkull4_2.UserValue = 53
		Lampz.Callback(61)= "TwoLEDs 61,FlSkull2_3, FlSkull2_4,": FlSkull2_3.UserValue = 61: FlSkull2_4.UserValue = 61
		Lampz.Callback(62)= "TwoLEDs 62,FlSkull2_5, FlSkull2_6,": FlSkull2_5.UserValue = 62 : FlSkull2_6.UserValue = 62 
		Lampz.Callback(63)= "TwoLEDs 63,FlSkull2_1, FlSkull2_2,": FlSkull2_1.UserValue = 63 : FlSkull2_2.UserValue = 63 
	end if



	'----GI Assignments----
	With ModLampz
		for x = 0 to 4 : .Filter(x) = "GIFilter" : Next
		for x = 5 to 49
			.FadeSpeedUp(x) = 1/64
			.FadeSpeedDown(x) = 1/64
			.Filter(x) = "FlasherFilter"
			.Burn(x) = True	'New feature, filters the fading speed a bit when fading out for a 'burning filament' effect
		Next
			'.Burn(20) = False 'Lightning Bolt
		.MassAssign(0) = Array(GI0, GI0p, GI0T1, GI0T2)	'upper
		.MassAssign(0) = ColToArray(GIballRefl0)
		.MassAssign(1) = Array(GI1, GI1P1, GI1P2,Gi_BallRefl9)	'middle
		.MassAssign(2) = Array(GI2, GI2T1, GI2T2, GI2T3, GI2T4, GI2T5, GI2T6)	'lower \ 1 \ 2
		.MassAssign(2) = ColToArray(GIballRefl2)
		.Callback(0) = "GIupdates"
		.Callback(1) = "GIupdates"
		.Callback(2) = "GIupdates"
	end With
	'-----------------------------
	'---GI color swapping object----
	GIc0.Assign Array(GI0, GI0p, GI0T1, GI0T2)	'upper (GIcolorswapper)
	GIc1.Assign Array(GI1, GI1P1, GI1P2)	'middle(GIcolorswapper)
	GIc2.Assign Array(GI2, GI2T1, GI2T2, GI2T3, GI2T4, GI2T5, GI2T6)	'lower \ 1 \ 2(GIcolorswapper)
	GIc0.Callback = "GIcolorupdate0" 'call, not execute, no arguments
	'GIc1.Callback = "GIcolorupdate1" 'call, not execute, no arguments
	GIc2.Callback = "GIcolorupdate2" 'call, not execute, no arguments

	dim a : a = Array(GIc0, GIc1, GIc2)
	for x = 0 to uBound(a)
		a(x).Color = ARRAY(255,255,255)
		a(x).ColorAssign(0) = ARRAY(255,255,255)'#0 - White
		a(x).ColorAssign(1) = array(400,161,70) '#1 - Incan 2700 (>255 abuses lum function to reduce brightness keep <1000)
		a(x).ColorAssign(2) = ARRAY(15,45,255)	'#2 - Ice Blue
		a(x).ColorAssign(3) = ARRAY(255,255,255)'#3 - Color (Special)
	Next
	GIc1.ColorAssign(3) = ARRAY(7,22,255)	'#3 Continued - middle Blue

	GIc0.ColorAssign(4) = Array(0,255,127)	'#4 - Color (Special) with the red stripped out
	GIc1.ColorAssign(4) = Array(255,35,0)	'#4 - With warm middle string
	GIc2.ColorAssign(4) = Array(0,255,127)	'#4	
	'-------------------------------

	'Some lampscript based Animations
	if SingleScreenFS then Lampz.MassAssign(0) = Array(FLspider, FLspiderback) : Lampz.Callback(0) = "SpiderPop" : end If	'FS spider
	Lampz.Callback(1)= "CardAnim"
	ScoreCardDT.Visible = Table1.ShowDT
	ScoreCardFS.Visible = Not Table1.ShowDT

	modlampz.LVL(0) = 0.99	'start GI on
	modlampz.LVL(1) = 0.99	'start GI on
	modlampz.LVL(2) = 0.99	'start GI on
	lampz.init : modlampz.init
	if SingleScreenFS and not table1.showdt then ShowSpider 0 else ShowSpider 1 end If
	if Not proto then for each x in SkullLEDs : x.state = 0 : Next end If
End Sub
'----Wrapper subs----
Sub ShowSpider(aBool) : Lampz.state(0) = abs(aBool) : End Sub
Sub ShowCard(aBool)	  : Lampz.state(1) = abs(aBool) : End Sub
Sub CrateOpen(aBool) : ModLampz.State(16) = abs(aBool) : End Sub

'------Lampscript-Animation Callbacks--------
Sub CardAnim(ByVal aLvl)
	if ModLampz.UseFunction(50) then aLvl = ModLampz.FilterOut(50, aLvl)
	ScoreCardDT.ShowFrame aLvl
	ScoreCardFS.ShowFrame aLvl
End Sub

dim BaseOpacity : BaseOpacity = l73.Opacity
Sub SpiderPop(aLvl)	'FS spider also affects award lamps
	dim x : for each x in Awards : x.opacity = BaseOpacity * aLvl : Next
End Sub
'------------GI color changer object callbacks -------------
Sub GIcolorUpdate0() 'Updates images and ball reflections for the special legacy GI colors
	dim x, a
	If GIc0.Index=0 or GIc0.Index=1 then GIFadingSpeedsNormal 0 else GIFadingSpeedsFast 0 End If
	If GIc0.Index=3 then 
		a = Array(Gi_BallRefl1 ,Gi_BallRefl2) : for each x in a : x.Color = RGB(3,255,3) : next  '1 2 green
		a = Array(Gi_BallRefl3 ,Gi_BallRefl4, Gi_BallRefl11 ,Gi_BallRefl12) : for each x in a : x.Color = RGB(85,13,255) : next  '3 4 11 12	= purple
		gi0.ImageA = "GI0_Color" : GI0P.ImageA = "GI0p_Color" 
		GI0T1.ColorFull = RGB(85,13,255) : GI0T2.ColorFUll = RGB(85,13,255)
	Elseif GIc0.Index=4 then
		a = Array(Gi_BallRefl1 ,Gi_BallRefl2) : for each x in a : x.Color = RGB(3,255,3) : next  '1 2 green
		a = Array(Gi_BallRefl3 ,Gi_BallRefl4, Gi_BallRefl11 ,Gi_BallRefl12) : for each x in a : x.Color = RGB(7,22,255) : next  '3 4 11 12	= blue
		gi0.ImageA = "GI0_Color" : GI0P.ImageA = "GI0p_Color" 
		GI0T1.ColorFull = RGB(3,45,255) : GI0T2.ColorFUll = RGB(3,45,255)	  '3 4 11 12	= blue / purple
	else 
		gi0.ImageA = "GI0" : GI0P.ImageA = "GI0p" 
		dim C : C = GIc0.Color : For each x in GIballRefl0 : x.Color = RGB(c(0), c(1),c(2)) : Next
	end If
End Sub
Sub GIcolorUpdate1() 
	dim C : C = GIc1.Color : Gi_BallRefl9.Color = RGB(c(0), c(1),c(2))
	If GIc1.Index=0 or GIc1.Index=1 then GIFadingSpeedsNormal 1 else GIFadingSpeedsFast 1 End If 
	if GIc2.Index=3 then Gi_BallRefl9.Color = RGB(7,22,255)		'Blue Reflection
	if GIc2.Index=4 then Gi_BallRefl9.Color = RGB(255,35,0)		'Reddish Reflection (4)
End Sub
Sub GIcolorUpdate2() 
	If GIc2.Index=0 or GIc2.Index=1 then GIFadingSpeedsNormal 2 else GIFadingSpeedsFast 2 End If
	dim x,a
	If GIc2.Index=3 then 
		gi2.ImageA = "GI2_Color" 
		a = Array(GI2T1, GI2T2) : for each x in a : x.ColorFull = rgb(3,255,3) : Next	'green Slings
		a = Array(GI2T5, GI2T6) : for each x in a : x.ColorFull = Desat(Array(3,255,3),0.75) : Next	'green Slings

		a = Array(Gi_BallRefl5,Gi_BallRefl6,Gi_BallRefl7) : for each x in a : x.Color = rgb(255,17,64) : Next	'Magenta Rollovers
		a = Array(GI2T3, GI2T4) : for each x in a : x.ColorFull = RGB(87,3,255): Next	'Purple Inlanes
		Gi_BallRefl10.Color = RGB(87,3,255)
	elseif GIc2.Index=4 then 
		gi2.ImageA = "GI2_Color" 
		a = Array(GI2T1, GI2T2) : for each x in a :x.ColorFull=rgb(45,200,0) : Next	'green Slings
		a = Array(GI2T5, GI2T6) : for each x in a : x.ColorFull = Desat(Array(5,127,1),0.75) : Next	'green Slings
		a = Array(Gi_BallRefl5,Gi_BallRefl6,Gi_BallRefl7) : for each x in a : x.Color = rgb(0,15,255) : Next	'Blue Rollovers
		a = Array(GI2T3, GI2T4) : for each x in a : x.ColorFull = RGB(3,45,255): Next'Blue Inlanes
		Gi_BallRefl10.Color = RGB(3,45,255)
	else 
		gi2.ImageA = "GI2" 
		dim C : C = GIc2.Color : For each x in GIballRefl2 : x.Color = RGB(c(0), c(1),c(2)) : Next
	end If
End Sub
Sub GIFadingSpeedsNormal(idx) :  ModLampz.FadeSpeedUp(idx) = 0.01  : ModLampz.FadeSpeedDown(idx) = 0.01  : End Sub
Sub GIFadingSpeedsFast(idx) :  ModLampz.FadeSpeedUp(idx) = 1/20 : ModLampz.FadeSpeedDown(idx) = 1/24 : End Sub

'-----------Special lamp callbacks----------------
Sub PrimEyes(aObj, ByVal aLVL)
	if lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	
	if aLvl > 0.05 then aObj.image = "Bulb1" else aObj.Image = "Bulb0" end If
	aObj.BlendDisableLighting = aLvl*4
End Sub

Sub WheelLamp(aObj, byVal aLvl)
	if lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)
	aObj.IntensityScale = (aLvl+0.12)
	'dim str : str = aObj.IntensityScale : if tb.text<>str then tb.text=str
End Sub

Sub TwoLEDs(aIdx, aObj1, aObj2, aLVL)
	If Not SkullLEDsequence Then
		aObj1.Intensityscale = Lampz.OnOff(aIDX)
		aObj2.Intensityscale = Lampz.OnOff(aIDX)
	end If
End Sub

'Sub FlashCapBulb(aObj, aLvl)
'	'If aLvl < 0.3 then aObj.amount=pslope(aLvl, 0, 150, 0.3, 0) else aobj.amount = 0
'	'aObj.amount=pslope(aLvl, 0, 550, 1, 0)' else aobj.amount = 0
'	'aObj.amount=pslope(aLvl, 0, 800, 1, 800)
'End Sub

Sub BumperWall(aLVL) : 
	dim out: out = (modlampz.LVL(17) + modlampz.LVL(18) + modlampz.LVL(19))/3
	Bumperw.IntensityScale = out
End Sub

'tb.timerenabled=1
'Sub TB_Timer() 'debug lamp object locks
'	dim str,x,c1,c2, c1str, c2str
'	for x = 0 to 50 : if not modlampz.lock(x) then c1str = c1str & x & ",": c1 = c1+1 end if : Next
'	for x = 0 to 100 : if not lampz.lock(x) then c2str = c2str & x & ",":c2 = c2+1 end If: Next
'	str = c1 & " =" & c1str &vbnewline&c2 & " =" & c2str : if me.text<>str then me.text=str end If
'end Sub

'--------GI callback---------
dim GIstate 'used for fastflips
dim GIscale : GIscale =1.5
Sub GIupdates(ByVal aLvl)
	if aLvl then GIstate = True else GIstate = False
	aLvl = GIFilter(aLVL)

	if LutFading then 		'crank lamps when GI is off 
	dim x
	x = cInt(aLvl*20)+5 : Table1.ColorGradeImage = "cc"&x
		dim offset : Offset = (GiScale-1) * (ABS(aLvl-1 )  ) + 1	'invert
		for x = 5 to 50
			lampz.modulate(x) = Offset
			Modlampz.modulate(x)=Offset
		Next
		for x = 51 to 98
			lampz.modulate(x) = Offset
		Next
	end If
End Sub

'---------lamp output filters--------------- 
Function GIFilter(aLvl)
	GIFilter = -(2*aLvl^3)/3 + aLvl^2 + (2*aLvl)/3'balanced
	if GIFilter < 0 then GIFilter = 0
End Function

Function LampFilter(aLvl) 
	LampFilter = aLvl^1.6
End Function

Function FlasherFilter(aLvl)
	FlasherFilter = -(2*aLvl^3)/3 + aLvl^2 + (2*aLvl)/3'balanced
	'FlasherFilter = 2*aLvl - aLvl^2 'Top curve heavy
	'FlasherFilter = (4*aLvl)/3 - aLvl^3/3 'Top curveheavy 2
	'FlasherFilter = (aLvl^2)/2+aLvl/2	'bottom
	'if FlasherFilter < 0 then FlasherFilter = 0
End Function

'***************
'* Triggers and Switches
'***************

sub sw16_hit():controller.Switch(16) = 1:end sub		'Kickback
sub sw16_unhit():controller.Switch(16) = 0:end sub
sub sw17_hit():controller.Switch(17) = 1:end sub		'Right Flipper Lane
sub sw17_unhit():controller.Switch(17) = 0:end sub

dim BallInPlunger :BallInPlunger = False
sub PlungerLane_hit():ballinplunger = True: End Sub
Sub PlungerLane_unhit():BallInPlunger = False : End Sub

sub sw18_hit():controller.Switch(18) = 1: BIPL=1 :end sub		'Shooter Lane
sub sw18_unhit():controller.Switch(18) = 0: BIPL=0 :end sub

sub sw25_hit():controller.Switch(25) = 1:end sub		'Extra Ball Lane
sub sw25_unhit():controller.Switch(25) = 0:end sub
sub sw26_hit():controller.Switch(26) = 1:end sub		'Left Flipper Lane
sub sw26_unhit():controller.Switch(26) = 0:end sub
sub sw27_hit():controller.Switch(27) = 1:end sub		'Right Outlane
sub sw27_unhit():controller.Switch(27) = 0:end sub
sub sw28_hit():vpmTimer.PulseSw 28:end sub				'Right Standup 

sub sw38_hit():LswitchA LastCrateHitLocation:controller.Switch(38) = 1:end sub		'Crate Enter
sub sw38_unhit():controller.Switch(38) = 0:end sub

sub sw41_hit():controller.Switch(41) = 1:end sub		'Coffin left
sub sw41_unhit():controller.Switch(41) = 0:end sub
sub sw42_hit():controller.Switch(42) = 1:end sub		'Coffin middle
sub sw42_unhit():controller.Switch(42) = 0:end sub
sub sw43_hit():controller.Switch(43) = 1:end sub		'Coffin right
sub sw43_unhit():controller.Switch(43) = 0:end sub
sub sw44_hit():controller.Switch(44) = 1: end sub		'Left Ramp Entry
sub sw44_unhit():controller.Switch(44) = 0:end sub
sub sw46_hit() :controller.Switch(46) = 1:end sub		'Left Ramp Made
sub sw46_unhit():controller.Switch(46) = 0: LswitchA Array(210,507):end sub

sub sw45_hit():controller.Switch(45) = 1:end sub		'Right Ramp Enter
sub sw45_unhit():controller.Switch(45) = 0:end sub
sub sw47_hit():controller.Switch(47) = 1:end sub		'Right Ramp Made
sub sw47_unhit():controller.Switch(47) = 0: LswitchA Array(730,675):end sub

Sub LeftSlingShot_Slingshot():me.timerenabled = True : controller.Switch(51) = 1 : LswitchBall:	RandomSoundSlingshotLeft SlingK1 : End Sub
Sub LeftSlingShot_Timer()	:	me.timerenabled = False : controller.Switch(51) = 0	:	End Sub
Sub RightSlingShot_Slingshot():me.timerenabled = True : controller.Switch(52) = 1	: LswitchBall: RandomSoundSlingshotRight SlingK2 : End Sub
Sub RightSlingShot_Timer()	:	me.timerenabled = False : controller.Switch(52) = 0	:	End Sub

Sub SolBumper1(enabled) : If enabled and vpmflipsSam.romcontrol Then Bumper1.PlayHit() : SFXt 33 end if : End Sub
Sub SolBumper2(enabled) : If enabled and vpmflipsSam.romcontrol Then Bumper2.PlayHit() : SFXt 34 end if : End Sub
Sub SolBumper3(enabled) : If enabled and vpmflipsSam.romcontrol Then Bumper3.PlayHit() : SFXt 35 end if : End Sub

sub Bumper1_hit() :LswitchBumper: vpmtimer.PulseSw 53 : RandomSoundBumperTop bumper1 : end sub	
sub Bumper2_hit() 
	if activeball.x < 680 then :LswitchBall :else : LswitchBumper: end if
	vpmtimer.PulseSw 54 : RandomSoundBumperMiddle bumper2 : end sub
sub Bumper3_hit() :LswitchBumper: vpmtimer.PulseSw 55 : RandomSoundBumperBottom bumper3 : end sub	 

Sub SW56_SlingShot() : vpmTimer.PulseSw 56 : RandomSoundSlingshotLeft SlingK3 : LswitchBall: End Sub

sub sw57_hit()	'Crate Sensor. Crate spinner also trips this switch
	controller.Switch(57) = 1
	TiCratesw.Timerinterval=1200	'may help
	LswitchBall
	If BallVel(activeball) > 12 then SFXt 8 Else SFXt 11 end If
end sub
sub sw57_unhit():controller.Switch(57) = 0:end sub

sub sw58_hit() : controller.Switch(58) = 1:end sub	'Left Loop
sub sw58_unhit():controller.Switch(58) = 0:end sub

sub sw61_hit():SweepLeftTargets 0, "top":end sub	'Three bank upper
sub sw62_hit():SweepLeftTargets 1, "middle":end sub	'Three bank middle
sub sw63_hit():SweepLeftTargets 2, "bottom":end sub	'Three bank lower

Sub SweepLeftTargets(aIDX, aStr)
	dim aSw : aSw = array(61, 62, 63)		'Switch numbers top to bottom
	dim aTg : aTg = array(sw61, sw62, sw63)	'switches, top to bottom 
	dim Tolerance : Tolerance = 6	'Tolerance, in VP units
	dim Midpoint, BallY : BallY = activeball.Y
	dim str	'debug
	str = aStr &vbnewline
	'str = "switch hit" & aSw(aIDX) &vbnewline
	vpmTimer.PulseSw aSw(aIdx)
	if aIDX > LBound(atG) and BallY <= aTg(aIdx).Y then 'check higher (-) than hit target ...
		midpoint = ((aTg(aIdx).Y + aTg(aIdx-1).Y) / 2) +15.252 'Find the midpoint. Last is the ball offset, has to be set manually
		'debug.print "MidPoint above " & aStr & " = " & midpoint
		str = str & round(bally,2) & "<" & round(Midpoint,2) &"+" &tolerance &vbnewline
		If BallY <= midpoint + Tolerance AND BallY >= midpoint - Tolerance Then
			str = str & "sweeping Up!"' sw" & aidx & " sw" & aIdx-1
			vpmTimer.PulseSw aSw(aIdx-1)
		End If
	elseif aIDX < UBound(atG) and BallY >= aTg(aIdx).Y then 'check lower (+) than hit target ...
		midpoint = ((aTg(aIdx).Y + aTg(aIdx+1).Y) / 2) + 15.252 'find the midpoint
		'debug.print "MidPoint below " & aStr & " = " & midpoint
		str = str & round(bally,2) & ">" & round(Midpoint,2) &"+" &tolerance &vbnewline
		If BallY <= midpoint + Tolerance AND BallY >= midpoint - Tolerance Then
			str = str & "sweeping Down!"' sw" & aidx & " sw" & aIdx-1
			vpmTimer.PulseSw aSw(aIdx+1)
		End If
	end if
	'tb.text = str

End Sub


sub Col_Rubber_Band_sw67_hit():LswitchBall : controller.Switch(67) = 1:end sub	'Left Ramp 10 point
sub Col_Rubber_Band_sw67_unhit():controller.Switch(67) = 0:end sub
sub sw68_hit():controller.Switch(68) = 1:end sub	'Right Loop
sub sw68_unhit():controller.Switch(68) = 0:end sub

sub sw71_hit():controller.Switch(71) = 1:end sub	'Left Skull Lane
sub sw71_unhit():controller.Switch(71) = 0:end sub
sub sw72_hit():controller.Switch(72) = 1:end sub	'Center Skull Lane
sub sw72_unhit():controller.Switch(72) = 0:end sub
sub sw73_hit():controller.Switch(73) = 1:end sub	'Right Skull Lane
sub sw73_unhit():controller.Switch(73) = 0:end sub
sub sw74_hit():controller.Switch(74) = 1:end sub	'Secret Passage
sub sw74_unhit():controller.Switch(74) = 0:end sub

Sub Drain_Hit()
	LswitchA Array(455,1710) 'floating text
	BallSearch
	dim int : int = ubound(getballs)+1+bstrough.balls+bscoffin.balls+bsSpider.balls+bsLeftKick.balls
	if int > 4 then me.destroyball :  exit Sub
'	SFXt 21
	bsTrough.AddBall Me
RandomSoundDrain drain
End Sub

Sub BallSearch() ' find any balls that have fallen off the table
	dim x : for each x in getballs : if x.y > 3000 then x.x = 536 : x.y = 1991 : x.vely = 0 : end if : Next
End Sub

Sub CoffinEntrance_Hit() : Lswitcha Array(205,928) : vpmTimer.PulseSw 48 : bsCoffin.Addball Me : SFXt 16: End Sub

'-------------
'Crate
'-------------
'Uses fallthrough holes and a submarine switch
'The crate switch, sw38, is handled by automatic switch handling
dim LastCrateHitLocation : LastCrateHitLocation = Array(440,420)' for floating Text
Sub cratetrigger_hit() :LswitchBall :LastCrateHitLocation = Array(activeball.x,activeball.y) : DropBalls activeball : end Sub
Sub cratetrigger2_hit() : DropBalls activeball : end Sub

Sub DropBalls(aBall) 'drop ball
	SFXt 18
	aBall.z = -30 : aBall.vely = aBall.vely * 0.5 : aBall.velz = 0
End Sub

sub sw37_dropwall_hit() : if bsLeftKick.balls > 0 then SFXt 12 end if: end sub	'ball collide SFX


'****************
'* Hole Handling by nFozzy
'Crate Kickout / Skillshot
'And Spider Holes

'Method:
'Replicates square holes in the playfield by using square triggers to enable kickers
'Some use two triggers to better emulate the square shape of the holes
'Works just okay
'-------------

sub sw37Trigger_hit():sw37a.enabled = 1: SoundSaucerLock :end sub
sub sw37Trigger_unhit():sw37a.enabled = 0: SoundSaucerKick 1, sw37Trigger :end sub

Dim Ballsw37a, Ballsw37aHeight
dim Ballsw36a, Ballsw36b, Ballsw36aHeight, Ballsw36bHeight

Dim BallDropSpeed : BallDropSpeed = 1.1	'1.81688'?	'vpu per ms 

Sub sw37a_Hit()
	LswitchBall
	Set Ballsw37a = ActiveBall
	Ballsw37aHeight = 25
	Me.TimerEnabled = 1
	me.enabled = 0
	sw37_dropwall.isdropped = 0
end sub

Sub sw37a_Timer
	Ballsw37aHeight = Ballsw37aHeight -BallDropSpeed*FrameTime
	Ballsw37a.z = Ballsw37aHeight
	If Ballsw37aHeight <-50 Then	'40
		Me.TimerEnabled = 0
		if bsLeftKick.balls > 0 then SFXt 12
		bsLeftKick.AddBall Me
'		SFXt 17
	End If
end sub

Sub sw37_Hit()											'left kickout from crate
	sw37_dropwall.isdropped = 0
	Me.DestroyBall
	bsLeftKick.AddBall Me

End Sub


Sub SolCrateKickout(Enabled)	'Solenoid Callback
	if not enabled then exit Sub
	if bsLeftKick.balls > 0 then SFXt 31 else SFXt 32 end If
	bsLeftKick.ExitSol_On : vpmtimer.addtimer 140, "bsLeftKick.ExitSol_On'"	'Kick out two balls at once
	sw37a.enabled=0 ': vpmtimer.AddTimer 160, "sw37a.Enabled=1'"
	sw37_dropwall.isdropped = 1
End Sub

'Spider Hole vuk
'Prim_RampDiverter2 ---	animated primitive
'RampGateFlipper ---	for animation, goes from 0 to -40
'sw36 			---		the kicker (not enabled!)
'sw36a and sw36b ---	top kicker and bottom kicker, respectively
'sw36trigger	 ---	rectangular Trigger
'sw36triggerexit ---	big star-shaped trigger
'RaLeft_Closed 	---		ramp gate when open
'RaLeft_Open	---		ramp gate when closed
						'Square triggers enabling the hole
sub sw36trigger_hit():sw36a.enabled=1:sw36b.enabled=1: SoundSaucerLock :end sub
sub sw36triggerexit_unhit():sw36a.enabled=0:sw36b.enabled=0: SoundSaucerKick 1, sw36triggerexit :end sub
'dim time
Sub sw36a_Hit()			'Lower Spider Hole
	'time = gametime 
	'debug.print time-gametime & ": " & "Hit..."
	LswitchBall
	Set Ballsw36a = ActiveBall
	Ballsw36aHeight = 20 : Ballsw36a.Z = Ballsw36bHeight
	Me.TimerEnabled = 1
	me.enabled = 0
end sub

Sub sw36a_Timer
	Ballsw36aHeight = Ballsw36aHeight - BallDropSpeed*FrameTime '1 vpu per ms
	Ballsw36a.Z = Ballsw36aHeight
	If Ballsw36a.Z <-50 Then	'40
		'debug.print time-gametime & ": " & "Added..."
		Me.TimerEnabled = 0
		bsSpider.AddBall Me
'		SFXt 19
	End If
end sub

Sub sw36b_Hit()			'Upper Spider Hole
	LswitchBall
	Set Ballsw36b = ActiveBall
	Ballsw36bHeight = 25 : Ballsw36b.Z = Ballsw36bHeight
	Me.TimerEnabled = 1
	me.enabled = 0
end sub

Sub sw36b_Timer
	Ballsw36bHeight = Ballsw36bHeight - BallDropSpeed*FrameTime '1 vpu per ms
	Ballsw36b.Z = Ballsw36bHeight
	If Ballsw36b.Z <-35 Then	'40
		Me.TimerEnabled = 0
		bsSpider.AddBall Me
'		SFXt 20
	End If
end sub

Sub SolSpiderPopper(Enabled) 'Solenoid Callback
	If Enabled Then
		If bsSpider.Balls Then
			'time = gametime 
			'debug.print time-gametime & ": " & "Kicked..."
			if FSSpiderenabled then ShowSpider 0'lampz.state(0)=0'setlamp cSpiderFade, 0 
			bsSpider.ExitSol_On : SFXt 36
			RaLeft_Closed.collidable = 0
			RaLeft_Open.collidable = 1
			sw36a.enabled = 0
			sw36b.enabled = 0
			sw36trigger.enabled = 0 	'necessary?	'added back 1.43
			sw36triggerExit.enabled = 0 'necessary?	'added back 1.43
			sw36.timerinterval = 75		'Closes the gate again after a timer
			sw36.timerenabled = 1		'Closes the gate again after a timer
			RampGateFlipper.timerinterval = -1
			RampGateFlipper.timerenabled = 1
			RampGateFlipper.rotatetoend
		Else
			sfxT 37
		End If
	End If
End Sub

sub RampGateFlipper_Timer()	'Animates ramp popper gate
	Prim_RampDiverter2.RotX = RampGateFlipper.currentangle
	if RampGateFlipper.CurrentAngle = RampGateFlipper.StartAngle then me.Enabled = 0	'todo this might not work
end sub

sub sw36_Timer()		'Closes the gate again after a timer
	RampGateFlipper.rotatetostart
	RaLeft_Closed.collidable = 1
	RaLeft_Open.collidable = 0
	sw36trigger.enabled = 1 'added back 1.43
	sw36triggerExit.enabled = 1 'added back 1.43
	'debug.print time-gametime & ": " & "Reenabled..."
	if sw36.timerinterval = 750 then me.timerenabled = 0:RampGateFlipper.timerenabled = 0	'disables both timers
	if sw36.timerinterval = 75 then sw36.timerinterval = 750	'reuses the timer for disabling updates after 3/4s of a second
end sub



'****************
'* Frog Target Animations
'* Rstep and Lstep  are the variables that increment the animation
'****************
dim FrogDir1, frogdir2, frogdir3, Frog1Vel, Frog2Vel, Frog3Vel
frogdir1 = 1 :frogdir2 = 1 :frogdir3 = 1

'Center targets
'Sub Sw64a_Hit() : Frog1Vel = BallSpeed(activeball)*2:	vpmTimer.PulseSw 64: 	sw64t.Enabled = 1: End Sub
Sub Sw64a_Hit() : Frog1Vel = BallSpeed(activeball)*2:	vpmTimer.PulseSw 64: 	sw64t.Enabled = 1: End Sub
Sub Sw65a_Hit() : Frog2Vel = BallSpeed(activeball)*2:	vpmTimer.PulseSw 65: 	sw65t.Enabled = 1: End Sub
Sub Sw66a_Hit() : Frog3Vel = BallSpeed(activeball)*2:	vpmTimer.PulseSw 66: 	sw66t.Enabled = 1: End Sub
Sub sw64_Hit				'Left leaper 
	Frog1Vel = BallSpeed(activeball)/2 : sw64t.Enabled = 1
	vpmTimer.PulseSw 64
End Sub
Sub sw65_Hit				'Center Leaper
	Frog2Vel = BallSpeed(activeball)/2 : 	sw65t.Enabled = 1 : vpmTimer.PulseSw 65
	sw65t.Enabled = 1
End Sub
Sub sw66_Hit				'Right Leaper
	Frog3Vel = BallSpeed(activeball)/2 : 	sw66t.Enabled = 1 : vpmTimer.PulseSw 66
	sw66t.Enabled = 1
End Sub


Dim Dir1, chdir1, updown1, slowmo
slowmo = 1'.98						'Make this number lower for slow-mo frogs
Dir1 = 1
updown1 = 1
ChDir1 = 0
Sub Sw64t_Timer()
dim rotdir
	If updown1 = -1 AND ChDir1 = 0 Then ChDir1 = 1
	If ChDir1 = 1 Then
		If PrLeaper1.Z >= 160 Then PlaySound "metalhit2", 0, LVL(0.1), -0.5, 0:ChDir1 = 2
		If PrLeaper1.Z >= 155 AND PrLeaper1.Z < 160 Then PlaySound "metalhit2", 0, LVL(0.1), -0.01, 0:ChDir1 = 2
		If PrLeaper1.Z >= 150 AND PrLeaper1.Z < 155 Then PlaySound "metalhit2", 0, LVL(0.05), -0.01, 0:ChDir1 = 2
	End If
	PrLeaper1.Z = dSin(dir1) * Frog1Vel * 2 + 55

	if PrLeaper1.Rotz > 20 then 
'		frogdir1 = -1
		frogdir1 = 1
	elseif prleaper1.rotz < -40 Then
		frogdir1 = 1
	end if

'	PrLeaper1.RotZ = PrLeaper1.RotZ + (Frog1Vel * 0.005 * frogdir1)	'simple rotation
	PrLeaper1.RotZ = PrLeaper1.RotZ + (Frog1Vel * 0.05 * frogdir1)	'simple rotation
	If dir1 >= 80 Then updown1 = -1
'	debug.Print dir1
	dir1 = dir1 + dCos(dir1) * updown1 * slowmo
	If PrLeaper1.Z <= 55 Then
		PrLeaper1.Z = 55
		Me.Enabled = 0
		Dir1 = 1
		ChDir1 = 0
		updown1 = 1
	End If
End Sub

Dim Dir2, chdir2, updown2
Dir2 = 1
updown2 = 1
ChDir2 = 0
Sub Sw65t_Timer()
	If updown2 = -1 AND ChDir2 = 0 Then ChDir2 = 1
	If ChDir2 = 1 Then
		If PrLeaper2.Z >= 160 Then PlaySound "metalhit2", 0, LVL(0.1), -0.2, 0:ChDir2 = 2
		If PrLeaper2.Z >= 155 AND PrLeaper2.Z < 160 Then PlaySound "metalhit2", 0, LVL(0.1), 0, 0:ChDir2 = 2
		If PrLeaper2.Z >= 150 AND PrLeaper2.Z < 155 Then PlaySound "metalhit2", 0, LVL(0.05), 0, 0:ChDir2 = 2
	End If
	PrLeaper2.Z = dSin(dir2) * Frog2Vel * 2 + 55

	if PrLeaper2.Rotz > 40 then 
'		frogdir2 = -1
		frogdir2 = 1
	elseif prleaper2.rotz < -60 Then
		frogdir2 = 1
	end if

	PrLeaper2.RotZ = PrLeaper2.RotZ + (Frog2Vel * 0.05 * frogdir2)
	If dir2 >= 80 Then updown2 = -1
	dir2 = dir2 + dCos(dir2) * updown2 * slowmo
	If PrLeaper2.Z <= 55 Then
		PrLeaper2.Z = 55
		Me.Enabled = 0
		Dir2 = 1
		ChDir2 = 0
		updown2 = 1
	End If
End Sub

Dim Dir3, chdir3, updown3
Dir3 = 1
updown3 = 1
ChDir3 = 0
Sub Sw66t_Timer()
	If updown3 = -1 AND ChDir3 = 0 Then ChDir3 = 1
	If ChDir3 = 1 Then
		If PrLeaper3.Z >= 160 Then PlaySound "metalhit2", 0, LVL(0.1), 0.4, 0:ChDir3 = 2
		If PrLeaper3.Z >= 155 AND PrLeaper3.Z < 160 Then PlaySound "metalhit2", 0, LVL(0.08), 0.01, 0:ChDir3 = 2
		If PrLeaper3.Z >= 150 AND PrLeaper3.Z < 155 Then PlaySound "metalhit2", 0, LVL(0.05), 0.01, 0:ChDir3 = 2
	End If
	PrLeaper3.Z = dSin(dir3) * Frog3Vel * 2 + 55
	if PrLeaper3.Rotz > 60 then 
'		frogdir3 = -1
		frogdir3 = 1
	elseif prleaper3.rotz < -20 Then
		frogdir3 = 1
	end if
	PrLeaper3.RotZ = PrLeaper3.RotZ + (Frog3Vel * 0.05 * frogdir3)
	If dir3 >= 80 Then updown3 = -1
	dir3 = dir3 + dCos(dir3) * updown3 * slowmo
	If PrLeaper3.Z <= 55 Then
		PrLeaper3.Z = 55
		Me.Enabled = 0
		Dir3 = 1
		ChDir3 = 0
		updown3 = 1
	End If
End Sub


'*****************
'Class jungle nf
'*****************


'Floating Text
dim FTlow, FTmed, FThigh, FTbumperSum
Sub InitFloatingText()
	Set FTlow = New FloatingText
	with FTlow 
		.Sprites(0) = Array(FtLow1_1, FtLow1_2, FtLow1_3, FtLow1_4, FtLow1_5, FTlow1_6)
		.Sprites(1) = Array(FtLow2_1, FtLow2_2, FtLow2_3, FtLow2_4, FtLow2_5, FtLow2_6)
		.Sprites(2) = Array(FtLow3_1, FtLow3_2, FtLow3_3, FtLow3_4, FtLow3_5, FtLow3_6)
		.Sprites(3) = Array(FtLow4_1, FtLow4_2, FtLow4_3, FtLow4_4, FtLow4_5, FtLow4_6)
		.Sprites(4) = Array(FtLow5_1, FtLow5_2, FtLow5_3, FtLow5_4, FtLow5_5, FtLow5_6)
		
		.Prefix = "SpookyFont_"
		.Size = 23
		.FadeSpeedUp = 1/700
		.RotX = -37

	end With

	Set FTmed = New FloatingText
	With FTmed
		.Sprites(0) = Array(FtMed1_1, FtMed1_2, FtMed1_3, FtMed1_4, FtMed1_5, FtMed1_6)
		.Sprites(1) = Array(FtMed2_1, FtMed2_2, FtMed2_3, FtMed2_4, FtMed2_5, FtMed2_6)
		.Sprites(2) = Array(FtMed3_1, FtMed3_2, FtMed3_3, FtMed3_4, FtMed3_5, FtMed3_6)
		.Sprites(3) = Array(FtMed4_1, FtMed4_2, FtMed4_3, FtMed4_4, FtMed4_5, FtMed4_6)
		.Sprites(4) = Array(FtMed5_1, FtMed5_2, FtMed5_3, FtMed5_4, FtMed5_5, FtMed5_6)
		.Prefix = "SpookyFont_"
		.Size = 23*2
		.FadeSpeedUp = 1/1400
		.RotX = -37
	End With

	Set FThigh = New FloatingText
	With FThigh
		.Sprites(0) = Array(FtHi1_1, FtHi1_2, FtHi1_3, FtHi1_4, FtHi1_5, FtHi1_6, FtHi1_7)
		.Sprites(1) = Array(FtHi2_1, FtHi2_2, FtHi2_3, FtHi2_4, FtHi2_5, FtHi2_6, FtHi2_7)
		.Sprites(2) = Array(FtHi3_1, FtHi3_2, FtHi3_3, FtHi3_4, FtHi3_5, FtHi3_6, FtHi3_7)
		.Prefix = "SpookyFont_"
		.Size = 23*3
		.FadeSpeedUp = 1/2100
		.RotX = -37
	End With

	Set FTbumperSum = New FloatingText
	With FTbumperSum
		.Sprites(0) = Array(FTbumper1, FTbumper2, FTbumper3, FTbumper4, FTbumper5, FTbumper6, FTbumper7)
		.Size = 23*3 : 	.Prefix = "SpookyFont_"
		.FadeSpeedUp = 1/1800
		.RotX = -37
	End With

End Sub

dim LastSwitch : LastSwitch = Array(410, 6000) 'drain 
dim LastScore : LastScore = 0
dim BumperScore

Sub BumperArea_UnHit()	'When the ball leaves the bumper area, display the sum of bumper scores
	If BumperScore > 30000 then PlaceFloatingText ftBumperSum, BumperScore, array(800,700)
	BumperScore = 0
End Sub

Sub FloatingTextUpdates()
	Dim NVRAM : NVRAM = Controller.NVRAM
	dim str : str = _
	ConvertBCD(NVRAM(CInt("&h16A0"))) & _
	ConvertBCD(NVRAM(CInt("&h16A1"))) & _
	ConvertBCD(NVRAM(CInt("&h16A2"))) & _
	ConvertBCD(NVRAM(CInt("&h16A3"))) & _
	ConvertBCD(NVRAM(CInt("&h16A4")))' & _
	'ConvertBCD(NVRAM(CInt("&h16A5")))		'WPC current score
'	ConvertBCD(NVRAM(CInt("&h200"))) & _
'	ConvertBCD(NVRAM(CInt("&h201"))) & _
'	ConvertBCD(NVRAM(CInt("&h202"))) & _
'	ConvertBCD(NVRAM(CInt("&h203")))		'sys 11 current score
	str = round(str)

	dim PointGain
	PointGain = Str - LastScore	
	LastScore = str

	if IsObject(LastSwitch) then 'bumper score sum
		If LastSwitch.ID = 2 and PointGain < 50000 then BumperScore = BumperScore + PointGain end If
	end If

	if PointGain >= 750000 Then	'hi point scores
		PlaceFloatingTextHi PointGain, LastSwitch
	elseif pointgain >= 200000 then 'medium point scores	
		PlaceFloatingText ftMed, PointGain, LastSwitch
	elseif pointgain > 0 then	'low point scores
		PlaceFloatingText ftLow, PointGain, LastSwitch
	end if

	FTlow.Update2
	FTmed.Update2
	FThigh.Update2
	FTbumperSum.Update2
End Sub

Function ConvertBCD(v) : ConvertBCD = "" & ((v AND &hF0) / 16) & (v AND &hF) : End Function

Sub PlaceFloatingText (aObj, aPointGain, aInput)
	if IsArray(aInput) then 
		aObj.TextAt aPointGain, aInput(0), aInput(1)
	else
		aObj.TextAt aPointGain, aInput.x, aInput.y
	end If
End Sub

'Helper placer sub
Sub PlaceFloatingTextHi(aPointGain, aInput)	'center text a bit for the big scores
	dim aX, aY
	if IsArray(aInput) then 
		aX = (aInput(0) + (table1.width/2))/2
		aY = (aInput(1) + (table1.Height/2))/2
	else
		aX = (aInput.x + (table1.width/2))/2
		aY = (aInput.y + (table1.Height/2))/2
	end If
	FThigh.TextAt aPointGain, aX, aY
End Sub

'Switch location handling, or at least the method I used to do it. 
Sub cRollovers_Hit(aIDX)
	RandomSoundRollover
	'Set LastSwitch = aSwitches(aIDX)
	'TB.TEXT = lastswitch.name
	LswitchBall
End Sub

'Various wrapper subs
Sub Lswitch(aObj)  : set LastSwitch = aObj  : End Sub
Sub LswitchA(aArray)  : LastSwitch = aArray  : End Sub
Sub LswitchBall() : Set LastSwitch = TempPos : TempPos.Update : End Sub
Sub LswitchBumper()  : Set LastSwitch = BumperPos : BumperPos.Update  : End Sub

Class WallSwitchPos 
	Public x,y,name,id
	Public Sub Update() : x=activeball.x : y=activeball.y : end Sub
End Class
Dim TempPos : Set TempPos = New WallSwitchPos : TempPos.x = 0 : TempPos.y = 0 : TempPos.Name = "TempPos"
Dim BumperPos : Set BumperPos = New WallSwitchPos : BumperPos.x = 0 : BumperPos.y = 0 : BumperPos.Name = "BumperPos": BumperPos.ID=2





'Floating Text 0.02a by nFozzy

'--Setup--
'Sprites(idx)	- Input Array of flasher objects. Overfilled text will be cut off. 
'(Please add this first, and only add indexes sequentially. The more arrays indexed, the more text frames can be displayed)

'Size (Public)  - Adjusts the type spacing. (Default 30)
'RotX (Property)- Adjust RotX. 
'FadeSpeedUp 	- Adjust scrolling speed

'--Methods--
'TextAt	(Sub)	- Input String, X coord, Y Coord. Primary method. Displays text at this coordinate.

'---Fading updates--
'Update2 - Handles all fading. REQUIRES SCRIPT FRAMETIME CALCULATION!


Class FloatingText
	Private Count, Prfx
	public Size
	Public Frame, Text, lock, loaded, lvl, z 'arrays
	Public FadeSpeedUp
	Public LastFrame, LastFrameTime
	Private Sub Class_Initialize 
		Redim Frame(0), Text(0), lock(0), loaded(0), lvl(0), z(0)
		FadeSpeedUp = 1/1500 
		lvl(0) = 0 : loaded(0) = 1
		Count = 0 : size = 30
		LastFrame=0 : LastFrameTime=100
	end sub

	Public Property Let RotX(aInput) 
		'dim debugstr
		dim tmp, x, xx : for each x in Frame 
			tmp = x
			if IsArray(tmp) then 
				for each xx in tmp
					xx.RotX = aInput
					'debugstr = debugstr & xx.name & ".rotX = " & aInput & "..." & vbnewline
				next
			Else
				'debugstr = debugstr & "...not any array..." & vbnewline
			end If
		Next
		'if tb.text <> debugstr then tb.text = debugstr
	End Property

	Public Property Let Sprites(aIdx, aArray)
		if IsArray(aArray) Then
			Count = aIdx
			Redim Preserve Frame(aIdx)
			Redim Preserve Text(aIdx)
			Redim Preserve lock(aIdx)
			Redim Preserve loaded(aIdx)
			Redim Preserve lvl(aIdx)
			Redim Preserve z(aIdx)
			
			Lvl(aIdx) = 0 : Loaded(aIDX) = 1
			Frame(aIDX) = aArray	'Char contains sprites in 1d array. Use local variables to access sprites.
			z(aIDX) = aArray(0).height
			'msgbox "assigning " & aidx & vbnewline & ubound(mask)
		Else
			msgbox "FloatingText Error, 'Sprites' must be an array!"
		End If

	End Property

	Public Property Get Sprites(aIDX) : Sprites = Frame(aIDX) : End Property

	Public Property Let Prefix(aStr) : Prfx = aStr : End Property
	Public Property Get Prefix : Prefix = prfx : End Property

	Private Function MaxIDX(byval aArray, byref index)	'max, but also returns Index number of highest
		dim idx, MaxItem', str
		for idx = 0 to uBound(aArray)
			if IsEmpty(MaxItem) then 
				if not IsEmpty(aArray(idx)) then 
					MaxItem = aArray(idx)
					index = idx
				end If
			end if
			if not IsEmpty(aArray(idx) ) then 
				If aArray(idx) > MaxItem then MaxItem = aArray(idx) : index = idx
			end If
		Next
		MaxIDX = MaxItem
	End Function 

	Public Sub TextAt(aStr, aX, aY)		'Position text
		dim idx, xx, tmp
		
		'Choose a frame to assign
		dim ChosenFrame 
		If GameTime-LastFrameTime < 40 then	'Modify existing frame if under this MS threshold
			ChosenFrame = LastFrame
			'tb.text = gametime &vbnewline& GameTime-LastFrameTime
		Else
			'Find the highest value in Lvl and return it as ChosenFrame
			Call MaxIDX(Lvl, ChosenFrame)
			LastFrame = ChosenFrame
		end If
		LastFrameTime = GameTime
		'Update Position
		'0 '1 '2
		'a(0) = aX
		'a(1) = aX + Size * index
		Text(ChosenFrame) = aStr 
		tmp = Frame(ChosenFrame)		' tmp = Sprite array contained by char array
		for xx = 0 to uBound(tmp)
			tmp(xx).x = aX + (Size * xx) - (Len(aStr)*Size)/2	'len part centers text 
			tmp(xx).y = aY
		Next'

		'Update Text
		for idx = 0 to uBound(tmp)
			xx = Mid(aStr, idx+1, 1)
			if xx <> "" then
				tmp(idx).visible = True
				tmp(idx).ImageA = Prfx & xx
				tmp(idx).ImageB = ""
			Else
				tmp(idx).visible = False
			end If
		Next
		If TypeName(aStr) <> "String" then FormatNumbers aStr, tmp
	
		'start fading / floating up
		lock(ChosenFrame) = False : Loaded(chosenframe) = False : lvl(chosenframe) = 0
	End Sub

	Private Sub FormatNumbers(aStr, aArray)
		If Len(aStr) >12 then Commalate len(aStr)-12,aArray
		If Len(aStr) > 9 then Commalate len(aStr)-9, aArray
		If Len(aStr) > 6 then Commalate len(aStr)-6, aArray
		If Len(aStr) > 3 Then Commalate len(aStr)-3, aArray
	End Sub
		
	Private Sub Commalate(aIDX, aArray)
		if aIdx-1 > uBound(aArray) then Exit Sub
		aArray(aIdx-1).ImageB = Prfx & "Comma"
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		dim x : for x = 0 to Count
			if not Lock(x) then
				Lvl(x) = Lvl(x) + FadeSpeedUp * frametime	'TODO this requires frametime
				if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
			end if
		next
		Update
	End Sub

	Private Sub Update()	'Handle object updates
		dim x : for x = 0 to Count
			if not Loaded(x) then
				dim opacitycurve	'TODO section this off and make it a function or something
				if lvl(x) > 0.5 then 
					opacitycurve = pSlope(lvl(x), 0, 1, 1, 0)
				Else
					opacitycurve = 1
				end If

				dim xx
				for each xx in Frame(x)
					xx.height = z(x) + (lvl(x) * 100) 
					xx.IntensityScale = opacitycurve
				Next
				If Lock(x) Then
					if Lvl(x) = 1 then Loaded(x) = True	'finished fading
				end if
			end if
		next
	End Sub

End Class



'GIcolorswapper class by nfozzy (0.01a)
'Changes GI colors. Does automatic luminance correction. 
'Be aware: this will change GI Intensity/Opacity values!

'Designed primarily for flashers with script-based fading routines.
'This doesn't handle Light objects and color/colorfull stuff well. 

'Methods:
' - Init - 
'Assign (Sub) 			'Array input. Assigns GI objects. If collection, use ColToArray function to conver to indexed array

' - Usage - 
'Color (Property)			'Input: RGB values in an array. Primary method of changing GI color. IE 'GIc.Color = Array(255,255,255)'
' - Alt Usage - 
'ColorAssign (Property)	'Assign colors for automatic color switching. Setup like this: GIc.ColorAssign(0) = Array(255,5,5)
'changeGI (Sub)			'No Arguments, swaps through the colors defined in ColorAssign sequentially


' - Extra features - 
'ColorRGB (Property) 	'Returns RGB value of last rgb

' - Saving and Loading - 
'Name (Public) 		'String input. Sets name of game in VPReg.stg. IE 'SpaceStationNF'
'Value (Public) 	'String input. Sets Key for color in VPReg.stg.IE 'LastGIcolor'
'SaveColors (Sub)	'No arguments. Call on Table1_exit() sub
'LoadColors (Sub)	'No arguments. Call on Table1_Init() sub

'tb.timerenabled=1
'Sub Tb_Timer() : me.text = gic0.ColorSeq : end Sub

Class GIcolorswapper

	Public ObjArray, BaseOpacity, ColorsArray 'set private
	Public ColorSeq	'set private
	Public Name, Value 'save/load stuff
	Private LastColor	'gets saved on me.SaveColors
	Private cCallback 'will call this sub (just call, not execute!) when color updates

	Private Sub Class_Initialize
		Redim ColorsArray(0)
		ColorSeq = 0
	End Sub

	Public Sub Assign(aArray)
		if not isarray(aArray) then msgbox "GIcolorswapper 'assign' error, input must be an array" : exit Sub
		dim idx, a : a = aArray
		Redim BaseOpacity((uBound(a)))
		for idx = 0 to uBound(a) : 
			if typename(a(idx)) = "Flasher" then 
				BaseOpacity(idx) = a(idx).opacity 
			elseif typename(a(idx) ) = "Light" then
				BaseOpacity(idx) = a(idx).Intensity 
			end if 
		Next
		ObjArray = a
	End Sub

	Public Property Let ColorAssign(aIdx, aArray)
		if aIdx > uBound(ColorsArray) Then
			Redim Preserve ColorsArray(aIDX)
		end If
		if not IsArray(aArray) then msgbox "ColorAssign error, RGB input must be an array" & vbnlewline & " IE: ColorAssign(0) = Array(255,255,255)"
		ColorsArray(aIDX) = aArray '1d within 1d

	End Property
	Public Property Get ColorAssign(aIDX) : ColorAssign = Colorsarray(aIDX) : End Property
	Public Property Get ColorRGB	'return last RGB color 
		if IsArray(LastColor) Then ColorRGB = RGB(LastColor(0),LastColor(1),LastColor(2)) else ColorRGB = RGB(255,255,255) End If 
	End Property

	Public Property Let Color(aRGB)	'in - Array(R, G, B) (integers within array)
		if Not IsArray(aRGB) then debug.print "use an array, not an RGB function idiot!" : Exit Property
		UpdateColors aRGB
	End Property
	Public Property Get Color : Color = LastColor : End Property

	Public Property Let Callback(aStr) : Set cCallback = GetRef(aStr) : End Property

	Public Sub changeGI()	'swap through all colors in ColorAssign
		if ColorSeq >= uBound(ColorsArray) then ColorSeq = 0 :  else ColorSeq = ColorSeq + 1 end If
		dim tmp : tmp = ColorsArray(ColorSeq)	'pick the color array out of ColorsArray (1d within 1d)
		if IsEmpty(tmp) then msgbox "changeGI error: Index '" & ColorSeq & "' is empty" 
		UpdateColors tmp
	End Sub

	Public Property Get Index() : Index = ColorSeq : End property

	Private Sub UpdateColors(aRGB)
		LastColor = aRGB	' for SaveColors
		dim x: for x = 0 to uBound(ObjArray)
			if typename(ObjArray(x) ) = "Flasher" then 
				ObjArray(x).Opacity = BaseOpacity(x) * Lum(aRGB)
				'tb.text = "set" & ObjArray(x).name & "to " & BaseOpacity(x) & " * " & round(Lum(aRGB),4) & vbnewline & round(Lum(aRGB),3)
				ObjArray(x).color = rgb(argb(0), aRGB(1), aRGB(2) )
'				if ObjArray(x).name = "GIsideL" or ObjArray(x).name = "GIsideR" then	'T2 specific thing
'					if rgb(argb(0), aRGB(1), aRGB(2)) = 16777215 then   	'if white, slightly less boring reflection color
'						ObjArray(x).color = rgb(65,127,255)
'					End If
'				End If
			elseif Typename(ObjArray(x) ) = "Light" Then
				ObjArray(x).Intensity = BaseOpacity(x) * Lum(aRGB)
				if ObjArray(x).colorfull then ObjArray(x).colorfull = rgb(argb(0), aRGB(1), aRGB(2) )			
				if ObjArray(x).color then ObjArray(x).color = rgb(argb(0), aRGB(1), aRGB(2) )			
			end If
		Next
		if not IsEmpty(cCallback) then cCallback
		'tb.text = round(lumincoef,3)
	End Sub


	Public Function Lum(aRgb)	'Luminance. input: array, output: value between 0 and 1
		Lum = 255/(argb(0)*0.3 + argb(1)*0.59 + argb(2)*0.11)/1
	End Function



	'Save GI colors to VPReg.stg

	Public Sub SaveColors()
		if IsEmpty(Name) then msgbox "SaveColors error, 'name' is undefined" : Exit Sub
		if IsEmpty(Value) then msgbox "LoadColors error, 'Value' is undefined" : Exit Sub
		if IsEmpty(LastColor) then exit Sub
		SaveValue Name,Value, formatRGB(LastColor)
		'tb.text = "saving:" & vbnewline & formatRGB(GiColorL) & vbnewline & FormatRGB(GiColorR)
	End Sub

	Public Sub LoadColors()
		if IsEmpty(Name) then msgbox "LoadColors error, 'name' is undefined" : Exit Sub
		if IsEmpty(Value) then msgbox "LoadColors error, 'Value' is undefined" : Exit Sub
		if LoadValue(Name, Value) = "" then exit sub
		dim tmp : tmp = LoadValue(Name, Value)
		UpdateColors Array(mid(tmp, 1, 3),mid(tmp, 4, 3),mid(tmp, 7, 3))

	End Sub

	'Save GI colors to VPReg.stg but with indexes
	Public Sub SaveColorsIDX()
		if IsEmpty(Name) then msgbox "SaveColors error, 'name' is undefined" : Exit Sub
		if IsEmpty(Value) then msgbox "LoadColors error, 'Value' is undefined" : Exit Sub
		if IsEmpty(LastColor) then exit Sub
		SaveValue Name,Value, ColorSeq
		'SaveValue Name,Value, 9	'test, inducing error
		'SaveValue Name,Value, "xxx" 'test, inducing error
	End Sub

	Public Sub LoadColorsIDX()
		if IsEmpty(Name) then msgbox "LoadColors error, 'name' is undefined" : Exit Sub
		if IsEmpty(Value) then msgbox "LoadColors error, 'Value' is undefined" : Exit Sub
		if LoadValue(Name, Value) = "" then exit sub
		dim idx : idx = LoadValue(Name, Value)
		If not isnumeric(idx) then SaveValue Name,Value, 0 : exit sub 'if junked up,set value to 0 and exit
		'idx = cInt(mid(idx, 1, 1))	'just pick the first number
		idx = cInt(idx)	'any integer is valid
		if idx > uBound(ColorsArray) then idx = uBound(ColorsArray)
		if IsArray(ColorsArray(idx)) then 'check value of 1d within 1d
			ColorSeq = idx
			dim tmp : tmp = ColorsArray(idx)	'pick the color array out of ColorsArray (1d within 1d)
			if IsEmpty(tmp) then msgbox "LoadColorsIDX error: Index '" & idx & "' is empty" 
			UpdateColors tmp
		end If
	End Sub

	Private Function FormatRGB(Byval aArray)
		dim idx: for idx = 0 to 2 
			if aArray(idx) < 10 and len(aArray(idx)) = 1 then 
				aArray(idx) = "00" & aArray(idx)
			elseif aArray(idx) < 100 and Len(aArray(idx)) = 2 then
				'debug.print "array" & x & "(" & aArray(x) & ") < 100, adding a 0 before it"
				aArray(idx) = "0"  & aArray(idx)
			end if
		Next
		FormatRGB = aArray(0) & aArray(1) & aArray(2)
	End Function
'
'	function DeSat(ByVal aRGB, ByVal aSat)	'simple desaturation function (returns rgb)
'		dim r, g, b, L
'		L = 0.3*aRGB(0) + 0.59*aRGB(1) + 0.1*aRGB(2)
'		r = aRGB(0) + aSat * (L - aRGB(0))
'		g = aRGB(1) + aSat * (L - aRGB(1))
'		b = aRGB(2) + aSat * (L - aRGB(2))
'		'desat = array(r,g,b)	'return array 
'		desat = rgb(r,g,b)		'return rgb
'	End Function
'
'	dim GiColorL, GiColorR 'Save GIcolor in memory
'	Sub GIcolor(input)	'can input rgb in array form
'	'	dim aL   : aL = Array(gil, gilp, gihand)	'cut image swap stuff
'	'	dim aL1 : aL1 = Array("gil", "gilp" ,"gihand")
'	'	dim aR   : aR = Array(gir, girp1, girp2, girp3, girp_corner, girp_corner1, gihand1)
'	'	dim aR1 : aR1 = Array("gir", "girp1", "girp2","girp3","girp_corner","girp_corner","gihand")
'		dim x, c : c = Array(255,255,255)
'		if isarray(input) then 
'			c = input 
'		else
'			select case input
'				'gicolor array(255, 15, 100)	'pink meh
'				case 0	: c = Array(255, 162, 37)'(255, 127, 37)	'Warm White (LED)
'				case 1  : c = Array(255,255,255)	'white
'				case 2	: c = Array(255, 15, 3)		'Red flood 
'				'case 3	: c = Array(133, 11, 255)	'purple Flood
'				case 3	: c = Array(85,13,255)		'95% sat Violet
'				case 4	: c = Array(36, 54, 255)	'Blue
'				case 5	: c = Array(20, 255, 12)	'Green
'				case 6	: c = Array(5, 255, 127)	'aqua
'			end Select	'gicolor array(255,45,0) 'orange
'		end if
'		MatchColorWithLuminance Flashers.obj(2), c, gilevels1 : GiColorL = c
'		if not catchinput(1) then MatchColorWithLuminance Flashers.obj(4), c, gilevels2 : GiColorR = c
'	'	for x = 0 to ubound(aL)
'	'		if aL(x).imagea <> aL1(x) then aL(x).imagea = aL1(x)
'	'	Next
'	'	for x = 0 to ubound(aR)
'	'		if aR(x).imagea <> aR1(x) then aR(x).imagea = aR1(x)
'	'	Next
'	End Sub
		
	
	
End Class


function DeSat(ByVal aRGB, ByVal aSat)	'Input Array Output RGB
	dim r, g, b, L
	L = 0.3*aRGB(0) + 0.59*aRGB(1) + 0.1*aRGB(2)
	r = aRGB(0) + aSat * (L - aRGB(0))
	g = aRGB(1) + aSat * (L - aRGB(1))
	b = aRGB(2) + aSat * (L - aRGB(2))
	'desat = array(r,g,b)	'return array 
	desat = rgb(r,g,b)		'return rgb
End Function




'Keyframe Animation Class

'Setup
'.Update1 - update logic. Use 1 interval
'.Update - update objects. recommended -1 interval
'.Update2 - alternative, updates both on -1

'Properties
'.State - returns if animation state (true or False)
'.Addpoint - Add keyframes. 3 argument sub : Keyframe#, Time Value, Output Value. Keep keyframes sequential, and timeline straight.
'.Modpoint - Modify an existing point
'.Debug - display debug animation (set before .addpoint to get full debug info)
'.Play - Play Animation
'.Pause - Pause mid-animation
'.Callback - string. Sub to call when animation is updated, with one argument sending the interpolated animation info

'Events
'.Callback(argument) - whatever you set callback to. Manually attach animation to this value - ie Showframe, Height, RotX, RotY, whatever...

'Keyframe Animation Class

'Setup
'.Update1 - update logic. Use 1 interval
'.Update - update objects. recommended -1 interval
'.Update2 - TODO alternative, updates both on -1 TODO

'Properties
'.State - returns if animation state (true or False)
'.Addpoint - Add keyframes. 3 argument sub : Keyframe#, Time Value, Output Value. Keep keyframes sequential, and timeline straight.
'.Modpoint - Modify an existing point
'.Debug - display debug animation (set before .addpoint to get full debug info)
'.Play - Play Animation
'.Pause - Pause mid-animation
'.Callback - string. Sub to call when animation is updated, with one argument sending the interpolated animation info

'Events
'.Callback(argument) - whatever you set callback to. Manually attach animation to this value - ie Showframe, Height, RotX, RotY, whatever...

Class cAnimation
	Public DebugOn
	Private KeyTemp(99,1)
	Private Lock, Loaded, StopAnim, UpdateSub
	Private ms, lvl, KeyStep, KeyLVL 'make these private later
	private LoopAnim

	Private Sub Class_Initialize : redim KeyStep(99) : redim KeyLVL(99) : Lock = True : Loaded = True : ms = 0: End Sub

	Public Property Get State : State = not Lock : End Property
	Public Property Let CallBack(String) : UpdateSub = String : End Property

	public Sub AddPoint(aKey, aMS, aLVL)
		KeyTemp(aKey, 0) = aMS : KeyTemp(aKey, 1) = aLVL
		Shuffle aKey
	End Sub

	'  v  v   v    keyframes IDX / (0)
	'	  .
	'	 / \lvl (1)
	'___/	 \___
	'-----MS--------->

	'in -> AddPoint(KeyFrame#, 0) = KeyFrame(Time) 
	'in -> AddPoint(KeyFrame#, 1) = KeyFrame(LVL) 
	'	(1d array conversion)
	'into -> KeyStep(99)
	'into -> KeyLvl(99)
	Private Sub Shuffle(aKey) 'shuffle down keyframe data into 1d arrays 'this sucks, it does't actually shuffle anything
		redim preserve KeyStep(99) : redim preserve KeyLvl(99)
		dim str : str = "shuffling @ " & akey & vbnewline
		dim x : for x = 0 to uBound(KeyTemp)
			if KeyTemp(x,0) <> "" Then
				KeyStep(x) = KeyTemp(x,0) : KeyLvl(x) = KeyTemp(x,1)
			Else
				if x = 0 then msgbox "cAnimation error: Please start at keyframe 0!" : exit Sub
				redim preserve KeyStep(x-1) : redim preserve KeyLvl(x-1) : Exit For
			end If
		Next
		str = str & "uBound step:" & uBound(keystep) & vbnewline & "uBound KeyLvl:" & uBound(KeyLvl) & vbnewline
		If DebugOn then TBanima.text = str & "printing steps:" & vbnewline & PrintArray(keystep) & vbnewline & "printing step values:" & vbnewline & PrintArray(keylvl)
	End Sub

	Private function PrintArray(aArray)	'debug
		dim str, x : for x = 0 to uBound(aArray) : str = str & x & ":" & aArray(x) & vbnewline : Next : printarray = str
	end Function

	Public Sub ModPoint(idx, aMs, aLvl) : KeyStep(idx) = aMs : KeyLVL(idx) = aLvl : End Sub  'modify a point after it's set

	Public Sub Play()	: StopAnim = False : Lock = False : Loaded = False : LoopAnim = False :  End Sub 'play animation
	Public Sub PlayLoop()	: StopAnim = False : Lock = False : Loaded = False : LoopAnim = True: End Sub 'play animation
	Public Sub Pause()	: StopAnim = True : end Sub	'pause animation


	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		'FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		if not lock then
			if ms > keystep(uBound(keystep)) then 
				If LoopAnim then ms = 0 else StopAnim = True : ms = 0	'No looping
			End If
			if StopAnim then Lock = True	'if stopped by script or by end of animation
			if Not Lock Then ms = ms + 1*FrameTime : lvl = LinearEnvelope(ms, KeyStep, KeyLVL)
		end if
		Update
	End Sub

	Public Sub Update1()	'update logic
		if not lock then
			if ms > keystep(uBound(keystep)) then 
				If LoopAnim then ms = 0 else StopAnim = True : ms = 0	'No looping
			End If
			if StopAnim then Lock = True	'if stopped by script or by end of animation
			if Not Lock Then ms = ms + 1 : lvl = LinearEnvelope(ms, KeyStep, KeyLVL)
		end if
	End Sub

	Public Sub Update() 	'Update object
		if Not Loaded then
			if Lock then Loaded = True
			if DebugOn then dim str : str = "ms:" & ms & vbnewline & "lvl:" & lvl & vbnewline & _
									Lock & " " & loaded & vbnewline :	tbanim.text = str
			proc UpdateSub, lvl
		end if
	End Sub



End Class 

'Variable Envelope. Infinite amount of points!
'Keep keyframes in order, bounds equal, and all that.
'	L1			L2		  L3	    L4		L5... etc...	  L(uBound(xKeyFrame)
'          .
'        /  \
'      /      \				   .
'    /          \           _/  \__
'  /              \       _/       \__
'/                  \ . /             \__
'0.........1..........2........3.........4..........etc........uBound(xKeyFrame)


'in animation's case, xInput = MS

Function ColtoArray(aDict)	'converts a collection to an indexed array. Indexes will come out random probably.
	redim a(999)
	dim count : count = 0
	dim x  : for each x in aDict : set a(Count) = x : count = count + 1 : Next
	redim preserve a(count-1) : ColtoArray = a
End Function






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
	Public Lock(140), Loaded(140), OnOff(140)
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
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'Just runs TurnOnStates right now
		TurnOnStates
		dim x,xx : for x = 0 to uBound(OnOff)
			if IsArray(obj(x) ) Then	'if array
				for each xx in obj(x) : xx.IntensityScale = Lvl(x)*Mult(x) : Next
			else						'if single lamp or flasher
				obj(x).Intensityscale = Lvl(x)
			end if
		Next
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
				'if TypeName(lvl(x)) <> "Double" and typename(lvl(x)) <> "Integer" then msgbox "glitch " & 2 & " = " & lvl(x) 
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
	Public Lock(50), SolModValue(50)
	Private UseCallback(50), cCallback(50)
	Public Lvl(50)
	Public Obj(50)
	Public UseFunction(50), cFilter(50)
	private Mult(50)
	Public Name

	Public Burn(50)

	'Public FrameTime
	'Private InitFrame

	Private Sub Class_Initialize() 
		'InitFrame = 0
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
	'Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property	'universal filter
	Public Property Let Filter(Idx, String) : Set cFilter(idx) = GetRef(String) : UseFunction(idx) = True : End Property	'universal filter
	'Public Function FilterOut(idx,aInput) : if UseFunction Then FilterOut = cFilter(idx,aInput) Else FilterOut = aInput End If : End Function
	Public Function FilterOut(idx,aInput) : dim tmp : if UseFunction(idx) Then set tmp = cFilter(idx) : FilterOut = tmp(aInput) Else FilterOut = aInput End If : End Function

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
		dim x,xx : for x = 0 to uBound(lvl)
			if IsArray(obj(x) ) Then	'if array
				for each xx in obj(x) : xx.IntensityScale = Lvl(x)*Mult(x) : Next
			else						'if single lamp or flasher
				obj(x).Intensityscale = Lvl(x)
			end if
		Next
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
		'FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim VelDown
		dim x : for x = 0 to uBound(Lvl)
			if not Lock(x) then 'and not Loaded(x) then
				If lvl(x) <= SolModValue(x) then '+
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				ElseIf Lvl(x) >= SolModValue(x) Then '-
					if Burn(x) then 
						VelDown = BurnFormula(LVL(x),FadeSpeedDown(x))
					Else
						VelDown = FadeSpeedDown(x)
					end If

					Lvl(x) = Lvl(x) - VelDown * FrameTime' * Coef
					if Lvl(x) <= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True					
				End If
			end if
		Next
		Update
	End Sub

	Function BurnFormula(x, aLvl) 'wip
		if x <=0.15 then 
			'BurnFormula = linearenvelope(x, Array(0,0.0075,0.1,0.15), Array(0.001,0.009,0.6,1))
			'BurnFormula = (16.67*x^2 + 4.167*x)
			BurnFormula = (29.6296*x^2 + 2.22222*x)+0.001
			if BurnFormula > 1 then BurnFormula = 1
			if BurnFormula <= 0 then BurnFormula = 1
			BurnFormula = BurnFormula * aLvl
			'dim str : str = round(burnformula/aLvl,6) & vbnewline & vbnewline & round(modlampz.LVL(35),3)
			'if tb.text <> str then tb.text = str
		Else
			BurnFormula = aLvl
		end If
	End Function


	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx, tmp
		for x = 0 to uBound(Lvl)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction(x) then 
						set tmp = cFilter(x)	'pull function from array so it's usable
						for each xx in obj(x) : xx.IntensityScale = tmp(abs(Lvl(x))*mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*mult(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction(x) then 
						set tmp = cFilter(x)	'pull function from array so it's usable
						obj(x).Intensityscale = tmp(abs(Lvl(x))*mult(x))
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





'***********************
'	Flipper Hacks
'***********************
'Fastflips - Bypasses pinmame callback for faster flippers when possible
dim vpmFlipsSAM : set vpmFlipsSAM = New cvpmFlipsSAM : vpmFlipsSAM.Name = "vpmFlipsSAM"
Function NullFunctionSS(a)::End Function 'this should already be in core.vbs
'*************************************************
Sub InitVpmFlipsSAM()
    vpmFlipsSAM.CallBackL = SolCallback(vpmflipsSAM.FlipperSolNumber(0))          'Lower Flippers
    vpmFlipsSAM.CallBackR = SolCallback(vpmFlipsSAM.FlipperSolNumber(1))
'    On Error Resume Next
'        if cSingleLflip or Err then vpmFlipsSAM.CallbackUL=SolCallback(vpmFlipsSAM.FlipperSolNumber(2))
'        err.clear
'        if cSingleRflip or Err then vpmFlipsSAM.CallbackUR=SolCallback(vpmFlipsSAM.FlipperSolNumber(3))
'    On Error Goto 0
   ' msgbox "~Debug-Active Flipper subs~" & vbnewline & vpmFlipsSAM.SubL &vbnewline& vpmFlipsSAM.SubR &vbnewline& vpmFlipsSAM.SubUL &vbnewline& vpmFlipsSAM.SubUR' &
	vpmFLipsSAM.TiltObjects = False 'not functional anyways
End Sub
 
'indexed properties for now     '0=left 1=right 2=Uleft 3=URight
vpmFlipsSam.FlipperSolNumber(0) = slLFlipper
vpmFlipsSam.FlipperSolNumber(1) = slRFlipper
'vpmFlipsSam.FlipperSolNumber(2) = sULFlipper
'vpmFlipsSam.FlipperSolNumber(3) = 12
 
InitVpmFlipsSAM
vpmflipssam.tiltsol True    'make sure this is on for SS flippers
 
'New Command -
'vpmflipsSam.RomControl=True/False      -    True for rom controlled flippers, False for FastFlips (Assumes flippers are On)
dim bipstr 'Debug
dim LagCheck, LagTolerance
LagTolerance = 262	'Lag tolerance for GI off -> rom control (milliseconds)
Sub FastFlipsUpdate()	
	dim str, ballcount, bool
	ballcount = ubound(Getballs)+1'ubound(getballs)+2-bsLeftKick.balls

	if BallCount = 0 Then	'If no balls on table, then use rom flippers 
		Bool=True
	end If
	
	if not GIstate then 	'If GI is off, use rom flippers (after LagTolerance)
		if IsEmpty(LagCheck) then LagCheck = GameTime
		if gametime - LagCheck >= LagTolerance then
			Bool = True : str = "GO! " & LagTolerance & " LagTolerance"
		end If
	Else
		LagCheck = Empty
	End If
	vpmflipsSAM.RomControl = Bool
		
	BIPstr = vpmflipsSAM.RomControl & vbnewline & _ 
			 "Ballcount " & Ballcount & vbnewline & _
			 "GIoff? " & GIstate & vbnewline & _
			"(lagcheck " & lagcheck & vbnewline & str

End Sub


 
Class cvpmFlipsSAM  'test fastflips with support for both Rom and Game-On Solenoid flipping
    Public TiltObjects, DebugOn, Name, Delay
    Public SubL, SubUL, SubR, SubUR, FlippersEnabled,  LagCompensation, ButtonState(3), Sol 'set private
    Public RomMode, SolState(3)'set private
    Public FlipperSolNumber(3)  '0=left 1=right 2=Uleft 3=URight
 
    Private Sub Class_Initialize()
        dim idx :for idx = 0 to 3 :ButtonState(idx)=0:SolState(idx)=0: Next : Delay=0: FlippersEnabled=0: DebugOn=0 : LagCompensation=0 : Sol=0 : TiltObjects=1
        SubL = "NullFunctionSS": SubR = "NullFunctionSS" : SubUL = "NullFunctionSS": SubUR = "NullFunctionSS"
        RomMode=True :FlipperSolNumber(0)=sLLFlipper :FlipperSolNumber(1)=sLRFlipper :FlipperSolNumber(2)=sULFlipper :FlipperSolNumber(3)=sURFlipper
    End Sub
 
    'set callbacks
    Public Property Let CallBackL(aInput) : if Not IsEmpty(aInput) then SubL  = aInput :SolCallback(FlipperSolNumber(0)) = name & ".RomFlip(0)=":end if :End Property   'execute
    Public Property Let CallBackR(aInput) : if Not IsEmpty(aInput) then SubR  = aInput :SolCallback(FlipperSolNumber(1)) = name & ".RomFlip(1)=":end if :End Property
    Public Property Let CallBackUL(aInput): if Not IsEmpty(aInput) then SubUL = aInput :SolCallback(FlipperSolNumber(2)) = name & ".RomFlip(2)=":end if :End Property   'this should no op if aInput is empty
    Public Property Let CallBackUR(aInput): if Not IsEmpty(aInput) then SubUR = aInput :SolCallback(FlipperSolNumber(3)) = name & ".RomFlip(3)=":end if :End Property
   
    Public Property Let RomFlip(idx, ByVal aEnabled)
        aEnabled = abs(aEnabled)
        SolState(idx) = aEnabled
        If Not RomMode then Exit Property
        Select Case idx	
            Case 0 : execute subL & " " & aEnabled
            Case 1 : execute subR & " " & aEnabled
            Case 2 : execute subUL &" " & aEnabled
            Case 3 : execute subUR &" " & aEnabled
        End Select
    End property

    Public Property Let RomControl(aEnabled)        'todo improve choreography
		if aEnabled <> RomMode then 'disregard redundant updates
			RomMode = aEnabled
			If aEnabled then                    'Switch to ROM solenoid states or button states
				Execute SubL &" "& SolState(0)
				Execute SubR &" "& SolState(1)
				Execute SubUL &" "& SolState(2)
				Execute SubUR &" "& SolState(3)
			Else
				Execute SubL &" "& ButtonState(0)
				Execute SubR &" "& ButtonState(1)
				Execute SubUL &" "& ButtonState(2)
				Execute SubUR &" "& ButtonState(3)
			End If
		end If
    End Property
    Public Property Get RomControl : RomControl = RomMode : End Property
 
    Public Property Let Solenoid(aInput) : if not IsEmpty(aInput) then Sol = aInput : end if : End Property 'set solenoid
    Public Property Get Solenoid : Solenoid = sol : End Property
   
    'call callbacks
    Public Sub FlipL(ByVal aEnabled)
        aEnabled = abs(aEnabled) 'True / False is not region safe with execute. Convert to 1 or 0 instead.
        ButtonState(0) = aEnabled   'track flipper button states: the game-on sol flips immediately if the button is held down (1.1)
        If FlippersEnabled and Not Romcontrol or DebugOn then execute subL & " " & aEnabled end If
    End Sub
 
    Public Sub FlipR(ByVal aEnabled)
        aEnabled = abs(aEnabled) : ButtonState(1) = aEnabled
        If FlippersEnabled and Not Romcontrol or DebugOn then execute subR & " " & aEnabled end If
    End Sub
 
    Public Sub FlipUL(ByVal aEnabled)
        aEnabled = abs(aEnabled)  : ButtonState(2) = aEnabled
        If FlippersEnabled and Not Romcontrol or DebugOn then execute subUL & " " & aEnabled end If
    End Sub
 
    Public Sub FlipUR(ByVal aEnabled)
        aEnabled = abs(aEnabled)  : ButtonState(3) = aEnabled
        If FlippersEnabled and Not Romcontrol or DebugOn then execute subUR & " " & aEnabled end If
    End Sub
   
    Public Sub TiltSol(aEnabled)    'Handle solenoid / Delay (if delayinit)
        If delay > 0 and not aEnabled then  'handle delay
            vpmtimer.addtimer Delay, Name & ".FireDelay" & "'"
            LagCompensation = 1
        else
            If Delay > 0 then LagCompensation = 0
            EnableFlippers(aEnabled)
        end If
    End Sub
   
    Sub FireDelay() : If LagCompensation then EnableFlippers 0 End If : End Sub
   
    Public Sub EnableFlippers(aEnabled) 'private
        If aEnabled then execute SubL &" "& ButtonState(0) :execute SubR &" "& ButtonState(1) :execute subUL &" "& ButtonState(2): execute subUR &" "& ButtonState(3)':end if
        FlippersEnabled = aEnabled
        If TiltObjects then vpmnudge.solgameon aEnabled
        If Not aEnabled then
            execute subL & " " & 0 : execute subR & " " & 0
            execute subUL & " " & 0 : execute subUR & " " & 0
        End If
    End Sub
End Class


Function Atan2(x,y)
	If x > 0 Then
		Atan2 = Atn(y/x)
	ElseIf x < 0 Then
		Atan2 = Sgn(y) * ((4*Atn(1)) - Atn(Abs(y/x)))
	ElseIf y = 0 Then
		Atan2 = 0
	Else
		Atan2 = Sgn(y) * (4*Atn(1)) / 2
	End If
End Function

Function RotatePoint(Cx, Cy, ByVal aAngle, ByVal p) 'Point must be an array!
	dim out(1)
	Out(0) = (cos(aAngle) * (p(0) - cx) - sin(aAngle) * (p(1) - cy) + cx)
	Out(1) = (sin(aAngle) * (p(0) - cx) + cos(aAngle) * (p(1) - cy) + cy)
	RotatePoint = Out
End Function

Function RotatePoint2(ByVal Center, ByVal aAngle, ByVal p) 'Point must be an object! Center point must be an array!
	dim out(1)
	Out(0) = (cos(aAngle) * (p.x - Center(0)) - sin(aAngle) * (p.y - Center(1)) + Center(0))
	Out(1) = (sin(aAngle) * (p.x - Center(0)) + cos(aAngle) * (p.y - Center(1)) + Center(1))
	RotatePoint2 = Out
End Function

Function RotateVect(ByVal aAngle, ByVal p)	'swapped Out(1) with out(0). X first 
	dim out(1)
	Out(1) = (cos(aAngle) * (p.velx) - sin(aAngle) * (p.vely))
	Out(0) = (sin(aAngle) * (p.velx) + cos(aAngle) * (p.vely))
	RotateVect = Out
'	tbpl1.text = "angle = " & aangle & vbnewline & _
'			round(rf.radtodeg(aAngle),2) & vbnewline & _
'			"in" & vbnewline & _
'			round(p.velx,2) & vbnewline & round(p.vely,2) & vbnewline &  _
'			"out=" & vbnewline & _ 
'			round(out(0),2) & vbnewline & round(out(1),2)
End Function

Function RVect(ByVal aAngle, ByVal p)	'debug
	dim out(1)
	Out(0) = (cos(aAngle) * (p(0)) - sin(aAngle) * (p(1)))
	Out(1) = (sin(aAngle) * (p(0)) + cos(aAngle) * (p(1)))
	Rvect = Out
End Function


Function SlopeToDeg(byval p)	'debug
	SlopeToDeg = Atan2((p(1)*-1)+0.001, p(0)+0.001)* (180/pi)
End Function


Public Sub MatchToPoint(ByRef aDest, ByVal aSrc) 'for debug only
	if IsArray(aDest) then	'if dest is a 1d array
		dim a
		if IsArray(aSrc) then
			aDest(0) = aSrc(0) : aDest(1) = aSrc(1)
		Else
			aDest(0) = aSerc.x : aDest(1) = aSerc.y
		end If
	Else					'if dest is an object with x / y properties
		if IsArray(aSrc) then 
			aDest.x = aSrc(0) : aDest.y = aSrc(1)
		else
			aDest.x = aSrc.x : aDest.Y = aSrc.y
		end If
	end if
End Sub


'*******************************************
'  Flippers
'*******************************************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
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

' Flipper collide subs
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub

' This subroutine updates the flipper shadows and visual primitives
Sub FlipperVisualUpdate
	FlipperLSh.RotZ = LeftFlipper.CurrentAngle
	FlipperRSh.RotZ = RightFlipper.CurrentAngle
'	LFLogo.RotZ = LeftFlipper.CurrentAngle
'	RFlogo.RotZ = RightFlipper.CurrentAngle
End Sub


'******************************************************
'****  GNEREAL ADVICE ON PHYSICS
'******************************************************
'
' It's advised that flipper corrections, dampeners, and general physics settings should all be updated per these 
' examples as all of these improvements work together to provide a realistic physics simulation.
'
' Tutorial videos provided by Bord
' Flippers: 	https://www.youtube.com/watch?v=FWvM9_CdVHw
' Dampeners: 	https://www.youtube.com/watch?v=tqsxx48C6Pg
' Physics: 		https://www.youtube.com/watch?v=UcRMG-2svvE
'
'
' Note: BallMass must be set to 1. BallSize should be set to 50 (in other words the ball radius is 25) 
'
' Recommended Table Physics Settings
' | Gravity Constant             | 0.97      |
' | Playfield Friction           | 0.15-0.25 |
' | Playfield Elasticity         | 0.25      |
' | Playfield Elasticity Falloff | 0         |
' | Playfield Scatter            | 0         |
' | Default Element Scatter      | 2         |
'
' Bumpers
' | Force         | 9.5-10.5 |
' | Hit Threshold | 1.6-2    |
' | Scatter Angle | 2        |
' 
' Slingshots
' | Hit Threshold      | 2    |
' | Slingshot Force    | 4-5  |
' | Slingshot Theshold | 2-3  |
' | Elasticity         | 0.85 |
' | Friction           | 0.8  |
' | Scatter Angle      | 1    |



'******************************************************
'****  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level well need the following:
'	1. flippers with specific physics settings
'	2. custom triggers for each flipper (TriggerLF, TriggerRF)
'	3. an object or point to tell the script where the tip of the flipper is at rest (EndPointLp, EndPointRp)
'	4. and, special scripting
'
' A common mistake is incorrect flipper length.  A 3-inch flipper with rubbers will be about 3.125 inches long.  
' This translates to about 147 vp units.  Therefore, the flipper start radius + the flipper length + the flipper end 
' radius should  equal approximately 147 vp units. Another common mistake is is that sometimes the right flipper
' angle was set with a large postive value (like 238 or something). It should be using negative value (like -122).
'
' The following settings are a solid starting point for various eras of pinballs.
' |                    | EM's           | late 70's to mid 80's | mid 80's to early 90's | mid 90's and later |
' | ------------------ | -------------- | --------------------- | ---------------------- | ------------------ |
' | Mass               | 1              | 1                     | 1                      | 1                  |
' | Strength           | 500-1000 (750) | 1400-1600 (1500)      | 2000-2600              | 3200-3300 (3250)   |
' | Elasticity         | 0.88           | 0.88                  | 0.88                   | 0.88               |
' | Elasticity Falloff | 0.15           | 0.15                  | 0.15                   | 0.15               |
' | Fricition          | 0.8-0.9        | 0.9                   | 0.9                    | 0.9                |
' | Return Strength    | 0.11           | 0.09                  | 0.07                   | 0.055              |
' | Coil Ramp Up       | 2.5            | 2.5                   | 2.5                    | 2.5                |
' | Scatter Angle      | 0              | 0                     | 0                      | 0                  |
' | EOS Torque         | 0.3            | 0.3                   | 0.275                  | 0.275              |
' | EOS Torque Angle   | 4              | 4                     | 6                      | 6                  |
'


'******************************************************
' Flippers Polarity (Select appropriate sub based on era) 
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

''*******************************************
'' Early 90's and after
'
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

Function dAtn(degrees)
	dAtn = atn(degrees * Pi/180)
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


'*******************************************
'  Ramp Triggers
'*******************************************
Sub ramptrigger01_hit()
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub RampSound9_hit()
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub

Sub ramptrigger001_hit()
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub ramptrigger003_hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

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


'******************************************************
'**** END RAMP ROLLING SFX
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
'		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
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
'***

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

Sub cRollovers_Hit(idx)
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

Sub Metals2_Hit (idx)
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

Sub Aapron_Hit (idx)
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
'                TRACK ALL BALL VELOCITIES
'                 FOR RUBBER DAMPENER AND DROP TARGETS
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



'******************************************************
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************
' To add these slingshot corrections:
' 	- On the table, add the endpoint primitives that define the two ends of the Slingshot
'	- Initialize the SlingshotCorrection objects in InitSlingCorrection
' 	- Call the .VelocityCorrect methods from the respective _Slingshot event sub


dim LS : Set LS = New SlingshotCorrection
dim RS : Set RS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection

	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS

	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS

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

'' The following sub are needed, however they may exist somewhere else in the script. Uncomment below if needed
'Dim PI: PI = 4*Atn(1)
'Function dSin(degrees)
'	dsin = sin(degrees * Pi/180)
'End Function
'Function dCos(degrees)
'	dcos = cos(degrees * Pi/180)
'End Function
'
Function RotPoint(x,y,angle)
    dim rx, ry
    rx = x*dCos(angle) - y*dSin(angle)
   ry = x*dSin(angle) + y*dCos(angle)
'    RotPoint = Array(rx,ry)
End Function

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

' VR PLUNGER ANIMATION
Sub TimerPlunger_Timer
	If PinCab_Shooter.Y < -115.78 then
		PinCab_Shooter.Y = PinCab_Shooter.Y + 5
	End If
End Sub

Sub TimerPlunger2_Timer
	PinCab_Shooter.Y = -215.78 + (5* Plunger.Position) -20
End Sub

' History Log for previous versions:

'Version 1.43 
'Better error warnings and workarounds for some reported script errors
'Cleaned up the hole scripts
'Rerendered GI and fixed a few incorrect reflections
'Instruction / Scorecard and LEDmod now saves on exit
'Fixed some issues when running the prototype rom

'TODO / known issues
'Updated ramp visuals
'Level mesh
'GI DOF scripting(?)

'Version 1.42 
'More accurate GI, uses multiple rom-controlled strings again instead of all merged together
'	- Saves GI colors on exit like Space Station / T2.
'Fixed some sidewall reflections and updated masks for the new cab mesh
'Adjsted DT spider angle to be less distorted, adjusted SSFS spider to be much more playable
'Sweep target scripting on spell targets
'Cleaned up initialization scripting to hopefully reduce errors

'Version 1.41
'Split Frog target collisions. The ball will rebound less on direct hits.
'Fixed an issue with ball shadows
'Tweaked flasher caps
'Sped up high score values and a few other small tweaks to Floating Text
'Split apart the volume control into SoundLevelMult and SoundLevelMultCoils
'Set many objects to use static rendering

'Version 1.4
'Added a Fastflips hack. Works by switching seamlessly between Rom and Direct-Controlled flippers.
'Flash caps have been redesigned to play better with vp10.5's screen-space reflections
'Ramp collision meshes have been improved for accuracy -also the upper sling area.
'New physics featuring a redesigned Polarity flipper script (Can be disabled at the top of the script)
'Optional floating text scores (See options)
'New cabinet mesh with siderails, speaker panel, and backglass (nonfunctional atm, no FSS)
'Cleaned up the script with a few improvements along the way -
' - Better ball rolling SFX script
' - Boogieman animations have been improved a bit
' - Added a keyboard nudge script

'Version 1.31
'Replaced a few light images with better quality ones (new images are FlashAmbient512 and FlashQuad512. Use them, should be good common resources)
'Proper soundFX gain pass
'Added animated scorecard

'Version 1.3 Changelog by nFozzy
'Optimization
'-new GI in 3 flavors: Soft White, Cool White, and Colorized
'New Boogiemen
'New physics
'Added (limited) Support for pre-production roms with the kickback
'-The aux light board isn't emulated properly, so crate and deadhead LEDs are not working atm.
'-these roms have very early code and therefore simplified game rules.

'Notes:
'You can change the GI in-game by hitting the Right magnasave while holding down the left magnasave
'The dancing boogiemen feature must be toggled on in the ROM. It's Feature Adjustment 32.
'STUTTERING ON OLDER VIDEO CARDS: please consider utilizing 'max texture dimensions' in the video options
'-this table utilizes an 8K(!) Playfield and may overload your ram as a result!

