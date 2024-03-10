Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const BallSize = 52
Const BallMass = 2.2
'Const Lumen = 10

'*********************************************************************************************************
'***                                    to show dmd in desktop Mod                                     ***
Dim UseVPMDMD, DesktopMode
DesktopMode = Table1.ShowDT  
If NOT DesktopMode Then 
	UseVPMDMD = False		'hides the internal VPMDMD when using the color ROM or when table is in Full Screen and color ROM is not in use
	MetalSides.visible = False'put True or False if you want to see or not the wood side in fullscreen mod
	LockDown.visible = False
end if 
If DesktopMode Then UseVPMDMD = True							'shows the internal VPMDMD when in desktop mode 
'*********************************************************************************************************



'***************************************************
'*                  Options                        *
'***************************************************
'*******************************************************************
'*  Wire Color  1 : Orange   or   0 : Metal                        *
'*  BE CAREFUL, it doesn't work if RampWireA is Static Rendering   *
'*  And it is by default for optimising, so you can uncheck the    *
'*  box if you have a good computer ;)                             *
'*******************************************************************
Dim WireColor
	WireColor = 0	
'*******************************************************************
'***************************************************
'*  Ball  1 : BasketBall   or   0 : MetalBall      *
'***************************************************
Dim BallType
	BallType = 0
'***************************************************
'***************************************************
'*  Led = 0 : for some led parts or 0 for none     *
'***************************************************
dim led, ledramp
led = 1
ledramp = 0 '0 not led ramps, 1 OK for led ramps
'***************************************************
' ***************** Some volumes *******************
Const VolDiv = 6000	'Lower number, louder ballrolling/collition sound
Const VolCol = 10 	'Ball collition divider ( voldiv/volcol )
Const RolVol = 5000	'Ball Rolling
Const MroVol = 45	'Wire Ramps Rolling
Const MroVolA = 30	'Metal Ramps Rolling
Const ProVol = 0.2	'Plastic Ramps Rolling
Const MsideVol = 2	'Metal Sides sound
' **************************************************
'***************************************************
'*  Lola = 1 : for 3d Lola and her ball - 0 for no *
'***************************************************
Dim Lola, SpotInt
Lola = 1
'***************************************************
'***************************************************
'*     Difficulty for skill shot Basket Plunger    *
'*     4 to 10 max - 4 easy - 10 more difficult    *
'***************************************************
Dim PSSB
PSSB = 4
'***************************************************
'***********************************************************
'* Sides Mod by Mussinger  - 1 = yes - 0 = Original Sides  *
'***********************************************************
Dim SidesMod
SidesMod = 0
'***********************************************************
'*********************************************************************************
'*  FlipperLight = 1 : for more light over flippers - Slingshots - 0 for no more *
'*  and or the fl variable to add more light only to flippers                    *
'*********************************************************************************
Dim FlipperLight, fl
FlipperLight = 1
fl = 0.5 'Try numbers to 1 or 2 maxi, better is 0.1 to 0.9
'*********************************************************************************

LoadVPM "01000000","SEGA.vbs",3.02

Const UseLamps=0,UseGI=0,UseSolenoids=2, UseSync=true 
Const SSolenoidOn="solon",SSolenoidOff="soloff",SFlipperOn="FlipperUp",SFlipperOff="FlipperDown",sCoin="coin3"

Dim preload
Preload = 1


'**********************************************************
'*  LUT Options - Script Taken from The Flintstones       *
'* All lut examples taken from The Flintstones, TOTAN and *
'*       007 - Big Thanks for all the samples             *
'**********************************************************
Dim LUTmeUP:'LUTMeUp = 1 '0 = No LUT Initialized in the memory procedure
Dim DisableLUTSelector:DisableLUTSelector = 0  ' Disables the ability to change LUT option with magna saves in game when set to 1
Const MaxLut = 10
'********************************************************


'*************** LUT Memory by JPJ ****************
dim FileObj, File, LUTFile, Txt, TxtTF 'Dim for LUT Memory Outside Init's SUB
	Set FileObj = CreateObject("Scripting.FileSystemObject")
		If Not FileObj.FileExists(UserDirectory & "SpaceJamLUT.txt") then
			LutMeUp = 1:WriteLUT
		End if
		If FileObj.FileExists(UserDirectory & "SpaceJamLUT.txt") then
			Set LUTFile=FileObj.GetFile(UserDirectory & "SpaceJamLUT.txt")
			Set Txt=LUTFile.OpenAsTextStream(1,0) 'Number taken from the file
			TxtTF = cint(txt.readline)
			LUTMeUp = TxtTF:SetLUT
		if LutMeUp >MaxLut or LutMeUp <0 or LutMeUp = Null Then 
			LutMeUp = 1
		End if
	End if
'****************************************************




'***************************************
'  				Table Ini
'***************************************

Dim mbackboard, bsTrough, bsLaunch, DtBank, bsWabbitHole, bsVUK, bsSVUK, mBBMagnet,mSnagger, x, xx, bump1, bump2, bump3, PlungerIM, VUKT, VUKM, mhole1, mhole2, mhole3, cbleft

RealTime.enabled = 1

NoUpperLeftFlipper
NoUpperRightFlipper

	LUTBox.Visible = 0

Sub Table1_Init()

If lola = 0 Then
	LolaA.visible = 0
	LolaB.visible = 0
	Lolac.visible = 0
	LolaProjector.visible = 0
	LolaSpot.intensity = 0
End if

	'Create Controller Object, and read in options
	DiverterA_Off.IsDropped=1

	On Error Resume Next
		Controller.GameName = "spacejam"
		If Err Then MsgBox Err.Description, 0, "Game " & gameName & " Not found." : Exit Sub
	On Error Goto 0
	Controller.SplashInfoLine = "Space Jam - Sega, 1996" & vbNewLine & "Edizzle, JPJ, TeamPP"
	Controller.HandleKeyboard = False
	Controller.ShowTitle = False
	Controller.ShowFrame = False
	Controller.ShowDMDOnly = True
	Controller.HandleMechanics = 0
	Controller.DIP(0) = &H00 ' Set dipswitch to USA
	Controller.hidden=DesktopMode
	On Error Resume Next
		Controller.Run
		If Err Then MsgBox Err.Description
	On Error Goto 0

	PinMAMETimer.Interval = PinMAMEInterval

'********** Nudge **************
	vpmNudge.TiltSwitch=56
	vpmNudge.Sensitivity=3
	vpmNudge.TiltObj=Array(LeftSlingshot,RightSlingshot,Bumper2b,Bumper3b,Bumper1b)
'*******************************

set bsTrough = new cvpmBallStack      
    bsTrough.InitSw 0,15,14,13,12,11,0,0
	bsTrough.Balls = 5
	bsTrough.InitKick BallRelease,80,5
	bsTrough.InitEntrySnd SoundFX("BallRel",DOFContactors), SoundFX("Solenoid",DOFContactors)

Set bsWabbitHole = new cvpmBallStack
	bsWabbitHole.InitSaucer wabbit, 46, 177, 33 '46, 190, 33
	bsWabbitHole.KickZ =56 '56
	bsWabbitHole.InitExitSnd SoundFX("scoopexit",DOFContactors),SoundFX("Solenoid",DOFContactors)

Set DTBank = New cvpmDropTarget  
	With DTBank
   		.InitDrop Array(sw17,sw18,sw19),Array(17,18,19)
		.InitSnd SoundFX("target_drop",DOFDropTargets),SoundFX("reset_drop",DOFContactors)
	End With

	' Backboard Magnet
Set mbackboard = New cvpmMyMagnet
	With mbackboard
		.InitMagnet BMagnet, 300
		.Solenoid = 14
		.GrabCenter = 1
		.MagnetHeight = 270
	End With
 
     ' Impulse Plunger
    Const IMPowerSetting = 60
    Const IMTime = 0.6
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
		.Switch 16
        .Random 1.5
        .InitExitSnd "plunger2", "plunger"
        .CreateEvents "plungerIM"
    End With



	for each xx in DL:
		xx.blendDisableLighting = 0
		playfieldOff.opacity = 100
	Next
	for each xx in DL2:
		xx.blendDisableLighting = 0.4
		LolaB.blenddisablelighting = lightb + ll + 7
	Next
		
		PFShadows.opacity = 0
		RampB.image = "rampeBOff"
		FlasherbaseBumperA.blenddisablelighting = 0.5
		FlasherbaseBumperB.blenddisablelighting = 0.5
		FlasherbaseBumperC.blenddisablelighting = 0.5
		BoisA.blenddisablelighting = 0.2
		BoisB.blenddisablelighting = 0.2
		Panniersorange.blenddisablelighting = 0.5
		sol1.blenddisablelighting = 0.5
		Lflogo.image = "SJflipper-LOff"
		Rflogo.image = "SJflipper-ROff"
		RampB.blenddisablelighting = 0
		LightFF17.State = 0
		flashFF17.IntensityScale = 0
		FlashlightA.Amount = 0
		FlashlightA.IntensityScale = 0
		FlashlightB.Amount = 0
		FlashlightB.IntensityScale = 0
		FlashlightCC.Amount = 0
		FlashlightCC.IntensityScale = 0
		rampeMetal.blenddisablelighting = 0.1
		FlashlightB1.Amount = 0
		FlashlightB1.IntensityScale = 0
		FlashlightB2.Amount = 0
		FlashlightB2.IntensityScale = 0
			WhiteLumF.Amount = 10
			WhiteLumF.IntensityScale = 0
			WhiteLumF.opacity = 20

'	Flashers ini

Dim FlashLevelF(20), objbase(20), objlit(20), objflasher(20), objlight(20)
Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
InitFlasher 1, "white" : InitFlasher 2, "white" : InitFlasher 3, "white" : InitFlasher 4, "white" : InitFlasher 5, "white" 
InitFlasher 6, "red" : InitFlasher 7, "red" 
RotateFlasher 1,-40 : RotateFlasher 2,-40:RotateFlasher 3,90 : RotateFlasher 4,90 : RotateFlasher 5,90

If BallType = 1 then
		Table1.BallFrontDecal = "ballBasket"
		Table1.BallDecalMode = True
		Table1.Ballimage="black"
		Table1.DefaultBulbIntensityScale=0
		Table1.BallPlayfieldReflectionScale=0
		CapKicker1.Createball:CapKicker1.Kick 0,3:CapKicker1.enabled = 0
	Else
		'Table1.BallFrontDecal = ""
		Table1.BallDecalMode = False
		Table1.BallImage = "ball"
		'Table1.DefaultBulbIntensityScale = 0,1
		'Table1.BallPlayfieldReflectionScale= 0,1
		CapKicker1.Createball:CapKicker1.Kick 0,3:CapKicker1.enabled = 0
End If
if led = 1 then
		RainbowTimer.enabled = 1
	Else
		RainbowTimer.enabled = 0
End If
if WireColor = 1 Then
		RampWireA.material = "Metallic Orange"
	Else
		RampWireA.material = "Metal JP2"
End if

SpotInt = 20

if SidesMod = 1 Then
		Sides.image = "SidesMod"
	Else
		Sides.image = "Sides"
End if


End Sub
'**************************************************



SolCallback(1)		="bsTrough.SolOut"				'Trough Up-Kicker
SolCallback(2)		="solAutofire"					'Auto Launch
SolCallBack(3) 		= "ResetDrops"					'3-Bank Drop Target Reset
SolCallback(4)		="SolDiv1"						'Jump Ball Top Kicker
SolCallback(5)		="SolDiv2"						'Jump Ball Right Kicker
SolCallback(6)		="SolVUK"						'Jump Ball VUK								
SolCallback(7)		="Jumpmag"						'Jump Ball Magnet
'SolCallback(9)		="								'Top Turbo Bumper
'SolCallback(10)	="								'Left Turbo Bumper
'SolCallback(11)	="								'Right Turbo Bumper
'SolCallback(12)	="								'Left Slingshot
'SolCallback(13)	="								'Right Slingshot
SolCallback(14)	    	="solBasketMagnet"				'Basket Magnet
SolCallback(17)		="Flash17"						'17 - Flash Final JAM*1
SolCallback(18)		="SolRdiv"						'Ramp Diverter
SolCallback(19)		="Flash19"						'19 - Flash Jump Ball*2
SolCallback(20)		="bsWabbitHole.SolOut"			'Wabbit Hole VUK
SolCallback(21)		="Flash21"						'21 - Flash Skill*3
SolCallback(22)		="Flash22"						'22 - Flash Basket RAMP*4
SolCallback(23)		="Flash23"						'23 - Flash TOP-LT*2 TOP-RT*2
'SolCallback(24)		="SetFlash 124,"				'Optional Coin Meter
SolCallback(25)		="FlashF1s"						'F1 - Flash Orbit Arrow*1
SolCallback(26)		="FlashF2s"						'F2 - Flash C-Ball Arrow*1
SolCallback(27)		="FlashF3s"						'F3 - Flash B Ramp Arrow*1
SolCallback(28)		="FlashF4s"						'F4 - Flash J-Ball Arrow*1
SolCallback(29)		="FlashF5s"						'F5 - Flash R Ramp Arrow*1
SolCallback(30)		="FlashF6s"						'F6 - Flash R Orbit Arrow*1
SolCallback(31)		="FlashF7s"						'F7 - Flash 3 Bank Drop*2
SolCallback(32)		="FlashF8s"						'F8 - Flash POPS*3

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

'********************************************************************************

Sub flashf1s(Enabled)
	if enabled Then
			lightF1.state = 1
			flashF1.IntensityScale = 45
		else
			lightF1.state = 0
			flashF1.IntensityScale = 0
	end If
End Sub
Sub flashf2s(Enabled)
	if enabled Then
			lightF2.state = 1
			flashF2.IntensityScale = 45
		else
			lightF2.state = 0
			flashF2.IntensityScale = 0
	end If
End Sub
Sub flashf3s(Enabled)
	if enabled Then
			lightF3.state = 1
			flashF3.IntensityScale = 45
		else
			lightF3.state = 0
			flashF3.IntensityScale = 0
	end If
End Sub
Sub flashf4s(Enabled)
	if enabled Then
			lightF4.state = 1
			flashF4.IntensityScale = 45
		else
			lightF4.state = 0
			flashF4.IntensityScale = 0
	end If
End Sub
Sub flashf5s(Enabled)
	if enabled Then
			lightF5.state = 1
			flashF5.IntensityScale = 45
		else
			lightF5.state = 0
			flashF5.IntensityScale = 0
	end If
End Sub
Sub flashf6s(Enabled)
	if enabled Then
			lightF6.state = 1
			flashF6.IntensityScale = 45
		else
			lightF6.state = 0
			flashF6.IntensityScale = 0
	end If
End Sub
Sub flashf7s(Enabled)
	if enabled Then
			lightF7.state = 1
			flashF7.IntensityScale = 45
		else
			lightF7.state = 0
			flashF7.IntensityScale = 0
	end If
End Sub
Sub flashf8s(Enabled)
	if enabled Then
			lightPop1.state = 1:lightPop2.state = 1:lightPop3.state = 1
			flashF8a.IntensityScale = 25:flashF8b.IntensityScale = 35:flashF8c.IntensityScale = 20
		else
			lightPop1.state = 0:lightPop2.state = 0:lightPop3.state = 0
			flashF8a.IntensityScale = 0:flashF8b.IntensityScale = 0:flashF8c.IntensityScale = 0
	end If
End Sub
Sub flash17(flstate)
	If Flstate Then
		LightFF17.State = 1
		flashFF17.IntensityScale = 45
	Else
		LightFF17.State = 0
		flashFF17.IntensityScale = 0
	End If
End Sub
Sub flash19(flstate)
	If Flstate Then
		BallonLightA.opacity = 100
		BallonLightA.IntensityScale = 40
		Flasherlight19A.State = 1
		Flasherlight19B.State = 1
		Ballon.image = "ballonOn"
		Ballon.blenddisablelighting = light + 4
		FlashlightB1.Amount = 50
		FlashlightB1.IntensityScale = 50
		FlashlightB2.Amount = 30
		FlashlightB2.IntensityScale = 30
	Else
		BallonLightA.opacity = 0
		BallonLightA.IntensityScale = 0
		Ballon.image = "ballon"
		Flasherlight19A.State = 0
		Flasherlight19B.State = 0
'		if lightdir = -0.05 then Ballon.blenddisablelighting = 0
'		if lightdir = 0.05 then Ballon.blenddisablelighting = 0.6
		Ballon.blenddisablelighting = light
		FlashlightB1.Amount = 0
		FlashlightB1.IntensityScale = 0
		FlashlightB2.Amount = 0
		FlashlightB2.IntensityScale = 0
	End If
End Sub
Sub flash21(flstate)
	If Flstate Then
		FlashlevelF(3) = 1 : FlasherFlash3_Timer
		FlashlevelF(4) = 1 : FlasherFlash4_Timer
		FlashlevelF(5) = 1 : FlasherFlash5_Timer
		if lightdir = -0.05 then VitreB.blenddisablelighting = 0.35:RampA.blenddisablelighting = 0.35:end if
		if lightdir = 0.05 then VitreB.blenddisablelighting = 1:RampA.blenddisablelighting = 1:end if
		fl22a.Amount = 100
		fl22a.IntensityScale = 20
		fl22b.Amount = 100
		fl22b.IntensityScale = 4
		FlashlightE.Amount = 200
		FlashlightE.IntensityScale = 150
	Else
		fl22a.Amount = 0
		fl22a.IntensityScale = 0
		fl22b.Amount = 0
		fl22b.IntensityScale = 0
		FlashlightE.Amount = 0
		FlashlightE.IntensityScale = 0
		if lightdir = -0.05 then VitreB.blenddisablelighting = 0:RampA.blenddisablelighting = 0:end if
		if lightdir = 0.05 then VitreB.blenddisablelighting = 0.6:RampA.blenddisablelighting = 0.6:end if
	End If
End Sub
Sub flash22(flstate)
	If Flstate Then
		FlashlevelF(6) = 1 : FlasherFlash6_Timer
		FlashlevelF(7) = 1 : FlasherFlash7_Timer
		if lightdir = -0.05 then VitreA.blenddisablelighting = 0.35:VitreC.blenddisablelighting = 0.35:end if
		if lightdir = 0.05 then VitreA.blenddisablelighting = 3:VitreC.blenddisablelighting = 1.5:end if
	Else
		if lightdir = -0.05 then VitreA.blenddisablelighting = 0:VitreC.blenddisablelighting = 0:end if
		if lightdir = 0.05 then VitreA.blenddisablelighting = 0.6:VitreC.blenddisablelighting = 0.6:end if
	End If
End Sub
Sub flash23(flstate)
	If Flstate Then
		FlashlevelF(1) = 1 : FlasherFlash1_Timer
		FlashlevelF(2) = 1 : FlasherFlash2_Timer
		Flasherlight1plus.State = 1
		Flasherlight1plusb.State = 1
		fl23a.Amount = 100
		fl23a.IntensityScale = 10
		fl23b.Amount = 100
		fl23b.IntensityScale = 10
		fl23c.Amount = 100
		fl23c.IntensityScale = 10
		FlashlightA.Amount = 80
		FlashlightA.IntensityScale = 80
		FlashlightB.Amount = 60
		FlashlightB.IntensityScale = 50
		FlashlightC.Amount = 100
		FlashlightC.IntensityScale = 50
		FlashlightCC.Amount = 10
		FlashlightCC.IntensityScale = 10
		FlashlightD.Amount = 200
		FlashlightD.IntensityScale = 70
	Else
		Flasherlight1plus.state = 0
		Flasherlight1plusb.State = 0
		fl23a.Amount = 0
		fl23a.IntensityScale = 0
		fl23b.Amount = 0
		fl23b.IntensityScale = 0
		fl23c.Amount = 0
		fl23c.IntensityScale = 0
		FlashlightA.Amount = 0
		FlashlightA.IntensityScale = 0
		FlashlightB.Amount = 0
		FlashlightB.IntensityScale = 0
		FlashlightC.Amount = 0
		FlashlightC.IntensityScale = 0
		FlashlightCC.Amount = 0
		FlashlightCC.IntensityScale = 0
		FlashlightD.Amount = 0
		FlashlightD.IntensityScale = 0
	End If
End Sub



'***************************************************
'      Flupper's flashers V2
'***************************************************

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity

								' *****************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 		***
Set TableRef = Table1   			' *** change this, if your table has another name       		***
FlasherLightIntensity = 1		' *** lower this, if the VPX lights are too bright (i.e. 0.1)	***
FlasherFlareIntensity = 1		' *** lower this, if the flares are too bright (i.e. 0.1)		***
								' *****************************************************************

Dim FlashLevelF(20), objbase(20), objlit(20), objflasher(20), objlight(20)
Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
'InitFlasher 1, "white" : InitFlasher 2, "white" : InitFlasher 3, "white" : InitFlasher 4, "white" : InitFlasher 5, "white" 

'InitFlasher 2, "red" : InitFlasher 3, "white"
'InitFlasher 4, "green" : InitFlasher 5, "red" : InitFlasher 6, "white"
'InitFlasher 7, "green" : InitFlasher 8, "red"
'InitFlasher 9, "green" : InitFlasher 10, "red" : InitFlasher 11, "white" 
' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 4,17 : RotateFlasher 5,0 : RotateFlasher 6,90
'RotateFlasher 7,0 : RotateFlasher 8,0 
'RotateFlasher 9,-45 : RotateFlasher 10,90 : RotateFlasher 11,90

Sub InitFlasher(nrr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nrr) = Eval("Flasherbase" & nrr) : Set objlit(nrr) = Eval("Flasherlit" & nrr)
	Set objflasher(nrr) = Eval("Flasherflash" & nrr) : Set objlight(nrr) = Eval("Flasherlight" & nrr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nrr).RotY = 0 Then
		objbase(nrr).ObjRotZ =  atn( (tablewidth/2 - objbase(nrr).x) / (objbase(nrr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nrr).RotZ = objbase(nrr).ObjRotZ : objflasher(nrr).height = objbase(nrr).z + 60
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nrr).IntensityScale = 0 : objlit(nrr).visible = 0 : objlit(nrr).material = "Flashermaterial" & nrr
	objlit(nrr).RotX = objbase(nrr).RotX : objlit(nrr).RotY = objbase(nrr).RotY : objlit(nrr).RotZ = objbase(nrr).RotZ
	objlit(nrr).ObjRotX = objbase(nrr).ObjRotX : objlit(nrr).ObjRotY = objbase(nrr).ObjRotY : objlit(nrr).ObjRotZ = objbase(nrr).ObjRotZ
	objlit(nrr).x = objbase(nrr).x : objlit(nrr).y = objbase(nrr).y : objlit(nrr).z = objbase(nrr).z
	' set the texture and color of all objects
	select case objbase(nrr).image
		Case "dome2basewhite" : objbase(nrr).image = "dome2base" & col : objlit(nrr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nrr).image = "ronddomebase" & col : objlit(nrr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nrr).image = "domeearbase" & col : objlit(nrr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nrr).imageA = "domeflashwhite" : objflasher(nrr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nrr).color = RGB(4,120,255) : objflasher(nrr).color = RGB(200,255,255) : objlight(nrr).intensity = 5000
		Case "green" :  objlight(nrr).color = RGB(12,255,4) : objflasher(nrr).color = RGB(12,255,4)
		Case "red" :    objlight(nrr).color = RGB(255,32,4) : objflasher(nrr).color = RGB(255,32,4)
		Case "purple" : objlight(nrr).color = RGB(230,49,255) : objflasher(nrr).color = RGB(255,64,255) 
		Case "yellow" : objlight(nrr).color = RGB(200,173,25) : objflasher(nrr).color = RGB(255,200,50)
		Case "white" :  objlight(nrr).color = RGB(255,240,150) : objflasher(nrr).color = RGB(100,86,59)
	end select
	objlight(nrr).colorfull = objlight(nrr).color
	If TableRef.ShowDT and ObjFlasher(nrr).RotX = -45 Then 
		objflasher(nrr).height = objflasher(nrr).height - 20 * ObjFlasher(nrr).y / tableheight
		ObjFlasher(nrr).y = ObjFlasher(nrr).y + 10
	End If
End Sub

Sub RotateFlasher(nrr, angle) : angle = ((angle + 360 - objbase(nrr).ObjRotZ) mod 180)/30 : objbase(nrr).showframe(angle) : objlit(nrr).showframe(angle) : End Sub

Sub FlashFlasher(nrr)
	If not objflasher(nrr).TimerEnabled Then objflasher(nrr).TimerEnabled = True : objflasher(nrr).visible = 1 : objlit(nrr).visible = 1 : End If
	objflasher(nrr).opacity = 1000 *  FlasherFlareIntensity * FlashLevelF(nrr)^2.5
	objlight(nrr).IntensityScale = 0.5 * FlasherLightIntensity * FlashLevelF(nrr)^3
	objbase(nrr).BlendDisableLighting =  10 * FlashLevelF(nrr)^3	
	objlit(nrr).BlendDisableLighting = 10 * FlashLevelF(nrr)^2	
'	if FlashLevelF(nrr) >1 then 
'			FlashLevelF(nrr) = FlashLevelF(nrr) * 0.9 - 0.01
'		Else
			UpdateMaterial "Flashermaterial" & nrr,0,0,0,0,0,0,1,RGB(255,255,255),0,0,False,True,0,0,0,0 
			FlashLevelF(nrr) = FlashLevelF(nrr) * 0.9 - 0.01
'	End if
	If FlashLevelF(nrr) < 0 Then objflasher(nrr).TimerEnabled = False : objflasher(nrr).visible = 0 : objlit(nrr).visible = 0 : End If
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

'***************************************************



'***************************************************
'       GI ON OFF
'***************************************************

dim xxx
dim lightdir, Light, lightdirb, Lightb, pflight
pflight = 0
Light=0:Lightb=0
lightdir = 0.05:lightdirb = 0.4

set GICallback = GetRef("UpdateGI")
Sub UpdateGI(no, Enabled)
	If Enabled Then
		lightdir = 0.05:lightdirb = 0.4:Textlight.Enabled = 1
		For each xxx in GI:xxx.State = 1:Next
        PlaySound "fx_relay"
		DOF 103, DOFOn
		If B2SOn Then
			Controller.B2SSetData 90, 1	
		End If
	Else
		lightdir = -0.05:lightdirb = -0.4:Textlight.Enabled = 1
		For each xxx in GI:xxx.State = 0:Next
        PlaySound "fx_relay"
		DOF 103, DOFOff
		If B2SOn Then
			Controller.B2SSetData 90, 0	
		End If
	End If
End Sub

Sub Textlight_timer
	light=light+(lightdir) '/2 si 0,5
	lightb=lightb+(lightdirb)
	if lightdir = 0.05 then pflight = pflight - 10:end If
	if lightdir = -0.05 then pflight = pflight + 10:end If
	if pflight > 105 then pflight = 100
	if pflight < 0 then pflight = 0

	if lightdir = 0.05 and light > 0.6 then
				RampB.image = "rampeB"
				Plastics.image = "Plastics"
				Plastics.BlendDisableLighting = 0.5
				playfieldOff.opacity = 0
				PFShadows.opacity = 100
				BoisA.blenddisablelighting = 0.5
				BoisB.blenddisablelighting = 0.5
				Panniersorange.blenddisablelighting = 5
				sol1.blenddisablelighting = 5
				rampeMetal.blenddisablelighting = 0.6
		if FlipperLight = 1 then
			WhiteLumF.Amount = 10
			WhiteLumF.IntensityScale = 12
			WhiteLumF.opacity = 20
		End if
				light = 0.6
				lightb = 5.2
				pflight = 0
				me.enabled = 0
	End If
			
	if lightdir = -0.05 and light < 0 then 
				RampB.image = "rampeBOff"
				playfieldOff.opacity = 100
				PFShadows.opacity = 0
				Plastics.image = "PlasticsOff"
				BoisA.blenddisablelighting = 0.2
				BoisB.blenddisablelighting = 0.2
				Panniersorange.blenddisablelighting = 0.5
				sol1.blenddisablelighting = 0.5
				RampB.blenddisablelighting = 0
				rampeMetal.blenddisablelighting = 0.1
		if FlipperLight = 1 then
			WhiteLumF.Amount = 0
			WhiteLumF.IntensityScale = 0
		end if
				light = 0
				lightb = 0.4
				pflight = 100
				me.enabled = 0
'		Else
				Plastics.image = "plastics"
	End If
	for each xx in DL:
		xx.blendDisableLighting = light
		playfieldOff.opacity = pflight
		PFShadows.opacity = 100 - pflight
	Next
	for each xx in DL2:
		xx.blendDisableLighting = lightb
		LolaB.blenddisablelighting = lightb + ll + 7
	Next
if light < 0.05 then 
			Plastics.BlendDisableLighting = 0.05
			Lflogo.BlendDisableLighting = 0.05+fl
			Rflogo.BlendDisableLighting = 0.05+fl
		end if
		if light < 0.1 then 
			rampeMetal.blenddisablelighting = 0.1
		end if
		if light < 0.25 then 
			RampWireA.blenddisablelighting = 0.25
		end if

		if light > 0.49 then 
			Plastics.BlendDisableLighting = 0.5
			Lflogo.BlendDisableLighting = 0.7+fl
			Rflogo.BlendDisableLighting = 0.7+fl
			RampWireA.blenddisablelighting = 0.5
		end if
End Sub



'***************************************************


'***************************************************
'      Real Time Sub
'***************************************************
dim eyerot 
eyerot = 0.05
Sub RealTime_timer()

LolaEyeL.objRotZ = LolaEyeL.objRotZ + eyerot
LolaEyeR.objRotZ = LolaEyeR.objRotZ + eyerot
if LolaEyeL.objRotZ > 10 then eyerot = -0.05:end If
if LolaEyeL.objRotZ < -25 then eyerot = 0.05:end If

	if mm>0 then mm=mm+1:end If
	if mmm>0 then mmm=mmm+1:end If
	if mmmm>0 then mmmm=mmmm+1:end If
	if mm>50 then mm=0:end If
	if mmm>50 then mmm=0:end If
	if mmmm>50 then mmmm=0:end If

    LFLogo.RotZ = LeftFlipper.CurrentAngle-121
    RFlogo.RotZ = RightFlipper.CurrentAngle+121

	LeftFlipperSh.RotZ = LeftFlipper.currentangle
	RightFlipperSh.RotZ = RightFlipper.currentangle
	BallShadowUpdate

	gateA.ObjRotY = Gate2.currentangle
	gateB.ObjRotX = -(gate3.currentangle)
	gateC.ObjRotY = -(gate1.currentangle)
	gateD.ObjRotY = -(gate4.currentangle)
End Sub

Sub RealTimeLights
	'Compteur JPJ
	if Light17.state = 1 then c17.BlendDisableLighting = 3
	if Light17.state = 0 then c17.BlendDisableLighting = 0
	if Light18.state = 1 then c18.BlendDisableLighting = 3
	if Light18.state = 0 then c18.BlendDisableLighting = 0
	if Light19.state = 1 then c19.BlendDisableLighting = 3
	if Light19.state = 0 then c19.BlendDisableLighting = 0
	if Light20.state = 1 then c20.BlendDisableLighting = 3
	if Light20.state = 0 then c20.BlendDisableLighting = 0
	if Light21.state = 1 then c21.BlendDisableLighting = 3
	if Light21.state = 0 then c21.BlendDisableLighting = 0
	if Light22.state = 1 then c22.BlendDisableLighting = 3
	if Light22.state = 0 then c22.BlendDisableLighting = 0
	if Light23.state = 1 then c23.BlendDisableLighting = 3
	if Light23.state = 0 then c23.BlendDisableLighting = 0
	if Light41.state = 1 then c41.BlendDisableLighting = 3
	if Light41.state = 0 then c41.BlendDisableLighting = 0
	if Light42.state = 1 then c42.BlendDisableLighting = 3
	if Light42.state = 0 then c42.BlendDisableLighting = 0
	if Light43.state = 1 then c43.BlendDisableLighting = 3
	if Light43.state = 0 then c43.BlendDisableLighting = 0
	if Light44.state = 1 then c44.BlendDisableLighting = 3
	if Light44.state = 0 then c44.BlendDisableLighting = 0
	if Light45.state = 1 then c45.BlendDisableLighting = 3
	if Light45.state = 0 then c45.BlendDisableLighting = 0
	if Light46.state = 1 then c46.BlendDisableLighting = 3
	if Light46.state = 0 then c46.BlendDisableLighting = 0
	if Light47.state = 1 then c47.BlendDisableLighting = 3
	if Light47.state = 0 then c47.BlendDisableLighting = 0

	'Bumpers JPJ
	if Light58.state = 1 then 
		FlasherbaseBumperB.BlendDisableLighting = 14
		else
		FlasherbaseBumperB.BlendDisableLighting = 0.5
	end if
	if Light58.state = 1 then 
		FlasherbaseBumperA.BlendDisableLighting = 12
		else
		FlasherbaseBumperA.BlendDisableLighting = 0.5
	end if
	if Light59.state = 1 then 
		FlasherbaseBumperC.BlendDisableLighting = 13
		else
		FlasherbaseBumperC.BlendDisableLighting = 0.5
	end if

	'FIRE balls JPJ
	if Light36.state = 1 then 
		F.BlendDisableLighting = 10
		else
		F.BlendDisableLighting = 0.3
	end if
	if Light35.state = 1 then 
		I.BlendDisableLighting = 11
		else
		I.BlendDisableLighting = 0.2
	end if
	if Light34.state = 1 then 
		R.BlendDisableLighting = 12
		else
		R.BlendDisableLighting = 0.1
	end if
	if Light33.state = 1 then 
		E.BlendDisableLighting = 13
		else
		E.BlendDisableLighting = 0
	end if
End Sub

'****************************************************
'*                     Flippers                     *
'****************************************************
 
  Sub SolLFlipper(Enabled)
     If Enabled Then
			PlaySound SoundFXDOF("FlipperUpL",101,DOFOn,DOFFlippers)
			LeftFlipper.EOSTorque = 0.7
			LeftFlipper.RotateToEnd
			PlaySoundAtVol SoundFX("FlipperUpL2",DOFContactors), LeftFlipper, 1 

	Else
			Controller.Switch(63)=0
			PlaySound SoundFXDOF("flipperdownL",101,DOFOff,DOFFlippers)
			LeftFlipper.EOSTorque = 0.15
			LeftFlipper.RotateToStart
     End If
End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
			PlaySound SoundFXDOF("FlipperUpR",102,DOFOn,DOFFlippers)
			RightFlipper.EOSTorque = 0.7
			RightFlipper.RotateToEnd
			PlaySoundAtVol SoundFX("FlipperUpR2",DOFContactors), RightFlipper, 1
     Else
			Controller.Switch(64)=0
			PlaySound SoundFXDOF("flipperdownR",102,DOFOff,DOFFlippers)
		 	RightFlipper.EOSTorque = 0.15
			RightFlipper.RotateToStart
     End If
End Sub

'****************************************************

'-----------------------------------
' keyboard routines
'-----------------------------------

Sub Table1_KeyDown(ByVal KeyCode)
 	If vpmKeyDown(KeyCode) Then Exit Sub
	If keycode = PlungerKey Then PlaySoundAtVol "PlungerPull", Wall016, 1:PlungerIM.Pullback:Plunger.Pullback
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If vpmKeyUp(KeyCode) Then Exit Sub
	If keycode = PlungerKey Then StopSound "PlungerPull":PlungerIM.Fire:Plunger.Fire
	If keycode = RightMagnaSave Then
			if DisableLUTSelector = 0 then
				LUTmeUP = LUTMeUp + 1
				if LutMeUp > MaxLut then LUTmeUP = 0
				SetLUT
				ShowLUT
			end if
	end if
	If keycode = LeftMagnaSave Then
			if DisableLUTSelector = 0 then
				LUTmeUP = LUTMeUp - 1
				if LutMeUp < 0 then LUTmeUP = MaxLut
				SetLUT
				ShowLUT
			end if
	End if
End Sub

'-----------------------------------


Sub wabbit_Hit:bswabbithole.AddBall Me:PlaysoundAtVol "scoopenter", wabbit,1:End Sub
Sub wabbit_unHit:Sol1.z = 40:me.TimerEnabled = 1:End Sub
Sub Wabbit_Timer():Sol1.z = Sol1.z - 5:
	if Sol1.z >0 then 
		exit sub
	Else
		Me.TimerEnabled = 0
	end if
End Sub
Sub ballrelease_unHit: :PlaysoundAtVol SoundFX("ballrel",DOFContactors), ballrelease,1:End Sub
Sub SolTrough(Enabled)
	If Enabled Then
	GI_AllTroughCheck
		bsTrough.ExitSol_On
	vpmTimer.PulseSw 15
	End If
End Sub

Sub Drain_Hit:ClearBallID : bsTrough.AddBall Me:PlaySoundAtVol "drain", Drain, 1:End Sub
Sub launch_Hit:bsLaunch.AddBall 0:End Sub

Sub VUK1_Hit:Sol2.z = 40:sol2.TimerEnabled = 1:ClearBallid:End Sub
Sub VUK2_Hit:ClearBallid:End Sub

Sub Jumpmag(enabled)
	If enabled then
		VUKM=True
	Else VUKM=False
	End if
End Sub

Sub JumpballT(enabled)
	If enabled then
		VUKT=True
	Else VUKT=False
	End if
End Sub

Sub TrigPrehole_hit
	jumpballkicker.enabled = 0
	me.TimerEnabled = 1
End Sub

Dim preH
preH = 0
Sub TrigPrehole_timer
	preH = preH + 1
	if preH = 10 then 
		jumpballkicker.enabled = 1:preH = 0:me.TimerEnabled = 0
	else
		exit Sub
	End if
End Sub



Sub jumpballkicker_Hit
	clearballid
	Controller.Switch(45) = True
	PlaySoundAtVol "Scoopenter", jumpballkicker, 1
End Sub
Sub jumpballkicker_unHit:Sol2.z = 40:sol2.TimerEnabled = 1:End Sub

Sub Sol2Timer_Timer
	Sol2.z = Sol2.z - 5:
	if Sol2.z >0 then 
		exit sub
	Else
		Me.Enabled = 0
	end if
End Sub

'******************************************************
'			BASKET MAGNET
'******************************************************

dim xxxx, yyyy, zzzz, block
block = 0

Sub solBasketMagnet(enabled)
	if enabled then
		ringblock.collidable = 1
		mbackboard.MagnetOn = True
	Else
		mbackboard.MagnetOn = False
		magnetissue.enabled = 1
	end if
End Sub

Sub trigger27_hit:Controller.Switch(27)=1:PlaysoundAtVol "metalhit1", ActiveBall,1:End Sub
Sub trigger27_unhit:Controller.Switch(27)=0:End Sub

dim balltrap, ballsonmagnet


sub BigBackBoard_hit
	PlaySoundAtVol "plastichit", ActiveBall, 1
End Sub
sub BigBackBoard001_hit
	PlaySoundAtVol "plastichit", ActiveBall, 1
End Sub
sub Wall22_hit
	PlaySoundAtVol "plastichit", ActiveBall, 1
End Sub

Sub BMagnet_Hit
		PlaySoundAtVol "magnethit", ActiveBall, 1
		balltrap = 1
		mbackboard.AddBall ActiveBall
		mbackboard.grabcenter = 1
		BasketBlok.collidable = 1
		xxxx = ActiveBall.X
		yyyy = ActiveBall.Y
		zzzz = ActiveBall.Z
		ActiveBall.velX=0
		ActiveBall.velY=0
		ActiveBall.velZ=0
		ActiveBall.X=xxxx
		ActiveBall.Y=yyyy
		ActiveBall.Z=zzzz
End Sub	

Sub bMagnet_unHit
	mbackboard.RemoveBall ActiveBall
		ActiveBall.velZ=-0.2
		ActiveBall.velX=0
		ActiveBall.velY=0
		magnetissue.enabled = 1
End Sub

dim mbb
mbb = 0

sub magnetissue_timer
	mbb=mbb + 1
	if mbb = 29 Then
		BasketBlok.collidable = 0
		ringblock.collidable = 0
		mbb = 0
		me.enabled = 0
	Else
		exit Sub
	end if
end Sub


Sub BasketMiddle_Hit
	netshake1
End sub

Sub Helper1_Hit:ActiveBall.VelX = 8: End Sub

'*******Skill Shot Basket********
Sub trigger30_hit:vpmTimer.PulseSw 30:netshake:End Sub
sub TriggPlungerRamp_hit:
	Helper2.collidable = 0
				Select Case Int(Rnd*PSSB)+1
					Case 1 : ActiveBall.velZ = 2:'debug.print "Choix 1"
 					Case 2 : ActiveBall.velX = -15:ActiveBall.velZ = 0.9:'debug.print "Choix 2"
					Case 3 : ActiveBall.velX = 15:ActiveBall.velZ = 2:'debug.print "Choix 3"
					Case 4 : 'debug.print "Choix 4"
					Case 5 : 'debug.print "Choix 5"
					Case 6 : 'debug.print "Choix 6"
					Case 7 : ActiveBall.velX = 20:ActiveBall.velZ = 0.9:'debug.print "Choix 7"
					Case 8 : ActiveBall.velX = -10:ActiveBall.velZ = 0.95:'debug.print "Choix 8"
					Case 9 : ActiveBall.velX = 10:ActiveBall.velZ = 2.5:'debug.print "Choix 9"
					Case 10 : debug.print "Choix 10"
				End Select
	if ActiveBall.velY<-43 then
		ActiveBall.velY=-43
	end if
end sub
sub TriggPlungerRamp_unhit:
	Helper2.collidable = 1
End sub

Dim netvib, netvib1

Sub netshake
    netvib = 10
    ShakenetTimer.Enabled = 1
End Sub
Sub ShakenetTimer_Timer
    Primitive51.RotAndTra4 = netvib
    If netvib = 0 Then ShakenetTimer.Enabled = False:Exit Sub
    If netvib < 0 Then
        netvib = ABS(netvib) - 1
    Else
        netvib = - netvib + 1
    End If
End Sub

Sub netshake1
    netvib1 = 14
    ShakenetTimer1.Enabled = 1
End Sub
Sub ShakenetTimer1_Timer
    Primitive53.RotAndTra5 = netvib1
    If netvib1 = 0 Then ShakenetTimer1.Enabled = False:Exit Sub
    If netvib1 < 0 Then
        netvib1 = ABS(netvib1) - 1
    Else
        netvib1 = - netvib1 + 1
    End If
End Sub
'******************************************************


Sub SolVUK(Enabled)
	If Enabled Then
		If Controller.Switch(45) = True Then
			PlaySound SoundFX("solenoid",DOFContactors)
			Controller.Switch(45) = False
			jumpballkicker.destroyball	
			PlaySoundAtVol SoundFX("VUKout",DOFContactors), jump, 1
			VUK1.CreateBall
			vpmTimer.AddTimer 70, "VUKLevel1"
		end if
	end if
end sub

Sub VUKLevel1(swNo)
	Sol2.z = 40
	sol2Timer.Enabled = 1
	VUK1.DestroyBall
	VUK2.CreateBall
	vpmTimer.AddTimer 70,"VUKLevel2"
End Sub

Sub VUKLevel2(swNo)
	Sol2.z = 40
	sol2Timer.Enabled = 1
	VUK2.DestroyBall
	Createballid(Kicker1)
	vpmTimer.AddTimer 500, "Kicker1.Kick 250,33'"
End Sub


'*****************
' Some sounds
'*****************

Sub trigger4_hit:PlaySoundAtVol "rail", activeball, 1:End Sub
Sub trigger5_hit:PlaySoundAtVol "drop", activeball, 1:End Sub
Sub trigger6_hit:PlaySoundAtVol "drop", activeball, 1:End Sub
Sub Trigger8_hit: PlaySoundAtVol "fx_chapa", activeball, 1:End Sub
Sub Trigger9_hit: PlaySoundAtVol "fx_chapa", activeball, 1:End Sub
Sub basketmagnet_hit:PlaySoundAtVol "flip_hit_2", Activeball, 1:End Sub
dim mmmm, mm, mmm
mm=0:mmm=0:mmmm=0
Sub Wall15_hit:if mm = 0 then PlaySoundAtVol "magnethit", Wall15, 1:mm=1:end if:End Sub
Sub Wall2_hit:if mmm = 0 then PlaySoundAtVol "magnethit", Wall2, 1:mmm=1:end if:End Sub
Sub Wall14_hit:if mmmm = 0 then PlaySoundAtVol "magnethit", Wall14, 1:mmmm=1:end if:End Sub

	sub MetalSA_hit()
		playsound "sramp", 0, Vol(ActiveBall)*MsideVol , pan(ActiveBall), 0, Pitch(ActiveBall)*25, 0, 0
	end sub
	sub MetalSA_unhit()
		stopsound "sramp"
	end sub

	sub MetalSB_hit()
		playsound "sramp2", 0, Vol(ActiveBall)*MsideVol , pan(ActiveBall), 0, Pitch(ActiveBall)*25, 0, 0
	end sub
	sub MetalSB_unhit()
		stopsound "sramp2"
	end sub

	sub MetalSC_hit()
		playsound "sramp3", 0, Vol(ActiveBall)*MsideVol , pan(ActiveBall), 0, Pitch(ActiveBall)*25, 0, 0
	end sub
	sub MetalSC_unhit()
		stopsound "sramp3"
	end sub

	sub MetalSD_hit()
		playsound "sramp4", 0, Vol(ActiveBall)*MsideVol , pan(ActiveBall), 0, Pitch(ActiveBall)*25, 0, 0
	end sub
	sub MetalSD_unhit()
		stopsound "sramp4"
	end sub
'*****************
' Ramp Diverter
'*****************

Sub Soldiv1(enabled)
 	if enabled then
		Playsound SoundFX("solenoid",DOFContactors)
 		DiverterA_off.isdropped=true
 	else
 		DiverterA_off.isdropped=false
 	end if
 end sub

Sub Soldiv2(enabled)
 	if enabled then
		Playsound SoundFX("solenoid",DOFContactors)
 		DiverterA_off.isdropped=false
 	else
 		DiverterA_off.isdropped=true
 	end if
 end sub

 Sub SolRdiv(enabled)
 	if enabled then
		Playsound SoundFX("solenoid",DOFContactors)
 		Diverter_off.isdropped=false
 	else
 		Diverter_off.isdropped=true
 	end if
 end sub

Sub Kicker3_Hit:
	ClearBallID
	Kicker3.destroyball	
	CreateballID(Kicker2)
	Kicker2.Kick 200,10
End Sub

Sub solAutofire(Enabled)
	If Enabled Then
		PlungerIM.AutoFire:Plunger.pullback:Plunger.Fire
		Playsound SoundFX("solenoid",DOFContactors)
	End If
End Sub


'* Trough Check routines
DIM MultiballFlag
Function GI_AllTroughCheck
Dim Ballcount
	Ballcount = 0
	'If all Balls in Trough, turn off GI
	If Controller.Switch(11) = TRUE then Ballcount = Ballcount + 1
    If Controller.Switch(12) = TRUE then Ballcount = Ballcount + 1
    If Controller.Switch(13) = TRUE then Ballcount = Ballcount + 1
    If Controller.Switch(14) = TRUE then Ballcount = Ballcount + 1
	If Controller.Switch(15) = TRUE then Ballcount = Ballcount + 1

	If Ballcount > 2 Then 
		MultiballFlag = 0: 'debug.print "No Multiball"
	Else
		MultiballFlag = 1: 'debug.print "Multiball"
	End If

	GI_AllTroughCheck = Ballcount
	'debug.print "GI_AllTroughCheck: " & GI_AllTroughCheck
End Function


'********Ramp Drop Sounds******

Sub Trigger1_hit:PlaySoundAtVol "dropJPJPlastic", Wall14, 1:End Sub
Sub Trigger2_hit:PlaySoundAtVol "wirefall", Wall15, 1:End Sub


'********** Sling Shot Animations *******************************
' Rstep and Lstep  are the variables that increment the animation
'****************************************************************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    vpmTimer.PulseSw 62
    PlaySoundAtVol SoundFX("Rightsling",DOFContactors), sling1, 1
'	RightSlingFlash.state =0
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -26
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    PlaySoundAtVol SoundFX("RightSlingSub",DOFContactors), sling1, 1
    PlaySoundAtVol SoundFX("RightSlingSub2",DOFContactors), sling1, 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -16
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
'	RightSlingFlash.state =1
	'if RSLing1.Visible = 0 then end If 'RightSlingFlash.state =0
End Sub

Sub LeftSlingShot_Slingshot
    vpmTimer.PulseSw 59
	PlaySoundAtVol SoundFX("Leftsling",DOFContactors), sling2, 1
'	LeftSlingFlash.state = 0
	LSling.Visible = 0
	LSling1.Visible = 1
	sling2.TransZ = -26
	LStep = 0
	LeftSlingShot.TimerEnabled = 1
	PlaySoundAtVol SoundFX("LeftSlingSub",DOFContactors), sling2, 1
	PlaySoundAtVol SoundFX("LeftSlingSub2",DOFContactors), sling2, 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -16
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
'	LeftSlingFlash.state = 1
	'if LSLing1.Visible = 0 then end If 'LeftSlingFlash.state = 0:
End Sub

'***********************************************************************************
'*************                 Bumpers                               ***************
'***********************************************************************************
Sub Bumper1b_Hit
    vpmTimer.PulseSw 49
	PlaySoundAtVol SoundFX("fx_bumper1",DOFContactors), ActiveBall, 1
	PlaySoundAtVol SoundFX("bumper",DOFContactors), ActiveBall, 1
	spotInt = 20:Me.TimerEnabled = 1
	xxxxx=0:ll=70:BallRotX.Enabled = 1
End Sub

Sub Bumper1b_Timer
	if Lola = 0 then 
			Me.Timerenabled = 0
		Else
			LolaSpot.intensity = SpotInt
			SpotInt = SpotInt -0.5
			If spotInt = 0  then 
				me.Timerenabled = 0
			Else
				exit sub
			end if
	end if

End Sub

Sub Bumper2b_Hit
    vpmTimer.PulseSw 50
	PlaySoundAtVol SoundFX("fx_bumper2",DOFContactors), ActiveBall, 1
	PlaySoundAtVol SoundFX("bumper",DOFContactors), ActiveBall, 1
	spotInt = 20:Me.TimerEnabled = 1
	yyyyy=0:ll=60:BallRotY.Enabled = 1
End Sub

Sub Bumper2b_Timer
	if Lola = 0 then 
			Me.Timerenabled = 0
		Else
			LolaSpot.intensity = SpotInt
			SpotInt = SpotInt -0.5
			If spotInt = 0  then 
				me.Timerenabled = 0
			Else
				exit sub
			end if
	end if

End Sub

Sub Bumper3b_Hit
    vpmTimer.PulseSw 51
	PlaySoundAtVol SoundFX("fx_bumper3",DOFContactors), ActiveBall, 1
	PlaySoundAtVol SoundFX("bumper",DOFContactors), ActiveBall, 1
	spotInt = 20:Me.TimerEnabled = 1
	zzzzz=0:ll=50:BallRotZ.Enabled = 1
End Sub

Sub Bumper3b_Timer
	if Lola = 0 then 
			Me.Timerenabled = 0
		Else
			LolaSpot.intensity = SpotInt
			SpotInt = SpotInt -0.5
			If spotInt = 0  then 
				me.Timerenabled = 0
			Else
				exit sub
			end if
	end if

End Sub
'**********************************************************************************

'***********************************************************************************
'*************                 Ball rotation JPJ                     ***************
'***********************************************************************************
Dim LolaProj
Sub LolaProj_Timer
	LolaProjector.intensity = SpotInt
	SpotInt = SpotInt -0.5
	If spotInt = 0  then me.Timerenable = 0:end if
End Sub


'***********************************************************************************
'*************                 Ball rotation JPJ                     ***************
'***********************************************************************************
dim BallRotaSpeed
BallRotaSpeed = 10
dim xxxxx, yyyyy, zzzzz, ll

sub BallRotX_timer()
	if xxxxx < 90 then
				BallRotaSpeed = 10
				LolaC.objrotx = LolaC.objrotx + BallRotaSpeed
	end If

	if xxxxx>89 and xxxxx<120 then
				BallRotaSpeed = 5
				LolaC.objrotx = LolaC.objrotx + BallRotaSpeed
	end if

	if xxxxx>119 and xxxxx<150 then
				BallRotaSpeed = 2
				LolaC.objrotx = LolaC.objrotx + BallRotaSpeed
	end If


	if xxxxx>149 and xxxxx<200 then
				BallRotaSpeed = 1
				LolaC.objrotx = LolaC.objrotx + BallRotaSpeed
	end If

	xxxxx=xxxxx+1
	LolaC.blenddisablelighting = lightb + ll + 7
	ll=ll-0.5
	if ll<0 then ll = 0:end If
	if xxxxx = 200 then me.enabled = 0:end If
End Sub
sub BallRotY_timer()
	if yyyyy < 90 then
				BallRotaSpeed = 10
				LolaC.objroty = LolaC.objroty + BallRotaSpeed
	end If

	if yyyyy>89 and yyyyy<120 then
				BallRotaSpeed = 5
				LolaC.objroty = LolaC.objroty + BallRotaSpeed
	end if

	if yyyyy>119 and yyyyy<150 then
				BallRotaSpeed = 2
				LolaC.objroty = LolaC.objroty + BallRotaSpeed
	end If


	if yyyyy>149 and yyyyy<200 then
				BallRotaSpeed = 1
				LolaC.objroty = LolaC.objroty + BallRotaSpeed
	end If

	yyyyy=yyyyy+1
	LolaC.blenddisablelighting = lightb + ll +7
	ll=ll-0.5
	if ll<0 then ll = 0:end If
	if yyyyy = 200 then me.enabled = 0:end If
End Sub
sub BallRotZ_timer()
	if zzzzz < 90 then
				BallRotaSpeed = 10
				LolaC.objrotz = LolaC.objrotz + BallRotaSpeed
	end If

	if zzzzz>89 and zzzzz<120 then
				BallRotaSpeed = 5
				LolaC.objrotz = LolaC.objrotz + BallRotaSpeed
	end if

	if zzzzz>119 and zzzzz<150 then
				BallRotaSpeed = 2
				LolaC.objrotz = LolaC.objrotz + BallRotaSpeed
	end If


	if zzzzz>149 and zzzzz<200 then
				BallRotaSpeed = 1
				LolaC.objrotz = LolaC.objrotz + BallRotaSpeed
	end If

	zzzzz=zzzzz+1
	LolaC.blenddisablelighting = lightb + ll +7
	ll=ll-0.5
	if ll<0 then ll = 0:end If
	if zzzzz = 200 then me.enabled = 0:end If
End Sub
'********************************* DROP TARGET ************************************

	dim sw17Dir, sw18Dir, sw19Dir, sw17Pos, sw18Pos, sw19Pos, sw17l, sw18l, sw19l
	sw17Dir = 1:sw18Dir = 1:sw19Dir = 1:sw17Pos = 0:sw18Pos = 0:sw19Pos = 0

  Sub sw17_Hit:DtBank.Hit 1:sw17Dir = 0:sw17l=0:me.TimerEnabled = 1:End Sub
  Sub sw18_Hit:DtBank.Hit 2:sw18Dir = 0:sw18l=0:me.TimerEnabled = 1:End Sub
  Sub sw19_Hit:DtBank.Hit 3:sw19Dir = 0:sw19l=0:me.TimerEnabled = 1:End Sub

 Sub sw17_Timer()
  Select Case sw17Pos
        Case 0: sw17P.z=0
				If sw17Dir = 1 then 
					sw17l = 1
					sw17.TimerEnabled = 0
'					LightTargetsB17.state = 0:LightTargetsA17.state = 0
				else
					sw17Dir = 0
					sw17.TimerEnabled = 1
				end if
        Case 1: sw17P.z=0
        Case 2: sw17P.z=-4
        Case 3: sw17P.z=-8
        Case 4: sw17P.z=-12
        Case 5: sw17P.z=-16
        Case 6: sw17P.z=-20
        Case 7: sw17P.z=-24
        Case 8: sw17P.z=-28
        Case 9: sw17P.z=-32
        Case 10: sw17P.z=-36
        Case 11: sw17P.z=-40
        Case 12: sw17P.z=-44
        Case 13: sw17P.z=-48:sw17P.ReflectionEnabled = true
        Case 14: sw17P.z=-52:sw17P.ReflectionEnabled = false
'				LightTargetsB17.state = 1:LightTargetsA17.state = 1
				 If sw17Dir = 1 then
				 else
					sw17.TimerEnabled = 0
			     end if

End Select
	If sw17Dir = 1 then
		If sw17pos>0 then sw17pos=sw17pos-1
	else
		If sw17pos<14 then sw17pos=sw17pos+1
	end if
  End Sub


 Sub sw18_Timer()	
  Select Case sw18Pos
        Case 0: sw18P.z=0
				 If sw18Dir = 1 then
					sw18l = 1
					sw18.TimerEnabled = 0
'					LightTargetsB18.state = 0:LightTargetsA18.state = 0
				 else
					sw18Dir = 0
					sw18.TimerEnabled = 1
			     end if        
        Case 1: sw18P.z=0
        Case 2: sw18P.z=-4
        Case 3: sw18P.z=-8
        Case 4: sw18P.z=-12
        Case 5: sw18P.z=-16
        Case 6: sw18P.z=-20
        Case 7: sw18P.z=-24
        Case 8: sw18P.z=-28
        Case 9: sw18P.z=-32
        Case 10: sw18P.z=-36
        Case 11: sw18P.z=-40
        Case 12: sw18P.z=-44
        Case 13: sw18P.z=-48:sw18P.ReflectionEnabled = true
        Case 14: sw18P.z=-52:sw18P.ReflectionEnabled = false
'				LightTargetsB18.state = 1:LightTargetsA18.state = 1
				 If sw18Dir = 1 then
				 else
					sw18.TimerEnabled = 0
			     end if
 
End Select
	If sw18Dir = 1 then
		If sw18pos>0 then sw18pos=sw18pos-1
	else
		If sw18pos<14 then sw18pos=sw18pos+1
	end if
  End Sub


 Sub sw19_Timer()	
  Select Case sw19Pos
        Case 0: sw19P.z=0
				 If sw19Dir = 1 then
					sw19l = 1
					sw19.TimerEnabled = 0
'					LightTargetsB19.state = 0:LightTargetsA19.state = 0
				 else
					sw19Dir = 0
					sw19.TimerEnabled = 1
			     end if        
        Case 1: sw19P.z=0
        Case 2: sw19P.z=-4
        Case 3: sw19P.z=-8
        Case 4: sw19P.z=-12
        Case 5: sw19P.z=-16
        Case 6: sw19P.z=-20
        Case 7: sw19P.z=-24
        Case 8: sw19P.z=-28
        Case 9: sw19P.z=-32
        Case 10: sw19P.z=-36
        Case 11: sw19P.z=-40
        Case 12: sw19P.z=-44
        Case 13: sw19P.z=-48:sw19P.ReflectionEnabled = true
        Case 14: sw19P.z=-52:sw19P.ReflectionEnabled = false
'				LightTargetsB19.state = 1:LightTargetsA19.state = 1
				 If sw19Dir = 1 then
				 else
					sw19.TimerEnabled = 0
			     end if

 
End Select
	If sw19Dir = 1 then
		If sw19pos>0 then sw19pos=sw19pos-1
	else
		If sw19pos<14 then sw19pos=sw19pos+1
	end if
  End Sub


'DT Subs
   Sub ResetDrops(Enabled)
		If Enabled Then
'			LightTargetsB17.state = 0
'			LightTargetsB18.state = 0
'			LightTargetsB19.state = 0
			sw17Dir = 1:sw18Dir = 1:sw19Dir = 1
			sw17l=1:sw18l=1:sw19l=1
			sw17.TimerEnabled = 1:sw18.TimerEnabled = 1:sw19.TimerEnabled = 1
			DTBank.DropSol_On
		End if
   End Sub



'=========================================================================



'*********************** Targets **********************
Sub SW24_hit():T24.transY = -7:Me.TimerEnabled = 1:vpmTimer.PulseSw 24:End Sub
Sub SW24_Timer():T24.transY = T24.transY+1
	if T24.transY = 0 then 
		Me.TimerEnabled = 0
	else 
		exit sub
	End If
End Sub

Sub SW28_hit():T28.transY = -7:Me.TimerEnabled = 1:vpmTimer.PulseSw 28:End Sub
Sub SW28_Timer():T28.transY = T28.transY+1
	if T28.transY = 0 then 
		Me.TimerEnabled = 0
	else 
		exit sub
	End If
End Sub
Sub SW29_hit():T29.transY = -7:Me.TimerEnabled = 1:vpmTimer.PulseSw 29:End Sub
Sub SW29_Timer():T29.transY = T29.transY+1
	if T29.transY = 0 then 
		Me.TimerEnabled = 0
	else 
		exit sub
	End If
End Sub

Sub SW33_hit():T33.transY = -7:Me.TimerEnabled = 1:vpmTimer.PulseSw 33:End Sub
Sub SW33_Timer():T33.transY = T33.transY+1
	if T33.transY = 0 then 
		Me.TimerEnabled = 0
	else 
		exit sub
	End If
End Sub
Sub SW34_hit():T34.transY = -7:Me.TimerEnabled = 1:vpmTimer.PulseSw 34:End Sub
Sub SW34_Timer():T34.transY = T34.transY+1
	if T34.transY = 0 then 
		Me.TimerEnabled = 0
	else 
		exit sub
	End If
End Sub
Sub SW35_hit():T35.transY = -7:Me.TimerEnabled = 1:vpmTimer.PulseSw 35:End Sub
Sub SW35_Timer():T35.transY = T35.transY+1
	if T35.transY = 0 then 
		Me.TimerEnabled = 0
	else 
		exit sub
	End If
End Sub
Sub SW36_hit():T36.transY = -7:Me.TimerEnabled = 1:vpmTimer.PulseSw 36:End Sub
Sub SW36_Timer():T36.transY = T36.transY+1
	if T36.transY = 0 then 
		Me.TimerEnabled = 0
	else 
		exit sub
	End If
End Sub
Sub SW37_hit():T37.transY = -7:Me.TimerEnabled = 1:vpmTimer.PulseSw 37:End Sub
Sub SW37_Timer():T37.transY = T37.transY+1
	if T37.transY = 0 then 
		Me.TimerEnabled = 0
	else 
		exit sub
	End If
End Sub

Sub SW38_hit():T38.transY = -7:Me.TimerEnabled = 1:vpmTimer.PulseSw 38:End Sub
Sub SW38_Timer():T38.transY = T38.transY+1
	if T38.transY = 0 then 
		Me.TimerEnabled = 0
	else 
		exit sub
	End If
End Sub
Sub SW39_hit():T39.transY = -7:Me.TimerEnabled = 1:vpmTimer.PulseSw 39:End Sub
Sub SW39_Timer():T39.transY = T39.transY+1
	if T39.transY = 0 then 
		Me.TimerEnabled = 0
	else 
		exit sub
	End If
End Sub
Sub SW40_hit():T40.transY = -7:Me.TimerEnabled = 1:vpmTimer.PulseSw 40:End Sub
Sub SW40_Timer():T40.transY = T40.transY+1
	if T40.transY = 0 then 
		Me.TimerEnabled = 0
	else 
		exit sub
	End If
End Sub


Sub SW42_hit():T42.transY = -7:Me.TimerEnabled = 1:vpmTimer.PulseSw 42:End Sub
Sub SW42_Timer():T42.transY = T42.transY+1
	if T42.transY = 0 then 
		Me.TimerEnabled = 0
	else 
		exit sub
	End If
End Sub

Sub SW43_hit():T43.transY = -7:Me.TimerEnabled = 1:vpmTimer.PulseSw 43:End Sub
Sub SW43_Timer():T43.transY = T43.transY+1
	if T43.transY = 0 then 
		Me.TimerEnabled = 0
	else 
		exit sub
	End If
End Sub
'******************************************************

sub sw45_hit()
	If mPFMagnet.MagnetOn = 1 Then
	targetpos12=0:target12.Enabled=1:Me.TimerEnabled = 1
	Else
	targetpos12=0:target12.Enabled=1: vpmtimer.PulseSw 45:Me.TimerEnabled = 1
	End If
End Sub
Sub sw45_Timer:Me.TimerEnabled = 0:End Sub


'********Switches**********

Sub trigger22_hit:PlaySoundAtVol "gate", activeball, 1:Controller.Switch(22)=1:End Sub
Sub trigger22_unhit:Controller.Switch(22)=0:End Sub
Sub trigger23_hit:Controller.Switch(23)=1:End Sub
Sub trigger23_unhit:Controller.Switch(23)=0:End Sub
Sub trigger25_hit:Controller.Switch(25)=1:End Sub
Sub trigger25_unhit:Controller.Switch(25)=0:End Sub
Sub trigger43_hit:Controller.Switch(43)=1:End Sub
Sub trigger43_unhit:Controller.Switch(43)=0:End Sub
Sub trigger44_hit:Controller.Switch(44)=1:End Sub
Sub trigger44_unhit:Controller.Switch(44)=0:End Sub
Sub trigger47_hit:Controller.Switch(47)=1:End Sub
Sub trigger47_unhit:Controller.Switch(47)=0:End Sub
Sub trigger48_hit:Controller.Switch(48)=1:End Sub
Sub trigger48_unhit:Controller.Switch(48)=0:End Sub
Sub trigger57_hit:Controller.Switch(57)=1:End Sub
Sub trigger57_unhit:Controller.Switch(57)=0:End Sub
Sub trigger58_hit:Controller.Switch(58)=1:End Sub
Sub trigger58_unhit:Controller.Switch(58)=0:End Sub
Sub trigger60_hit:Controller.Switch(60)=1:End Sub
Sub trigger60_unhit:Controller.Switch(60)=0:End Sub
Sub trigger61_hit:Controller.Switch(61)=1:End Sub
Sub trigger61_unhit:Controller.Switch(61)=0:End Sub
'Sub trigger001_hit:PlaySoundAtVol "rolloverJPJ", activeball, 1:End Sub



Sub BallLight_hit: 
	PRefresh.state = 1
End Sub
Sub BallLight_unhit: 
	PRefresh.state = 0
End Sub


'***************************************************
'  JP's Fading Lamps & Flashers version 9 for VP921
'   Based on PD's Fading Lights
' SetLamp 0 is Off
' SetLamp 1 is On
' FadingLevel(x) = fading state
' LampState(x) = light state
' Includes the flash element (needs own timer)
' Flashers can be used as lights too
'***************************************************

Dim LampState(200), FadingLevel(200)
'Dim FlashState(200), FlashLevel(200)
'Dim FlashSpeedUp, FlashSpeedDown

AllLampsOff()
LampTimer.Interval = 10 'lamp fading speed
LampTimer.Enabled = 1

' Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)
            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4
        Next
    End If

    UpdateLamps

End Sub


Sub UpdateLamps
	NFadeL 1, Light1
	NFadeL 2, Light2
	NFadeL 3, Light3
	NFadeL 4, Light4
	NFadeL 5, Light5
	NFadeL 6, Light6
	NFadeL 7, Light7
	NFadeL 8, Light8
	NFadeL 9, Light9
	NFadeL 10, Light10
	NFadeL 11, Light11
	NFadeL 12, Light12
	NFadeL 13, Light13
	NFadeL 14, Light14
	NFadeL 15, Light15
	NFadeL 16, Light16
	NFadeL 17, Light17
	NFadeL 18, Light18
	NFadeL 19, Light19
	NFadeL 20, Light20
	NFadeL 21, Light21
	NFadeL 22, Light22
	NFadeL 23, Light23
'	NFadeL 24, Light24
	NFadeL 25, Light25
	NFadeL 26, Light26
	NFadeL 27, Light27
	NFadeL 28, Light28
	NFadeL 29, Light29
	NFadeL 30, Light30
	NFadeL 31, Light31
	NFadeL 32, Light32
	NFadeL 33, Light33 'E
	NFadeL 34, Light34 'R
	NFadeL 35, Light35 'I
	NFadeL 36, Light36 'F
	NFadeL 37, Light37
	NFadeL 38, Light38
	NFadeL 39, Light39
	NFadeL 40, Light40
    NFadeL 41, Light41
    NFadeL 42, Light42
    NFadeL 43, Light43
    NFadeL 44, Light44
    NFadeL 45, Light45
    NFadeL 46, Light46
    NFadeL 47, Light47
	NFadeL 48, Light48
	NFadeL 49, Light49
	NFadeL 50, Light50
	NFadeL 51, Light51
	NFadeL 52, Light52
	NFadeL 53, Light53
	NFadeL 54, Light54
	NFadeL 55, Light55
	NFadeL 57, Light57 'Bumper
	NFadeL 58, Light58 'Bumper
	NFadeL 59, Light59 'Bumper
	NFadeL 60, Light60
	NFadeL 61, Light61
	NFadeL 62, Light62
	NFadeL 63, Light63
	NFadeL 64, Light64
	NFadeL 65, Light65
	NFadeL 66, Light66
	NFadeL 67, Light67
	NFadeL 68, Light68
	NFadeL 69, Light69
	NFadeL 70, Light70
	NFadeL 71, Light71
	NFadeL 72, Light72
	NFadeL 73, Light73
	NFadeL 74, Light74
	NFadeL 75, Light75
	NFadeL 76, Light76
	NFadeL 77, Light77
	NFadeL 78, Light78
	NFadeL 79, Light79
	NFadeL 80, Light80
	RealTimeLights
End Sub

Sub AllLampsOff()
    For x = 0 to 200
        LampState(x) = 0
        FadingLevel(x) = 4
    Next

UpdateLamps:UpdateLamps:Updatelamps
End Sub

Sub SetLamp(nr, value)
    If value <> LampState(nr) Then
        LampState(nr) = abs(value)
        FadingLevel(nr) = abs(value) + 4
    End If
End Sub

' div flasher subs

'Sub FlashInit
'    Dim i
'    For i = 0 to 200
'        FlashState(i) = 0
'        FlashLevel(i) = 0
'    Next
'
'    FlashSpeedUp = 50   ' fast speed when turning on the flasher
'    FlashSpeedDown = 10 ' slow speed when turning off the flasher, gives a smooth fading
'    ' you could also change the default images for each flasher or leave it as in the editor
'    ' for example
'    ' Flasher1.Image = "fr"
'    AllFlashOff()
'End Sub
'
'Sub AllFlashOff
'    Dim i
'    For i = 0 to 200
'        FlashState(i) = 0
'    Next
'End Sub

'Sub SetFlash(nr, stat)
'    FlashState(nr) = ABS(stat)
'End Sub


' Flasher objects
' Uses own faster timer

Sub Flash(nr, object)
    Select Case FlashState(nr)
        Case 0 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown
            If FlashLevel(nr) < 0 Then
                FlashLevel(nr) = 0
                FlashState(nr) = -1 'completely off
            End if
			Object.IntensityScale = Lumen*FlashLevel(nr)
        Case 1 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp
            If FlashLevel(nr) > 255 Then
                FlashLevel(nr) = 255
                FlashState(nr) = -2 'completely on
            End if
	Object.IntensityScale = Lumen*FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it doesn't change the flashstate
    Select Case FlashState(nr)
        Case 0         'off
			Object.IntensityScale = Lumen*FlashLevel(nr)
        Case 1         ' on
			Object.IntensityScale = Lumen*FlashLevel(nr)
    End Select
End Sub

'Special

Sub FadeLn(nr, Light, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:Light.Offimage = d:FadingLevel(nr) = 0 'Off
        Case 3:Light.Offimage = c:FadingLevel(nr) = 2 'fading...
        Case 4:Light.Offimage = b:FadingLevel(nr) = 3 'fading...
        Case 5:Light.Offimage = a:FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub FadeLnm(nr, Light, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:Light.Offimage = d
        Case 3:Light.Offimage = c
        Case 4:Light.Offimage = b
        Case 5:Light.Offimage = a
    End Select
End Sub

Sub LMapn(nr, Light, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:Light.Onimage = d:FadingLevel(nr) = 0 'Off
        Case 3:Light.ONimage = c:FadingLevel(nr) = 2 'fading...
        Case 4:Light.ONimage = b:FadingLevel(nr) = 3 'fading...
        Case 5:Light.ONimage = a:FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub LMapnm(nr, Light, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:Light.ONimage = d
        Case 3:Light.ONimage = c
        Case 4:Light.ONimage = b
        Case 5:Light.ONimage = a
    End Select
End Sub
' Walls

Sub FadeW(nr, a, b, c)
    Select Case FadingLevel(nr)
        Case 2:c.IsDropped = 1:FadingLevel(nr) = 0                 'Off
        Case 3:b.IsDropped = 1:c.IsDropped = 0:FadingLevel(nr) = 2 'fading...
        Case 4:a.IsDropped = 1:b.IsDropped = 0:FadingLevel(nr) = 3 'fading...
        Case 5:a.IsDropped = 0:FadingLevel(nr) = 1                 'ON
    End Select
End Sub

Sub FadeWm(nr, a, b, c)
    Select Case FadingLevel(nr)
        Case 2:c.IsDropped = 1
        Case 3:b.IsDropped = 1:c.IsDropped = 0
        Case 4:a.IsDropped = 1:b.IsDropped = 0
        Case 5:a.IsDropped = 0
    End Select
End Sub

Sub NFadeW(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.IsDropped = 1:FadingLevel(nr) = 0
        Case 5:a.IsDropped = 0:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeWm(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.IsDropped = 1
        Case 5:a.IsDropped = 0
    End Select
End Sub

Sub NFadeWi(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.IsDropped = 0:FadingLevel(nr) = 0
        Case 5:a.IsDropped = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeWim(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.IsDropped = 0
        Case 5:a.IsDropped = 1
    End Select
End Sub

'Lights

Sub FadeL(nr, a, b)
	'if nr = 1 then debug.print FadingLevel(nr)
    Select Case FadingLevel(nr)
        Case 2:b.state = 0:FadingLevel(nr) = 0
        Case 3:b.state = 1:FadingLevel(nr) = 2
        Case 4:a.state = 0:FadingLevel(nr) = 3
        Case 5:a.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub FadeLm(nr, a, b)
    Select Case FadingLevel(nr)
        Case 2:b.state = 0
        Case 3:b.state = 1
        Case 4:a.state = 0
        Case 5:a.state = 1
    End Select
End Sub

Sub NFadeL(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.state = 0:FadingLevel(nr) = 0
        Case 5:a.State = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeLm(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.state = 0
        Case 5:a.State = 1
    End Select
End Sub

Sub LMap(nr, a, b, c) 'can be used with normal/olod style lights too
    Select Case FadingLevel(nr)
        Case 2:c.state = 0:FadingLevel(nr) = 0
        Case 3:b.state = 0:c.state = 1:FadingLevel(nr) = 2
        Case 4:a.state = 0:b.state = 1:FadingLevel(nr) = 3
        Case 5:b.state = 0:c.state = 0:a.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub LMapm(nr, a, b, c)
    Select Case FadingLevel(nr)
        Case 2:c.state = 0
        Case 3:b.state = 0:c.state = 1
        Case 4:a.state = 0:b.state = 1
        Case 5:b.state = 0:c.state = 0:a.state = 1
    End Select
End Sub

'Reels

Sub FadeR(nr, a)
    Select Case FadingLevel(nr)
        Case 2:a.SetValue 3:FadingLevel(nr) = 0
        Case 3:a.SetValue 2:FadingLevel(nr) = 2
        Case 4:a.SetValue 1:FadingLevel(nr) = 3
        Case 5:a.SetValue 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub FadeRm(nr, a)
    Select Case FadingLevel(nr)
        Case 2:a.SetValue 3
        Case 3:a.SetValue 2
        Case 4:a.SetValue 1
        Case 5:a.SetValue 1
    End Select
End Sub

'Texts

Sub NFadeT(nr, a, b)
    Select Case FadingLevel(nr)
        Case 4:a.Text = "":FadingLevel(nr) = 0
        Case 5:a.Text = b:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeTm(nr, a, b)
    Select Case FadingLevel(nr)
        Case 4:a.Text = ""
        Case 5:a.Text = b
    End Select
End Sub

' Flash a light, not controlled by the rom

Sub FlashL(nr, a, b)
    Select Case FadingLevel(nr)
        Case 1:b.state = 0:FadingLevel(nr) = 0
        Case 2:b.state = 1:FadingLevel(nr) = 1
        Case 3:a.state = 0:FadingLevel(nr) = 2
        Case 4:a.state = 1:FadingLevel(nr) = 3
        Case 5:b.state = 1:FadingLevel(nr) = 4
    End Select
End Sub

' Light acting as a flash. C is the light number to be restored

Sub MFadeL(nr, a, b, c)
    Select Case FadingLevel(nr)
        Case 2:b.state = 0:FadingLevel(nr) = 0:SetLamp c, FadingLevel(c)
        Case 3:b.state = 1:FadingLevel(nr) = 2
        Case 4:a.state = 0:FadingLevel(nr) = 3
        Case 5:a.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub MFadeLm(nr, a, b, c)
    Select Case FadingLevel(nr)
        Case 2:b.state = 0:SetLamp c, FadingLevel(c)
        Case 3:b.state = 1
        Case 4:a.state = 0
        Case 5:a.state = 1
    End Select
End Sub

'Alpha Ramps used as fading lights
'ramp is the name of the ramp
'a,b,c,d are the images used for on...off
'r is the refresh light

Sub FadeAR(nr, ramp, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:ramp.image = d:FadingLevel(nr) = 0 'Off
        Case 3:ramp.image = c:FadingLevel(nr) = 2 'fading...
        Case 4:ramp.image = b:FadingLevel(nr) = 3 'fading...
        Case 5:ramp.image = a:FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub FadeARm(nr, ramp, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:ramp.image = d
        Case 3:ramp.image = c
        Case 4:ramp.image = b
        Case 5:ramp.image = a
    End Select
End Sub

Sub FlashAR(nr, ramp, a, b, c)                                   'used for reflections when there is no off ramp
    Select Case FadingLevel(nr)
        Case 2:ramp.alpha = 0:FadingLevel(nr) = 0                'Off
        Case 3:ramp.image = c:FadingLevel(nr) = 2                'fading...
        Case 4:ramp.image = b:FadingLevel(nr) = 3                'fading...
        Case 5:ramp.image = a:ramp.alpha = 1:FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub FlashARm(nr, ramp, a, b, c)
    Select Case FadingLevel(nr)
        Case 2:ramp.alpha = 0
        Case 3:ramp.image = c
        Case 4:ramp.image = b
        Case 5:ramp.image = a:ramp.alpha = 1
    End Select
End Sub

Sub NFadeAR(nr, ramp, a, b)
    Select Case FadingLevel(nr)
        Case 4:ramp.image = b:FadingLevel(nr) = 0 'off
        Case 5:ramp.image = a:FadingLevel(nr) = 1 'on
    End Select
End Sub

Sub NFadeARm(nr, ramp, a, b)
    Select Case FadingLevel(nr)
        Case 4:ramp.image = b
        Case 5:ramp.image = a
    End Select
End Sub

Sub MNFadeAR(nr, ramp, a, b, c)
    Select Case FadingLevel(nr)
        Case 4:ramp.image = b:FadingLevel(nr) = 0:SetLamp c, FadingLevel(c) 'off
        Case 5:ramp.image = a:FadingLevel(nr) = 1                           'on
    End Select
End Sub

Sub MNFadeARm(nr, ramp, a, b, c)
    Select Case FadingLevel(nr)
        Case 4:ramp.image = b:SetLamp c, FadingLevel(c) 'off
        Case 5:ramp.image = a                           'on
    End Select
End Sub

' Flashers using PRIMITIVES
' pri is the name of the primitive
' a,b,c,d are the images used for on...off

Sub FadePri(nr, pri, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:pri.image = d:FadingLevel(nr) = 0 'Off
        Case 3:pri.image = c:FadingLevel(nr) = 2 'fading...
        Case 4:pri.image = b:FadingLevel(nr) = 3 'fading...
        Case 5:pri.image = a:FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub FadePrih(nr, pri, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:pri.image = d:SetFlash nr, 0:FadingLevel(nr) = 0 'Off
        Case 3:pri.image = c:FadingLevel(nr) = 2 'fading...
        Case 4:pri.image = b:FadingLevel(nr) = 3 'fading...
        Case 5:pri.image = a:SetFlash nr, 1:FadingLevel(nr) = 1 'ON
    End Select
End Sub

Sub FadePrim(nr, pri, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:pri.image = d
        Case 3:pri.image = c
        Case 4:pri.image = b
        Case 5:pri.image = a
    End Select
End Sub

Sub NFadePri(nr, pri, a, b)
    Select Case FadingLevel(nr)
        Case 4:pri.image = b:FadingLevel(nr) = 0 'off
        Case 5:pri.image = a:FadingLevel(nr) = 1 'on
    End Select
End Sub

Sub NFadePrim(nr, pri, a, b)
    Select Case FadingLevel(nr)
        Case 4:pri.image = b
        Case 5:pri.image = a
    End Select
End Sub

'Fade a collection of lights

Sub FadeLCo(nr, a, b) 'fading collection of lights
    Dim obj
    Select Case FadingLevel(nr)
        Case 2:vpmSolToggleObj b, Nothing, 0, 0:FadingLevel(nr) = 0
        Case 3:vpmSolToggleObj b, Nothing, 0, 1:FadingLevel(nr) = 2
        Case 4:vpmSolToggleObj a, Nothing, 0, 0:FadingLevel(nr) = 3
        Case 5:vpmSolToggleObj a, Nothing, 0, 1:FadingLevel(nr) = 1
    End Select
End Sub


'****************************************
' B2B Collision by Steely & Pinball Ken
'****************************************
' For use with core.vbs 3.37 or greater to grab BSize variable
	
Dim tnopb, nosf, iball, cnt, errMessage, B2BOn

B2BOn = 2 '0=Off, 1=On, 2=AutoDetect
'CheckB2B
XYdata.interval = 10 ' <<<<< ADD timer named XYData to table
tnopb = 7	' <<<<< SET to the "Total Number Of Possible Balls" in play at any one time
nosf = 8	' <<<<< SET to the "Number Of Sound Files" used / B2B collision volume levels

ReDim CurrentBall(tnopb), BallStatus(tnopb)

For cnt = 0 to ubound(BallStatus) : BallStatus(cnt) = 0 : Next


'======================================================
' <<<<<<<<<<<<<< Ball Identification >>>>>>>>>>>>>>
'======================================================
	
'******************************
' Destruk's alternative vpmCreateBall for use with B2B Enabled tables
' Core.vbs calls vpmCreateBall when a ball is created from a ball stack
'******************************
 Set vpmCreateBall = GetRef("B2BvpmCreateBall")		' Override the core.vbs and redefine vpmCreateBall

 Function B2BvpmCreateBall(aKicker)
	For cnt = 1 to ubound(ballStatus)				' Loop through all possible ball IDs
		If ballStatus(cnt) = 0 Then					' If ball ID is available...
			If Not IsEmpty(vpmBallImage) Then		' Set ball object with the first available ID
				Set CurrentBall(cnt) = aKicker.Createsizedball(bsize).Image
			Else
				Set CurrentBall(cnt) = aKicker.Createsizedball(bsize)
			End If
'			CheckBallShading (cnt)
			Set B2BvpmCreateBall = aKicker
			CurrentBall(cnt).uservalue = cnt		' Assign the ball's uservalue to it's new ID
			ballStatus(cnt) = 1						' Mark this ball status active
			ballStatus(0) = ballStatus(0)+1			' Increment ballStatus(0), the number of active balls
			If B2BOn > 0 Then						' If B2BOn is 0, it overrides auto-turn on collision detection
													' If more than one ball active, start collision detection process
				If ballStatus(0) > 1 and XYdata.enabled = False Then XYdata.enabled = True
			End If
			Exit For								' New ball ID assigned, exit loop
		End If
	Next
 End Function

' Use CreateBallID(kickername) to manually create a ball with a BallID
' Can also be used on nonVPM tables (EM or Custom)

 Sub CreateBallID(aKicker)
	For cnt = 1 to ubound(ballStatus)				' Loop through all possible ball IDs
		If ballStatus(cnt) = 0 Then					' If ball ID is available...
			Set CurrentBall(cnt) = aKicker.Createsizedball(bsize)		' Set ball object with the first available ID
		'	CheckBallShading (cnt)
			CurrentBall(cnt).uservalue = cnt		' Assign the ball's uservalue to it's new ID
			ballStatus(cnt) = 1						' Mark this ball status active
			ballStatus(0) = ballStatus(0)+1			' Increment ballStatus(0), the number of active balls
'			If BallType=1 Then
				On Error Resume Next

'			end if
			If B2BOn > 0 Then						' If B2BOn is 0, it overrides auto-turn on collision detection
													' If more than one ball active, start collision detection process
				If ballStatus(0) > 1 and XYdata.enabled = False Then XYdata.enabled = True
			End If
			Exit For								' New ball ID assigned, exit loop
		End If
	Next 
 End Sub
 
' Use CreateBallID2(kickername, ballsize) to manually create a custom sized ball with a BallID
' Can also be used on nonVPM tables (EM or Custom)

 Sub CreateBallID2(aKicker, bsize2)					' Use to manually create a ball with a BallID with a custom size
	For cnt = 1 to ubound(ballStatus)				' Loop through all possible ball IDs
		If ballStatus(cnt) = 0 Then					' If ball ID is available...
			Set CurrentBall(cnt) = aKicker.Createsizedball(bsize2/2)		' Set ball object with the first available ID
	'		CheckBallShading (cnt)
			CurrentBall(cnt).uservalue = cnt		' Assign the ball's uservalue to it's new ID
			ballStatus(cnt) = 1						' Mark this ball status active
			ballStatus(0) = ballStatus(0)+1			' Increment ballStatus(0), the number of active balls
			If B2BOn > 0 Then						' If B2BOn is 0, it overrides auto-turn on collision detection
													' If more than one ball active, start collision detection process
				If ballStatus(0) > 1 and XYdata.enabled = False Then XYdata.enabled = True
			End If
			Exit For								' New ball ID assigned, exit loop
		End If
	Next 
 End Sub
 
'Call this sub from every kicker that destroys a ball, before the ball is destroyed.
	
 Sub ClearBallid
	'StopRollingSound
	On Error Resume Next							' Error handling for debugging purposes
	iball = ActiveBall.uservalue					' Get the ball ID to be cleared
	If Err Then Msgbox Err.description & vbCrLf & iball
	ballStatus(iBall) = 0							' Clear the ball status
	ballStatus(0) = ballStatus(0)-1					' Subtract 1 ball from the # of balls in play
	On Error Goto 0
 End Sub



'******************************************
' Use RollingSoundTimer to call div subs
'******************************************

Sub RollingSoundTimer_Timer()
	RollingSound	
End Sub


Sub Table1_exit()
	Controller.Pause = False
	Controller.Stop
End Sub


Dim BallShadow
BallShadow = Array (BallShadow0,BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6,BallShadow7,BallShadow8)
Const fakeballs =  1 ' if there is a fake ball used to help shaking objects for example
sub BallShadowUpdate()
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
'****** JPJ's adaptation to change shadow direction. Don't want to see the shadow thru the wall when you loose by the right outlane.
    For b = 2 to UBound(BOT)-1
       If BOT(b).X < table1.Width/2 Then
			BallShadow(b).X = ((BOT(b).X) - 3.25) + ABS((1-(Exp((ABS(((BOT(b).X)) - (table1.Width/2)))/1000)))*60)
      Else
            BallShadow(b).X = ((BOT(b).X) - 3.25) - ABS((1-(Exp((ABS(((BOT(b).X)) - (table1.Width/2)))/1000)))*60)
       End If
'****** 
           BallShadow(b).visible = 1
  
        If BOT(b).Z > 20 and BOT(b).Z < 40 Then '***** JPJ's adaptation to hide shadows when the ball is mounting thru wires
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub


' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX and Rothbauerw
' PlaySound sound, 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
' *******************************************************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position

Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
  PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Set position as table object (Use object or light but NOT wall) and Vol to 1

Sub PlaySoundAt(soundname, tableobj)
  PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed.

Sub PlaySoundAtBall(soundname)
  PlaySoundAt soundname, ActiveBall
End Sub

'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Volume)
  PlaySound sound, 1, Volume, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
  PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

'Set position as bumperX and Vol manually.

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBOTBallZ(sound, BOT)
    PlaySound sound, 0, ABS(BOT.velz)/17, Pan(BOT), 0, Pitch(BOT), 1, 0, AudioFade(BOT)
End Sub

' play a looping sound at a location with volume
Sub PlayLoopSoundAtVol(sound, tableobj, Vol)
	PlaySound sound, -1, Vol, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub



'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function RndNum(min, max)
    RndNum = Int(Rnd() * (max-min + 1) ) + min ' Sets a random number between min and max
End Function

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = tableobj.y * 2 / Torpedo.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = tableobj.x * 2 / Torpedo.width-1
  If tmp > 0 Then
    AudioPan = Csng(tmp ^10)
  Else
    AudioPan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = ball.x * 2 / Torpedo.width-1
  If tmp > 0 Then
    Pan = Csng(tmp ^10)
  Else
    Pan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
  Vol = Csng(BallVel(ball) ^2 / VolDiv)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
  Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
  BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function BallVelZ(ball) 'Calculates the ball speed in the -Z
    BallVelZ = INT((ball.VelZ) * -1 )
End Function

Function VolZ(ball) ' Calculates the Volume of the sound based on the ball speed in the Z
    VolZ = Csng(BallVelZ(ball) ^2 / 200)*1.2
End Function

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

'*****************************************
'    JP's VP10 Collision & Rolling Sounds
'*****************************************

Const tnob = 6 ' total number of balls
ReDim rolling(tnob)
ReDim collision(tnob)
Initcollision

Sub Initcollision
    Dim i
    For i = 0 to tnob
        collision(i) = -1
        rolling(i) = False
    Next
End Sub

Dim Issue
Issue = 0

Sub Rollingsound_Timer()
    Dim BOT, B, B1, B2, dx, dy, dz, distance, radii
    BOT = GetBalls

	For B = UBound(BOT) +1 to tnob
        rolling(b) = False
		StopSound("fx_ballrolling" & b)
		StopSound("fx_plasticrolling" & b)
		StopSound("fx_metalrollingA" & b)
		StopSound("fx_metalrollingB" & b)
	Next

    If UBound(BOT) = -1 Then 
'			GO = 1:Exit Sub
'		Else
'			GO = 0
	End if

    For b = 0 to UBound(BOT)
'debug.print "botZ :"& BOT(b).z

'********** protection Magnetbasket issue **********
	If BOT(b).z>269 and BOT(b).z < 275 then issue = Issue + 1:end If
	If issue >600 then issue = 0:magnetissue.enabled = 1:end if 
'***************************************************

      If BallVel(BOT(b) ) > 1 Then
        rolling(b) = True
		if InRect(BOT(b).x, BOT(b).y, 865,850,910,850,910,925,865,925) And BOT(b).z < 280+27 And BOT(b).z > 195+27 Then
				HelpDunkPlunger.collidable = 1
			Else
				HelpDunkPlunger.collidable = 0
		end if
        if BOT(b).z < 27 Then ' Ball on playfield
			StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b):StopSound("fx_plasticrolling" & b)
			PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*RolVol, AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )

		Else
'metal Ramp B Left from Ballon
		If InRect(BOT(b).x, BOT(b).y, 331,45,915,45,915,835,331,835) And BOT(b).z < 220+27 And BOT(b).z > 130+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_plasticrolling" & b):StopSound("fx_metalrollingA" & b)
				PlaySound("fx_metalrollingB" & b), -1, Vol(BOT(b) )*3*MroVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )*2, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 37,835,596,835,596,1190,37,1190) And BOT(b).z < 148+27 And BOT(b).z > 140+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_plasticrolling" & b):StopSound("fx_metalrollingA" & b)
				PlaySound("fx_metalrollingB" & b), -1, Vol(BOT(b) )*3*MroVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )*3, 1, 0, AudioFade(BOT(b) )
'metal Ramp B Down from Ballon
			ElseIf InRect(BOT(b).x, BOT(b).y, 700,833,808,833,808,1010,700,1010) And BOT(b).z < 190+27 And BOT(b).z > 168+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_plasticrolling" & b):StopSound("fx_metalrollingA" & b)
				PlaySound("fx_metalrollingB" & b), -1, Vol(BOT(b) )*3*MroVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )*7, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 690,1010,755,1010,755,1312,690,1312) And BOT(b).z < 168+27 And BOT(b).z > 139+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_plasticrolling" & b):StopSound("fx_metalrollingA" & b)
				PlaySound("fx_metalrollingB" & b), -1, Vol(BOT(b) )*3*MroVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )*7, 1, 0, AudioFade(BOT(b) )
'metal Plunger Ramp
			ElseIf InRect(BOT(b).x, BOT(b).y, 857,1180,920,1180,920,1720,857,1720) And BOT(b).z < 130+27 And BOT(b).z > 0+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_plasticrolling" & b):StopSound("fx_metalrollingA" & b)
				PlaySound("fx_metalrollingB" & b), -1, Vol(BOT(b) )*3*MroVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )*9, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 865,1167,910,1167,910,1179,865,1179) And BOT(b).z < 140+27 And BOT(b).z > 120+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_plasticrolling" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
'metal Ramp right end
			ElseIf InRect(BOT(b).x, BOT(b).y, 584,1161,720,1161,720,1215,584,1215) And BOT(b).z < 69+27 And BOT(b).z > 63+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_plasticrolling" & b):StopSound("fx_metalrollingA" & b)
				PlaySound("fx_metalrollingB" & b), -1, Vol(BOT(b) )*3*MroVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )*12, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 681,1215,755,1215,755,1465,681,1465) And BOT(b).z < 65+27 And BOT(b).z > 62+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_plasticrolling" & b):StopSound("fx_metalrollingA" & b)
				PlaySound("fx_metalrollingB" & b), -1, Vol(BOT(b) )*3*MroVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )*12, 1, 0, AudioFade(BOT(b) )
'metal RampA left end
			ElseIf InRect(BOT(b).x, BOT(b).y, 203,288,274,288,274,458,203,458) And BOT(b).z < 185+27 And BOT(b).z > 80+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_plasticrolling" & b):StopSound("fx_metalrollingB" & b)
				PlaySound("fx_metalrollingA" & b), -1, Vol(BOT(b) )*2*MroVolA, Pan(BOT(b) ), 0, Pitch(BOT(b) )*18, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 227,458,345,458,345,678,227,678) And BOT(b).z < 83+27 And BOT(b).z > 8+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_plasticrolling" & b):StopSound("fx_metalrollingB" & b)
				PlaySound("fx_metalrollingA" & b), -1, Vol(BOT(b) )*2*MroVolA, Pan(BOT(b) ), 0, Pitch(BOT(b) )*18, 1, 0, AudioFade(BOT(b) )
'Top Ramp plastic
			ElseIf InRect(BOT(b).x, BOT(b).y, 515,15,795,15,795,720,515,720) And BOT(b).z < 190+27 And BOT(b).z > 10+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
				PlaySound("fx_plasticrolling" & b), -1, Vol(BOT(b) )*6*ProVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )+10000, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 415,15,515,15,515,83,415,83) And BOT(b).z < 187+27 And BOT(b).z > 190+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
				PlaySound("fx_plasticrolling" & b), -1, Vol(BOT(b) )*6*ProVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )+10000, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 165,15,415,15,415,209,165,209) And BOT(b).z < 190+27 And BOT(b).z > 115+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
				PlaySound("fx_plasticrolling" & b), -1, Vol(BOT(b) )*6*ProVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )+10000, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 20,110,165,110,165,790,20,790) And BOT(b).z < 119+27 And BOT(b).z > 91+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
				PlaySound("fx_plasticrolling" & b), -1, Vol(BOT(b) )*6*ProVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )+10000, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 20,790,82,790,82,990,20,990) And BOT(b).z < 119+27 And BOT(b).z > 88+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
				PlaySound("fx_plasticrolling" & b), -1, Vol(BOT(b) )*6*ProVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )+10000, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 20,990,165,990,165,1450,20,1450) And BOT(b).z < 119+27 And BOT(b).z > 88+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
				PlaySound("fx_plasticrolling" & b), -1, Vol(BOT(b) )*6*ProVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )+10000, 1, 0, AudioFade(BOT(b) )
'Right Ramp plastic
			ElseIf InRect(BOT(b).x, BOT(b).y, 770,833,945,833,945,1090,770,1090) And BOT(b).z < 102+27 And BOT(b).z > 88+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
				PlaySound("fx_plasticrolling" & b), -1, Vol(BOT(b) )*4*ProVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )+10000, 1, 0, AudioFade(BOT(b) )
			ElseIf InRect(BOT(b).x, BOT(b).y, 742,1090,945,1090,945,1228,742,1228) And BOT(b).z < 90+27 And BOT(b).z > 83+27 Then
				StopSound("fx_ballrolling" & b):StopSound("fx_metalrollingA" & b):StopSound("fx_metalrollingB" & b)
				PlaySound("fx_plasticrolling" & b), -1, Vol(BOT(b) )*4*ProVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )+10000, 1, 0, AudioFade(BOT(b) )
			end if
		End If

		Else
            If rolling(b) = True Then
				StopSound("fx_ballrolling" & b)
				StopSound("fx_plasticrolling" & b)
				StopSound("fx_metalrollingA" & b)
				StopSound("fx_metalrollingB" & b)
                rolling(b) = False
            End If
        End If


'********************************************
'*** Ball Drop Sounds - Thx to RothBauerw ***
		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			PlaySound "ball_bounce" & b, 0, ABS(BOT(b).velz)/13, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
		End If
'********************************************

	Next
End Sub


'**********************
' Ball Collision Sounds
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySoundAtVol "target", ActiveBall, 2
End Sub

Sub Wood_Hit (idx)
	PlaySound "wood", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metal_Hit (idx)
	RandomSoundMetal()
End Sub

Sub RandomSoundMetal()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "MetalHit1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "MetalHit2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "MetalHit3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub Gates_Hit (idx)
	RandomSoundGates()
End Sub

Sub RandomSoundGates()
	Select Case Int(Rnd*2)+1
		Case 1 : debug.print "1":PlaySound "Gate", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : debug.print "4":PlaySound "Gate4", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub Gates2_Hit(idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Plastic_Hit(idx)
	PlaySound "fx_plastichit", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Rollovers_Hit (idx)
	PlaySound "rolloverJPJ", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Bumper_Hit (idx)
	RandomSoundBumper()
End Sub

Sub RandomSoundBumper()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtVol SoundFX("fx_bumper1",DOFContactors), ActiveBall, 1
		Case 2 : PlaySoundAtVol SoundFX("fx_bumper2",DOFContactors), ActiveBall, 1
		Case 3 : PlaySoundAtVol SoundFX("fx_bumper3",DOFContactors), ActiveBall, 1
		Case 4 : PlaySoundAtVol SoundFX("fx_bumper4",DOFContactors), ActiveBall, 1
	End Select
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 15 then
		PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*1.7, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 15 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*1.4, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*1.4, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*1.4, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub


Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper(parm)
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper(parm)
End Sub

Sub RandomSoundFlipper(parm)
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, parm / 10, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, parm / 10, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, parm / 10, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub



'*****  cvpmMagnet replacement: added magnet height and code to stop ball spinning *****
'***** taken from NBA Stern - Thanks for this awesome piece of code - JPJ          *****
Class cvpmMyMagnet
	Private mEnabled, mBalls, mTrigger
	Public X, Y, Strength, Size, GrabCenter, Solenoid, MagnetHeight					'added new property: MagnetHeight 

	Private Sub Class_Initialize
		Size = 1 : Strength = 0 : Solenoid = 0 : mEnabled = False
		Set mBalls = New cvpmDictionary
	End Sub

	Private Property Let NeedUpdate(aEnabled) : vpmTimer.EnableUpdate Me, True, aEnabled : End Property

	Public Sub InitMagnet(aTrigger, aStrength)
		Dim tmp
		If vpmIsArray(aTrigger) Then Set tmp = aTrigger(0) Else Set tmp = aTrigger
		X = tmp.X : Y = tmp.Y : Size = tmp.Radius : vpmTimer.InitTimer tmp, True
		If IsArray(aTrigger) Then mTrigger = aTrigger Else Set mTrigger = aTrigger
		Strength = aStrength : GrabCenter = aStrength > 14
	End Sub

	Public Sub CreateEvents(aName)
		If vpmCheckEvent(aName, Me) Then
			vpmBuildEvent mTrigger, "Hit", aName & ".AddBall ActiveBall"
			vpmBuildEvent mTrigger, "UnHit", aName & ".RemoveBall ActiveBall"
		End If
	End Sub

	Public Property Let MagnetOn(aEnabled) : mEnabled = aEnabled : End Property
	Public Property Get MagnetOn
		If Solenoid > 0 Then MagnetOn = Controller.Solenoid(Solenoid) Else MagnetOn = mEnabled
	End Property

	Public Sub AddBall(aBall)
		With mBalls
			If .Exists(aBall) Then .Item(aBall) = .Item(aBall) + 1 Else .Add aBall, 1 : NeedUpdate = True
		End With
	End Sub

	Public Sub RemoveBall(aBall)
		With mBalls
			If .Exists(aBall) Then .Item(aBall) = .Item(aBall) - 1 : If .Item(aBall) <= 0 Then .Remove aBall
			NeedUpdate = (.Count > 0)
		End With
	End Sub

	Public Property Get Balls : Balls = mBalls.Keys : End Property

	Public Sub Update
		Dim obj
		If MagnetOn Then
			On Error Resume Next
			For Each obj In mBalls.Keys
				If obj.X < 0 Or Err Then mBalls.Remove obj Else AttractBall obj
			Next
			On Error Goto 0
		End If
	End Sub

	Public Sub AttractBall(aBall)
		Dim dX, dY, dist, force, ratio
		dX = aBall.X - X : dY = aBall.Y - Y : dist = Sqr(dX*dX + dY*dY)
		If dist > Size Or dist < 1 Or aBall.Z<MagnetHeight Then Exit Sub 'Just to be safe										'added new code to attract ball only if its height is more than the "MagnetHeight" property
		If GrabCenter And dist < 20 Then
			aBall.VelX = 0 : aBall.VelY = 0 : aBall.X = X : aBall.Y = Y
			aBall.AngMomX=0:aBall.AngMomY=0:aBall.AngMomZ=0:aBall.AngVelX=0:aBall.AngVelY=0:aBall.AngVelZ=0						'added new code to stop ball spinning
		Else
			ratio = dist / (1.5*Size)
			force = Strength * exp(-0.2/ratio)/(ratio*ratio*56) * 1.5
			aBall.VelX = (aBall.VelX - dX * force / dist) * 0.985
			aBall.VelY = (aBall.VelY - dY * force / dist) * 0.985
		End if
	End Sub
	' obsolete
	Public Property Let Range(aSize) : Size = aSize : End Property
	Public Property Get Range        : Range = Size : End Property
End Class

'******************* LUT CHoice **************************

Sub SetLUT
	Select Case LUTmeUP
		Case 0:Table1.ColorGradeImage = 0
		Case 1:Table1.ColorGradeImage = "ColorGradeLUT256x16_ConSat"
		Case 2:Table1.ColorGradeImage = "LUT1_1_09"
		Case 3:Table1.ColorGradeImage = "LUTbassgeige1"
		Case 4:Table1.ColorGradeImage = "LUTbassgeige2"
		Case 5:Table1.ColorGradeImage = "LUTbassgeigemeddark"
		Case 6:Table1.ColorGradeImage = "LUTfleep"
		Case 7:Table1.ColorGradeImage = "LUTmandolin"
		Case 8:Table1.ColorGradeImage = "LUTVogliadicane70"
		Case 9:Table1.ColorGradeImage = "LUTchucky4"
		Case 10:Table1.ColorGradeImage = "LUTTHX_Refference"
	end Select
	WriteLUT 'Write LUT each time you change it
end sub 

'***************** Writing file for LUT's memory JPJ **********
sub WriteLUT
Set File = FileObj.createTextFile(UserDirectory & "SpaceJamLUT.txt",True) 'write new file, and delete one if it exists
	File.writeline(LUTmeUP)
	File.close
End sub
'**********************************************************

Sub LUTBox_Timer
	LUTBox.TimerEnabled = 0 
	LUTBox.Visible = 0
End Sub

Sub ShowLUT
	LUTBox.visible = 1
	LUTBox.text = "LUTmeUP: " & CStr(LUTmeUP)
	LUTBox.TimerEnabled = 1
End Sub


'********************************************
'*	Led part, adapted from JP Salas script  *
'********************************************

Dim obj, rGreen, rRed, rBlue, RGBFactor, RGBStep

Sub RainbowTimer_Timer 'rainbow led light color changing
	RGBFactor =20 
						Select Case RGBStep
							Case 0 'Green
								rGreen = rGreen + RGBFactor
								If rGreen > 255 then
									rGreen = 255
									RGBStep = 1
								End If
							Case 1 'Red
								rRed = rRed - RGBFactor
								If rRed < 0 then
									rRed = 0
									RGBStep = 2
								End If
							Case 2 'Blue
								rBlue = rBlue + RGBFactor
								If rBlue > 255 then
									rBlue = 255
									RGBStep = 3
								End If
							Case 3 'Green
								rGreen = rGreen - RGBFactor
								If rGreen < 0 then
									rGreen = 0
									RGBStep = 4
								End If
							Case 4 'Red
								rRed = rRed + RGBFactor
								If rRed > 255 then
									rRed = 255
									RGBStep = 5
								End If
							Case 5 'Blue
								rBlue = rBlue - RGBFactor
								If rBlue < 0 then
									rBlue = 0
									RGBStep = 0
								End If
						End Select


		if PRefresh.state = 1 then 
							For each obj in Led1
								obj.color = RGB(rRed \ 10, rGreen \ 10, rBlue \ 10)
								obj.colorfull = RGB(rRed, rGreen, rBlue)
							Next
		end if
	If led = 1 Then
				For each obj in RainbowLights
					obj.color = RGB(rRed \ 10, rGreen \ 10, rBlue \ 10)
					obj.colorfull = RGB(rRed, rGreen, rBlue)
				Next
			if ledramp = 1 then 
				UpdateMaterial "3dLolaB",0,0,0,0,0,0,1,RGB(rRed \ 10, rGreen \ 10, rBlue \ 10),0,0,False,True,0,0,0,0 
				UpdateMaterial "3dPlastic glass1",0,1,0,1,0.2,1,0.35,RGB(rRed \ 10, rGreen \ 10, rBlue \ 10),RGB(255,255,255),RGB(234,234,234),False,True,0,0,0,0 
			end if
				exit Sub
	end if
End Sub
