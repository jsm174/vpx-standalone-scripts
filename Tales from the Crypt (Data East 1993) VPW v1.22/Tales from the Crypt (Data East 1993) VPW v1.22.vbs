' Tales from the Crypt - IPDB No. 2493
' © Data East 1993
' https://www.ipdb.org/machine.cgi?gid=2493
'
' Please use VPX 10.6, there may be issues with 10.7
'
'*** V-Pin Workshop TFTC Monsters ***
'- Project Manager: Tomate
'- Models and textures with Blender & Octane: Tomate
'- Ramps: Tomate
'- Primitive fading code for GI and flashers: iaakki
'- "Three layer" 3D Inserts: iaakki
'- Tombstone Code: Sixtoe, DJRobX
'- PF edits and insert texts: iaakki
'- Additional lighting: iaakki, Sixtoe, Skitso, G5k, Tomate
'- Wylte RTX ball shadows: Wylte, apophis, iaakki
'- VR Stuff: Sixtoe, Rawd-Leojreimroc-Hauntfreaks (Backglass), Gear323 (Plunger)
'- nFozzy physics: iaakki, Benji
'- Rubberizer and TargetBouncer: iaakki
'- Fleep Sounds: iaakki, Benji
'- Miscellaneous fixes and tweaks: Sixtoe, CyberPez, apophis, kingdids, baldgeek, fluffhead35, HauntFreaks
'- Testing: Rik, VPW team

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

'///////////////////////-----General Sound Options-----///////////////////////
'//  VolumeDial is the actual global volume multiplier for the mechanical sounds.
'//  Values smaller than 1 will decrease mechanical sounds volume.
'//  Recommended values should be no greater than 1.
Const VolumeDial = 0.8

'//////////////////////-----VR Room-----///////////////////////
Const VRRoom = 0				'0 - VR Room off, 1 - Minimal Room without RTX Ball shadows, 2 - Minimal Room, 3 - Ultra Minimal without RTX Ball shadows
Const VRFlashingBackglass = 1	'0 - VR Backglass Flashers off, 1- VR Backglass Flashers on, 2- VR Backglass Flasher on with extra blinking light
Const PlungerEyeMod = 1			'0 - Regular Plunger, 1 - Lighted Eyes Mod

'///////////////////////////////////////////////////////////////

Const ImageSwapsForFlashers = 1		' 0 - regular flashers (for old and slow cabinets), 1 - Image swaps and fades for flashers.


'///////////////////////-----Cabinet Mode-----////////////////////
Const CabinetMode = 0 'Cabinet mode - will skew sideblades and help correct the aspect ratio to match your screen.

DisableLUTSelector = 1  ' Disables the ability to change LUT option with magna saves in game when set to 1

'///////////////////////-----Ball Shadows-----////////////////////
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									'2 = flasher image shadow, but it moves like ninuzzu's

'///////////////////////-----3D Insert NormalMaps---------////////////////////
dim NormalsForInserts
NormalsForInserts  = 0	'May look odd with certain POV's in cabinetmode, gets enabled for VR


const bSlingSpin = false 		'experimental Sling corner spin feature. This is a random thing that happens with pinball. Disabled by default


'***********	Set the default LUT set	*********************************
'LUTset Types:
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
'11 = Tomate washed out 
'12 = LUT1on1
'13 = LUTbassgeige1
'14 = LUTblacklight

'You can change LUT option within game with left and right CTRL keys
Dim LUTset, DisableLUTSelector, LutToggleSound
LutToggleSound = True
LoadLUT
'LUTset = 11	'override saved LUT for debug
SetLUT


'if VRRoom = 1 Or VRRoom = 3 Then 'RTX shadow disable for some VRRoom styles
'	RTXBallShadows = 0
'end if

if VRRoom <> 0 Then	'disabling cabinet mode for VRroom, they wont work together
	CabinetMode = 0
	NormalsForInserts = 1
end if

dim insrt
If NormalsForInserts = 0 Then
	for each insrt in OffInserts:insrt.NormalMap = "":Next
end if

Const PFGIOFFOpacity = 100




If CabinetMode = 1 Then
	angleSides.size_y = 1000
	angleSidesOFF.size_y = 1000
	sidewalls.size_y = 0.01
	sidewallsOFF.size_y = 0.01
	angleSides.size_x = 1000
	angleSidesOFF.size_x = 1000
	sidewalls.size_x = 0.01
	sidewallsOFF.size_x = 0.01
'	sidewalls.size_y = 1300
'	sidewallsOFF.size_y = 1300
	PinCab_Rails.visible = 0
	flasherbloomLF.height = 395
	flasherbloomLF.rotx = -8.5
	flasherbloomRF.height = 394.5
	flasherbloomRF.rotx = -8.5
Else
	angleSides.size_y = 0.01
	angleSidesOFF.size_y = 0.01
	sidewalls.size_y = 1000
	sidewallsOFF.size_y = 1000
	angleSides.size_x = 0.01
	angleSidesOFF.size_x = 0.01
	sidewalls.size_x = 1000
	sidewallsOFF.size_x = 1000
	PinCab_Rails.visible = 1
	flasherbloomLF.height = 295
	flasherbloomLF.rotx = -6.5
	flasherbloomRF.height = 294.5
	flasherbloomRF.rotx = -6.5
End If

Const BallSize = 25
Const Ballmass = 1


Const cGameName="tftc_400",UseSolenoids=2,UseLamps=0,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff"

Dim DesktopMode:DesktopMode = Table1.ShowDT
Dim UseVPMDMD
If VRRoom <> 0 Then UseVPMDMD = True Else UseVPMDMD = DesktopMode

LoadVPM "01120100", "de.vbs", 3.02

'clearPlastic.blenddisablelighting = 2
'plasticsOFF.blenddisablelighting=5

' using table width and height in script slows down the performance
dim tablewidth: tablewidth = Table1.width
dim tableheight: tableheight = Table1.height

'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************
 SolCallback(1) = "kisort"
 SolCallback(2) = "KickBallToLane"
 SolCallback(3) = "Auto_Plunger"
 SolCallback(4) = "ResetDrops"
 SolCallback(5) = "ScoopKick"
 SolCallback(6) = "KickBallUp38"
 SolCallback(7) = "KickBallUp52"
 SolCallback(9) = "SolDiv"
 SolCallback(8) = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
 SolCallback(11) = "SetGI"
 SolCallback(16) = "SolShake"
 SolCallback(22) = "Solkickback"
 SolCallback(25) = "Sol1R"	'"SetLamp 65,"  '1R
 SolCallBack(26) = "Sol2R"	'"SetLamp 66,"  '2R
 SolCallBack(27) = "Sol3R"	'"SetLamp 67,"  '3R
 SolCallBack(28) = "Sol4R"	'"SetLamp 68,"  '4R
 SolCallBack(29) = "Sol5R"  '"SetLamp 69,"  '5R
 SolCallBack(30) = "Sol6R"  '"SetLamp 70,"  '6R
 SolCallBack(31) = "Sol7R"	'"SetLamp 71,"  '7R
 SolCallback(32) = "Sol8R"	'"SetLamp 72,"  '8R

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

dim Sol4RGIsync : Sol4RGIsync = -1
dim Sol2RGIsync : Sol2RGIsync = -1
Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objlight(20), ObjTargetLevel(20)
dim VrObj

ObjLevel(1) = 0
ObjLevel(2) = 0

sub Sol1R(flstate)
	If Flstate Then
		Sol1Rlevel = 1
		Sol1Rflash_Timer
	else
		Sol1Rlevel = Sol1Rlevel * 0.6 'minor tweak to force faster fade
	End If
end sub
'1R init
dim Sol1Rlevel, sol1Rfactor, sol1RTmrInterval
sol1Rfactor = 10
sol1RTmrInterval = 20

Sol1Rflash.interval = sol1RTmrInterval
S65.IntensityScale = 0:S65a.IntensityScale = 0:S65b.IntensityScale = 0:S65c.IntensityScale = 0
Fsol1R001.visible = 0


sub Sol1Rflash_timer()
	dim vrobj
'	debug.print "1Rflash level: " & Sol1Rlevel

   'If not Sol1Rflash.Enabled Then Sol1Rflash.Enabled = True : Fsol1R001.visible = 1 : End If
    If not Sol1Rflash.Enabled Then 
		Sol1Rflash.Enabled = True  
		If VRRoom > 0 and VRFlashingBackglass > 0 Then for each vrobj in VRBGFL25: vrobj.visible = 1: VRBBTOPLEFT.visible = true: Next
		Fsol1R001.visible = 1  
    End If

	S65.IntensityScale  = sol1Rfactor * Sol1Rlevel^1.2
	S65a.IntensityScale = sol1Rfactor * Sol1Rlevel^2
	S65b.IntensityScale = sol1Rfactor * Sol1Rlevel^1.2
	S65c.IntensityScale = sol1Rfactor * Sol1Rlevel^2

	If VRRoom > 0 and VRFlashingBackglass > 0 Then
		for each vrobj in VRBGFL25: vrobj.opacity = 100 * Sol1Rlevel^2: next
		VRBBTOPLEFT.opacity = 20 * Sol1Rlevel^2 
		If Sol1Rlevel > 0.4 then PinCab_Backbox.Image = "PincabTopLeft2" Else PinCab_Backbox.Image = "PinCab_Backbox"
	End If

	Fsol1R001.opacity = 100 * Sol1Rlevel

	Sol1Rlevel = Sol1Rlevel * 0.7 - 0.01

   'If Sol1Rlevel < 0 Then Sol1Rflash.Enabled = False : Fsol1R001.visible = 0 : End If
    If Sol1Rlevel < 0 Then 
		Sol1Rflash.Enabled = False 
		If VRRoom > 0 and VRFlashingBackglass > 0 Then for each vrobj in VRBGFL25: vrobj.visible = 0: VRBBTOPLEFT.visible = false: Next
		Fsol1R001.visible = 0 
    End If

end sub




sub Sol2R(flstate)
	'debug.print gametime & " Sol2R: " & flstate & " GI state: " & giprevalvl
	If Flstate Then
'		PlaySoundAtLevelStatic ("fx_relay_on"), 0.2 * RelaySoundLevel, p70off
		Sound_Flash_Relay 1, Flasher_Relay_pos
		if Sol2RGIsync = -1 then 'previous fading has ended
			if giprevalvl > 0.1 then	'gi on
				Sol2RGIsync = 1
			else							'gi off
				Sol2RGIsync = 0
			end if
		Else
			'debug.print "***sol4r while fading"
		end if
		'Objlevel(1) = 1
		ObjTargetLevel(1) = 1
	else
		Sound_Flash_Relay 1, Flasher_Relay_pos
		'Objlevel(1) = Objlevel(1) * 0.6 'minor tweak to force faster fade
		ObjTargetLevel(1) = 0
	End If
	FlasherFlash1_Timer
	'debug.print "Sol4RGIsync: " & Sol4RGIsync
	'SetLamp 66,flstate
end sub




sub Sol3R(flstate)
	If Flstate Then
		Sol3Rlevel = 1
		Sol3Rflash_Timer
	else
		Sol3Rlevel = Sol3Rlevel * 0.6 'minor tweak to force faster fade
	End If
end sub
'3R init
dim Sol3Rlevel, sol3Rfactor, sol3RTmrInterval
sol3Rfactor = 10
sol3RTmrInterval = 20

Sol3Rflash.interval = sol3RTmrInterval
S67.IntensityScale = 0:S67a.IntensityScale = 0
f67TOP.opacity = 0 : f67aTOP.opacity = 0




sub Sol3Rflash_timer()
'	debug.print "3Rflash level: " & Sol3Rlevel

    If not Sol3Rflash.Enabled Then 
		Sol3Rflash.Enabled = True 
		f67TOP.visible = 1  
		f67aTOP.visible = 1 
		If VRRoom > 0 and VRFlashingBackglass > 0 Then for each vrobj in VRBGFL27: vrobj.visible = 1: VRBBMIDDLERIGHT.visible = 1: Next
    End If

	'inserts
	S67.IntensityScale  = 5*Sol3Rlevel^1.2
	S67a.IntensityScale  = 5*Sol3Rlevel^1.2

	f67TOP.opacity = 100 * Sol3Rlevel	'center insert top
	f67aTOP.opacity = 100 * Sol3Rlevel	'center insert top

	DisableLightingFlash p67, 10, Sol3Rlevel
	DisableLightingFlash p67bulb, 30, Sol3Rlevel

	DisableLightingFlash p67a, 10, Sol3Rlevel
	DisableLightingFlash p67abulb, 30, Sol3Rlevel

	If VRRoom > 0 and VRFlashingBackglass > 0 Then
		for each vrobj in VRBGFL27: vrobj.opacity = 100 * Sol3Rlevel^2: next
		VRBBMIDDLERIGHT.height = 1400
		VRBBMIDDLERIGHT.opacity = 15 * Sol3Rlevel^2
	End If

	Sol3Rlevel = Sol3Rlevel * 0.7 - 0.01

     If Sol3Rlevel < 0 Then 
		Sol3Rflash.Enabled = False  
		f67TOP.visible = 0 
		f67aTOP.visible = 0 
		If VRRoom > 0 and VRFlashingBackglass > 0 Then for each vrobj in VRBGFL27: vrobj.visible = 0: VRBBMIDDLERIGHT.visible = 0: Next
     End If

end sub



sub Sol4R(flstate)
	'debug.print gametime & " Sol4R: " & flstate & " GI state: " & giprevalvl
	If Flstate Then
'		Sound_Flash_Relay 1, Flasher_Relay_pos
		if Sol4RGIsync = -1 then 'previous fading has ended
			'if giprevalvl > 0 then	'gi on or fading
			if giprevalvl > 0.1 then	'gi on or fading
				Sol4RGIsync = 1
			else							'gi off
				Sol4RGIsync = 0
			end if
		Else
			'debug.print "***sol4r while fading"
		end if
		'Objlevel(2) = 1
		ObjTargetLevel(2) = 1
	else
'		Sound_Flash_Relay 0, Flasher_Relay_pos
'		Objlevel(2) = Objlevel(2) * 0.6 'minor tweak to force faster fade
		ObjTargetLevel(2) = 0
	End If
	FlasherFlash2_Timer
	'debug.print "Sol4RGIsync: " & Sol4RGIsync
end sub



sub Sol5R(flstate)
	If Flstate Then
		Sol5Rlevel = 1
		Sol5Rflash_Timer
	else
		Sol5Rlevel = Sol5Rlevel * 0.6 'minor tweak to force faster fade
	End If
	'debug.print "Sol4RGIsync: " & Sol4RGIsync
end sub

'5R init
dim Sol5Rlevel, sol5Rfactor, sol5RTmrInterval
sol5Rfactor = 10
sol5RTmrInterval = 20

Sol5Rflash.interval = sol5RTmrInterval
S69.IntensityScale = 0:S69a.IntensityScale = 0:S69b.IntensityScale = 0:S69c.IntensityScale = 0 :S69d.IntensityScale = 0 ':S69e.IntensityScale = 0


sub Sol5Rflash_timer()
'	debug.print "5Rflash level: " & Sol5Rlevel

    If not Sol5Rflash.Enabled Then 
		Sol5Rflash.Enabled = True  
		If VRRoom > 0 and VRFlashingBackglass > 0 Then for each vrobj in VRBGFL29: vrobj.visible = 1: VRBBTOPRIGHT.visible = 1: Next
    End If

	S69.IntensityScale  = sol5Rfactor * Sol5Rlevel^1.2
	S69a.IntensityScale = sol5Rfactor * Sol5Rlevel^1.2
	S69c.IntensityScale = sol5Rfactor * Sol5Rlevel^2
	S69d.IntensityScale = sol5Rfactor * Sol5Rlevel^2

	'inserts
	S69b.IntensityScale = Sol5Rlevel * 10
	DisableLightingFlash p69b, 20, Sol7Rlevel
	DisableLightingFlash p69bbulb, 40, Sol7Rlevel

	If VRRoom > 0 and VRFlashingBackglass > 0 Then
		for each vrobj in VRBGFL29: vrobj.opacity = 100 * Sol5Rlevel^2: next
		VRBBTOPRIGHT.opacity = 10 * Sol5Rlevel^2
		If Sol5Rlevel > 0.5 then PinCab_Backbox.Image = "PincabTopRight2" Else PinCab_Backbox.Image = "PinCab_Backbox"
	End If

	Sol5Rlevel = Sol5Rlevel * 0.7 - 0.01

    If Sol5Rlevel < 0 Then 
		Sol5Rflash.Enabled = False
		If VRRoom > 0 and VRFlashingBackglass > 0 Then for each vrobj in VRBGFL29: vrobj.visible = 0: VRBBTOPRIGHT.visible = 0: Next
    End If


end sub


sub Sol6R(flstate)
	If Flstate Then
		Sol6Rlevel = 1
		Sol6Rflash_Timer
	else
		Sol6Rlevel = Sol6Rlevel * 0.6 'minor tweak to force faster fade
	End If
end sub


'6R init
dim Sol6Rlevel, sol6Rfactor, sol6RTmrInterval
sol6Rfactor = 10
sol6RTmrInterval = 20

Sol6Rflash.interval = sol6RTmrInterval
S70.IntensityScale = 0:S70a.IntensityScale = 0:S70b.IntensityScale = 0:S70c.IntensityScale = 0
Fsol6R001.visible = 0

sub Sol6Rflash_timer()
'	debug.print "6Rflash level: " & Sol6Rlevel
    If not Sol6Rflash.Enabled Then 
		Sol6Rflash.Enabled = True  
		Fsol6R001.visible = 1
		If VRRoom > 0 and VRFlashingBackglass > 0 Then for each vrobj in VRBGFL30: vrobj.visible = 1: VRBBBOTTOMRIGHT.visible = 1: Next
    End If

	Fsol6R001.opacity = 100 * Sol6Rlevel
	S70b.IntensityScale  = 5*Sol6Rlevel
	S70c.IntensityScale  = 10*Sol6Rlevel

	'inserts
	S70.IntensityScale  = 5*Sol6Rlevel^1.2
	S70a.IntensityScale  = 5*Sol6Rlevel^1.2

	DisableLightingFlash p70, 50, Sol6Rlevel
	DisableLightingFlash p70bulb, 70, Sol6Rlevel

	DisableLightingFlash p70a, 20, Sol6Rlevel
	DisableLightingFlash p70abulb, 40, Sol6Rlevel

	If VRRoom > 0 and VRFlashingBackglass > 0 Then
		for each vrobj in VRBGFL30: vrobj.opacity = 100 * Sol6Rlevel^2: next
		VRBBBOTTOMRIGHT.opacity = 10 * Sol6Rlevel^2
	End If

	Sol6Rlevel = Sol6Rlevel * 0.65 - 0.01

    If Sol6Rlevel < 0 Then 
		Sol6Rflash.Enabled = False  
		Fsol6R001.visible = 0
		If VRRoom > 0 and VRFlashingBackglass > 0 Then for each vrobj in VRBGFL30: vrobj.visible = 0: VRBBBOTTOMRIGHT.visible = 0: Next
    End If

end sub


sub Sol7R(flstate)
	If Flstate Then
		Sol7Rlevel = 1
		Sol7Rflash_Timer
	else
		Sol7Rlevel = Sol7Rlevel * 0.6 'minor tweak to force faster fade
	End If
end sub

dim Sol7Rlevel, sol7Rfactor, sol7RTmrInterval
sol7Rfactor = 10
sol7RTmrInterval = 20

Sol7Rflash.interval = sol7RTmrInterval
S71.IntensityScale = 0:S71a.IntensityScale = 0:S71b.IntensityScale = 0:S71c.IntensityScale = 0
f71TOP.opacity = 0

sub Sol7Rflash_timer()
	'debug.print "7Rflash level: " & Sol7Rlevel
    If not Sol7Rflash.Enabled Then 
		Sol7Rflash.Enabled = True 
		f71TOP.visible = 1 
    End If

	'under ramp
	S71.IntensityScale  = 5 * sol7Rfactor * Sol7Rlevel^1.2
	S71c.IntensityScale  = sol7Rfactor * Sol7Rlevel^2	'probably not visible

	'inserts
	S71a.IntensityScale  = 10 * Sol7Rlevel^1.2
	S71b.IntensityScale  = 10 * Sol7Rlevel^1.2

	f71TOP.opacity = 100 * Sol7Rlevel	'center insert top

	DisableLightingFlash p71, 10, Sol7Rlevel * 10
	DisableLightingFlash p71bulb, 40, Sol7Rlevel * 50

	DisableLightingFlash p60, 10, Sol7Rlevel * 15	'crash???
	DisableLightingFlash p60bulb, 40, Sol7Rlevel * 50

	Sol7Rlevel = Sol7Rlevel * 0.7 - 0.01

    If Sol7Rlevel < 0 Then 
		Sol7Rflash.Enabled = False  
		f71TOP.visible = 0  
    End If

end sub


sub Sol8R(flstate)
	If Flstate Then
		Sol8Rlevel = 1
		Sol8Rflash_Timer
	else
		Sol8Rlevel = Sol8Rlevel * 0.6 'minor tweak to force faster fade
	End If
end sub

dim Sol8Rlevel, sol8Rfactor, sol8RTmrInterval
sol8Rfactor = 10
sol8RTmrInterval = 20

Sol8Rflash.interval = sol8RTmrInterval
S72a.IntensityScale = 0:S72b.IntensityScale = 0
f72TOP.opacity = 0

sub Sol8Rflash_timer()
	'debug.print "8Rflash level: " & Sol8Rlevel
	If not Sol8Rflash.Enabled Then 
		Sol8Rflash.Enabled = True  
		f72TOP.visible = 1
		If VRRoom > 0 and VRFlashingBackglass > 0 Then for each vrobj in VRBGFL32: vrobj.visible = 1: VRBBTOPRIGHT.visible = 1: Next
    End If

	'inserts
	S72a.IntensityScale  = Sol8Rlevel^1.2
	S72b.IntensityScale  = Sol8Rlevel^1.2

	f72TOP.opacity = 100 * Sol8Rlevel	'center insert top

	DisableLighting p72, 10, Sol8Rlevel * 10
	DisableLighting p72bulb, 40, Sol8Rlevel * 50

	DisableLightingFlash p52, 10, Sol8Rlevel * 15
	DisableLightingFlash p52bulb, 40, Sol8Rlevel * 60

	If VRRoom > 0 and VRFlashingBackglass > 0 Then
		for each vrobj in VRBGFL32: vrobj.opacity = 100 * Sol8Rlevel^2: next
		VRBBTOPRIGHT.opacity = 20 * Sol8Rlevel^2
		If Sol8Rlevel > 0.4 then PinCab_Backbox.Image = "PincabTopRight2" Else PinCab_Backbox.Image = "PinCab_Backbox"
	End If

	Sol8Rlevel = Sol8Rlevel * 0.7 - 0.01

	If Sol8Rlevel < 0 Then 
		Sol8Rflash.Enabled = False  
		f72TOP.visible = 0 
		If VRRoom > 0 and VRFlashingBackglass > 0 Then for each vrobj in VRBGFL32: vrobj.visible = 0: VRBBTOPRIGHT.visible = 0: Next
    End If



end sub



'******************************************************
'  SLINGSHOT CORNER SPIN
'******************************************************

if bSlingSpin Then
	LSlingSpin.enabled = true
	RSlingSpin.enabled = true
end if

const MaxSlingSpin = 250
const MinCollVelX = 7
const MinCorVelocity = 10

dim lspinball
sub LSlingSpin_hit
	'debug.print "lhit: " & activeball.angmomz & " velx: " & activeball.velx & " vely: " & activeball.vely & " corvel: " & cor.ballvel(activeball.id)
	if activeball.velx < -MinCollVelX And cor.ballvel(activeball.id) > MinCorVelocity then
'		debug.print "lspinball set, vel: " & cor.ballvel(activeball.id)
		lspinball = activeball.id
	end if

end sub

sub LSlingSpin_unhit
	if lspinball <> -1 then
		activeball.angmomz = (cor.ballvel(activeball.id)/2) * activeball.angmomz
		if activeball.angmomz  > MaxSlingSpin then activeball.angmomz  = MaxSlingSpin
		if activeball.angmomz  < -MaxSlingSpin then activeball.angmomz  = -MaxSlingSpin 
		'debug.print "lunhit: " & activeball.angmomz & " velx: " & activeball.velx & " vely: " & activeball.vely & ", vel: " & cor.ballvel(activeball.id)
		lspinball = -1
	end if
end sub


dim rspinball
sub RSlingSpin_hit
	'debug.print "rhit: " & activeball.angmomz & " velx: " & activeball.velx & " vely: " & activeball.vely & " corvel: " & cor.ballvel(activeball.id)
	if activeball.velx > MinCollVelX And cor.ballvel(activeball.id) > MinCorVelocity then
'		debug.print "rspinball set, vel: " & cor.ballvel(activeball.id)
		rspinball = activeball.id
	end if

end sub

sub RSlingSpin_unhit
	if rspinball <> -1 then
		activeball.angmomz = (cor.ballvel(activeball.id)/2) * activeball.angmomz
		if activeball.angmomz  > MaxSlingSpin then activeball.angmomz  = MaxSlingSpin
		if activeball.angmomz  < -MaxSlingSpin then activeball.angmomz  = -MaxSlingSpin  
		'debug.print "runhit: " & activeball.angmomz & " velx: " & activeball.velx & " vely: " & activeball.vely & ", vel: " & cor.ballvel(activeball.id)
		rspinball = -1
	end if
end sub



'******************************************************
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************

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
	AddSlingsPt 0, 0.00,	-7
	AddSlingsPt 1, 0.45,	-7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	7

End Sub


Sub AddSlingsPt(idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LS, RS)
	dim x : for each x in a
		x.addpoint idx, aX, aY
	Next
End Sub


'******************************************************
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************

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
'			debug.print " BallPos=" & BallPos &" Angle=" & Angle 
'			debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled then aBall.Velx = RotVxVy(0)
			If Enabled then aBall.Vely = RotVxVy(1)
'			debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
'			debug.print " " 
		End If
	End Sub

End Class

'******************************************************
'					FLIPPERS
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
		RightFlipper1.RotateToEnd
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
			RandomSoundReflipUpRight RightFlipper1
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
			SoundFlipperUpAttackRight RightFlipper1
			RandomSoundFlipperUpRight RightFlipper1
		End If
	Else
		RightFlipper.RotateToStart
		RightFlipper1.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
			RandomSoundFlipperDownRight RightFlipper1
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub LeftFlipper_Collide(parm)
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	RightFlipperCollide parm
End Sub

'******************************************************
'		FLIPPER CORRECTION INITIALIZATION
'******************************************************

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
                                        BOT(b).velx = BOT(b).velx / 1.5
                                        BOT(b).vely = BOT(b).vely - 0.5
'										debug.print "nudge"
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
Const PI = 3.1415927

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

dim LFPress, RFPress, RFPress1, LFCount, RFCount
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
			'debug.print "catch bounce: " & LiveCatchBounce
            If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
            ball.vely = LiveCatchBounce * (16 / LiveCatch) ' Multiplier for inaccuracy bounce
            ball.angmomx= 0
            ball.angmomy= 0
            ball.angmomz= 0
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm, 2
    End If
End Sub

'*****************************************************************************************************
'*******************************************************************************************************
'END nFOZZY FLIPPERS''******************************************************
'				FLIPPER AND RUBBER CORRECTION
'******************************************************

'****************************************************************************
'PHYSICS DAMPENERS

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

Sub dPlastics_Hit(idx)
	PlasticsD.Dampen Activeball
End Sub

dim RubbersD : Set RubbersD = new Dampener	'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 0.96	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.96
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener	'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

dim PlasticsD : Set PlasticsD = new Dampener	'this is just rubber but cut down to 95%...
PlasticsD.name = "Plastics"
PlasticsD.debugOn = False	'shows info in textbox "TBPout"
PlasticsD.Print = False	'debug, reports in debugger (in vel, out cor)
PlasticsD.CopyCoef RubbersD, 0.95

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
		dim RealCOR, DesiredCOR, str, coef
		If ver = 1 Then

			DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
			if cor.ballvel(aBall.id) = 0 then
                RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.001) 'hack
            Else
                RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
            end If
			coef = desiredcor / realcor 
			If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
				'debug.print "     parm: " & parm & " momz: " & aball.angmomz &" velx: "& aball.vely
				aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef

				if (aball.velx > 0 And aball.angmomz > 0) Or (aball.velx < 0 And aball.angmomz < 0) then
			        aball.angmomz = aball.angmomz * -0.7								'spin reversal
					'debug.print "reverse"
				Else
					aball.angmomz = aball.angmomz * 1.2
				end if
				'debug.print " --> parm: " & parm & " momz: " & aball.angmomz &" velx: "& aball.vely
			End If
		Elseif ver = 2 Then
			If parm < 10 And parm > 2 And Abs(aball.angmomz) < 15 And aball.vely < 0 then	'medium collision
				'debug.print "     parm: " & parm & " momz: " & aball.angmomz &" velx: "& aball.vely
				aball.angmomz = aball.angmomz * 1.2
				aball.vely = aball.vely * 1.2
				'debug.print "---> parm: " & parm & " momz: " & aball.angmomz &" velx: "& aball.vely
			Elseif parm <= 2 and parm > 0.2 And aball.vely < 0 Then							'soft collision
				'debug.print "***     parm: " & parm & " momz: " & aball.angmomz &" velx: "& aball.vely
				if (aball.velx > 0 And aball.angmomz > 0) Or (aball.velx < 0 And aball.angmomz < 0) then
			        aball.angmomz = aball.angmomz * -0.7								'spin reversal
					'debug.print "reverse"
				Else
					aball.angmomz = aball.angmomz * 1.2
				end if
				aball.vely = aball.vely * 1.4
				'debug.print "*** --->parm: " & parm & " momz: " & aball.angmomz &" velx: "& aball.vely
			End if
		Elseif ver = 3 Then
'			dim RealCOR, DesiredCOR, str, coef
			DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
			if cor.ballvel(aBall.id) = 0 then
                RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.001) 'hack
            Else
                RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
            end If
			coef = desiredcor / realcor 
			If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
				'debug.print "     parm: " & parm & " momz: " & aball.angmomz &" velx: "& aball.vely
				aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
				'debug.print " --> parm: " & parm & " momz: " & aball.angmomz &" velx: "& aball.vely
			End If
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



'******************************************************
'****  DROP TARGETS by Rothbauerw
'******************************************************


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
Dim DT41, DT42, DT43

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



'********This is a modified version to DT's. Bend animation is reversed. Do not mix with other codes.*****
Set DT41 = (new DropTarget)(sw41w, sw41wa, sw41p, 41, 0, false)
Set DT42 = (new DropTarget)(sw42w, sw42wa, sw42p, 42, 0, false)
Set DT43 = (new DropTarget)(sw43w, sw43wa, sw43p, 43, 0, false)


Dim DTArray
DTArray = Array(DT41, DT42, DT43)


	'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 52 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 10 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 0 'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0 'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "" 'Drop Target Hit sound
Const DTDropSound = "" 'Drop Target Drop sound
Const DTResetSound = "" 'Drop Target reset sound

Const DTMass = 0.2 'Mass of the Drop Target (between 0 and 1), higher values provide more resistance

'******************************************************
'  DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch)

	PlayTargetSound
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
		prim.rotx = -DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
		DTAnimate = animate
		Exit Function
		elseif (animate = 1 or animate = 4) and animtime > DTDropDelay Then
		primary.collidable = 0
		If animate = 1 then secondary.collidable = 1 else secondary.collidable= 0
		prim.rotx = -DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
		animate = 2
		SoundDropTargetDrop prim
	End If

	if animate = 2 Then
		transz = (animtime - DTDropDelay)/DTDropSpeed *  DTDropUnits * -1
		if prim.transz > -DTDropUnits  Then
			prim.transz = transz
		end if

		prim.rotx = -DTMaxBend * cos(rangle)/2
		prim.roty = DTMaxBend * sin(rangle)/2

		if prim.transz <= -DTDropUnits Then 
			prim.transz = -DTDropUnits
			secondary.collidable = 0
			controller.Switch(Switchid) = 1
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
		prim.rotx = -DTMaxBend * cos(rangle)
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
			Dim b

			For b = 0 to UBound(gBOT)
				If InRotRect(gBOT(b).x,gBOT(b).y,prim.x, prim.y, prim.rotz, -25,-10,25,-10,25,25,-25,25) and gBOT(b).z < prim.z+DTDropUnits+25 Then
					gBOT(b).velz = 20
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

Sub DTAnim_Timer()
	DoDTAnim
'	DoSTAnim
'	If psw1.transz < -DTDropUnits/2 Then drop1.visible = 0 else drop1.visible = 1
'	If psw2.transz < -DTDropUnits/2 Then drop2.visible = 0 else drop2.visible = 1
'	If psw3.transz < -DTDropUnits/2 Then drop3.visible = 0 else drop3.visible = 1
'	If psw4.transz < -DTDropUnits/2 Then drop4.visible = 0 else drop4.visible = 1
End Sub


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
'****  END DROP TARGETS
'******************************************************

'****************************************************************
'		STAND-UP TARGET INITIALIZATION
'****************************************************************

'Define a variable for each stand-up target
'Dim ST24, ST52

'Set array with stand-up target objects

'StandupTargetvar = Array(primary, prim, swtich)
' 	primary: 			vp target to determine target hit
'	prim:				primitive target used for visuals and animation
'						****IMPORTANT!!!****
'	------	transy must be used to offset the target animation  ------
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instructions, set to 0
'	target identifier:	The target

'ST24 = Array(sw24, sw24p, 24, 0, 24)
'ST52 = Array(sw52, sw52p, 52, 0, 52)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
'Dim STArray
'STArray = Array(ST24, ST52)

'Configure the behavior of Stand-up Targets
Const STAnimStep =  1.5 		'vpunits per animation step (control return to Start)
Const STMaxOffset = 9 			'max vp units target moves when hit
'Const STHitSound = "target"	'Stand-up Target Hit sound - **Replaced with Fleep Code
Const STMass = 0.2				'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

'****************************************************************
'		STAND-UP TARGETS FUNCTIONS
'****************************************************************

Sub STHit(switch)
	Dim i
	i = STArrayID(switch)

'	PlayTargetSound		'Replaced with Fleep Code
	STArray(i)(3) =  STCheckHit(Activeball,STArray(i)(0))

	If STArray(i)(3) <> 0 Then
		DTBallPhysics Activeball, STArray(i)(0).orientation, STMass
	End If
	DoSTAnim
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 to uBound(STArray) 
		If STArray(i)(4) = switch Then STArrayID = i:Exit Function 
	Next
End Function

'Check if target is hit on it's face
Function STCheckHit(aBall, target) 
	dim bangle, bangleafter, rangle, rangle2, perpvel, perpvelafter
	rangle = (target.orientation - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)

	perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
	perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle) 

	If perpvel <= 0 or perpvelafter >= 0 Then
		STCheckHit = 0 
	Else
		STCheckHit = 1
	End If
End Function

Sub DoSTAnim()
	Dim i
	For i=0 to Ubound(STArray)
		STArray(i)(3) = STAnimate(STArray(i)(0),STArray(i)(1),STArray(i)(2),STArray(i)(3))
	Next
End Sub

Function STAnimate(primary, prim, switch, animate)
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
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

'sub TargetBouncer(aBall,defvalue)
'    dim zMultiplier, vel, vratio
'    if TargetBouncerEnabled <> 0 and aball.z < 30 then
'        debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
'        vel = BallSpeed(aBall)
'        if aBall.velx = 0 then vratio = 1 else vratio = aBall.vely/aBall.velx
'        Select Case Int(Rnd * 6) + 1
'            Case 1: zMultiplier = 0.1*defvalue
'			 Case 2: zMultiplier = 0.2*defvalue
'            Case 3: zMultiplier = 0.3*defvalue
'			 Case 4: zMultiplier = 0.4*defvalue
'            Case 5: zMultiplier = 0.5*defvalue
'            Case 6: zMultiplier = 0.6*defvalue
'        End Select
'        aBall.velz = abs(vel * zMultiplier * TargetBouncerFactor)
'        aBall.velx = sgn(aBall.velx) * sqr(abs((vel^2 - aBall.velz^2)/(1+vratio^2)))
'        aBall.vely = aBall.velx * vratio
'        debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
'        debug.print "conservation check: " & BallSpeed(aBall)/vel
'    end if
'end sub

sub TargetBouncer(aBall,defvalue)
	Dim zMultiplier, vel
	vel = BallSpeed(aBall)
'	debug.print "bounce"
	if aball.z < 30 then
		'debug.print "velz: " & activeball.velz
		Select Case Int(Rnd * 4) + 1
			Case 1: zMultiplier = defvalue+1.1
			Case 2: zMultiplier = defvalue+1.05
			Case 3: zMultiplier = defvalue+0.7
			Case 4: zMultiplier = defvalue+0.3
		End Select
		aBall.velz = aBall.velz * zMultiplier * 1.1
		'debug.print "----> velz: " & activeball.velz
		'debug.print "conservation check: " & BallSpeed(aBall)/vel
	end if
end sub



'**********************************************************************************************************

'Solenoid Controlled toys
'**********************************************************************************************************

' Sub SolRelease(Enabled)
'     If Enabled Then
'         bsTrough.ExitSol_On
'         BallRelease.CreateBall
'         bsBallRelease.AddBall 0
'		 RandomSoundBallRelease BallRelease
'		 'msgbox "release"
'     End If
' End Sub
 
Sub Auto_Plunger(Enabled)
     If Enabled Then
         PlungerIM.AutoFire
	     SoundPlungerReleaseBall()
     End If
End Sub
 

dim DiverterTarget
Sub SolDiv(Enabled):
     If Enabled Then
		Diverter.IsDropped = 0
		DiverterTarget = 125
		Diverter.timerenabled = 1
		playsound SoundFX("diverter",DOFContactors)
	Else
		Diverter.IsDropped = 1
		DiverterTarget = 180
		Diverter.timerenabled = 1
		playsound SoundFX("diverter",DOFContactors)
	End IF
End Sub

Sub Diverter_timer
	if diverterPrim.roty > DiverterTarget Then
		diverterPrim.roty = diverterPrim.roty - 11
	Elseif diverterPrim.roty < DiverterTarget Then
		diverterPrim.roty = diverterPrim.roty + 11
	end If
	
	if diverterPrim.roty = DiverterTarget Then
		Diverter.timerenabled = 0
	end If
	
	diverterPrimOFF.roty = diverterPrim.roty

end Sub


 
Sub SolShake(enabled)
	If enabled Then
	    ShakerMotor.Enabled = 1
		'playsound SoundFX("quake",DOFContactors) 'TODO
	Else
    	ShakerMotor.Enabled = 0
	End If
End Sub

Sub ShakerMotor_Timer()
	Nudge 0,0.2
	Nudge 90,0.2
	Nudge 180,0.2
	Nudge 270,0.2
End Sub 

'Playfield GI
'Sub PFGI(Enabled)
'	If Enabled Then
'		dim xx
'		For each xx in GI:xx.State = 0: Next
'        'PlaySound "fx_relay"
'		PinCab_Backglass.blenddisablelighting = 0.2
'	Else
'		For each xx in GI:xx.State = 1: Next
'        'PlaySound "fx_relay"
'		PinCab_Backglass.blenddisablelighting = 4
'	End If
'End Sub

Sub Solkickback(Enabled):
     If Enabled Then
		plunger1.Fire
		playsound SoundFX("KickBack2",DOFContactors)
	Else
		plunger1.PullBack
	End IF
End Sub

'**********************************************************************************************************
'Initiate Table
'**********************************************************************************************************

 Dim bsTrough, bsBallRelease, bsVuk, bsPScoop, dtBank, mTombStone, activeShadowBall

Dim Ball(6)
Dim InitTime
Dim TroughTime
Dim EjectTime
Dim MaxBalls
Dim TroughCount
Dim TroughBall(7)
Dim TroughEject
Dim Momentum
Dim UpperGIon
Dim Multiball
Dim BallsInPlay
Dim	iBall
Dim	fgBall


Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Tales from the Crypt Data East "&chr(13)&"VPW"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .hidden = 0
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
     On Error Goto 0

     PinMAMETimer.Interval = PinMAMEInterval
     PinMAMETimer.Enabled = 1

     vpmNudge.TiltSwitch = 1
     vpmNudge.Sensitivity = 2
     vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)
 
'    Set dtBank = new cvpmdroptarget
'         dtBank.InitDrop Array(sw41, sw42, sw43), Array(41, 42, 43)
'         dtBank.Initsnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

	Set activeShadowBall = new cvpmDictionary

     ' RIP Tombstone
     Set mTombStone = new cvpmMech
     With mTombStone
         .Mtype = vpmMechOneSol + vpmMechReverse
         .Sol1 = 15
         .Length = 180
         .Steps = 180
         .Acc = 30
         .Ret = 0
         .AddSw 33, 160, 180
         .AddSw 36, 0, 20
         .Callback = GetRef("RipUpdate")
         .Start
     End With

' ball through system
	MaxBalls=3	
	InitTime=61
	EjectTime=0
	TroughEject=1
	TroughCount=0
	iBall = 6
	fgBall = false

    CreatBalls


    plunger1.PullBack
    Diverter.IsDropped = 1

 End Sub
 
 Sub table1_Paused:Controller.Pause = 1:End Sub
 Sub table1_unPaused:Controller.Pause = 0:End Sub
 
'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)

	'TestTableKeyDownCheck (KeyCode)

	If keycode = PlungerKey Or keycode = LockBarKey Then Controller.Switch(62) = 1
    If keycode = LeftTiltKey Then Nudge 90, 2:SoundNudgeLeft()
    If keycode = RightTiltKey Then Nudge 270, 2:SoundNudgeRight()
    If keycode = CenterTiltKey Then Nudge 0, 4:SoundNudgeCenter()

    If keycode = keyFront Then vpmTimer.pulsesw 8 'Buy-in Button - 2 key

'	if keycode = StartGameKey then DisableLUTSelector = 1

	'nFozzy Begin'
	If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then 
		FlipperActivate RightFlipper, RFPress
		FlipperActivate RightFlipper1, RFPress1
	end If
	'nFozzy End'


	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If


	If KeyDownHandler(keycode) Then Exit Sub

	If keycode = RightMagnaSave Then 'AXS 'Fleep
'		Sol2R true
'		vpmtimer.addtimer 100, "Sol2R false '"
'		vpmtimer.addtimer 130, "SetGI false '"
		if DisableLUTSelector = 0 then
            LUTSet = LUTSet  + 1
			if LutSet > 14 then LUTSet = 0
			lutsetsounddir = 1
			If LutToggleSound then
				If lutsetsounddir = 1 And LutSet <> 14 Then
					Playsound "click", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, 1
				End If
				If lutsetsounddir = -1 And LutSet <> 14 Then
					Playsound "click", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, -1
				End If
				If LutSet = 14 Then
					Playsound "scream", 0, 1 * VolumeDial, 0, 0.2, 0, 0, 0, -1
				End If
				LutSlctr.enabled = true
			end if
			SetLUT
			ShowLUT
		end if
	end if
	If keycode = LeftMagnaSave Then
'		Sol4r true
'		vpmtimer.addtimer 100, "Sol4R false '"
'		vpmtimer.addtimer 130, "SetGI true '"
		if DisableLUTSelector = 0 then
			LUTSet = LUTSet - 1
			if LutSet < 0 then LUTSet = 14
			lutsetsounddir = -1
			If LutToggleSound then
				If lutsetsounddir = 1 And LutSet <> 14 Then
					Playsound "click", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, 1
				End If
				If lutsetsounddir = -1 And LutSet <> 14 Then
					Playsound "click", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, -1
				End If
				If LutSet = 14 Then
					Playsound "scream", 0, 1 * VolumeDial, 0, 0.2, 0, 0, 0, -1
				End If
				LutSlctr.enabled = true
			end if
			SetLUT
			ShowLUT
		end if
	end if

End Sub




dim BIPL
Sub Table1_KeyUp(ByVal KeyCode)

	'TestTableKeyUpCheck (KeyCode)

	If keycode = PlungerKey Or keycode = LockBarKey Then 
		Controller.Switch(62) = 0
		If BIPL = 1 Then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
			'msgbox "yes ball"
		Else
			'SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
			'msgbox "no ball"
		End If
	end if

	'nFozzy Begin'
	If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then 
		FlipperDeActivate RightFlipper, RFPress
		FlipperDeActivate RightFlipper1, RFPress1
	end if
	'nFozzy End'

 	If KeyUpHandler(keycode) Then Exit Sub
End Sub



Dim plungerIM
     ' Impulse Plunger
     Const IMPowerSetting = 55 ' Plunger Power
     Const IMTime = 0.6        ' Time in seconds for Full Plunge
     Set plungerIM = New cvpmImpulseP
     With plungerIM
         .InitImpulseP swplunger, IMPowerSetting, IMTime
         .Random 0.3
         '.InitExitSnd  SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
         .CreateEvents "plungerIM"
     End With



dim lutsetsounddir
sub LutSlctr_timer
	If lutsetsounddir = 1 And LutSet <> 14 Then
		Playsound "squeek", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, -1
	End If
	If lutsetsounddir = -1 And LutSet <> 14 Then
		Playsound "squeek", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, 1
	End If
	If LutSet = 14 Then
		Playsound "scream", 0, 0.1*VolumeDial, 0, 0.2, 0, 0, 0, 1
	End If
	LutSlctr.enabled = False
end sub



'iaakki: tweak to make ball bounce properly from drop targets. Without spin it would go to left outlane.
sub plungerLaneHelper_hit()
	if activeball.vely < -38 then
		'debug.print activeball.x &" - "& activeball.y
		'activeball.angmomz = -activeball.angmomz * 2
		activeball.x = 893
		activeball.y = 1197
		activeball.angmomz = -105
		activeball.velx = -12.9
		activeball.vely = -43.5
		'debug.print "--> "&activeball.vely
	end if
end Sub



''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'''''''''''''''''  Ball Through system''''''''''''''''''''''''''
'''''''''''''''''''''by cyberpez''''''''''''''''''''''''''''''''
''''''''''''''''based off of EalaDubhSidhe's''''''''''''''''''''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Dim BallCount
Dim cball0, cBall1, cBall2, cBall3,cBall4, cBall5, cBall6

dim bstatus

Sub CreatBalls()
	Controller.Switch(9) = 1
	Controller.Switch(10) = 1
	Controller.Switch(11) = 1
	Controller.Switch(12) = 1
	Controller.Switch(13) = 1
	Controller.Switch(14) = 1
'	Controller.Switch(15) = 1
	Set cBall0 = drain.CreateSizedballWithMass(BallSize,Ballmass)
	Set cBall1 = sw10.CreateSizedballWithMass(BallSize,Ballmass)
	Set cBall2 = sw11.CreateSizedballWithMass(BallSize,Ballmass)
	Set cBall3 = sw12.CreateSizedballWithMass(BallSize,Ballmass)
	Set cBall4 = sw13.CreateSizedballWithMass(BallSize,Ballmass)
	Set cBall5 = sw14.CreateSizedballWithMass(BallSize,Ballmass)
'	Set cBall6 = sw15.CreateSizedballWithMass(BallSize,Ballmass)



End Sub

Sub Drain_Hit():Controller.Switch(9) = 1:RandomSoundDrain Drain:UpdateTrough:End Sub
Sub Drain_UnHit():Controller.Switch(9) = 0:UpdateTrough:End Sub
Sub sw10_Hit():Controller.Switch(10) = 1:UpdateTrough:End Sub
Sub sw10_UnHit():Controller.Switch(10) = 0:UpdateTrough:End Sub
Sub sw11_Hit():Controller.Switch(11) = 1:UpdateTrough:End Sub
Sub sw11_UnHit():Controller.Switch(11) = 0:UpdateTrough:End Sub
Sub sw12_Hit():Controller.Switch(12) = 1:UpdateTrough:End Sub
Sub sw12_UnHit():Controller.Switch(12) = 0:UpdateTrough:End Sub
Sub sw13_Hit():Controller.Switch(13) = 1:UpdateTrough:End Sub
Sub sw13_UnHit():Controller.Switch(13) = 0:UpdateTrough:End Sub
Sub sw14_Hit():Controller.Switch(14) = 1:UpdateTrough:End Sub
Sub sw14_UnHit():Controller.Switch(14) = 0:UpdateTrough:End Sub
Sub sw15_Hit():Controller.Switch(15) = 1:UpdateTrough:End Sub
Sub sw15_UnHit():Controller.Switch(15) = 0:UpdateTrough:End Sub

Sub UpdateTrough()
	CheckBallStatus.Interval = 500
	CheckBallStatus.Enabled = 1
End Sub

Sub CheckBallStatus_timer()
'	If sw15.BallCntOver = 0 Then sw14.kick 60, 12
	If sw14.BallCntOver = 0 Then sw13.kick 60, 9
	If sw13.BallCntOver = 0 Then sw12.kick 60, 12
	If sw12.BallCntOver = 0 Then sw11.kick 60, 9
	If sw11.BallCntOver = 0 Then sw10.kick 60, 12
	If sw10.BallCntOver = 0 Then drain.kick 60, 12
	Me.Enabled = 0
End Sub

Dim Kicker1active, Kicker2active, Kicker3active, Kicker4active, Kicker5active, Kicker6active

dim DontKickAnyMoreBalls


Sub KickBallToLane(Enabled)
'	If DontKickAnyMoreBalls = 0 then
		RandomSoundBallRelease sw15
		sw15.Kick 70,12
		bstatus = 2
		Kicker1active = 0
		iBall = iBall - 1
		fgBall = false
		Controller.Switch(15)=0
'		DontKickAnyMoreBalls = 1
		DKTMstep = 1
		DontKickToMany.enabled = true
		BallsInPlay = BallsInPlay + 1
'UpdateTrough
'	End If
End Sub


Dim DKTMstep

Sub DontKickToMany_timer ()
	Select Case DKTMstep
	Case 1:
	Case 2:
	Case 3: DontKickAnyMoreBalls = 0:DontKickToMany.Enabled = False: DontKickAnyMoreBalls = 0
	End Select
	DKTMstep = DKTMstep + 1
End Sub


sub kisort(enabled)

'	if fgBall then
		sw14.Kick 60,12
'	playsound "knocker_1"
'		iBall = iBall + 1
'		fgBall = false
'	Controller.Switch(14) = 0
'	end if
UpdateTrough
end sub







'**********************************************************************************************************

 ' Center Vuk


'VUK Lock

Dim BallInKicker1, BallSaucer1, BallInKicker2, BallSaucer2

Sub sw38_Hit()
	Select Case Int(Rnd*2)+1	
		Case 1: PlaySoundAtLevelStatic ("Saucer_Enter_1"), SaucerLockSoundLevel, pUpKicker
		Case 2: PlaySoundAtLevelStatic ("Saucer_Enter_2"), SaucerLockSoundLevel, pUpKicker
	End Select
	'iaakki_110
'	debug.print "sw38 hit: " & activeball.id
	activeball.velx = activeball.velx / 3 : activeball.vely = activeball.vely / 3
	Controller.Switch(38) = 1
	sw38.enabled = true
	BallInKicker1 = 1
	Set BallSaucer1 = activeball
End Sub


Sub sw38_unhit()
'	debug.print "sw38 unhit: " & activeball.id
	BallInKicker1 = 0
End Sub


Sub KickBallUp38(Enabled)
		sw38.timerenabled = 1
End Sub

Dim sw38step

Sub sw38_timer()
	Select Case sw38step
		Case 0:pUpKicker.TransY = 10:Playsoundat SoundFX("fx_vukExit_wire",DOFContactors),sw38  'AddSound Sol VUK Kick
		Case 1:pUpKicker.TransY = 20:If BallInKicker1 = 1 then BallSaucer1.x = 291:BallSaucer1.y = 405:BallSaucer1.vely = 0:BallSaucer1.velx = 0:BallSaucer1.velz = 60 End If
		Case 2:pUpKicker.TransY = 30:
		Case 3:
		Case 4:
		Case 5:pUpKicker.TransY = 25
		Case 6:pUpKicker.TransY = 20
		Case 7:pUpKicker.TransY = 15
		Case 8:pUpKicker.TransY = 10:Controller.Switch(38) = 0
		Case 9:pUpKicker.TransY = 5
		Case 10:pUpKicker.TransY = 0:sw38.timerEnabled = 0:sw38step = 0
	End Select
	sw38step = sw38step + 1
End Sub



'Subway
 Sub sw53_Hit()
	PlaySound "popper_ball"
	Controller.Switch(53) = 1
End Sub

Sub sw53_unHit()
	Controller.Switch(53) = 0
End Sub

 Sub sw54_Hit()
	PlaySound "popper_ball"
	Controller.Switch(54) = 1
End Sub

Sub sw54_unHit()
	Controller.Switch(54) = 0
End Sub
 
 '***********************************
 'sw52 Back VUK - From Subway
 '***********************************


Sub sw52_Hit()
	Select Case Int(Rnd*2)+1	
		Case 1: PlaySoundAtLevelStatic ("Saucer_Enter_1"), SaucerLockSoundLevel, sw52
		Case 2: PlaySoundAtLevelStatic ("Saucer_Enter_2"), SaucerLockSoundLevel, sw52
	End Select
	Controller.Switch(52) = 1
	'iaakki_110
	'debug.print "sw52 hit: " & activeball.id
	sw52.enabled = true
	BallInKicker2 = 1
	Set BallSaucer2 = activeball
End Sub

Sub sw52_unHit()
	'debug.print "sw52 unhit: " & activeball.id
	Controller.Switch(52) = 0
end Sub




Sub KickBallUp52(Enabled)
	sw52.timerenabled = 1
End Sub

Dim sw52step:sw52step = 0

Sub sw52_timer()
	'debug.print "sw52 timer: " & sw52step & " ball in saucer: " & BallInKicker2
	Select Case sw52step
		Case 0:'Playsoundat SoundFX("fx_vukExit_wire",DOFContactors),sw52  'AddSound Sol VUK Kick
		Case 1:
			If BallInKicker2 = 1 then 
				BallSaucer2.x = 857
				BallSaucer2.y = 70
				BallSaucer2.z = -25
				BallSaucer2.vely = 0
				BallSaucer2.velx = 0
				BallSaucer2.velz = 60
				Playsoundat SoundFX("fx_vukExit_wire",DOFContactors),sw52
			End If
		Case 2:
		Case 3:
		Case 4:
		Case 5:
		Case 6:
		Case 7:
		Case 8::'Light52.state = 0
		Case 9:':Controller.Switch(52) = 0
		Case 10::sw52.timerEnabled = 0:sw52step = 0
	End Select
	sw52step = sw52step + 1
End Sub



 
 '***********************************
 ' sw55 Sc00p
 '***********************************

Sub sw55_Hit
	PlaySoundAtLevelStatic ("fx_scoopEnter"), GlobalSoundLevel, sw55	''''AddSound Enter Scoop
	Controller.Switch(55) = 1
End Sub




Sub ScoopKick(Enabled)

	PlaySoundAtLevelStatic ("fx_scoopExit"), GlobalSoundLevel, sw55 ''''AddSound Exit Scoop
' 	sw55.Kick 0,100,1.56 '85=Strength
 	sw55.Kick 0,57 '100=Strength
	Controller.Switch(55) = 0

End Sub


'************************************
' LoopHelper
'************************************


Sub LoopHelper_Hit()
	dim speed:speed = ActiveBall.VelY
'	If speed < 0 Then
'		'PlaySoundAtBall "comet_ramp"
'	Else
'		'StopSound "comet_ramp"
'	End If
	'debug.print ActiveBall.VelX
'	debug.print ActiveBall.VelY
	If speed < -30 and speed > -48 Then
		speed = speed * 1.38
		If speed < -48 Then
			speed = -48
		end If
		ActiveBall.VelY = speed
		ActiveBall.VelX = ActiveBall.VelX * 1.34
		ActiveBall.VelZ = 4
		'debug.print "X >> to: " & ActiveBall.VelX
		'debug.print "Y >> to: " & ActiveBall.VelY
	end if
End Sub

''%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
''   Drop Targets
''%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'
'	dim sw41Dir, sw42Dir, sw43Dir
'	dim sw41Pos, sw42Pos, sw43Pos
'	Dim sw41step, sw42step, sw43step
' 
'	sw41Dir = 1:sw42Dir = 1:sw43Dir = 1
'	sw41Pos = 0:sw42Pos = 0:sw43Pos = 0
'
'  'Targets Init
'	sw41.TimerEnabled = 1:sw42.timerEnabled = 1:sw43.TimerEnabled = 1
'
''Sub DoubleDrop1_HIt:sw41.timerenabled = True:sw42.timerenabled = True: End Sub
''Sub DoubleDrop2_HIt:sw42.timerenabled = True:sw43.timerenabled = True: End Sub
'
Sub sw41w_Hit
	TargetBouncer activeball, 1
	DTHit 41
End Sub
Sub sw42w_Hit
	TargetBouncer activeball, 1
	DTHit 42		
End Sub

Sub sw43w_Hit
	TargetBouncer activeball, 1
	DTHit 43		
End Sub
'
'
'Sub sw41_timer()
'	Select Case sw41step
'		Case 0:
'		Case 1:sw41P.RotX = 92
'		Case 2:sw41P.RotX = 95
'		Case 3:DTBank.Hit 1:sw41Dir = 0:sw41t.Enabled = 1
'		Case 4:sw41P.RotX = 93
'		Case 5:sw41P.RotX = 90:me.timerEnabled = 0:sw41step = 0
'	End Select
'	sw41pOFF.RotX = sw41p.RotX : sw41POFF.TransY=sw41P.TransY
'	sw41step = sw41step + 1
'End Sub
'
''''Target animation
'
' Sub sw41t_Timer()	
'  Select Case sw41Pos
'        Case 0: sw41P.TransY=0
'			If sw41Dir = 1 then
'				sw41t.Enabled = 0
'			else
'			end if        
'        Case 1: sw41P.TransY=0
'        Case 2: sw41P.TransY=-6
'        Case 3: sw41P.TransY=-8
'        Case 4: sw41P.TransY=-18
'        Case 5: sw41P.TransY=-24
'        Case 6: sw41P.TransY=-30
'        Case 7: sw41P.TransY=-36
'        Case 8: sw41P.TransY=-42
'        Case 9: sw41P.TransY=-48
'        Case 10: sw41P.TransY=-52
'			If sw41Dir = 1 then
'			else
'				sw41t.Enabled = 0
'			end if
'	End Select
'	If sw41Dir = 1 then
'		If sw41pos>0 then sw41pos=sw41pos-2
'	else
'		If sw41pos<10 then sw41pos=sw41pos+2
'	end if
'	sw41POFF.TransY=sw41P.TransY
'End Sub
'
'
'Sub sw42_timer()
'	Select Case sw42step
'		Case 0:
'		Case 1:sw42P.RotX = 92
'		Case 2:sw42P.RotX = 95
'		Case 3:DTBank.Hit 2:sw42Dir = 0:sw42t.Enabled = 1
'		Case 4:sw42P.RotX = 93
'		Case 5:sw42P.RotX = 90:me.timerEnabled = 0:sw42step = 0
'	End Select
'	sw42pOFF.RotX = sw42p.RotX
'	sw42step = sw42step + 1
'End Sub
'
'
' Sub sw42t_Timer()	
'  Select Case sw42Pos
'        Case 0: sw42P.TransY=0
'				 If sw42Dir = 1 then
'					sw42t.Enabled = 0
'				 else
'			     end if        
'        Case 1: sw42P.TransY=0
'        Case 2: sw42P.TransY=-6
'        Case 3: sw42P.TransY=-12
'        Case 4: sw42P.TransY=-18
'        Case 5: sw42P.TransY=-24
'        Case 6: sw42P.TransY=-30
'        Case 7: sw42P.TransY=-36
'        Case 8: sw42P.TransY=-42
'        Case 9: sw42P.TransY=-48
'        Case 10: sw42P.TransY=-52
'				 If sw42Dir = 1 then
'				 else
'					sw42t.Enabled = 0
'			     end if
'	End Select
'	If sw42Dir = 1 then
'		If sw42pos>0 then sw42pos=sw42pos-2
'	else
'		If sw42pos<10 then sw42pos=sw42pos+2
'	end if
'	sw42POFF.TransY=sw42P.TransY
'End Sub
'
'Sub sw43_timer()
'	Select Case sw43step
'		Case 0:
'		Case 1:sw43P.RotX = 92
'		Case 2:sw43P.RotX = 95
'		Case 3:DTBank.Hit 3:sw43Dir = 0:sw43t.Enabled = 1
'		Case 4:sw43P.RotX = 93
'		Case 5:sw43P.RotX = 90:me.timerEnabled = 0:sw43step = 0
'	End Select
'	sw43pOFF.RotX = sw43p.RotX
'	sw43step = sw43step + 1
'End Sub
'
'
'Sub sw43t_Timer()	
'	Select Case sw43Pos
'        Case 0: sw43P.TransY=0
'				 If sw43Dir = 1 then
'					sw43t.Enabled = 0
'				 else
'			     end if        
'        Case 1: sw43P.TransY=0
'        Case 2: sw43P.TransY=-6
'        Case 3: sw43P.TransY=-12
'        Case 4: sw43P.TransY=-18
'        Case 5: sw43P.TransY=-24
'        Case 6: sw43P.TransY=-30
'        Case 7: sw43P.TransY=-36
'        Case 8: sw43P.TransY=-42
'        Case 9: sw43P.TransY=-48
'        Case 10: sw43P.TransY=-52
'				 If sw43Dir = 1 then
'				 else
'					sw43t.Enabled = 0
'			     end if
'	End Select
'	If sw43Dir = 1 then
'		If sw43pos>0 then sw43pos=sw43pos-2
'	else
'		If sw43pos<10 then sw43pos=sw43pos+2
'	end if
'	sw43POFF.TransY=sw43P.TransY
'End Sub



Sub RDampen_Timer()
	Cor.Update
End Sub

DTRaise 41
DTRaise 42
DTRaise 43

'DT Subs
   Sub ResetDrops(Enabled)
		If Enabled Then
'			sw41Dir = 1:sw42Dir = 1:sw43Dir = 1
'			SW41.Collidable = True:SW42.Collidable = True:SW43.Collidable = True
'			SW41a.IsDropped = False:SW42a.IsDropped = False:SW43a.IsDropped = False
'			SW41a.Collidable = True:SW42a.Collidable = True:SW43a.Collidable = True
'			sw41t.Enabled = 1:sw42t.Enabled = 1:sw43t.Enabled = 1
'			dtBank.DropSol_On
			RandomSoundDropTargetReset sw42p
			DTRaise 41
			DTRaise 42
			DTRaise 43
		End If
   End Sub













 
 ' Rollovers
 Sub sw16_Hit:Controller.Switch(16) = 1:BIPL=1:End Sub
 Sub sw16_UnHit:Controller.Switch(16) = 0:BIPL=0:End Sub
 Sub sw17_Hit:Controller.Switch(17) = 1:End Sub
 Sub sw17_UnHit:Controller.Switch(17) = 0:End Sub
 Sub sw18_Hit:Controller.Switch(18) = 1:End Sub
 Sub sw18_UnHit:Controller.Switch(18) = 0:End Sub
 Sub sw23_Hit:Controller.Switch(23) = 1:End Sub
 Sub sw23_UnHit:Controller.Switch(23) = 0:End Sub
 Sub sw24_Hit:Controller.Switch(24) = 1:End Sub
 Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub
 Sub sw25_Hit:Controller.Switch(25) = 1:End Sub
 Sub sw25_UnHit:Controller.Switch(25) = 0:End Sub
 Sub sw26_Hit:Controller.Switch(26) = 1:End Sub
 Sub sw26_UnHit:Controller.Switch(26) = 0:End Sub
 Sub sw31_Hit:Controller.Switch(31) = 1:End Sub
 Sub sw31_UnHit:Controller.Switch(31) = 0:End Sub
 Sub sw32_Hit:Controller.Switch(32) = 1:End Sub
 Sub sw32_UnHit:Controller.Switch(32) = 0:End Sub
 
'Ramp entrance Gate Triggers
 Sub sw44_Hit:vpmTimer.PulseSw 44:End Sub
 Sub sw46_Hit:vpmTimer.PulseSw 46:End Sub


'TODO sounds
 ' Ramp Triggers

Sub LREnter_Hit():If ActiveBall.VelY < 0 Then PlaySoundAtLevelActiveBall ("fx_ramp_enter1"), 1 * VolumeDial:WireRampOn True:End If:End Sub			'ball is going up
Sub LREnter_UnHit():If ActiveBall.VelY > 0 Then StopSound "fx_ramp_enter1":WireRampOff:End If:End Sub		'ball is going down

' Sub sw001_Hit
'	PlaySoundAtLevelActiveBall ("fx_ramp_metal"), 1 * VolumeDial
' End Sub

dim sw45Target
Sub sw45_Hit
	switch01OFF.roty = 180:switch01.roty = 180
	Controller.Switch(45) = 1
	sw45Target = 160
	sw45.timerenabled = 1
End Sub
Sub sw45_UnHit
	switch01OFF.roty = 160:switch01.roty = 160
	Controller.Switch(45) = 0
	sw45Target = 180
	sw45.timerenabled = 1
End Sub

Sub sw45_timer
	if switch01.roty > sw45Target Then
		switch01.roty = switch01.roty - 4
	Elseif switch01.roty < sw45Target Then
		switch01.roty = switch01.roty + 4
	end If
	
	if switch01.roty = sw45Target Then
		sw45.timerenabled = 0
	end If
	
	switch01OFF.roty = switch01.roty

end Sub



sub wr_Trigger004_Hit
	PlaySoundAtLevelActiveBall ("wire_enter"), 1 * VolumeDial
	WireRampOn False
	'debug.print activeball.velx
	if activeball.velx < -10 Then	'speed limiter for loop ramp
		activeball.velx = activeball.velx * 0.6
		if activeball.velx < -11 Then	'speed limiter for loop ramp
			activeball.velx = -11
		end If
	end If
	'debug.print "---> " & activeball.velx
end Sub


dim sw47Target
Sub sw47_Hit
	switch02OFF.roty = 180
	switch02.roty = 180
	Controller.Switch(47) = 1
	sw47Target = 200
	sw47.timerenabled = 1
End Sub
Sub sw47_UnHit
	switch02OFF.roty = 200
	switch02.roty = 200
	Controller.Switch(47) = 0
	sw47Target = 180
	sw47.timerenabled = 1
End Sub


Sub sw47_timer
	if switch02.roty > sw47Target Then
		switch02.roty = switch02.roty - 4
	Elseif switch02.roty < sw47Target Then
		switch02.roty = switch02.roty + 4
	end If
	
	if switch02.roty = sw47Target Then
		sw47.timerenabled = 0
	end If
	
	switch02OFF.roty = switch02.roty

end Sub


dim sw57Target
Sub sw57_Hit
	switch03OFF.roty = 180:switch03.roty = 180
    Controller.Switch(57) = 1
	sw57Target = 160
	sw57.timerenabled = 1
End Sub

Sub sw57_UnHit
	switch03OFF.roty = 160:switch03.roty = 160
    Controller.Switch(57) = 0
	sw57Target = 180
	sw57.timerenabled = 1
End Sub

Sub sw57_timer
	if switch03.roty > sw57Target Then
		switch03.roty = switch03.roty - 4
	Elseif switch03.roty < sw57Target Then
		switch03.roty = switch03.roty + 4
	end If
	
	if switch03.roty = sw57Target Then
		sw57.timerenabled = 0
	end If
	
	switch03OFF.roty = switch03.roty

end Sub


 'Stand Up Targets

Dim Target20Step, Target21Step, Target22Step, Target28Step, Target29Step, Target30Step

Sub sw20_Hit:TargetBouncer activeball, 1:vpmTimer.PulseSw(20):sw20p.RotX = 95:sw20pOFF.RotX = sw20p.RotX:Target20Step = 0:sw20.TimerEnabled = True:PlaySoundAt SoundFX("target",DOFTargets),sw20p:End Sub			
Sub sw20_timer()
	Select Case Target20Step
		Case 1:sw20p.RotX = 93
        Case 2:sw20p.RotX = 88
        Case 3:sw20p.RotX = 91
        Case 4:sw20p.RotX = 90:sw20.TimerEnabled = False:Target20Step = 0
     End Select
	sw20pOFF.RotX = sw20p.RotX
	Target20Step = Target20Step + 1
End Sub

Sub sw21_Hit:TargetBouncer activeball, 1:vpmTimer.PulseSw(21):sw21p.RotX = 95:sw21pOFF.RotX = sw21p.RotX:Target21Step = 0:sw21.TimerEnabled = True:PlaySoundAt SoundFX("target",DOFTargets),sw21p:End Sub			
Sub sw21_timer()
	Select Case Target21Step
		Case 1:sw21p.RotX = 93
        Case 2:sw21p.RotX = 88
        Case 3:sw21p.RotX = 91
        Case 4:sw21p.RotX = 90:sw21.TimerEnabled = False:Target21Step = 0
     End Select
	sw21pOFF.RotX = sw21p.RotX
	Target21Step = Target21Step + 1
End Sub

Sub sw22_Hit:TargetBouncer activeball, 1:vpmTimer.PulseSw(22):sw22p.RotX = 95:sw22pOFF.RotX = sw22p.RotX:Target22Step = 0:sw22.TimerEnabled = True:PlaySoundAt SoundFX("target",DOFTargets),sw22p:End Sub			
Sub sw22_timer()
	Select Case Target22Step
		Case 1:sw22p.RotX = 93
        Case 2:sw22p.RotX = 88
        Case 3:sw22p.RotX = 91
        Case 4:sw22p.RotX = 90:sw22.TimerEnabled = False:Target22Step = 0
     End Select
	sw22pOFF.RotX = sw22p.RotX
	Target22Step = Target22Step + 1
End Sub

Sub sw28_Hit:TargetBouncer activeball, 1:vpmTimer.PulseSw(28):sw28p.RotX = 95:sw28pOFF.RotX = sw28p.RotX:Target22Step = 0:sw28.TimerEnabled = True:PlaySoundAt SoundFX("target",DOFTargets),sw28p:End Sub			
Sub sw28_timer()
	Select Case Target28Step
		Case 1:sw28p.RotX = 93
        Case 2:sw28p.RotX = 88
        Case 3:sw28p.RotX = 91
        Case 4:sw28p.RotX = 90:sw28.TimerEnabled = False:Target28Step = 0
     End Select
	sw28pOFF.RotX = sw28p.RotX
	Target28Step = Target28Step + 1
End Sub

Sub sw29_Hit:TargetBouncer activeball, 1:vpmTimer.PulseSw(29):sw29p.RotX = 95:sw29pOFF.RotX = sw29p.RotX:Target22Step = 0:sw29.TimerEnabled = True:PlaySoundAt SoundFX("target",DOFTargets),sw29p:End Sub			
Sub sw29_timer()
	Select Case Target29Step
		Case 1:sw29p.RotX = 93
        Case 2:sw29p.RotX = 88
        Case 3:sw29p.RotX = 91
        Case 4:sw29p.RotX = 90:sw29.TimerEnabled = False:Target29Step = 0
     End Select
	sw29pOFF.RotX = sw29p.RotX
	Target29Step = Target29Step + 1
End Sub

Sub sw30_Hit:TargetBouncer activeball, 1:vpmTimer.PulseSw(30):sw30p.RotX = 95:sw30pOFF.RotX = sw30p.RotX:Target30Step = 0:sw30.TimerEnabled = True:PlaySoundAt SoundFX("target",DOFTargets),sw30p:End Sub			
Sub sw30_timer()
	Select Case Target30Step
		Case 1:sw30p.RotX = 93
        Case 2:sw30p.RotX = 88
        Case 3:sw30p.RotX = 91
        Case 4:sw30p.RotX = 90:sw30.TimerEnabled = False:Target30Step = 0
     End Select
	sw30pOFF.RotX = sw30p.RotX
	Target30Step = Target30Step + 1
End Sub

' Sub sw20_Hit:vpmTimer.PulseSw 20:sw20.TimerEnabled = 1:sw20p.TransX = -12 : playsound"Target" : End Sub
' Sub sw20_unHit:sw20p.TransX = 0:End Sub
' Sub sw21_Hit:vpmTimer.PulseSw 21:sw21.TimerEnabled = 1:sw21p.TransX = -12 : playsound"Target" : End Sub
' Sub sw21_unHit:sw21p.TransX = 0:End Sub
' Sub sw22_Hit:vpmTimer.PulseSw 22:sw22.TimerEnabled = 1:sw22p.TransX = -12 : playsound"Target" : End Sub
' Sub sw22_unHit:sw22p.TransX = 0:End Sub
' Sub sw28_Hit:vpmTimer.PulseSw 28:sw28.TimerEnabled = 1:sw28p.TransX = -12 : playsound"Target" : End Sub
' Sub sw28_unHit:sw28p.TransX = 0:End Sub
' Sub sw29_Hit:vpmTimer.PulseSw 29:sw29.TimerEnabled = 1:sw29p.TransX = -12 : playsound"Target" : End Sub
' Sub sw29_unHit:sw29p.TransX = 0:End Sub
' Sub sw30_Hit:vpmTimer.PulseSw 30:sw30.TimerEnabled = 1:sw30p.TransX = -12 : playsound"Target" : End Sub
' Sub sw30_unHit:sw30p.TransX = 0:End Sub
 Sub sw39_Hit:vpmTimer.PulseSw 39:End Sub

'PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)

 ' Spinners
 Sub sw40_Spin:vpmTimer.pulsesw 40 :SoundSpinner(sw40): End Sub 'playsound"fx_spinner" : End Sub
 Sub sw48_Spin:vpmTimer.pulsesw 48 :SoundSpinner(sw48): End Sub 'playsound"fx_spinner" : End Sub
 Sub sw56_Spin:vpmTimer.pulsesw 56 :SoundSpinner(sw56): End Sub 'playsound"fx_spinner" : End Sub
 
 'Bumpers
Dim bump1, bump2, bump3

Sub Bumper1_Hit : vpmTimer.PulseSw(49) : RandomSoundBumperTop (Bumper1) :bump1 = 1:Me.TimerEnabled = 1:End Sub
Sub Bumper1_Timer()
    Select Case bump1
        Case 1:bump1 = 2: BumperRing1.Z = -30 : BumperRing1OFF.Z = -30
        Case 2:bump1 = 3: BumperRing1.Z = -20 : BumperRing1OFF.Z = -20
        Case 3:bump1 = 4: BumperRing1.Z = -10 : BumperRing1OFF.Z = -10
        Case 4:Me.TimerEnabled = 0: BumperRing1.Z = 0 : BumperRing1OFF.Z = 0
    End Select

    'Bumper1R.State = ABS(Bumper1R.State - 1) 'refresh light
End Sub

Sub Bumper2_Hit : vpmTimer.PulseSw(50) : RandomSoundBumperBottom (Bumper2) :bump2 = 1:Me.TimerEnabled = 1:End Sub
Sub Bumper2_Timer()
    Select Case bump2
        Case 1:bump2 = 2 : BumperRing2.Z = -30 : BumperRing1OFF.Z = -30
        Case 2:bump2 = 3 : BumperRing2.Z = -20 : BumperRing1OFF.Z = -20
        Case 3:bump2 = 4 : BumperRing2.Z = -10 : BumperRing1OFF.Z = -10
        Case 4:Me.TimerEnabled = 0 :  : BumperRing2.Z = 0 : BumperRing1OFF.Z = 0
    End Select
 '   Bumper2R.State = ABS(Bumper2R.State - 1) 'refresh light
End Sub

Sub Bumper3_Hit : vpmTimer.PulseSw(51) : RandomSoundBumperMiddle (Bumper3) :bump3 = 1:Me.TimerEnabled = 1:End Sub
Sub Bumper3_Timer()
    Select Case bump3
        Case 1:bump3 = 2: BumperRing3.Z = -30 : BumperRing1OFF.Z = -30
        Case 2:bump3 = 3: BumperRing3.Z = -20 : BumperRing1OFF.Z = -20
        Case 3:bump3 = 4: BumperRing3.Z = -10 : BumperRing1OFF.Z = -10
        Case 4:Me.TimerEnabled = 0: BumperRing3.Z = 0 : BumperRing1OFF.Z = 0
    End Select
  '  Bumper3R.State = ABS(Bumper3R.State - 1) 'refresh light
End Sub


' Tombstone 

dim RIPHitSpeed, RIPHitAngle, ripx, ripz

Sub rip_Hit

  	RIPHitSpeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)

	dim ripVol

	ripVol = RIPHitSpeed * 0.1 : if ripVol > 1 then ripVol = 1

	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_1"), ripVol * 0.25
		Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_2"), ripVol * 0.25
		Case 3 : PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_3"), ripVol * 0.25
	End Select
	'debug.print activeball.vely &" finalspeed: "& RIPHitSpeed

	if activeball.vely > 4 then 'accept hits that comes from bottom
		vpmTimer.pulsesw 37
	end if

	RIPHitAngle = atn2(cor.ballvely(activeball.id),cor.ballvelx(activeball.id))
	dim rangle,bangle,calc1, calc2, calc3, mass, angle
	mass = 0.3	'tombstone mass 0-1

	angle = 0 'some angle
	rangle = (angle - 90) * 3.1416 / 180
	bangle = RIPHitAngle

	calc1 = cos(bangle - rangle) * (1 - mass) / (1 + mass)
	calc2 = sin(bangle - rangle) * cos(rangle + 4*Atn(1)/2)
	calc3 = sin(bangle - rangle) * sin(rangle + 4*Atn(1)/2)

	'debug.print "RIP hit speed: " & RIPHitSpeed

	ripz = RIPHitSpeed * calc1 * cos(rangle) + calc2
	ripx = 0.5 * RIPHitSpeed * calc1 * sin(rangle) + calc3
	tombstone.rotx = ripx
	tombstone.rotz = ripz
	Me.TimerEnabled = 1
End Sub

dim ripSpringExp, ripdirection
ripdirection = 0
ripSpringExp = 1
Sub rip_Timer()
	if ripx < 0 then
		ripx = abs(ripx) * 0.9 - 0.01
	Else
		ripx = -abs(ripx) * 0.9 + 0.01
	end if

	if ripz < 0 then
		ripz = abs(ripz) * 0.9 - 0.01
	Else
		ripz = -abs(ripz) * 0.9 + 0.01
	end if

	'debug.print ripx &" - "& ripz
	tombstone.rotx = ripx : tombstone.rotz = ripz
	tombstoneOFF.rotx = tombstone.rotx : tombstoneOFF.rotz = tombstone.rotz

	if abs(ripx) < 0.1 And abs(ripz) < 0.1 then	Me.TimerEnabled = 0
End Sub


Sub RipUpdate(newpos, speed, lastpos)
	tombstone.z = newpos-10-174
	tombstoneOFF.z = tombstone.z
	'debug.print "ripupdate newpos: " & newpos &" and Z: "& tombstone.z
	If newpos >= 21 Then
		rip.IsDropped = 0
	Else
		rip.IsDropped = 1
	End If
End Sub

'TODO
'Generic Sounds
'Sub Trigger1_Hit:PlaySound "fx_ballrampdrop":StopSound "Wire Ramp":End Sub
'Sub Trigger2_Hit:PlaySound "fx_ballrampdrop":StopSound "Wire Ramp":End Sub

Sub Trigger1_Hit
 WireRampOff
 PlaySoundAtLevelActiveBall ("WireRamp_Stop"), 1* VolumeDial
End Sub

Sub Trigger2_Hit
 WireRampOff
 PlaySoundAtLevelActiveBall ("WireRamp_Stop"), 1* VolumeDial
End Sub


Sub Trigger3_Hit:WireRampOff: activeball.vely = activeball.vely / 10 :End Sub 'ball speed reduced so it wont bounce back that much
Sub Trigger4_Hit:WireRampOn False:BallInKicker2 = 0:End Sub

'******************************************
'			Plastic Wobble
'******************************************

Dim WobbleValue

tWobbleWire.interval = 15 ' Controls the speed of the wobble



Sub tWobbleWire_timer()
	If not tWobbleWire.Enabled Then
		WobbleValue = 5
		tWobbleWire.Enabled = True
	end if

	wireRamps.z=WobbleValue:wireRampsOFF.z=WobbleValue

	if WobbleValue < 0 then
		WobbleValue = abs(WobbleValue) * 0.9 - 0.1
	Else
		WobbleValue = -abs(WobbleValue) * 0.9 + 0.1
	end if

'	debug.print WobbleValue

	if abs(WobbleValue) < 0.1 Then
		wireRamps.z=0:wireRampsOFF.z=0
		tWobbleWire.Enabled = false
	end If
end Sub

Sub tWobbleRamp_timer()
	If not tWobbleRamp.Enabled Then
		WobbleValue = -4
		tWobbleRamp.Enabled = True
	end if

	metalRamps.y=WobbleValue:metalRampsOFF.y=WobbleValue
	wireRamps.z=WobbleValue:wireRampsOFF.z=WobbleValue

	if WobbleValue < 0 then
		WobbleValue = abs(WobbleValue) * 0.9 - 0.1
	Else
		WobbleValue = -abs(WobbleValue) * 0.9 + 0.1
	end if

'	debug.print WobbleValue

	if abs(WobbleValue) < 0.1 Then
		metalRamps.y=0:metalRampsOFF.y=0
		tWobbleRamp.Enabled = false
	end If
end Sub

'****************************************************************
'							Timer Code
'****************************************************************

Sub FrameTimer_Timer()
	'LampTimer			'moved this to dedicated timer
	Lampztimer
	BallFXUpdate
	FlipperTimer
'	Anyothertimeryouwant!
'	cor.Update
	sw41pOFF.RotX = sw41p.RotX : sw41POFF.TransZ=sw41P.TransZ
	sw42pOFF.RotX = sw42p.RotX : sw42POFF.TransZ=sw42P.TransZ
	sw43pOFF.RotX = sw43p.RotX : sw43POFF.TransZ=sw43P.TransZ
End Sub


' Hack to return Narnia ball back in play
Sub Narnia_Timer()
    Dim b
    'BOT = GetBalls
	For b = 0 to UBound(gBOT)
		if gBOT(b).z < -200 Then
			'msgbox "Ball " &b& " in Narnia X: " & gBOT(b).x &" Y: "&gBOT(b).y & " Z: "&gBOT(b).z
			debug.print "Move narnia ball ("& gBOT(b).x &" Y: "&gBOT(b).y & " Z: "&gBOT(b).z&") to center scoop"
			gBOT(b).x = 346
			gBOT(b).y = 742
			gBOT(b).z = -58
		end if
	next
end sub

'****************************************************************
'					Begin nFozzy lamp handling
'****************************************************************

Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
Dim ModLampz : Set ModLampz = New DynamicLamps
InitLampsNF              ' Setup lamp assignments
LampTimer.Interval = 20
LampTimer.Enabled = 1


Sub LampTimer_timer()
	dim x, chglamp
	chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		next
	End If
	Lampz.Update1	'update (fading logic only)
	'ModLampz.Update1
'	Lampz.Update	'update (fading logic only)
'	ModLampz.Update
'	If F19.IntensityScale > 0 then
'		fmfl27.visible = False
'		l27.visible = False
'	Else
'		fmfl27.visible = True
'		l27.visible = True
'	end if
End Sub

dim FrameTime, InitFrameTime : InitFrameTime = 0
Sub Lampztimer()
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime. Unused atm?
	Lampz.Update 'updates on frametime (Object updates only)
	'ModLampz.Update
End Sub

Function FlashLevelToIndex(Input, MaxSize)
	FlashLevelToIndex = cInt(MaxSize * Input)
End Function

'Material swap arrays.
Dim TextureArray1: TextureArray1 = Array("Plastic with an image trans", "Plastic with an image trans","Plastic with an image trans","Plastic with an image")
Dim DLintensity

'****************************************************************
'					Prim *Image* Swaps
'****************************************************************
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

'****************************************************************
'					Prim *Material* Swaps
'****************************************************************
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

Sub DisableLightingBumpertops(pri, DLintensity, aGIValue, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically

	if giprevalvl = 0 then
		if aLvl > 0.3 then
			if aGIValue <> 0 then
				pri.blenddisablelighting = aLvl * DLintensity + aGIValue
			Else
				pri.blenddisablelighting = aLvl * DLintensity*0.3 + aGIValue
			end if
		Else
			pri.blenddisablelighting = aLvl * DLintensity + 1 + aGIValue*2
		end if

		setBumperTopImageGIOFF pri, alvl
	else
		if aLvl > 0.3 then
			if aGIValue <> 0 then
				pri.blenddisablelighting = aLvl * DLintensity + 0.7*aGIValue
			Else
				pri.blenddisablelighting = aLvl * DLintensity*0.3 + 0.1*aGIValue
			end if
		Else
			pri.blenddisablelighting = aLvl * DLintensity + 0.7*aGIValue
		end if

		setBumperTopImageGION pri

	end if
End Sub

sub setBumperTopImageGIOFF(pri, alvl)
		if aLvl > 0.3 then
			if ObjLevel(1) = 0 And ObjLevel(1) = 0 then
				pri.image = "g03_ON"
			Else
				if ObjLevel(1) > ObjLevel(2) Then
					pri.image = "g03_LF"
				Else
					pri.image = "g03_RF"
				end if
			end If
		else 
			if ObjLevel(1) = 0 And ObjLevel(1) = 0 then
				pri.image = "g03_OFF"
			Else
				if ObjLevel(1) > ObjLevel(2) Then
					pri.image = "g03_LF_gioff"
				Else
					pri.image = "g03_RF_gioff"
				end if
			end If
		end If
		'debug.print "level: " & aLvl & "  DL: " & pri.blenddisablelighting
end sub

sub setBumperTopImageGION(pri)
		if pri.name = "bumpertop1" Then
			if ObjLevel(1) = 0 And ObjLevel(1) = 0 then
				pri.image = "g03_ON"
			Else
				if ObjLevel(1) > ObjLevel(2) Then
					pri.image = "g03_LF"
				Else
					pri.image = "g03_RF"
				end if
			end If
		end If
		'debug.print "level: " & aLvl & "  DL: " & pri.blenddisablelighting
end sub

Sub DisableLightingFlash(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	pri.blenddisablelighting = aLvl * DLintensity * 0.4
End Sub

dim apron_group : apron_group = Array("00_apron plastic", "00_apron plastic", "00_apron plastic_ON", "00_apron plastic_ON")

Sub InitLampsNF()
	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating
	ModLampz.Filter = "LampFilter"

	'Adjust fading speeds (1 / full MS fading time)
	dim x
	for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/3 : Lampz.FadeSpeedDown(x) = 1/14 : next
	for x = 0 to 28 : ModLampz.FadeSpeedUp(x) = 1/3 : ModLampz.FadeSpeedDown(x) = 1/40 : Next

	'for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/80 : Lampz.FadeSpeedDown(x) = 1/100 : next
	Lampz.FadeSpeedUp(111) = 1/3 'GI
	Lampz.FadeSpeedDown(111) = 1/30

	'Lamp Assignments
	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays

	Lampz.MassAssign(1)= l1
	Lampz.Callback(1) = "DisableLighting p1, 5,"
	Lampz.Callback(1) = "DisableLighting p1bulb, 50,"
	Lampz.MassAssign(2)= l2
	Lampz.Callback(2) = "DisableLighting p2, 50,"
	Lampz.Callback(2) = "DisableLighting p2bulb, 70,"
	Lampz.MassAssign(3)= l3
	Lampz.Callback(3) = "DisableLighting p3, 50,"
	Lampz.Callback(3) = "DisableLighting p3bulb, 70,"
	Lampz.MassAssign(4)= l4
	Lampz.Callback(4) = "DisableLighting p4, 50,"
	Lampz.Callback(4) = "DisableLighting p4bulb, 70,"
	Lampz.MassAssign(5)= l5
	Lampz.MassAssign(5)= f5TOP
	Lampz.Callback(5) = "DisableLighting p5, 50,"
	Lampz.Callback(5) = "DisableLighting p5bulb, 100,"
	Lampz.MassAssign(6)= l6
	Lampz.Callback(6) = "DisableLighting p6, 50,"
	Lampz.Callback(6) = "DisableLighting p6bulb, 70,"
	Lampz.MassAssign(7)= l7
	Lampz.Callback(7) = "DisableLighting p7, 50,"
	Lampz.Callback(7) = "DisableLighting p7bulb, 100,"
	Lampz.MassAssign(8)= l8
	Lampz.Callback(8) = "DisableLighting p8, 50,"
	Lampz.Callback(8) = "DisableLighting p8bulb, 70,"
	Lampz.MassAssign(9)= l9
	Lampz.Callback(9) = "DisableLighting p9, 50,"
	Lampz.Callback(9) = "DisableLighting p9bulb, 70,"
	Lampz.MassAssign(10)= l10
	Lampz.Callback(10) = "DisableLighting p10, 5,"
	Lampz.Callback(10) = "DisableLighting p10bulb, 50,"
	Lampz.MassAssign(11)= l11
	Lampz.Callback(11) = "DisableLighting p11, 50,"
	Lampz.Callback(11) = "DisableLighting p11bulb, 70,"
	Lampz.MassAssign(12)= l12
	Lampz.Callback(12) = "DisableLighting p12, 50,"
	Lampz.Callback(12) = "DisableLighting p12bulb, 70,"
	'NFadeObjm 13)= P13)= "bulbcover1_redOn")= "bulbcover1_red"
	Lampz.MassAssign(13)= F13
	Lampz.MassAssign(13)= f13Light
	Lampz.Callback(13) = "DisableLighting g02_bulb2, 5,"
	Lampz.Callback(13) = "DisableLighting g02_bulbOFF2, 10,"
	'Lampz.MassAssign(14)= l14 'Buy in Button
	Lampz.MassAssign(15)= l15 'Plunger LED
	Lampz.MassAssign(15)= F15
	Lampz.MassAssign(15)= f_bulbshooter
	Lampz.Callback(15) = "DisableLighting g02_bulbshooter, 10,"
	Lampz.Callback(15) = "DisableLighting g02_bulbshooteroff, 10,"

     'VR Room Plunger model eyes
    Lampz.Callback(15) = "DisableLighting PlungerEye1, 10,"
    Lampz.Callback(15) = "DisableLighting PlungerEye2, 10,"

	'Lampz.MassAssign(16)= l16a 'Start button
	Lampz.MassAssign(17)= l17
	Lampz.Callback(17) = "DisableLighting p17, 180,"
	Lampz.Callback(17) = "DisableLighting p17bulb, 200,"
	Lampz.MassAssign(17)= l17a
	Lampz.Callback(17) = "DisableLighting p17a, 10,"
	Lampz.Callback(17) = "DisableLighting p17abulb, 30,"
	Lampz.MassAssign(18)= l18
	Lampz.Callback(18) = "DisableLighting p18, 50,"
	Lampz.Callback(18) = "DisableLighting p18bulb, 70,"
	Lampz.MassAssign(19)= l19
	Lampz.Callback(19) = "DisableLighting p19, 50,"
	Lampz.Callback(19) = "DisableLighting p19bulb, 700,"
	Lampz.MassAssign(20)= l20
	Lampz.Callback(20) = "DisableLighting p20, 10,"
	Lampz.Callback(20) = "DisableLighting p20bulb, 30,"
	Lampz.MassAssign(21)= l21
	Lampz.Callback(21) = "DisableLighting p21, 10,"
	Lampz.Callback(21) = "DisableLighting p21bulb, 30,"
	Lampz.MassAssign(22)= l22
	Lampz.Callback(22) = "DisableLighting p22, 10,"
	Lampz.Callback(22) = "DisableLighting p22bulb, 30,"
	Lampz.MassAssign(23)= l23
	Lampz.Callback(23) = "DisableLighting p23, 50,"
	Lampz.Callback(23) = "DisableLighting p23bulb, 700,"
	Lampz.MassAssign(24)= l24
	Lampz.Callback(24) = "DisableLighting p24, 50,"
	Lampz.Callback(24) = "DisableLighting p24bulb, 700,"
	Lampz.MassAssign(25)= l25
	Lampz.Callback(25) = "DisableLighting p25, 50,"
	Lampz.Callback(25) = "DisableLighting p25bulb, 70,"
	Lampz.MassAssign(26)= l26
	Lampz.Callback(26) = "DisableLighting p26, 180,"
	Lampz.Callback(26) = "DisableLighting p26bulb, 200,"
	Lampz.MassAssign(26)= l26a
	Lampz.MassAssign(26)= f26aTOP 'l26aTOP
	Lampz.Callback(26) = "DisableLighting p26a, 180,"
	Lampz.Callback(26) = "DisableLighting p26abulb, 200,"
	Lampz.MassAssign(27)= l27
	Lampz.Callback(27) = "DisableLighting p27, 60,"
	Lampz.Callback(27) = "DisableLighting p27bulb, 130,"
	Lampz.MassAssign(28)= l28
	Lampz.Callback(28) = "DisableLighting p28, 10,"
	Lampz.Callback(28) = "DisableLighting p28bulb, 30,"
	Lampz.MassAssign(29)= l29
	Lampz.Callback(29) = "DisableLighting p29, 10,"
	Lampz.Callback(29) = "DisableLighting p29bulb, 30,"
	Lampz.MassAssign(30)= l30
	Lampz.Callback(30) = "DisableLighting p30, 10,"
	Lampz.Callback(30) = "DisableLighting p30bulb, 30,"
	Lampz.MassAssign(31)= l31
	Lampz.Callback(31) = "DisableLighting p31, 50,"
	Lampz.Callback(31) = "DisableLighting p31bulb, 70,"
	Lampz.MassAssign(32)= l32
	Lampz.Callback(32) = "DisableLighting p32, 60,"
	Lampz.Callback(32) = "DisableLighting p32bulb, 80,"
	Lampz.MassAssign(33)= l33
	Lampz.Callback(33) = "DisableLighting p33, 60,"
	Lampz.Callback(33) = "DisableLighting p33bulb, 130,"
	Lampz.MassAssign(34)= l34
	Lampz.Callback(34) = "DisableLighting p34, 50,"
	Lampz.Callback(34) = "DisableLighting p34bulb, 70,"
	Lampz.MassAssign(35)= l35
	Lampz.Callback(35) = "DisableLighting p35, 60,"
	Lampz.Callback(35) = "DisableLighting p35bulb, 80,"
	Lampz.MassAssign(36)= l36
	Lampz.Callback(36) = "DisableLighting p36, 60,"
	Lampz.Callback(36) = "DisableLighting p36bulb, 130,"
	Lampz.MassAssign(37)= l37
	Lampz.Callback(37) = "DisableLighting p37, 80,"
	Lampz.Callback(37) = "DisableLighting p37bulb, 90,"
	Lampz.MassAssign(38)= l38
	Lampz.Callback(38) = "DisableLighting p38, 40,"
	Lampz.Callback(38) = "DisableLighting p38bulb, 100,"
	Lampz.MassAssign(39)= l39
	Lampz.Callback(39) = "DisableLighting p39, 10,"
	Lampz.Callback(39) = "DisableLighting p39bulb, 30,"
	Lampz.MassAssign(40)= l40
	Lampz.Callback(40) = "DisableLighting p40, 60,"
	Lampz.Callback(40) = "DisableLighting p40bulb, 90,"
	Lampz.MassAssign(41)= l41
	Lampz.MassAssign(41)= f41TOP
	Lampz.Callback(41) = "DisableLighting p41, 10,"
	Lampz.Callback(41) = "DisableLighting p41bulb, 30,"
	Lampz.MassAssign(42)= l42
	Lampz.MassAssign(42)= f42TOP
	Lampz.Callback(42) = "DisableLighting p42, 10,"
	Lampz.Callback(42) = "DisableLighting p42bulb, 30,"
	Lampz.MassAssign(43)= l43
	Lampz.MassAssign(43)= f43TOP
	Lampz.Callback(43) = "DisableLighting p43, 10,"
	Lampz.Callback(43) = "DisableLighting p43bulb, 30,"
	Lampz.MassAssign(44)= l44
	Lampz.Callback(44) = "DisableLighting p44, 10,"
	Lampz.Callback(44) = "DisableLighting p44bulb, 30,"
	Lampz.MassAssign(45)= l45
	Lampz.Callback(45) = "DisableLighting p45, 10,"
	Lampz.Callback(45) = "DisableLighting p45bulb, 30,"
	Lampz.MassAssign(46)= l46
	Lampz.Callback(46) = "DisableLighting p46, 10,"
	Lampz.Callback(46) = "DisableLighting p46bulb, 30,"
	Lampz.MassAssign(47)= l47
	Lampz.Callback(47) = "DisableLighting p47, 10,"
	Lampz.Callback(47) = "DisableLighting p47bulb, 30,"
	Lampz.MassAssign(48)= l48
	Lampz.Callback(48) = "DisableLighting p48, 10,"
	Lampz.Callback(48) = "DisableLighting p48bulb, 30,"
	Lampz.MassAssign(49)= l49
	Lampz.Callback(49) = "DisableLighting p49, 40,"
	Lampz.Callback(49) = "DisableLighting p49bulb, 60,"
	Lampz.MassAssign(50)= l50
	Lampz.Callback(50) = "DisableLighting p50, 80,"
	Lampz.Callback(50) = "DisableLighting p50bulb, 80,"
	Lampz.MassAssign(51)= l51
	Lampz.Callback(51) = "DisableLighting p51, 40,"
	Lampz.Callback(51) = "DisableLighting p51bulb, 60,"
	Lampz.MassAssign(52)= l52
	Lampz.Callback(52) = "DisableLighting p52, 80,"
	Lampz.Callback(52) = "DisableLighting p52bulb, 80,"
	Lampz.MassAssign(53)= l53
	Lampz.MassAssign(53)= f53TOP
	Lampz.Callback(53) = "DisableLighting p53, 60,"
	Lampz.Callback(53) = "DisableLighting p53bulb, 90,"
	Lampz.MassAssign(54)= l54
	Lampz.Callback(54) = "DisableLighting p54, 10,"
	Lampz.Callback(54) = "DisableLighting p54bulb, 30,"
	Lampz.MassAssign(55)= l55
	Lampz.Callback(55) = "DisableLighting p55, 40,"
	Lampz.Callback(55) = "DisableLighting p55bulb, 100,"
	Lampz.MassAssign(56)= l56
	Lampz.Callback(56) = "DisableLighting p56, 80,"
	Lampz.Callback(56) = "DisableLighting p56bulb, 90,"

	Lampz.MassAssign(57)= l57 'Bumpers
	Lampz.MassAssign(57)= l57a
	Lampz.Callback(57) = "DisableLightingBumpertops bumpertop1, 2, giprevalvl,"

	Lampz.MassAssign(58)= l58 'Bumpers
	Lampz.MassAssign(58)= l58a
	Lampz.Callback(58) = "DisableLightingBumpertops bumpertop2, 2, giprevalvl,"

	Lampz.MassAssign(59)= l59 'Bumpers
	Lampz.MassAssign(59)= l59a
	Lampz.Callback(59) = "DisableLightingBumpertops bumpertop3, 2, giprevalvl,"

	Lampz.MassAssign(60)= l60
	Lampz.Callback(60) = "DisableLighting p60, 80,"
	Lampz.Callback(60) = "DisableLighting p60bulb, 80,"
	Lampz.MassAssign(61)= l61
	Lampz.Callback(61) = "DisableLighting p61, 50,"
	Lampz.Callback(61) = "DisableLighting p61bulb, 700,"

	'NFadeObjm 62)= P62)= "bulbcover1_redOn")= "bulbcover1_red"
	Lampz.MassAssign(62)= F62
	Lampz.MassAssign(62)= F62Light
	Lampz.Callback(62) = "DisableLighting g02_bulb1, 5,"
	Lampz.Callback(62) = "DisableLighting g02_bulbOFF1, 5,"


	'NFadeObjm 63)= P63)= "bulbcover1_redOn")= "bulbcover1_red"
	Lampz.MassAssign(63)= F63
	Lampz.MassAssign(63)= F63Light
	Lampz.Callback(63) = "DisableLighting g02_bulb3, 5,"
	Lampz.Callback(63) = "DisableLighting g02_bulbOFF3, 5,"


	Lampz.MassAssign(64)= l64
	Lampz.Callback(64) = "DisableLighting p64, 50,"
	Lampz.Callback(64) = "DisableLighting p64bulb, 70,"








'Lampz.MassAssign(67)= l67
'Lampz.Callback(67) = "DisableLighting p67, 50,"
'Lampz.Callback(67) = "DisableLighting p67bulb, 70,"










	'Sol 10 GI relay and assignments
	Lampz.obj(111) = ColtoArray(GI)	

	Lampz.Callback(111) = "GIUpdates"
	Lampz.state(111) = 1		'Turn on GI to Start


	'Turn off all lamps on startup
	lampz.Init	'This just turns state of any lamps to 1
	ModLampz.Init

	'Immediate update to turn on GI, turn off lamps
	lampz.update
	ModLampz.Update

End Sub


'***************************************
'System 11 GI On/Off
'***************************************
Sub GIOn  : SetGI False: End Sub 'These are just debug commands now
Sub GIOff : SetGI True : End Sub


Dim GIoffMult : GIoffMult = 2 'adjust how bright the inserts get when the GI is off
Dim GIoffMultFlashers : GIoffMultFlashers = 2	'adjust how bright the Flashers get when the GI is off


'Dim TextureArray1: TextureArray1 = Array("Plastic with an image trans", "Plastic with an image trans","Plastic with an image trans","Plastic with an image")
dim giprevalvl, kk, ballbrightness
const ballbrightMax = 105
const ballbrightMin = 15

Dim GIX,swapflag
Sub GIupdates(ByVal aLvl)	'GI update odds and ends go here
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically

	if ObjLevel(1) <= 0 And ObjLevel(2) <= 0 Then
		'commenting this out for now, as it has issues with flashers
		if aLvl = 0 then										'GI OFF, let's hide ON prims
			OnPrimsVisible False
'			for each GIX in GI:GIX.state = 0:Next
			if ballbrightness <> -1 then ballbrightness = ballbrightMin
		Elseif aLvl = 1 then									'GI ON, let's hide OFF prims
			OffPrimsVisible False
'			for each GIX in GI:GIX.state = 1:Next
			if ballbrightness <> -1 then ballbrightness = ballbrightMax
		Else
			if giprevalvl = 0 Then								'GI has just changed from OFF to fading, let's show ON
				OnPrimsVisible True
				ballbrightness = ballbrightMin + 1
			elseif giprevalvl = 1 Then							'GI has just changed from ON to fading, let's show OFF
				OffPrimsVisible true
				ballbrightness = ballbrightMax - 1
			Else
				'no change
			end if
		end if

		UpdateMaterial "GI_ON_CAB",		0,0,0,0,0,0,aLvl^1.5,RGB(255,255,255),0,0,False,True,0,0,0,0
		UpdateMaterial "GI_ON_Plastic",	0,0,0,0,0,0,aLvl^1,RGB(255,255,255),0,0,False,True,0,0,0,0
		UpdateMaterial "GI_ON_Metals",	0,0,0,0,0,0,aLvl^1,RGB(255,255,255),0,0,False,True,0,0,0,0
		UpdateMaterial "GI_ON_Bulbs",	0,0,0,0,0,0,aLvl^1,RGB(255,255,255),0,0,False,True,0,0,0,0
		swapflag=0
	Elseif ObjLevel(1) > 0 Or ObjLevel(2) > 0 then 
		if aLvl = 0 Or aLvl = 1 then
			'nothing, flashers just fading and no real change to gi
		end if

		'if giprevalvl < aLvl And swapflag=0 then 'gi went ON while some flasher was fading
		if giprevalvl < 1 And swapflag=0 then 'gi went ON while some flasher was fading
'			debug.print gametime & " ##prims to on image, while flasher fading"
			OnPrimSwap "ON"
			if ImageSwapsForFlashers = 1 then
				if ObjLevel(1) > ObjLevel(2) then
					OffPrimSwap 1,1,False
				else
					OffPrimSwap 2,1,False
				end if
			end if
			swapflag = 1
'		Else
'			debug.print gametime & " prevlevel 1 or s " & swapflag
		end if
		

		'if giprevalvl > aLvl And swapflag=0 Then 'gi went OFF while some flasher was fading
		'if giprevalvl = 1 And swapflag=0 Then 'gi went OFF while some flasher was fading
		if giprevalvl = 1 Then 'gi went OFF while some flasher was fading
'			debug.print gametime & " ##prims to OFF images, while flasher fading"
			OnPrimSwap "OFF"
			if ImageSwapsForFlashers = 1 then
				if ObjLevel(1) > ObjLevel(2) then
					OffPrimSwap 1,0,False
				else
					OffPrimSwap 2,0,False
				end if
			end if
			swapflag=0																					'Commenting this out, as it can look too binary
		end if


'		if aLvl = 0 Or aLvl = 1 then
'			'nothing, flashers just fading and no real change to gi
'		end if
'		if giprevalvl < 1 And swapflag=0 then 'gi went ON while some flasher was fading
'			'debug.print "##on prims to on image"
'			OnPrimSwap "ON"
'			swapflag = 1
'		end if
'		if giprevalvl = 1 Then 'gi went OFF while some flasher was fading
'			'debug.print "##on prims to OFF images"
'			OnPrimSwap "OFF"
'			swapflag=0																					'Commenting this out, as it can look too binary
'		end if

		
	end If

'Sideblades: ^5 (fastest to go off)
'Plastics: ^3 (medium speed)
'Bulbs: ^0.5 (not sure how this would look. Would be the slowest)
'metals:^2
'
'GI_ON_Bulbs
'GI_ON_CAB
'GI_ON_Metals
'GI_ON_Plastic

	'debug.print aLvl
	'debug.print aLvl^5

	PLAYFIELD_GI1.opacity = PFGIOFFOpacity * alvl^2 'TODO 60

	'debug.print "*** --> " & FlashLevelToIndex(aLvl, 3)

'    Select case FlashLevelToIndex(aLvl, 3)
'		Case 0:plastics.Image = "plastics_000"
'		Case 1:plastics.Image = "plastics_033"
'		Case 2:plastics.Image = "plastics_066"
'        Case 3:plastics.Image = "plastics_100"
'    End Select

	UpdateMaterial "PlasticFade",	0,0,0,0,0,0,aLvl^3,RGB(255,255,255),0,0,False,True,0,0,0,0

	UpdateMaterial "GI_ON_50",	0,0,0,0,0,0,0.5 * aLvl,RGB(255,255,255),0,0,False,True,0,0,0,0
	UpdateMaterial "GI_OFF_50",	0,0,0,0,0,0,0.5 * (1-aLvl),RGB(255,255,255),0,0,False,True,0,0,0,0

	
	'0.7 - 0.05
	FlasherOffBrightness = 0.7*aLvl
	Flasherbase2.blenddisablelighting = FlasherOffBrightness
	Flasherbase1.blenddisablelighting = FlasherOffBrightness

	lamp_bulbs.blenddisablelighting = 10 * aLvl : lamp_bulbsOFF.blenddisablelighting = 10 * aLvl
	bulbs.blenddisablelighting 		= 2.5 * aLvl + 0.5
	bulbsOFF.blenddisablelighting 	= 0.5 * aLvl + 0.5
	pupkicker.blenddisablelighting = aLvl

	clearPlastic.blenddisablelighting = 1 * aLvl

	'ball
	if ballbrightness <> ballbrightMax Or ballbrightness <> ballbrightMin Or ballbrightness <> -1 then ballbrightness = INT(alvl * (ballbrightMax - ballbrightMin) + ballbrightMin)
	

	giprevalvl = alvl

End Sub


'Lamp Filter
Function LampFilter(aLvl)

	LampFilter = aLvl^1.6	'exponential curve?
End Function



'Helper functions

Function ColtoArray(aDict)	'converts a collection to an indexed array. Indexes will come out random probably.
	redim a(999)
	dim count : count = 0
	dim x  : for each x in aDict : set a(Count) = x : count = count + 1 : Next
	redim preserve a(count-1) : ColtoArray = a
End Function

'Set GICallback2 = GetRef("SetGI")

'Sub SetGI(aNr, aValue)
'	msgbox "GI nro: " & aNr & " and step: " & aValue
'	ModLampz.SetGI aNr, aValue 'Redundant. Could reassign GI indexes here
'End Sub


Dim GiOnVar
Sub SetGI(aOn)
	Select Case aOn
		Case True  'GI off
			 
'			debug.print gametime & " GI OFF" & " R4 level:" & ObjLevel(2) & " R2 level:" & ObjLevel(1)
			'fx_relay_off
'			PlaySoundAtLevelStatic ("fx_relay_off"), RelaySoundLevel, p30off
			Sound_GI_Relay 1, gi_relay_pos
			SetLamp 111, 0	'Inverted, Solenoid cuts GI circuit on this era of game
			l57.intensity=66:l58.intensity=66:l59.intensity=66
			l57.falloff=250:l58.falloff=250:l59.falloff=250
			GiOnVar=False
		Case False 
'			debug.print gametime & " GI ON" & " R4 level:" & ObjLevel(2) & " R2 level:" & ObjLevel(1)
			'fx_relay_on
'			PlaySoundAtLevelStatic ("fx_relay_on"), RelaySoundLevel, p30off
			Sound_GI_Relay 0, gi_relay_pos
			SetLamp 111, 5
			l57.intensity=11:l58.intensity=11:l59.intensity=11
			l57.falloff=200:l58.falloff=200:l59.falloff=200
			GiOnVar=True
	End Select
End Sub

Sub SetLamp(aNr, aOn)

'	if aNr = 111 then
'		debug.print gametime & " GI: " & aOn
'	end if

	Lampz.state(aNr) = abs(aOn)
End Sub

Sub SetModLamp(aNr, aInput)
	ModLampz.state(aNr) = abs(aInput)/255
End Sub

'****************************************************************
'					End nFozzy lamp handling
'****


' #####################################
' ###### Flupper Domes #####
' #####################################

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherOffBrightness, FlasherBloomIntensity

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 0.2		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 0.5		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherBloomIntensity = 0.1
FlasherOffBrightness = 0.7		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************


'Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
InitFlasher 1, "white" : InitFlasher 2, "white" ': InitFlasher 3, "white"
'InitFlasher 4, "green" : InitFlasher 5, "red" : InitFlasher 6, "white"
'InitFlasher 7, "green" : InitFlasher 8, "red"
'InitFlasher 9, "green" : InitFlasher 10, "red" : InitFlasher 11, "white" 
' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
RotateFlasher 1,-10 ': RotateFlasher 5,0 : RotateFlasher 6,90
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

'Sub FlashFlasher(nr)
'	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objlit(nr).visible = 1 : End If
'	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
'	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
'	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
'	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
'	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
'	ObjLevel(nr) = ObjLevel(nr) * 0.8 - 0.01
'	If ObjLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objlit(nr).visible = 0 : End If
'End Sub


'setlamp 111, 5 : setlamp 111, 0:sol4r true

sub OnPrimsTransparency(aValue)
	UpdateMaterial "GI_ON_CAB"		,0,0,0,0,0,0,aValue,RGB(255,255,255),0,0,False,True,0,0,0,0
	UpdateMaterial "GI_ON_Plastic"	,0,0,0,0,0,0,aValue,RGB(255,255,255),0,0,False,True,0,0,0,0
	UpdateMaterial "GI_ON_Metals"	,0,0,0,0,0,0,aValue,RGB(255,255,255),0,0,False,True,0,0,0,0
	UpdateMaterial "GI_ON_Bulbs"	,0,0,0,0,0,0,aValue,RGB(255,255,255),0,0,False,True,0,0,0,0
	UpdateMaterial "GI_ON_50",	0,0,0,0,0,0,0.5 * aValue,RGB(255,255,255),0,0,False,True,0,0,0,0
	UpdateMaterial "GI_OFF_50",	0,0,0,0,0,0,0.5 * (1 - aValue),RGB(255,255,255),0,0,False,True,0,0,0,0
end sub

dim ii

sub OffPrimSwap(aFlashNro, aGIState, aReturn)'1 = LF, 2 = RF
	if aReturn Then
		For each ii in cPlastics_off:ii.image="plastics_OFF":Next
		For each ii in cG01_off:ii.image="g01_OFF":Next
		For each ii in cG02_off:ii.image="g02_OFF":Next		
		For each ii in cG03_off:ii.image="g03_OFF":Next		
		For each ii in cCab_off:ii.image="cab_OFF":Next
		For each ii in cRamps_off:ii.image="ramps_OFF":Next
	Else
		Select Case aFlashNro
			Case 2:
				if aGIState = 1 then
					For each ii in cPlastics_off:ii.image="plastics_RF":Next
					For each ii in cG01_off:ii.image="g01_RF":Next
					For each ii in cG02_off:ii.image="g02_RF":Next
					For each ii in cG03_off:ii.image="g03_RF":Next
					For each ii in cCab_off:ii.image="cab_RF":Next
					For each ii in cRamps_off:ii.image="ramps_RF":Next
				Elseif aGIState = 0 then
					For each ii in cPlastics_off:ii.image="plastics_RF_gioff":Next
					For each ii in cG01_off:ii.image="g01_RF_gioff":Next
					For each ii in cG02_off:ii.image="g02_RF_gioff":Next
					For each ii in cG03_off:ii.image="g03_RF_gioff":Next
					For each ii in cCab_off:ii.image="cab_RF_gioff":Next
					For each ii in cRamps_off:ii.image="ramps_RF_gioff":Next
				end if
			Case 1:
				if aGIState = 1 then
					For each ii in cPlastics_off:ii.image="plastics_LF":Next
					For each ii in cG01_off:ii.image="g01_LF":Next
					For each ii in cG02_off:ii.image="g02_LF":Next
					For each ii in cG03_off:ii.image="g03_LF":Next
					For each ii in cCab_off:ii.image="cab_LF":Next
					For each ii in cRamps_off:ii.image="ramps_LF":Next
				Elseif aGIState = 0 then
					For each ii in cPlastics_off:ii.image="plastics_LF_gioff":Next
					For each ii in cG01_off:ii.image="g01_LF_gioff":Next
					For each ii in cG02_off:ii.image="g02_LF_gioff":Next
					For each ii in cG03_off:ii.image="g03_LF_gioff":Next
					For each ii in cCab_off:ii.image="cab_LF_gioff":Next
					For each ii in cRamps_off:ii.image="ramps_LF_gioff":Next
				end If
		End Select
	end If
end sub

sub OnPrimSwap(aValue)
	Select Case aValue
		Case "ON":
			For each ii in cPlastics:ii.image="plastics_ON":Next
			For each ii in cG01:ii.image="g01_ON":Next
			For each ii in cG02:ii.image="g02_ON":Next
			For each ii in cG03:ii.image="g03_ON":Next
			For each ii in cCab:ii.image="cab_ON":Next
			For each ii in cRamps:ii.image="ramps_ON":Next
		Case "OFF":
			For each ii in cPlastics:ii.image="plastics_OFF":Next
			For each ii in cG01:ii.image="g01_OFF":Next
			For each ii in cG02:ii.image="g02_OFF":Next
			For each ii in cG03:ii.image="g03_OFF":Next
			For each ii in cCab:ii.image="cab_OFF":Next
			For each ii in cRamps:ii.image="ramps_OFF":Next
	end Select
end Sub

sub OnPrimsVisible(aValue)
	If aValue then
		For each kk in ON_Prims:kk.visible = 1:next
	Else
		For each kk in ON_Prims:kk.visible = 0:next
		if not wirerampsOFF.visible then OffPrimsVisible true':Debug.print "why onprims"
	end If
end Sub

sub OffPrimsVisible(aValue)
	If aValue then
		For each kk in OFF_Prims:kk.visible = 1:next
	Else
		For each kk in OFF_Prims:kk.visible = 0:next
		if not wireramps.visible then OnPrimsVisible true':Debug.print "why offprims"
	end If
end Sub

sub BothPrimsVisible
	For each kk in OFF_Prims:kk.visible = 1:next
	For each kk in ON_Prims:kk.visible = 1:next
end sub

S66.IntensityScale = 0
S66a.IntensityScale = 0
S66b.IntensityScale = 0

'	Lampz.MassAssign(66)= S66
'	Lampz.Callback(66) = "DisableLighting p66b, 20,"
'	Lampz.Callback(66) = "DisableLighting p66bbulb, 40,"

BumperTopImage 1
sub BumperTopImage(nro)
'0 = gioff
'1 = gion
'2 = LF
'3 = RF
'4 = LF_gioff
'5 = RF_gioff
	Select Case nro
		case 0: 
			if lampz.state(57) = 1 then
				bumpertop1.image = "g03_ON"
			Else
				bumpertop1.image = "g03_OFF"
			end if
			if lampz.state(58) = 1 then
				bumpertop2.image = "g03_ON"
			Else
				bumpertop2.image = "g03_OFF"
			end if
			if lampz.state(59) = 1 then
				bumpertop3.image = "g03_ON"
			Else
				bumpertop3.image = "g03_OFF"
			end if
		case 1: bumpertop1.image = "g03_ON":bumpertop2.image = "g03_ON":bumpertop3.image = "g03_ON"
		case 2: bumpertop1.image = "g03_LF":bumpertop2.image = "g03_LF":bumpertop3.image = "g03_LF"
		case 3: bumpertop1.image = "g03_RF":bumpertop2.image = "g03_RF":bumpertop3.image = "g03_RF"
		case 4: 
			if lampz.state(57) = 1 then
				bumpertop1.image = "g03_LF"
			Else
				bumpertop1.image = "g03_LF_gioff"
			end if
			if lampz.state(58) = 1 then
				bumpertop2.image = "g03_LF"
			Else
				bumpertop2.image = "g03_LF_gioff"
			end if
			if lampz.state(59) = 1 then
				bumpertop3.image = "g03_LF"
			Else
				bumpertop3.image = "g03_LF_gioff"
			end if
		case 5: 
			if lampz.state(57) = 1 then
				bumpertop1.image = "g03_RF"
			Else
				bumpertop1.image = "g03_RF_gioff"
			end if
			if lampz.state(58) = 1 then
				bumpertop2.image = "g03_RF"
			Else
				bumpertop2.image = "g03_RF_gioff"
			end if
			if lampz.state(59) = 1 then
				bumpertop3.image = "g03_RF"
			Else
				bumpertop3.image = "g03_RF_gioff"
			end if
	end Select
end sub


Sub FlashFlasher1(nr)
	If not objflasher(nr).TimerEnabled Then
		if ImageSwapsForFlashers = 1 then
			BothPrimsVisible
'			debug.print "*** Sol2RGIsync" & Sol2RGIsync
			if Sol2RGIsync = 1 then 'gi on or fading, need to swap OFF prims and fade ON materials
				'debug.print "*** R7 GI ON in start"
				'bumpertop1.image = "g03_LF":bumpertop2.image = "g03_LF":bumpertop3.image = "g03_LF"
				BumperTopImage 2
			else 
				'debug.print "*** R7 GI OFF in start"
				OnPrimSwap "OFF" 'make ON prims opague, so visible OFF image comes from ON prim
				OnPrimsTransparency 1 'Swap RF_gioff images to OFF prims
				BumperTopImage 4
			end if
			OffPrimSwap nr,Sol2RGIsync,False
		end If


		objflasher(nr).TimerEnabled = True
		objflasher(nr).visible = 1
		objlit(nr).visible = 1
		If VRRoom > 0 and VRFlashingBackglass > 0 Then for each vrobj in VRBGFL26: vrobj.visible = 1: VRBBBOTTOMLEFT.visible = 1: Next
	End If

	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0

	if ImageSwapsForFlashers = 1 then
		if ObjLevel(nr) > ObjLevel(2) then
			UpdateMaterial "GI_ON_CAB"		,0,0,0,0,0,0,1-ObjLevel(nr)^1.5,RGB(255,255,255),0,0,False,True,0,0,0,0
			UpdateMaterial "GI_ON_Plastic"	,0,0,0,0,0,0,1-ObjLevel(nr)^2,RGB(255,255,255),0,0,False,True,0,0,0,0
			UpdateMaterial "GI_ON_Metals"	,0,0,0,0,0,0,1-ObjLevel(nr)^1,RGB(255,255,255),0,0,False,True,0,0,0,0
			UpdateMaterial "GI_ON_Bulbs"	,0,0,0,0,0,0,1-ObjLevel(nr)^2,RGB(255,255,255),0,0,False,True,0,0,0,0
			UpdateMaterial "GI_ON_50",	0,0,0,0,0,0,0.5 * (1-ObjLevel(nr)),RGB(255,255,255),0,0,False,True,0,0,0,0
			UpdateMaterial "GI_OFF_50",	0,0,0,0,0,0,0.5 * ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0
			
			'if giprevalvl = 1 and GiOnVar 		Then 'gi is still On, do nothing
			'if giprevalvl = 0 and Not GiOnVar 	Then 'gi is still Off, do nothing
			if ObjLevel(nr) < 0.8 then
				if giprevalvl = 1 and Not GiOnVar and Sol2RGIsync = 1 	Then OffPrimSwap nr,0,False':debug.print "was on, but started to fade"		'gi was on, but just started to fade off
				if giprevalvl = 0 and GiOnVar and Sol2RGIsync = 0 		Then OffPrimSwap nr,1,False':debug.print "was off, but started to fade"		'gi was off, but just started to fade on
			end if
		end if
	end if
	If VRRoom > 0 and VRFlashingBackglass > 0 Then
		for each vrobj in VRBGFL26: vrobj.opacity = 100 * ObjLevel(nr)^3: next
		VRBBBOTTOMLEFT.opacity = 20 * ObjLevel(nr)^3
	End If
	PLAYFIELD_LF.opacity = 700 * ObjLevel(nr)^1
	flasherbloomLF.opacity = 500 * FlasherBloomIntensity * ObjLevel(nr)^1
	plastics.blenddisablelighting = 0.1 * ObjLevel(nr)

	S66.IntensityScale = 3 * ObjLevel(nr)
 	S66a.IntensityScale = 3 * ObjLevel(nr)
	S66b.IntensityScale = 3 * ObjLevel(nr)^2

	DisableLightingFlash p66b, 20,ObjLevel(nr)
	DisableLightingFlash p66bbulb, 40,ObjLevel(nr)

 
	if ObjTargetLevel(nr) = 1 and ObjLevel(nr) < ObjTargetLevel(nr) Then			'solenoid ON happened
		ObjLevel(nr) = (ObjLevel(nr) + 0.35) * RndNum(1.05, 1.15)					'fadeup speed.
		if ObjLevel(nr) > 1 then ObjLevel(nr) = 1
	Elseif ObjTargetLevel(nr) = 0 and ObjLevel(nr) > ObjTargetLevel(nr) Then		'solenoid OFF happened
		ObjLevel(nr) = ObjLevel(nr) * 0.8 - 0.01									'fadedown speed.
		if ObjLevel(nr) < 0.02 then ObjLevel(nr) = 0								'slight perf optimization to cut the very tail of the fade
	Else																			'do nothing here
		ObjLevel(nr) = ObjTargetLevel(nr)
		'debug.print objTargetLevel(nr) &" = " & ObjLevel(nr)
	end if

	If ObjLevel(nr) <= 0 Then
		if ObjLevel(2) <= 0 then
			'debug.print "end of fading: " & nr
			if Sol2RGIsync = 1 then 
				'debug.print "*** Swapping OFF prims with OFF and making them NON visible"
				OffPrimSwap nr,Sol2RGIsync,True

				if  giprevalvl < 1 then 'added this tweak to prevent prims turn off if GI is turned on while fading
					'make ON prims fully transparent, so OFF prims are shown
					'debug.print "***GI went OFF while fading 2R. wirerampsOFF visibility: " & wirerampsOFF.visible 
					PLAYFIELD_GI1.opacity = 0
					OnPrimsTransparency 0
					OnPrimsVisible False
				else 'remove
					'debug.print "**REMOVE 2R. Plastics visibility: " & plastics.visible 
					OnPrimsTransparency 1
					OffPrimsVisible False
				end if
			Else
				'ON prims are opague now but showing OFF images, let's replace OFF prims back to OFF images
				OffPrimSwap nr,Sol2RGIsync,True
				if  giprevalvl > 0 then 'added this tweak to prevent prims turn off if GI is turned on while fading
					'debug.print "*** GI went ON while fading 2R. Plastics visibility: " & wireramps.visible 
					PLAYFIELD_GI1.opacity = PFGIOFFOpacity
					OnPrimsTransparency 1
					OffPrimsVisible False				
				Else
					'debug.print "*** GI stayed off 2R. wirerampsOFF visibility: " & wirerampsOFF.visible 
					PLAYFIELD_GI1.opacity = 0
					OnPrimsTransparency 0
					OnPrimsVisible False
				end if
				'revert ON prims back to ON Images
				'OnPrimSwap "ON"
			end if
			OnPrimSwap "ON"
		end If
		if giprevalvl < 1 Then
			BumperTopImage 0
		Else
			BumperTopImage 1
		end if
		Sol2RGIsync = -1
		objflasher(nr).TimerEnabled = False
		objflasher(nr).visible = 0
		objlit(nr).visible = 0
		swapflag=0
		If VRRoom > 0 and VRFlashingBackglass > 0 Then for each vrobj in VRBGFL26: vrobj.visible = 0: VRBBBOTTOMLEFT.visible = 0: Next
	End If
End Sub



S68a.IntensityScale = 0
S68b.IntensityScale = 0
S68c.IntensityScale = 0
'
'	Lampz.MassAssign(68)= S68b
'	Lampz.Callback(68) = "DisableLighting p68b, 20,"
'	Lampz.Callback(68) = "DisableLighting p68bbulb, 40,"


Sub FlashFlasher2(nr)
	If not objflasher(nr).TimerEnabled Then
		if ImageSwapsForFlashers = 1 then
			BothPrimsVisible
			'debug.print "*** Sol4RGIsync" & Sol4RGIsync
			if Sol4RGIsync = 1 then 'gi on or fading, need to swap OFF prims and fade ON materials
				BumperTopImage 3
			else 
				'debug.print "*** R4 GI OFF in start"
				OnPrimSwap "OFF" 'make ON prims opague, so visible OFF image comes from ON prim
				OnPrimsTransparency 1 'Swap RF_gioff images to OFF prims
				BumperTopImage 5
			end if
			OffPrimSwap nr,Sol4RGIsync,False
		end if
		objflasher(nr).TimerEnabled = True
		objflasher(nr).visible = 1
		objlit(nr).visible = 1
		If VRRoom > 0 and VRFlashingBackglass > 0 Then for each vrobj in VRBGFL28: vrobj.visible = 1: Next
	End If

	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0

	if ImageSwapsForFlashers = 1 then
		if ObjLevel(nr) > ObjLevel(1) then
			UpdateMaterial "GI_ON_CAB"		,0,0,0,0,0,0,1-ObjLevel(nr)^1.5,RGB(255,255,255),0,0,False,True,0,0,0,0
			UpdateMaterial "GI_ON_Plastic"	,0,0,0,0,0,0,1-ObjLevel(nr)^2,RGB(255,255,255),0,0,False,True,0,0,0,0
			UpdateMaterial "GI_ON_Metals"	,0,0,0,0,0,0,1-ObjLevel(nr)^1,RGB(255,255,255),0,0,False,True,0,0,0,0
			UpdateMaterial "GI_ON_Bulbs"	,0,0,0,0,0,0,1-ObjLevel(nr)^2,RGB(255,255,255),0,0,False,True,0,0,0,0
			UpdateMaterial "GI_ON_50",	0,0,0,0,0,0,0.5 * (1-ObjLevel(nr)),RGB(255,255,255),0,0,False,True,0,0,0,0
			UpdateMaterial "GI_OFF_50",	0,0,0,0,0,0,0.5 * ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0
			if ObjLevel(nr) < 0.8 then
				if giprevalvl = 1 and Not GiOnVar and Sol4RGIsync = 1 	Then OffPrimSwap nr,0,False':debug.print "was on, but started to fade"		'gi was on, but just started to fade off
				if giprevalvl = 0 and GiOnVar and Sol4RGIsync = 0 		Then OffPrimSwap nr,1,False':debug.print "was off, but started to fade"		'gi was off, but just started to fade on
			end if
		end if
	end if
	If VRRoom > 0 and VRFlashingBackglass > 0 Then
		for each vrobj in VRBGFL28: vrobj.opacity = 100 * ObjLevel(nr)^3: next
	End If
	PLAYFIELD_RF.opacity = 700 * ObjLevel(nr)^1
	flasherbloomRF.opacity = 500 * FlasherBloomIntensity * ObjLevel(nr)^1
	plastics.blenddisablelighting = 0.1 * ObjLevel(nr)

	S68a.IntensityScale = 3 * ObjLevel(nr)
	S68b.IntensityScale = 3 * ObjLevel(nr)
	S68c.IntensityScale = 3 * ObjLevel(nr)^2

	DisableLightingFlash p68b, 20, ObjLevel(nr)
	DisableLightingFlash p68bbulb, 40, ObjLevel(nr)

	if ObjTargetLevel(nr) = 1 and ObjLevel(nr) < ObjTargetLevel(nr) Then			'solenoid ON happened
		ObjLevel(nr) = (ObjLevel(nr) + 0.35) * RndNum(1.05, 1.15)					'fadeup speed. ~4-5 frames * 30ms
		if ObjLevel(nr) > 1 then ObjLevel(nr) = 1
	Elseif ObjTargetLevel(nr) = 0 and ObjLevel(nr) > ObjTargetLevel(nr) Then		'solenoid OFF happened
		ObjLevel(nr) = ObjLevel(nr) * 0.8 - 0.01									'fadedown speed. ~16 frames * 30ms
		if ObjLevel(nr) < 0.02 then ObjLevel(nr) = 0								'slight perf optimization to cut the very tail of the fade
	Else																			'do nothing here
		ObjLevel(nr) = ObjTargetLevel(nr)
		'debug.print objTargetLevel(nr) &" = " & ObjLevel(nr)
	end if

	If ObjLevel(nr) <= 0 Then
		if ObjLevel(1) <= 0 then
			'debug.print "end of fading: " & nr
			if Sol4RGIsync = 1 then 
				'debug.print "*** Swapping OFF prims with OFF and making them NON visible"
				OffPrimSwap nr,Sol4RGIsync,True

				if giprevalvl < 1 then 'added this tweak to prevent prims turn off if GI is turned on while fading
					'make ON prims fully transparent, so OFF prims are shown
					'debug.print "***GI went OFF while fading 4R. wirerampsOFF visibility: " & wirerampsOFF.visible 
					PLAYFIELD_GI1.opacity = 0
					OnPrimsTransparency 0
					OnPrimsVisible False
				else 'remove
					'debug.print "**REMOVE 4R. Plastics visibility: " & plastics.visible 
					OnPrimsTransparency 1
					OffPrimsVisible False
				end if
			Else
				'ON prims are opague now but showing OFF images, let's replace OFF prims back to OFF images
				OffPrimSwap nr,Sol4RGIsync,True
				if giprevalvl > 0 then 'added this tweak to prevent prims turn off if GI is turned on while fading
					'msgbox "*** GI went ON while fading 4R. laneguides visibility: " & lineguides.visible
					PLAYFIELD_GI1.opacity = PFGIOFFOpacity
					OnPrimsTransparency 1
					OffPrimsVisible False				
				Else
'					'debug.print "*** GI stayed off 4R. wirerampsOFF visibility: " & wirerampsOFF.visible 
					PLAYFIELD_GI1.opacity = 0
					OnPrimsTransparency 0
					OnPrimsVisible False
				end if
				'revert ON prims back to ON Images
				'OnPrimSwap "ON"
			end if
			OnPrimSwap "ON"
		end If
		if giprevalvl < 1 Then
			BumperTopImage 0
		Else
			BumperTopImage 1
		end if
		Sol4RGIsync = -1
		objflasher(nr).TimerEnabled = False
		objflasher(nr).visible = 0
		objlit(nr).visible = 0

		swapflag=0
		If VRRoom > 0 and VRFlashingBackglass > 0 Then for each vrobj in VRBGFL28: vrobj.visible = 0: Next
	End If
End Sub



'RF gioff 	= yellow
'RF			= blue
'LF gioff   = purple
'LF			= green


Sub FlasherFlash1_Timer() : FlashFlasher1(1) : End Sub 
Sub FlasherFlash2_Timer() :	FlashFlasher2(2) : End Sub 


'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(activeball)
	vpmTimer.PulseSw 27
    'PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
	RandomSoundSlingshotRight gi004
    RSling.Visible = 0
	RSlingOFF.Visible = 0
    RSling1.Visible = 1
	RSling1OFF.Visible = 1
    sling1.rotx = 20
	sling1OFF.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10:RSLing1OFF.Visible = 0:RSLing2OFF.Visible = 1:sling1OFF.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RSLing2OFF.Visible = 0:RSLingOFF.Visible = 1:sling1OFF.rotx = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(activeball)
	vpmTimer.PulseSw 19
    'PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
	RandomSoundSlingshotLeft gi003
    LSling.Visible = 0
	LSlingOFF.Visible = 0
    LSling1.Visible = 1
	LSling1OFF.Visible = 1
    sling2.rotx = 20
    sling2OFF.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10:LSLing1OFF.Visible = 0:LSLing2OFF.Visible = 1:sling2OFF.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LSLing2OFF.Visible = 0:LSLingOFF.Visible = 1:sling2OFF.rotx = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub



'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.

'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

GlobalSoundLevel = 1
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

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel, RelaySoundLevel

GateSoundLevel = 0.5/5													'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10											'volume multiplier; must not be zero
DTSoundLevel = 0.25														'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                              					'volume level; range [0, 1]
SpinnerSoundLevel = 0.01												'volume level; range [0, 1]
RelaySoundLevel = 0.5

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8														'volume level; range [0, 1]
BallReleaseSoundLevel = 1												'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2									'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015										'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5													'volume multiplier; must not be zero

Dim LutToggleSoundLevel
LutToggleSoundLevel = 0.01

'/////////////////////////////  SPINNER  ////////////////////////////
Sub SoundSpinner(tableobj)
	PlaySoundAtLevelStatic ("TOM_NK_Spinner_12"), SpinnerSoundLevel, tableobj
End Sub

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
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / tableheight-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / tablewidth-1
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
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound ("Nudge_1"), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
		Case 2 : PlaySound ("Nudge_2"), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
		Case 3 : PlaySound ("Nudge_3"), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
	End Select
End Sub

Sub SoundNudgeRight()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound ("Nudge_1"), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
		Case 2 : PlaySound ("Nudge_2"), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
		Case 3 : PlaySound ("Nudge_3"), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
	End Select
End Sub

Sub SoundNudgeCenter()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound ("Nudge_1"), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
		Case 2 : PlaySound ("Nudge_2"), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
		Case 3 : PlaySound ("Nudge_3"), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
	End Select
End Sub


Sub SoundPlungerPull()
	PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, sw16
End Sub

Sub SoundPlungerReleaseBall()
	PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, sw16	
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, sw16
End Sub


'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////
Sub KnockerSolenoid()
	PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, KnockerPosition
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////
Sub RandomSoundDrain(drainswitch)
	Select Case Int(Rnd*11)+1
		Case 1 : PlaySoundAtLevelStatic ("Drain_1"), DrainSoundLevel, drainswitch
		Case 2 : PlaySoundAtLevelStatic ("Drain_2"), DrainSoundLevel, drainswitch
		Case 3 : PlaySoundAtLevelStatic ("Drain_3"), DrainSoundLevel, drainswitch
		Case 4 : PlaySoundAtLevelStatic ("Drain_4"), DrainSoundLevel, drainswitch
		Case 5 : PlaySoundAtLevelStatic ("Drain_5"), DrainSoundLevel, drainswitch
		Case 6 : PlaySoundAtLevelStatic ("Drain_6"), DrainSoundLevel, drainswitch
		Case 7 : PlaySoundAtLevelStatic ("Drain_7"), DrainSoundLevel, drainswitch
		Case 8 : PlaySoundAtLevelStatic ("Drain_8"), DrainSoundLevel, drainswitch
		Case 9 : PlaySoundAtLevelStatic ("Drain_9"), DrainSoundLevel, drainswitch
		Case 10 : PlaySoundAtLevelStatic ("Drain_10"), DrainSoundLevel, drainswitch
		Case 11 : PlaySoundAtLevelStatic ("Drain_11"), DrainSoundLevel, drainswitch
	End Select
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBallRelease(drainswitch)
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("BallRelease1",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 2 : PlaySoundAtLevelStatic SoundFX("BallRelease2",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 3 : PlaySoundAtLevelStatic SoundFX("BallRelease3",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 4 : PlaySoundAtLevelStatic SoundFX("BallRelease4",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 5 : PlaySoundAtLevelStatic SoundFX("BallRelease5",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 6 : PlaySoundAtLevelStatic SoundFX("BallRelease6",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 7 : PlaySoundAtLevelStatic SoundFX("BallRelease7",DOFContactors), BallReleaseSoundLevel, drainswitch
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

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft(sling)
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Sling_L1",DOFContactors), SlingshotSoundLevel, Sling
		Case 2 : PlaySoundAtLevelStatic SoundFX("Sling_L2",DOFContactors), SlingshotSoundLevel, Sling
		Case 3 : PlaySoundAtLevelStatic SoundFX("Sling_L3",DOFContactors), SlingshotSoundLevel, Sling
		Case 4 : PlaySoundAtLevelStatic SoundFX("Sling_L4",DOFContactors), SlingshotSoundLevel, Sling
		Case 5 : PlaySoundAtLevelStatic SoundFX("Sling_L5",DOFContactors), SlingshotSoundLevel, Sling
		Case 6 : PlaySoundAtLevelStatic SoundFX("Sling_L6",DOFContactors), SlingshotSoundLevel, Sling
		Case 7 : PlaySoundAtLevelStatic SoundFX("Sling_L7",DOFContactors), SlingshotSoundLevel, Sling
		Case 8 : PlaySoundAtLevelStatic SoundFX("Sling_L8",DOFContactors), SlingshotSoundLevel, Sling
		Case 9 : PlaySoundAtLevelStatic SoundFX("Sling_L9",DOFContactors), SlingshotSoundLevel, Sling
		Case 10 : PlaySoundAtLevelStatic SoundFX("Sling_L10",DOFContactors), SlingshotSoundLevel, Sling
	End Select
End Sub

Sub RandomSoundSlingshotRight(sling)
	Select Case Int(Rnd*8)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Sling_R1",DOFContactors), SlingshotSoundLevel, Sling
		Case 2 : PlaySoundAtLevelStatic SoundFX("Sling_R2",DOFContactors), SlingshotSoundLevel, Sling
		Case 3 : PlaySoundAtLevelStatic SoundFX("Sling_R3",DOFContactors), SlingshotSoundLevel, Sling
		Case 4 : PlaySoundAtLevelStatic SoundFX("Sling_R4",DOFContactors), SlingshotSoundLevel, Sling
		Case 5 : PlaySoundAtLevelStatic SoundFX("Sling_R5",DOFContactors), SlingshotSoundLevel, Sling
		Case 6 : PlaySoundAtLevelStatic SoundFX("Sling_R6",DOFContactors), SlingshotSoundLevel, Sling
		Case 7 : PlaySoundAtLevelStatic SoundFX("Sling_R7",DOFContactors), SlingshotSoundLevel, Sling
		Case 8 : PlaySoundAtLevelStatic SoundFX("Sling_R8",DOFContactors), SlingshotSoundLevel, Sling
	End Select
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperTop(Bump)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 2 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_2",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 3 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_3",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 4 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_4",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 5 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_5",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
	End Select
End Sub

Sub RandomSoundBumperMiddle(Bump)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 2 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_2",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 3 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_3",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 4 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_4",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 5 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_5",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
	End Select
End Sub

Sub RandomSoundBumperBottom(Bump)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 2 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_2",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 3 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_3",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 4 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_4",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 5 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_5",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
	End Select
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
	Select Case Int(Rnd*11)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_L01",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_L02",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_L07",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_L08",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_L09",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_L10",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_L12",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 8 : PlaySoundAtLevelStatic SoundFX("Flipper_L14",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 9 : PlaySoundAtLevelStatic SoundFX("Flipper_L18",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 10 : PlaySoundAtLevelStatic SoundFX("Flipper_L20",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 11 : PlaySoundAtLevelStatic SoundFX("Flipper_L26",DOFFlippers), FlipperLeftHitParm, Flipper
	End Select
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	Select Case Int(Rnd*11)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_R01",DOFFlippers), FlipperRightHitParm, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_R02",DOFFlippers), FlipperRightHitParm, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_R03",DOFFlippers), FlipperRightHitParm, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_R04",DOFFlippers), FlipperRightHitParm, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_R05",DOFFlippers), FlipperRightHitParm, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_R06",DOFFlippers), FlipperRightHitParm, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_R07",DOFFlippers), FlipperRightHitParm, Flipper
		Case 8 : PlaySoundAtLevelStatic SoundFX("Flipper_R08",DOFFlippers), FlipperRightHitParm, Flipper
		Case 9 : PlaySoundAtLevelStatic SoundFX("Flipper_R09",DOFFlippers), FlipperRightHitParm, Flipper
		Case 10 : PlaySoundAtLevelStatic SoundFX("Flipper_R10",DOFFlippers), FlipperRightHitParm, Flipper
		Case 11 : PlaySoundAtLevelStatic SoundFX("Flipper_R11",DOFFlippers), FlipperRightHitParm, Flipper
	End Select
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L01",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L02",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L03",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
	End Select
End Sub

Sub RandomSoundReflipUpRight(flipper)
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R01",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R02",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R03",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
	End Select
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_1",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_2",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_3",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_4",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_5",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_6",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_7",DOFFlippers), FlipperDownSoundLevel, Flipper
	End Select
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	Select Case Int(Rnd*8)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_1",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_2",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_3",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_4",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_5",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_6",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_7",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 8 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_8",DOFFlippers), FlipperDownSoundLevel, Flipper
	End Select
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm/10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	RandomSoundRubberFlipper(parm)
'	if RubberizerEnabled <> 0 then Rubberizer(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm/10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
 	RandomSoundRubberFlipper(parm)
'	if RubberizerEnabled <> 0 then Rubberizer(parm)
End Sub

Sub RightFlipper1Collide(parm)
	FlipperRightHitParm = parm/10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	'CheckLiveCatch Activeball, RightFlipper1, RFCount1, parm ' no live catch for top flip
 	RandomSoundRubberFlipper(parm)
End Sub


' iaakki Rubberizer
'sub Rubberizer(parm)
'	if parm < 10 And parm > 2 And Abs(activeball.angmomz) < 10 then
'		'debug.print "parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
'		activeball.angmomz = activeball.angmomz * 1.2
'		activeball.vely = activeball.vely * 1.2
'		'debug.print ">> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
'	Elseif parm <= 2 and parm > 0.2 Then
'		'debug.print "* parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
'		activeball.angmomz = activeball.angmomz * -1.1
'		activeball.vely = activeball.vely * 1.4
'		'debug.print "**** >> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
'	end if
'end sub



Sub RandomSoundRubberFlipper(parm)
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_1"), parm  * RubberFlipperSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_2"), parm  * RubberFlipperSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_3"), parm  * RubberFlipperSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_4"), parm  * RubberFlipperSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_5"), parm  * RubberFlipperSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_6"), parm  * RubberFlipperSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_7"), parm  * RubberFlipperSoundFactor
	End Select
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rollover_1"), RolloverSoundLevel
		Case 2 : PlaySoundAtLevelActiveBall ("Rollover_2"), RolloverSoundLevel
		Case 3 : PlaySoundAtLevelActiveBall ("Rollover_3"), RolloverSoundLevel
		Case 4 : PlaySoundAtLevelActiveBall ("Rollover_4"), RolloverSoundLevel
	End Select
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////


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
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rubber_1"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Rubber_2"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Rubber_3"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Rubber_3"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall ("Rubber_5"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall ("Rubber_6"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall ("Rubber_7"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 8 : PlaySoundAtLevelActiveBall ("Rubber_8"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 9 : PlaySoundAtLevelActiveBall ("Rubber_9"), Vol(ActiveBall) * RubberWeakSoundFactor
	End Select
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub aWoods_Hit(idx)
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
	Select Case Int(Rnd*13)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Metal_Touch_1"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Metal_Touch_2"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Metal_Touch_3"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Metal_Touch_4"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall ("Metal_Touch_5"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall ("Metal_Touch_6"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall ("Metal_Touch_7"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 8 : PlaySoundAtLevelActiveBall ("Metal_Touch_8"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 9 : PlaySoundAtLevelActiveBall ("Metal_Touch_9"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 10 : PlaySoundAtLevelActiveBall ("Metal_Touch_10"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 11 : PlaySoundAtLevelActiveBall ("Metal_Touch_11"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 12 : PlaySoundAtLevelActiveBall ("Metal_Touch_12"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 13 : PlaySoundAtLevelActiveBall ("Metal_Touch_13"), Vol(ActiveBall) * MetalImpactSoundFactor
	End Select
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

'Sub aMetals_Hit (idx)
'	RandomSoundMetal
'End Sub
'
'Sub ShooterDiverter_collide(idx)
'	RandomSoundMetal
'End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////
Sub RandomSoundBottomArchBallGuide()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
 		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_2"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
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
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_1"), BottomArchBallGuideSoundFactor * 0.25
		Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_2"), BottomArchBallGuideSoundFactor * 0.25
		Case 3 : PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_3"), BottomArchBallGuideSoundFactor * 0.25
	End Select
End Sub


Sub Apron_Hit(idx)
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
 		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Medium_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_2"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 3 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
		End Select
 	End If
	If finalspeed < 6 Then
 		Select Case Int(Rnd*7)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Soft_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Soft_2"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 3 : PlaySoundAtLevelActiveBall ("Apron_Soft_3"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 4 : PlaySoundAtLevelActiveBall ("Apron_Soft_4"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 5 : PlaySoundAtLevelActiveBall ("Apron_Soft_5"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 6 : PlaySoundAtLevelActiveBall ("Apron_Soft_6"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 7 : PlaySoundAtLevelActiveBall ("Apron_Soft_7"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
		End Select
	End if
End Sub

sub aLaneguides_hit(idx)
	RandomSoundFlipperBallGuide
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_5",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_6",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_7",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_8",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor		
	End Select
End Sub

Sub RandomSoundTargetHitWeak()
	Select Case Int(Rnd*4)+1		
		Case 1 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_1",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_2",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_3",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_4",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
	End Select
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
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_3"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_4"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_6"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
	End Select
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
	Select Case Int(Rnd*2)+1				
		Case 1 : PlaySoundAtLevelStatic ("Gate_FastTrigger_1"), GateSoundLevel, Activeball
		Case 2 : PlaySoundAtLevelStatic ("Gate_FastTrigger_2"), GateSoundLevel, Activeball
	End Select
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, Activeball
End Sub

'Sub Gates_hit(idx)
'	SoundHeavyGate
'End Sub
'
'Sub GatesWire_hit(idx)	
'	SoundPlayfieldGate	
'End Sub	

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Arch_L1"), Vol(ActiveBall) * ArchSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Arch_L2"), Vol(ActiveBall) * ArchSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Arch_L3"), Vol(ActiveBall) * ArchSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Arch_L4"), Vol(ActiveBall) * ArchSoundFactor
	End Select
End Sub

Sub RandomSoundRightArch()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Arch_R1"), Vol(ActiveBall) * ArchSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Arch_R2"), Vol(ActiveBall) * ArchSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Arch_R3"), Vol(ActiveBall) * ArchSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Arch_R4"), Vol(ActiveBall) * ArchSoundFactor
	End Select
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
	Select Case Int(Rnd*2)+1	
		Case 1: PlaySoundAtLevelStatic ("Saucer_Enter_1"), SaucerLockSoundLevel, Activeball
		Case 2: PlaySoundAtLevelStatic ("Saucer_Enter_2"), SaucerLockSoundLevel, Activeball
	End Select
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






'**********************************************************''
' end fleep sounds
'**********************************************************''




'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	FlipperRSh1.RotZ = RightFlipper1.currentangle

    LFLogo.RotY = LeftFlipper.CurrentAngle
    RFlogo.RotY = RightFlipper.CurrentAngle
    RFlogo2.RotY = RightFlipper1.CurrentAngle

    LFLogoOFF.RotY = LeftFlipper.CurrentAngle
    RFlogoOFF.RotY = RightFlipper.CurrentAngle
    RFlogo2OFF.RotY = RightFlipper1.CurrentAngle

'	PRIMS TRACKS
    sw44p.RotX =-(sw44.currentangle)
	sw44pOFF.RotX = sw44p.RotX
    sw46p.RotX =-(sw46.currentangle)
	sw46pOFF.RotX = sw46p.RotX
    sw40p.RotX =-(sw40.currentangle)
	sw40pOFF.RotX = sw40p.RotX
    sw48p.RotX =-(sw48.currentangle)
	sw48pOFF.RotX = sw48p.RotX
    sw56p.RotX =-(sw56.currentangle)
	sw56pOFF.RotX = sw56p.RotX
	gate1p.RotX =(gate1.currentangle)
	gate1pOFF.RotX = gate1p.RotX
	gate2p.RotX =(gate2.currentangle)
	gate2pOFF.RotX = gate2p.RotX


End Sub


sub wr_trigger001_hit():WireRampOff:WireRampOn False:end sub
sub wr_trigger002_hit():WireRampOn False:BallInKicker1 = 0:end sub
sub wr_trigger003_hit():WireRampOff:end sub


'**********************************
' Wylte's Ray Tracing Ball Shadows
'**********************************

Const tnob = 8		'total number of balls, 20 balls, from 0 to 19
Const lob = 0		'number of locked balls

Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source

dim pupkickerx, pupkickery
pupkickerx = pUpKicker.x
pupkickery = pUpKicker.y

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

Dim gBOT
gBOT = GetBalls

Sub BallFXUpdate
	Dim falloff:	falloff = 150			'Max distance to light sources, can be changed if you have a reason
	Dim ShadowOpacity, ShadowOpacity2 
	Dim s, t, Source, LSd, currentMat, AnotherSource, iii

	If UBound(gBOT) = lob - 1 Then Exit Sub				'No balls in play exit

	' hide shadow of deleted balls
	For t = lob to UBound(gBOT)
		' hide shadow of deleted balls
        If Not (InRect(gBOT(t).X, gBOT(t).y, 422,2060,850,1780,850,1920,460,2130) Or InRect(gBOT(t).x, gBOT(t).y, 15,880,88,880,105,970,44,1010)) Then
            'msgbox "on table"
            if Not (activeShadowBall.Exists(gBOT(t).ID)) Then
				activeShadowBall.Add gBOT(t).ID, ""
            end If
        else
			'msgbox "remove ball"
            if (activeShadowBall.Exists(gBOT(t).ID)) then
				activeShadowBall.Remove(gBOT(t).ID)
            end if
        end If
	Next

    Dim cntActBall : cntActBall = 0

	For s = lob to UBound(gBOT)
 
		'rolling sound
		If BallVel(gBOT(s)) > 1 AND gBOT(s).z < 30 Then
			rolling(s) = True
			PlaySound ("BallRoll_" & s), -1, VolPlayfieldRoll(gBOT(s)) * 1.1 * VolumeDial, AudioPan(gBOT(s)), 0, PitchPlayfieldRoll(gBOT(s)), 1, 0, AudioFade(gBOT(s))
		Else
			If rolling(s) = True Then
				StopSound("BallRoll_" & s)
				rolling(s) = False
			End If
		End If

		If Not InRect(gBOT(s).x, gBOT(s).y, 422,2060,850,1780,850,1920,460,2130) And GiOnVar then 	'if ball is not under apron and GI is on (inverted signal)
			If InRect(gBOT(s).x, gBOT(s).y, 15,880,88,880,105,970,44,1010) Then			'captive ball
				gBOT(s).color = ballbrightness + (ballbrightness * 256) + (ballbrightness * 256 * 256)
			Else
				'debug.print "ball nro in play:" & s
				'***Ball Drop Sounds***
				If gBOT(s).VelZ < -1 and gBOT(s).z < 55 and gBOT(s).z > 27 Then 'height adjust for ball drop sounds
					'debug.print "ball drop" & gBOT(s).velz
					If DropCount(s) >= 2 Then
						DropCount(s) = 0
						If gBOT(s).velz > -7 Then
							'debug.print "random sound soft"
							RandomSoundBallBouncePlayfieldSoft gBOT(s)
						Else
							'debug.print "random sound hard"
							RandomSoundBallBouncePlayfieldHard gBOT(s)
						End If				
					End If
		'		Else
		'			If gBOT(s).VelZ < -1 then
		'				debug.print "ball z: " & gBOT(s).z & " ball velz: " & gBOT(s).VelZ
		'			end if
				End If
				If DropCount(s) < 2 Then
					DropCount(s) = DropCount(s) + 1
				End If

				'brightness
				if ballbrightness <> -1 then 
					gBOT(s).color = ballbrightness + (ballbrightness * 256) + (ballbrightness * 256 * 256)
					if s = UBound(gBOT) then 'until last ball brightness is set, then reset to -1
						if ballbrightness = ballbrightMax Or ballbrightness = ballbrightMin then ballbrightness = -1
					end if
				end If

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible
				If AmbientBallShadowOn = 0 Then
					If gBOT(s).Z > 30 Then
						BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
					Else
						BallShadowA(s).height=gBOT(s).z - BallSize/2 + 5
					End If
					BallShadowA(s).Y = gBOT(s).Y + Ballsize/5 + fovY
					BallShadowA(s).X = gBOT(s).X
					BallShadowA(s).visible = 1
				Elseif AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
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
							If LSd < falloff Then						    'If the ball is within the falloff range of a light and light is on (we will set numberofsources to 0 when GI is off)
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
			End If
		End If
	Next
End Sub


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



'************************************
' What you need to add to your table
'************************************

' a timer called RollingTimer. With a fast interval, like 10
' one collision sound, in this script is called fx_collide
' as many sound files as max number of balls, with names ending with 0, 1, 2, 3, etc
' for ex. as used in this script: fx_ballrolling0, fx_ballrolling1, fx_ballrolling2, fx_ballrolling3, etc


'******************************************
' Explanation of the rolling sound routine
'******************************************

' sounds are played based on the ball speed and position

' the routine checks first for deleted balls and stops the rolling sound.

' The For loop goes through all the balls on the table and checks for the ball speed and 
' if the ball is on the table (height lower than 30) then then it plays the sound
' otherwise the sound is stopped, like when the ball has stopped or is on a ramp or flying.

' The sound is played using the VOL, AUDIOPAN, AUDIOFADE and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the AUDIOPAN & AUDIOFADE functions will change the stereo position
' according to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.


Sub Targets_Hit (idx)
	PlayTargetSound	
End Sub

Sub Metals_Thin_Hit (idx)
	RandomSoundMetal
End Sub

Sub Metals_Medium_Hit (idx)
	RandomSoundMetal
End Sub

Sub Metals2_Hit (idx)
	RandomSoundMetal
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)	
	SoundPlayfieldGate	
End Sub

'Sub Spinner_Spin
'	PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
'End Sub



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



'****************************************************************
'				Class jungle nf (what does this mean?!?)
'****************************************************************

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

'ruff preload
'Sol4R true

dim preloadCounter
sub preloader_timer
	preloadCounter = preloadCounter + 1
	if preloadCounter = 1 then
        BothPrimsVisible
        OffPrimSwap 1,1,False
	Elseif preloadCounter = 2 then
        OffPrimSwap 1,0,False
	Elseif preloadCounter = 3 then
        OffPrimSwap 2,1,False
	Elseif preloadCounter = 4 then
        OffPrimSwap 2,0,False
	Elseif preloadCounter = 5 then
        OffPrimSwap 1,1,True
        OffPrimsVisible False
	Elseif preloadCounter = 6 then
		sol4r true
		vpmtimer.addtimer 50, "Sol4R false '"
	Elseif preloadCounter = 7 then
		sol2r true
		vpmtimer.addtimer 50, "Sol2R false '"
	Elseif preloadCounter = 8 then
		sol2r false
		sol4r false
		CapKicker.CreateBall
		CapKicker.Kick 180,1
		lampz.state(111) = 5
	Elseif preloadCounter = 12 then
		me.enabled = false
	end if
	gBOT = GetBalls
	'msgbox preloadCounter
end sub

'sub preloader_timer
'	preloadCounter = preloadCounter + 1
'	if preloadCounter = 1 then
'        BothPrimsVisible
'        OffPrimSwap 1,1,False
'	Elseif preloadCounter = 2 then
'        OffPrimSwap 1,0,False
'	Elseif preloadCounter = 3 then
'        OffPrimSwap 2,1,False
'	Elseif preloadCounter = 4 then
'        OffPrimSwap 2,0,False
'	Elseif preloadCounter = 5 then
'        OffPrimSwap 1,1,True
'        OffPrimsVisible False
'	Elseif preloadCounter = 6 then
'		sol4r true
'	Elseif preloadCounter = 7 then
'		sol2r true
'	Elseif preloadCounter = 8 then
'		sol2r false
'		sol4r false
'		CapKicker.CreateBall
'		CapKicker.Kick 180,1
'		lampz.state(111) = 5
'	Elseif preloadCounter = 12 then
'		me.enabled = false
'	end if
'	gBOT = GetBalls
'	'msgbox preloadCounter
'end sub

'*********************
'VR Mode
'*********************
DIM VRThings
If VRRoom > 0 Then
	ScoreText.visible = 0
	DMD.visible = 1
	PinCab_Rails.visible = true
' Below added for Plunger Lighted eyes mod option.
    if PlungerEyeMod  = 1 then PlungerEye1.visible = true: PlungerEye2.visible = true

	If VRRoom = 1 Or VRRoom = 2 Then
		for each VRThings in VRCab:VRThings.visible = 1:Next
	End If
	If VRRoom = 3 Then
		for each VRThings in VRCab:VRThings.visible = 0:Next
		PinCab_Backbox.visible = 1
		PinCab_Backglass.visible = 1
	End If
	If VRFlashingBackglass > 0 Then
		SetBackglass
		BGDark.visible = 1
		PinCab_Backglass.visible = 0
		For each VRThings in VRBGGI:VRThings.visible = 1: Next
		If VRFlashingBackglass = 2 Then
			Backglassblink.enabled = True
		End If
	End If
Else
	for each VRThings in VRCab:VRThings.visible = 0:Next
	if CabinetMode = 0 then PinCab_Rails.visible = true else PinCab_Rails.visible = false
End if



'******************************************************
'*******	Set Up VR Backglass Flashers	*******
'******************************************************

Sub SetBackglass()
	Dim obj

	For Each obj In VRBackglass
		obj.x = obj.x + 3
		obj.height = - obj.y + 324
		obj.y = -45 'adjusts the distance from the backglass towards the user
		obj.rotx=-87
	Next
End Sub

Dim Flasherseq
Sub Backglassblink_Timer
	Select Case Flasherseq
		Case 1:VRBGBulb36.Visible=1 : VRBGBulb38.Visible=1
		Case 2:VRBGBulb36.Visible=1 : VRBGBulb38.Visible=1
		Case 3:VRBGBulb36.Visible=1 : VRBGBulb38.Visible=1
		Case 4:VRBGBulb36.Visible=0 : VRBGBulb38.Visible=0
		Case 5:VRBGBulb36.Visible=0 : VRBGBulb38.Visible=0
		Case 6:VRBGBulb36.Visible=0 : VRBGBulb38.Visible=0
	End Select	
	Flasherseq = Flasherseq + 1
	If Flasherseq > 6 Then Flasherseq = 1
End Sub


'*********************
' Section; Debug Table testing routines.  Add to bottom of script
' 1.  Raise/Lower outlanes and drain posts by pressing 2 key
' 2.  Capture and Launch ball, Press and hold one of the buttons (W, E, R, T, Y, U, I, P) below to capture ball by flipper.  Release key to shoot ball
'     To set up shot angles, press and hold one of the buttons and use Flipper keys to adjust angle
'*********************


'
'
'' Enable Disable Outlane and Drain Blocker Wall for debug testing
'Dim BLState
'debug_BLW1.IsDropped=1:debug_BLP1.Visible=0:debug_BLR1.Visible=0: debug_BLW2.IsDropped=1:debug_BLP2.Visible=0:debug_BLR2.Visible=0: debug_BLW3.IsDropped=1:debug_BLP3.Visible=0:debug_BLR3.Visible=0
'Sub BlockerWalls
'
'	BLState = (BLState + 1) Mod 4
'	debug.print "BlockerWalls"
'	PlaySound ("Start_Button")
'	Select Case BLState:
'		Case 0
'			debug_BLW1.IsDropped=1:debug_BLP1.Visible=0:debug_BLR1.Visible=0: debug_BLW2.IsDropped=1:debug_BLP2.Visible=0:debug_BLR2.Visible=0: debug_BLW3.IsDropped=1:debug_BLP3.Visible=0:debug_BLR3.Visible=0
'		Case 1:
'			debug_BLW1.IsDropped=0:debug_BLP1.Visible=1:debug_BLR1.Visible=1: debug_BLW2.IsDropped=0:debug_BLP2.Visible=1:debug_BLR2.Visible=1: debug_BLW3.IsDropped=0:debug_BLP3.Visible=1:debug_BLR3.Visible=1
'		Case 2:
'			debug_BLW1.IsDropped=0:debug_BLP1.Visible=1:debug_BLR1.Visible=1: debug_BLW2.IsDropped=0:debug_BLP2.Visible=1:debug_BLR2.Visible=1: debug_BLW3.IsDropped=1:debug_BLP3.Visible=0:debug_BLR3.Visible=0
'		Case 3:
'			debug_BLW1.IsDropped=1:debug_BLP1.Visible=0:debug_BLR1.Visible=0: debug_BLW2.IsDropped=1:debug_BLP2.Visible=0:debug_BLR2.Visible=0: debug_BLW3.IsDropped=0:debug_BLP3.Visible=1:debug_BLR3.Visible=1
'	End Select
'End Sub
'
'Const TestMode = 1
'
'Sub TestTableKeyDownCheck (Keycode)
'
'	'************************
'	' VUK Debugging Shot Maker
''*****************************
'' INSTRUCTIONS: Put this code snippet after "If TestMode = 1 Then"   about line 4915
'
'		' Test Guide Walls for VUK testing. Uses raisable walls to direct shot to VUKs
'		If keycode = 35 Then	'H
'			If TestGuide1.Visible = False and TestGuide2.Visible = False Then
'				TestGuide1.Visible = True: TestGuide1.Collidable = True
'			Elseif TestGuide1.Visible = True Then 
'				TestGuide1.Visible = False: TestGuide1.Collidable = False
'				TestGuide2.Visible = True: TestGuide2.Collidable = True
'			Else
'				TestGuide1.Visible = False: TestGuide1.Collidable = False
'				TestGuide2.Visible = False: TestGuide2.Collidable = False
'			End If
'		End If
''****************************
'
'	'Cycle through Outlane/Centerlane blocking posts
'	'-----------------------------------------------
'	If Keycode = 3 Then	
'		BlockerWalls
'	End If
'
'	If TestMode = 1 Then
'
'		'Capture and launch ball:	
'		'	Press and hold one of the buttons (W, E, R, T, Y, U, I, P) below to capture ball by flipper.  Release key to shoot ball
'		'	In the Keydown sub, set up ball launch angle and force
'		'--------------------------------------------------------------------------------------------
'		If keycode = 17 Then kickerdebug.enabled = true: TeskKickKey = keycode	'W key
'		If keycode = 18 Then kickerdebug.enabled = true: TeskKickKey = keycode	'E key
'		If keycode = 19 Then kickerdebug.enabled = true: TeskKickKey = keycode	'R key
'		If keycode = 21 Then kickerdebug.enabled = true: TeskKickKey = keycode	'Y key
'		If keycode = 22 Then kickerdebug.enabled = true: TeskKickKey = keycode	'U key
'		If keycode = 23 Then kickerdebug.enabled = true: TeskKickKey = keycode	'I key
'		If keycode = 25 Then kickerdebug.enabled = true: TeskKickKey = keycode	'P key
'		If keycode = 30 Then kickerdebug.enabled = true: TeskKickKey = keycode	'A key
'		If keycode = 31 Then kickerdebug.enabled = true: TeskKickKey = keycode	'S key
'		If keycode = 33 Then kickerdebug.enabled = true: TeskKickKey = keycode	'F key
'		If keycode = 34 Then kickerdebug.enabled = true: TeskKickKey = keycode	'G key
'		' If testkick key is held, flipper keys can be used to change shot angle
'		If kickerdebug.enabled = true Then
'			If keycode = LeftFlipperKey Then
'				TestKickerVar = TestKickerVar - 1
'				TestKickSaveNeeded = 1
'				debug.print TestKickerVar
'
'				If TeskKickKey     = 17 Then TestKickAngleW = TestKickerVar		'W key
'
'
'				Exit Sub
'			ElseIf keycode = RightFlipperKey Then
'				TestKickerVar = TestKickerVar + 1
'				TestKickSaveNeeded = 1
'				debug.print TestKickerVar
'
'				If TeskKickKey     = 17 Then TestKickAngleW = TestKickerVar		'W key
'
'
'				Exit Sub
'			End If
'
'			
'		End If
'
'	End If
'End Sub
'
''LoadTestKickAngles
'Dim TestKickerAngle, TestKickerAngle2, TestKickerAngle3, TestKickerVar, TeskKickKey, TestKickSaveNeeded, TestKickForce
'Dim TestKickAngleW, TestKickAngleE, TestKickAngleR, TestKickAngleY, TestKickAngleU, TestKickAngleI, TestKickAngleP, TestKickAngleA
'Dim TestKickAngleS, TestKickAngleF, TestKickAngleG
'TestKickForce = 55
'TestKickerVar = 0
'TestKickAngleW = -32
'TestKickAngleE = -23
'TestKickAngleR = -18
'TestKickAngleY = -12
'TestKickAngleU = -6
'TestKickAngleI = -19
'TestKickAngleP = 7
'TestKickAngleA = 7
'TestKickAngleS = 11
'TestKickAngleF = 15 
'TestKickAngleG = 35 
'
'Sub TestTableKeyUpCheck (Keycode)
'
'	' Capture and launch ball:
'	' Press and hold one of the buttons below to capture ball by flipper.  Release to shoot ball. Set up angle and force as needed for each shot.  
'	'--------------------------------------------------------------------------------------------
'	If TestMode = 1 Then
'
'
''		If keycode = 17 Then kickerdebug.kick TestKickAngleW, TestKickForce: kickerdebug.enabled = false: SaveTestKickAngles: debug.print TestKickAngleW		'W key		
'
'		If keycode = 17 Then 	'Cycle through all left target shots																'Multi kick angle example
'			If testkickerangle = -32 then 
'				testkickerangle = -30
'			ElseIf testkickerangle = -30 Then
'				testkickerangle = -26
'			Else
'				testkickerangle = -32
'			End If
'			kickerdebug.kick testkickerangle, 50: kickerdebug.enabled = false: SaveTestKickAngles		 							'W key	
'		End If
'
'		If keycode = 18 Then kickerdebug.kick TestKickAngleE, TestKickForce: kickerdebug.enabled = false: SaveTestKickAngles		'E key		
'		If keycode = 19 Then kickerdebug.kick TestKickAngleR, TestKickForce: kickerdebug.enabled = false: SaveTestKickAngles		'R key		
'		If keycode = 21 Then kickerdebug.kick TestKickAngleY, TestKickForce: kickerdebug.enabled = false: SaveTestKickAngles		'Y key		
'		If keycode = 22 Then kickerdebug.kick TestKickAngleU, TestKickForce: kickerdebug.enabled = false: SaveTestKickAngles		'U key		
'		If keycode = 23 Then KickerDebugBall.X = 650: KickerDebugBall.Y = 1350:KickerDebugBall.Z = 25: kickerdebug.kick TestKickAngleI, TestKickForce: kickerdebug.enabled = false: SaveTestKickAngles		'I key		
'		If keycode = 25 Then 	'Cycle through all right bank target shots
'			If testkickerangle2 = 1 then 
'				testkickerangle2 = 3
'			ElseIf testkickerangle2 = 3 Then
'				testkickerangle2 = 5
'			Else
'				testkickerangle2 = 1
'			End If
'			kickerdebug.kick testkickerangle2, TestKickForce: kickerdebug.enabled = false: SaveTestKickAngles		 	'P key	
'		End If
''		If keycode = 25 Then kickerdebug.kick TestKickAngleP, TestKickForce: kickerdebug.enabled = false: SaveTestKickAngles		'P key		
'		If keycode = 30 Then kickerdebug.kick TestKickAngleA, TestKickForce: kickerdebug.enabled = false: SaveTestKickAngles		'A key	
'		If keycode = 31 Then kickerdebug.kick TestKickAngleS, TestKickForce: kickerdebug.enabled = false: SaveTestKickAngles		'S key	
'		If keycode = 33 Then kickerdebug.kick TestKickAngleF, TestKickForce: kickerdebug.enabled = false: SaveTestKickAngles		'F key	
'		If keycode = 34 Then 
'				KickerDebugBall.X = 306: KickerDebugBall.Y = 1789
'				If testkickerangle3 = 35 then 
'					testkickerangle3 = 42
'				ElseIf testkickerangle3 = 42 Then
'					testkickerangle3 = 44
'				Else
'					testkickerangle3 = 35
'				End If
'				kickerdebug.kick testkickerangle3, TestKickForce: kickerdebug.enabled = false: SaveTestKickAngles		'G key	
'		End If
'	End If
'
'End Sub
'
'Dim KickerDebugBall
'Sub Kickerdebug_Hit ()
'	Set KickerDebugBall = ActiveBall
'End Sub
'
'Sub SaveTestKickAngles
'
'End Sub

'*********************
' END OF Section; Debug Table testing routines.  Add to bottom of script


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
dim RampBalls(10,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

dim RampType(10)	'Slapped together support for multiple ramp types... False = Wire Ramp, True = Plastic Ramp

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

Sub RampRoll_Timer():RampRollUpdate:End Sub 'set to FrameTimer

Sub RampRollUpdate()		'Timer update
	dim x : for x = 1 to uBound(RampBalls)
		if Not IsEmpty(RampBalls(x,1) ) then 
			if BallVel(RampBalls(x,0) ) > 1 then ' if ball is moving, play rolling sound
				If RampType(x) then 
					PlaySound("RampLoop" & x), -1, Vol(RampBalls(x,0) )*10, AudioPan(RampBalls(x,0) )*3, 0, BallPitchV(RampBalls(x,0) ), 1, 0,AudioFade(RampBalls(x,0) )'*3
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), -1, Vol(RampBalls(x,0) )*10, AudioPan(RampBalls(x,0) )*3, 0, BallPitch(RampBalls(x,0) ), 1, 0,AudioFade(RampBalls(x,0) )'*3
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
' 					LUT
'******************************************************


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
        Case 12: LUTBox.text = "VPW original 1on1"
        Case 13: LUTBox.text = "bassgeige"
        Case 14: LUTBox.text = "blacklight"          
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

	if LUTset = "" then LUTset = 0 'failsafe

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "TFTCLUT.txt",True)
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
	If Not FileObj.FileExists(UserDirectory & "TFTCLUT.txt") then
		LUTset=12
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "TFTCLUT.txt")
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

Sub Table1_exit()
	SaveLUT
	Controller.Stop
End sub



' 002 tomate - wireramps and metal ramps prims added
' 003 tomate - correct POV, backwall added
' 004 tomate - ramps height corrected 
' 005 iaakki - NFozzy lighting script in, New PF and insert Text ramp, First inserts layed
' 006 iaakki - insert bulb primitives added for testing
' 008 iaakki - OFF insert Normal Map added
' 011 iaakki - round inserts done
' 012 iaakki - rec star inserts
' 013 iaakki - some insert adjustmens
' 014 iaakki - updated PF and text, rest of the inserts done. lamps on layer 8 needs adjustment
' 018 tomate - some new prims and textures added
' 019 tomate - rest of the new primitives and textures added (layer 1)
' 020 tomate - some tweaks on VPX stuff, low poly colidable ramps added (layer 2), some tweaks on LUTs
' 021 tomate - more tweaks on LUTs, add flippers prims to work properly, add new holes on PF mesh, tumbstone prim and VUKs works properly now, some tweaks on upper plastic, apron plastic primitive fixed
' 022 Benji - PHysics scripting in place with working flippers, no rubber dampening applied to objects yet
' 023 kingdids - some tweaks on table lighting settings
' 024 tomate - correct all the textures from camera POV, some tweaks on lighting settings and LUTs 
' 025 tomate - separate eye target and remplace original prims, separate drop targets and spinners prims (doesn't work yet), redone slingshots and separate prims of SLING1/2
' 026 sixtoe - Refactored timers, added playfield trigger and top vuk hole sides, aligned ramp entrance lamp flashers, sorted out some of the vr cabinet prims (temp invisible), 
' 027 iaakki - Checked physics code, made new collections for rubberbands and posts, added fleep sounds on most of the collisions (all other sounds removed for now), laneguides had wrong physics so ball felt spinning all the time
' 028 iaakki - RightFlipper1 added back with ssf sounds
' 029 iaakki - Plunger, drain, bumper and wire gate sounds tied. Upper right flipper live catch should be now possible
' 030 tomate - OFF textures added
' 031 iaakki - Metals and MetalRamps duplicate prims done to layers 7 and 9. Created material for fading and tied them to GIUpdates.
' 032 iaakki - some new material and cab sides tied to gi
' 033 tomate - rubbers posts and sleeves separated and applied to the collection dPosts and dSleeves respectively. Rubber bands separated and assigned to the aRubberBands collection. Physical materials assigned to rubber posts, sleeves, metal walls, plastics, and metal ramps. Correction of the location of a rubber post that invaded the loop ramp
' 034 tomate - erase original rubber bands, remplace LSling1/2 and RSling1/2 with baked prims. Cleaning g02 texture so that it looks correct when the slingshots kick
' 035 tomate - spinners and gates prims in place and hooked up, standup target and eye targets in place unhooked.
' 036 iaakki - wireramps added to gi fading, metalramp db issue fixed, ball shadows, insert text, flip shadows fixed by aadjusting Z.
' 037 iaakki - off prim D default values changed. Added more stuff to giupdates
' 038 tomate - animation of gate1 and 2 fixed. Collections created according to textures, primitives assigned to collections. All the primitives has "colormaxnoreflectionhalf" material
' 039 iaakki - rest of the OFF prims done, double checked all settings for ON and OFF prims. Dedicated materials created. GIupdates is now using UpdateMaterial with aLvl^5 for gi events. RF_plastics image imported, but not taken in use.
' 040 iaakki - created materials for each type of prims: gi_on_plastic,metals,cab,blubs and those are faded differently in giupdate. Made fading speed faster.
' 041 cyberpez - Animated standup targets. Reworked drops to be animated and Roths Double Drop mod.
' 042 iaakki - Added standup and drop target off prims and added to fading. Fixed drop target transZ to transY. Updated to rom tftc_400. UseSolenoids=2 in too.
' 043 tomate - A few textures fixed and some LUTs added
' 044 iaakki - PF GI flasher reworked, some OFF prim default DL values adjusted, MetalsOFF prim size adjustment to avoid z fighting, New on/off collections to hide primis when GI full on/off
' 045 iaakki - tombstoneOFF made to move too
' 046 iaakki - Sol4R created for right flasher, only gion state for now.
' 048 iaakki - Sol4R kind of working in gion and gioff state. Various DL values should be fixed in each state. Simple preloader done
' 049 tomate - All prims with DL=1, plastics prims replaced, top_plastic unhooked from GI, plastics_off texture modified, collideable metal walls reworked (ball now acts naturally when left VUK kicks or when falls from left spinner), collideable prim for center vuk added , center vuk strength corrected
' 050 iaakki - flipper physics restored and loophelper implemented.
' 054 iaakki - RF and LF code implemented to debug version
' 055 iaakki - textures imported back. cab_RF was broken and fixed it. Plunger lane plastic is broken on some textures.
' 058 iaakki - flasher and gi fading finalized, 68 and 71 lamps assigned, sling animation items added to fading, but removed from visibility swaps. May flicker in VR??
' 059 iaakki - new plastics prim and GI control. Changed PLAYFIELD_GI1 depth bias as it had issues with plastics. OFF flips made darker
' 060 iaakki - ball brightness fading with GI. Consts ballbrightMin and ballbrightMax are used to set the limits. 
' 061 tomate - new set of prims added, new set of texures added, prims separated and placed into collections
' 062 iaakki - fixed some DB issues, added flasherblooms, adjusted lighting
' 063 tomate - bumpers prims divided, bumperRing 1/2/3 animation done, off primitives still missing
' 064 iaakki - fixed sling DL values that were not consistent, added some magnasave button action, made some tests to pf flashers, GI bulbs still broken, some other lighting tweaks
' 065 tomate - new set of backwall textures added, slings DL values bring to 0, erase slings from g02, scatter value added to left plunger to make shots more random, modified strenght of central VUK to match with gameplay videos
' 065a tomate - increase scatter value to the left plunger, bumperRing Off prims added to Script
' 066 Sixtoe - Split ramp bulbs out of g02 and added them to DL, split bumper caps out of bumpers and added them to DL, cut holes in PF, adjusted some lights and flashers, realigned and coded VR cabinet & modes, adjusted shooter lighting and disconnected bulb from g02 prim, added under table GI lights for ball reflections,
' 067 iaakki - tombstone redone and shaking. rubberizer and targetbouncer added; not adjusted
' 068 tomate - jurassic dome OFF texture tweaked, LF and RF flasherbloom moved up, main gate texture changed, PF thickness added to hole near upper flipper, bulbs primitive separated in ---> bulbs and lamp_bulbs and placed into collections
' 073 iaakki - Merge various triggers and ballshadow code from duplicate 065 version
' 074 baldgeek - set plunger to auto, added enter/exit sounds for scoop next to left ramp
' 075 baldgeek - fixed scoop exit sound
' 076 gtxjoe - Add debug shot testing.  Press 2 to block outlanes/drain.  Press and hold any of these keys to test shots:  W, E, R, Y, U(Vuk), I(Crypt Vuk), P, A, S, F, G
' 077 iaakki - At least the ball uservalue bug fixed by having an array to carry wireramp status. Uses BallGoesWire sub. TargetBouncer taken in use for targets, posts and sleeves.
' 078 fluffhead35 - Added RampRolling sound playback logic for wire ramp sounds.  Implemented WireRampOn for sw47 & sw58 and WireRampOff for Trigger1 & Trigger2
' 079 iaakki - ramproll stuff combined to ball shadow code. Triggers reworked one more time, Right vuk ramp fixed as ball jumped over the triggers, rdampen set to 10ms timer
' 080 benji - wire ramp loop sound swapped out for more seamless one. fx_vukExit_wire sound added to manager, added to VUK subs replacing 'popper' sound needs to be tested.
' 082 benji - reimported missing droptarget sounds, changed kickback sound from missing 'popper' to new KickBack2.wav 
' 083 fluffhead35 - tweaked ramp shadow code when looking up the ramptype from Wire RampRoll variables
' 084 iaakki - major merge for new trough from cyberpez. All the solflashers and related inserts recoded. tnob increased so can debug.
' 085 iaakki - adjusted flashers and added backplate vpx flashers
' 086 fluffhead35 - merged in 082 and 083 changes.  Added more balls to the RampRoll array because it was too small for the number of balls.  Added More RampRoll and WireRampRoll sounds as you need one per ball
' 087 iaakki - targetBouncer values reduced, scoop vuk fixed
' 089 tomate - wall008 at layer 1 deleted, underVUK prim at layer 2 reimported, pUpKicker material changed and lowered it to -10, cab/pf_adges/apron prims replaced, ScSp. relfections turned off
' 090 tomate - walls over the plastics at layer1 added, sw68 - captive Ball fixed (thanks gtxjoe!), central VUK fixed (thanks apophis!)
' 091 iaakki - Crypt shot fixed, BS depth bias issue solved, plunger -> droptarget -> drain fixed, 
' 092 iaakki - PF GI flasher style changed
' 092a tomate - collideable wall tweaked so ball doesn't touch slinshot after right orbit
' 093 iaakki - fixed bugs from ball shadow code and improved perf, new PF gion/gioff images with larger hard edges on insert holes, new insert text image with edges, spot lamp tied to gi. Narnia check should bring the ball back in game.maybe.. Report your findings.
' 094 benji - add sound scoopExit. Crypt VUK exit/eject sound should be "fx_vukExit_wire" but i cannot get it to play...
' 095 iaakki - f53TOP added, wood sound added to one wall, right orb return matched to few videos, rtxfactor 0.8, right wireramp exit hacked, coin sounds added, sw52 sounds improved, ball shadows removed when large flashers bright, diverter sound added, some debug cleanup, additional wall added under trough
' 096 Sixtoe - Organised some layers, sorted height walls, deleted old redundant assets, fixed plunger exit gate textures, cropped the bottom of 2 bulb prims that were showing through holes, dropped lamp under skillshot rubber as it was clipping, manually edited all the g02 textures because dropping the lamp under the rubber meant part of it was black..., redid texture of left flasher so it doesn't look odd when off, manually edited g01_ON.png to brighten back of spinners
' 0961 iaakki - Bug fix and script optimizations for RTXBS on VR
' 097 tomate - Switch and diverter prims separeted and placed in collections. New clear plastics texture added.
' 0971 iaakki - Diverter and wireramp switches animated with sounds.
' 0974 tomate - diverter DB issue fixed, jurassic dome material changed, slanted sideblades added, cabinet mode added, POV corrected to cabinet mode
' 098 apophis - included RTX BS distance calculation optimizations
' 099 fluffhead35 - added logic to only show RTX shadows for 3 balls
' 099.1 Wylte - changed shadow DB to -2000, upped flasher intensity required to hide shadows to 0.7
' 099.9 iaakki - 991 merge, Tied GI bulbs prim to updates, minor tweak to GI levels and collections, RTXBallShadows script option added, LockBarKey support added
' 100 Sixtoe - Split bulbs from g02 primitive (again!), tidied up layers a bit, deleted redundant stuff, added off prims to cabinet script, probably still needs more work. Left captive ball wall moved so ball isn't floating, Plugged hole in upper left plastic to stop ball trap.
' 101 iaakki - VUK issue solved, sideblades fixed, cabinetmode with fblooms fixed, TargetBouncerFactor introduced and default set 1.1 for now, flippernudge values corrected, red bulbs set to fading collections, metalramp DLFB values changed to make them blend better.
' 102 iaakki - Recoded LUT selector with 14 LUT files. save/load mechanism included.
' 103 iaakki - Insert OFF state normals added and insert balance tune, HauntFreak top notch tweak to red bulb flashers.
' 104 Sixtoe - Split off bumper prims, fixed script for them
' 105 apophis - fixed never ending ball rolling sounds. fixed slingshot sound effect locations
' 106 iaakki - ball shadow DB's adjusted more, RTX image changed (Thanks BorgDog for reference), RTX parameters changed to match new reference
' 107 tomate - post pass fixed, angleSidewalls for cabinet mode fixed (thanks Sixtoe!), angleSidewalls prims reworked and put into collections, OFF plastics textures tweaked
' 108 tomate - wall added over top plastics
' 109 iaakki - cleanup, crypt jam insert material tuned, spinner sounds added, rubberizer tuned, left ramp entrance gate tuned, DT threshold tuned
' 110 iaakki - subway redone, needs more testing
' 111 iaakki - Narnia rework, cleanup, subway tuned, collidable walls added under flips
' 112 iaakki - Lut changer tuned, insert off prim normals as script option, some insert prims were accidentally collidable, relay sounds added
' 113 iaakki - Some walls added on top of plastics, GTXJoe's "Debug Table testing routines" removed
' 114 Skitso - Fixed a bunch of lights
' 115 iaakki - upped sw39 hit threshold and tested it, under PF walls added to eliminate light bleed from inserts, VRRoom 1 and 3 disables RTX ball shadows, GI relay, Lutselector and spinner sound increased
' 116 Sixtoe - Fixed a couple of VR issues with the sidewalls, spent some time with how the ball looks
' 116DL1 tomate - all prims moved to DL=1, default LUT changed, apron texture fixed, flippers textures fixed
' 117 tomate - domes/ramps/bats textures fixed, bumper cap texture fixed Skitso!, trail strenght lowered to 30, POV tweaked, reflection strenght on PF lowered to 20
' 118 iaakki - vrroom mode fixed, plunger lane gate tuned, ball made a bit brighter with const ballbrightMax and ballbrightMin, improved GI bulbs that reflect to balls, plastics DB set to -110 to fix laneguides
' RC1 iaakki - bumpertop materials changed, metals and laneguide DL values to 0.5, bumpertop ID fix, default value checks
' RC2 iaakki - Bumpertop image swaps and DL tied to GI plus lampz, sw38 improved and wall added to center the ball, script cleanup, bumper lights adjusted for GIOFF state
' RC3 tomate - plastic texture over the bumper fixed, right slingshot rubber texture fixed, logo added
' RC4 tomate - VPW original LUT fixed, HauntFreaks LUT added, compressed images by g5k added
' RC5 tomate - SSR set OFF by default, LUT added
' 1.02 iaakki - removed one material swap that can look binary; may bring other issues so testing needed. Cleanup. Made inserts to have normals for VR
' 1.03 Rawd - Added vr stuff
' 1.04 Sixtoe - High invisible apron, script cleanup
' 1.05 iaakki - ImageSwapsForFlashers script option added. GI will still fade with double prims, but flasher image swaps can be skipped. Plungerlane bulb reflections, Rubberizer updated, Redo one fading hack
' 1.06 Leojreimroc - VR Backglass redone with image by Hauntfreaks.  Flashers fade time reduced.
' 1.07 Wylte	- Shadow update (integrated into BallFX Sub), rebuilt collection, lowered digital nudge strength 50%, very minor tweak to FS PoV (x/y relation same just below 1, and Layback 65->60 to remove sidewall jaggies)
' 1.08 iaakki - NF/Roth codes updated, plastic fading redone, most of the flasher code redone, layers organiced, most of the bakes converted to webp.
' 1.09 iaakki - Roth DT's added, prim orientations are not correct, semi-transparently fading laneguides done
' 1.10 iaakki - DT prims redone, Apophis Slings added, Wall053 slightly moved and sw38 made slightly larger to eliminate issue that BountyBob found.
' 1.11 iaakki - Slings fixed and sling corner spin feature tested
' 1.13 iaakki - rest of the todos done, fading code redone one more time, but not good enough. I left debugs in, so someone else can continue.
' 1.14 tomate - new pf bakes based on g5k playfield graphics
' 1.15 iaakki - pf reflections removed from standup targets on the sides, rollover material color change, DT drop height adjustment, debugs removed, restored POV from v1.01, modlampz removed from timers, f52top color adjusted, insert fading speed adjusted
' 1.17 iaakki - PF off mesh brightness adjustment, bumpertops are finally swapping textures for flashers and tied to lampz and gi at the same time
' 1.18 iaakki - Crypt VUK ball stuck on ledge fixed, Crypt RIP collidable wall size fixed.
' 1.19 iaakki - New PF bakes from Tomate, converted some textures to webp
' 1.20 iaakki - Slope 6.5 to 6.2, reduced flip power 3300 to 3100, reduced sling power 5 to 4.5
' 1.21 iaakki - bumper hue change, pupkicker sound and color fix, flasherbloom reduced even more
' 1.22 iaakki - One ball stuck fix with Wall054

'Commands to use for testing:
'Setlamp 111, 0 'set GI OFF
'Setlamp 111, 5 'set GI OFF
'sol4r true 'flash RF
'sol2r true 'flash LF