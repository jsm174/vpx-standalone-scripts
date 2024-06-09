'                                                           
'                     /@@@@@@@@@@@@@@@#                     
'                 &@@@@@@@@@@@@@@@@@@@@@@@@                 
'               @@@@@@@@@@@@@@@@@@@@@@@@@@@@@               
'             /@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@             
'            (@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@            
'            @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@,           
'           /@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@           
'           @@@@@@@@@@@@@@@@@@@@@@@@@@@ ..... @@@           
'           @@@@@@@@@@@@@@@@@@@@@@@@@@@ @@@@& @@@           
' #@@@      **(@@@******/@@@/*******(@@ @@@@& @&*      &@@@ 
'  @@@*    .@@@@@@@@@@@@@@@@@@@@@@@@@@@ @@@@& @@@#     @@@# 
'  /@@@@@@%(.                                   ./%@@@@@@@  
'    @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@    
'        .*#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@%/.        
'                                                           
'                                                           
'                                                           
'                    The Leprechaun King                                         
'                       By: Scottywic                                    
'                                                           
'                                                           
'                          #@& %@&                          
'           /@@%(&@  @@@ @@@@@@@@@@@ *@@, @@(%@@@           
'           *   %@@@@@&@@@@%@@@@@&@@@@#@@@@@@    (          
'         .@@@  %@@@  ,@@,  @@@@@*  @@%  &@@@  @@@#         
'        @@@(@@@@(/*                      (/&@@@%&@@        
'          &@@@@@@@@                     %@@@@@@@@          
'        .@@@@@@ (@@@      %@@@@@@      #@@@ @@@@@@(        
'            @@@& %@@@@@@@@@@@@@@@@@@@@@@@@ ,@@@            
'         /@@@@@@     (@@@@@@@ @@@@@@@@   , @@@@@@@         
'        #@* (@@@  @@@@@@@@@@@ &@@@@@@@@@@* @@@@ .@&        
'          @@@@@@@,  #@@@@@@@@(@@@@@@@@&.  @@@@@@@          
'          @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@.         
'          *@@@ @@@@@@@@& @@@@@@@@@ ,@@@@@@@@ /@@&          
'               @@@@@@@@@&   @@@   (@@@@@@@@@               
'               /@@@@*@@@@@@@   &@@@@@@*@@@@@               
'                 @@@ &@@@@@@@(@@@@@@@@ %@@.                
'                      @@@.@@@@@@@,&@@/                     
'                       (@ &@@@@@@ %&                       
'                           #@@@@                           
'                             %                             


	Option Explicit
	Randomize

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Player Options
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

	dim hardmode:hardmode = 0 'put this to 0 to make tha game easier, and probably more fun
	rockmusic = 1 'change this to 1 to switch out the background music with rocking irish tunes
	soundtrackvol = 70 'Set the background audio volume to whatever you'd like out of 100
	videovol = 60 'set the volme you'd like for the videos
	sfxvol = 60 'set the volme you'd like for the videos
	calloutvol = 100 ' set this to whatever you're like your callouts to be
	turnonultradmd = 0 ' change to 1 to turn on ultradmd
	toppervideo = 0 'set to 1 to turn on the topper
	dim havedof:havedof = 0 'set to 1 if you have dof and want to remove some mech sounds as you have your own mechs



'///////////////////////////////////////////////////////////////////////////////////


	Dim GlowBall, ChooseBall, CustomBulbIntensity(10), red3(10), green3(10), Blue3(10)
	Dim CustomBallImage(10), CustomBallLogoMode(10), CustomBallDecal(10), CustomBallGlow(10)
	ChooseBall = 0



	If hardmode = 1 Then
		Primitive6.visible = 0
		Primitive6.collidable = 0
		r1.visible = 0
		r1.collidable = 0
		closekickbacks
		hardoff.enabled = 1
	end if 

	sub hardoff_timer 
		kickbackrl.intensity = 0
		kickbackll.intensity = 0		
		closekickbacks
	end Sub


                                  
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Credits
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' Compiled and built by Scottywic
' Code snippets by JP Salas, Flupper, DJRobX, NailBuster, ninuzzu, HauntFreaks rothbauerw & many more.
' I'll attempt to make ammends for anyone left out.
' For details using this framework check https://www.youtube.com/channel/UC_vroB2Uboi8GytbQSIepPQ




'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  FRAMEWORK Options
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

	Const typefont = "Roboto Bk"
	Const numberfont = "Morris Roman"
	Const zoomfont = "Fundamental  Brigade"
	Const zoombgfont = "Fundamental 3D  Brigade"
	Const colorone = "12971496"
	Const cGameName = "leprechaun"
	Const TableName = "leprechaun"
	Const myVersion = "1.0.0"
	



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  GOLDEN BALL
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 
' *** prepare the variable with references to three lights for glow ball ***
Dim Glowing(10)
Set Glowing(0) = Glowball0
Set Glowing(1) = Glowball1
Set Glowing(2) = Glowball2
Set Glowing(3) = Glowball3
Set Glowing(4) = Glowball4
Set Glowing(5) = Glowball5
Set Glowing(6) = Glowball6
Set Glowing(7) = Glowball7
Set Glowing(8) = Glowball8
Set Glowing(9) = Glowball9

' default Ball
CustomBallGlow(0) = 		False
CustomBallImage(0) = 		"pinball"
CustomBallLogoMode(0) = 	False
CustomBallDecal(0) = 		"scratches"
CustomBulbIntensity(0) = 	0.01
Red3(0) = 0 : Green3(0)	= 0 : Blue3(0) = 0

' Magma GlowBall
CustomBallGlow(2) = 		True
CustomBallImage(2) = 		"pinball"
CustomBallLogoMode(2) = 	True
CustomBallDecal(2) = 		"scratches"
CustomBulbIntensity(2) = 	0
red3(2) = 255 : Green3(2)	= 180 : Blue3(2) = 100



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  FRAMEWORK VARIABLES
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 	

	'Constructions
	Const BallSize = 50
	Const MaxPlayers = 4
	Const BallSaverTime = 10
	Const MaxMultiplier = 6 
	Const MaxMultiballs = 5
	Const bpgcurrent = 3

	' Define Global Variables

	Dim toppervideo
	Dim rockmusic
	Dim soundtrackvol
	Dim videovol
	Dim sfxvol
	Dim calloutvol
	Dim ballrolleron
	Dim turnonultradmd
	Dim turnoffrules
	Dim PlayersPlayingGame
	Dim CurrentPlayer
	Dim Credits
	Dim BonusPoints(4)
	Dim BonusHeldPoints(4)
	Dim BonusMultiplier(4)
	Dim bBonusHeld
	Dim BallsRemaining(4)
	Dim ExtraBallsAwards(4)
	Dim Score(4)
	Dim HighScore(8)
	Dim HighScoreName(8)
	Dim WaffleScore(4)
	Dim WaffleScoreName(4)
	Dim Jackpot
	Dim SuperJackpot
	Dim Tilt
	Dim TiltSensitivity
	Dim Tilted
	Dim TotalGamesPlayed
	Dim mBalls2Eject
	Dim SkillshotValue(4)
	Dim bAutoPlunger
	Dim bInstantInfo
	Dim bromconfig
	Dim bAttractMode
	Dim LastSwitchHit
	Dim BallsOnPlayfield
	Dim BallsInHole
	Dim bFreePlay
	Dim bGameInPlay
	Dim bOnTheFirstBall
	Dim bBallInPlungerLane
	Dim bBallSaverActive
	Dim bBallSaverReady
	Dim bMultiBallMode
	Dim bMusicOn
	Dim bSkillshotReady
	Dim bExtraBallWonThisBall
	Dim bJustStarted

	LoadCoreFiles
	Sub LoadCoreFiles
		On Error Resume Next
		ExecuteGlobal GetTextFile("core.vbs")
		If Err Then MsgBox "Can't open core.vbs"
		On Error Goto 0
	End Sub

	Dim EnableBallControl
	EnableBallControl = false 'Change to true to enable manual ball control (or press C in-game) via the arrow keys and B (boost movement) keys


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Controller VBS stuff, but with b2s not started
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'***Controller.vbs version 1.2***'
'

Const directory = "HKEY_CURRENT_USER\SOFTWARE\Visual Pinball\Controller\"
Dim objShell
Dim PopupMessage
Dim B2SController
Dim Controller
Const DOFContactors = 1
Const DOFKnocker = 2
Const DOFChimes = 3
Const DOFBell = 4
Const DOFGear = 5
Const DOFShaker = 6
Const DOFFlippers = 7
Const DOFTargets = 8
Const DOFDropTargets = 9
Const DOFOff = 0
Const DOFOn = 1
Const DOFPulse = 2

Dim DOFeffects(9)
Dim B2SOn
Dim B2SOnALT

Sub LoadEM
	LoadController("EM")
End Sub

Sub LoadPROC(VPMver, VBSfile, VBSver)
	LoadVBSFiles VPMver, VBSfile, VBSver
	LoadController("PROC")
End Sub

Sub LoadVPM(VPMver, VBSfile, VBSver)
	LoadVBSFiles VPMver, VBSfile, VBSver
	LoadController("VPM")
End Sub

Sub LoadVPMALT(VPMver, VBSfile, VBSver)
	LoadVBSFiles VPMver, VBSfile, VBSver
	LoadController("VPMALT")
End Sub

Sub LoadVBSFiles(VPMver, VBSfile, VBSver)
	On Error Resume Next
	If ScriptEngineMajorVersion < 5 Then MsgBox "VB Script Engine 5.0 or higher required"
	ExecuteGlobal GetTextFile(VBSfile)
	If Err Then MsgBox "Unable to open " & VBSfile & ". Ensure that it is in the same folder as this table. " & vbNewLine & Err.Description	
	InitializeOptions
End Sub

Sub LoadVPinMAME
	Set Controller = CreateObject("VPinMAME.Controller")
	If Err Then MsgBox "Can't load VPinMAME." & vbNewLine & Err.Description
	If VPMver > "" Then If Controller.Version < VPMver Or Err Then MsgBox "VPinMAME ver " & VPMver & " required."
	If VPinMAMEDriverVer < VBSver Or Err Then MsgBox VBSFile & " ver " & VBSver & " or higher required."
	On Error Goto 0
End Sub

Sub LoadController(TableType)
	Dim FileObj
	Dim DOFConfig
	Dim TextStr2
	Dim tempC
	Dim count
	Dim ISDOF
	Dim Answer
	
	B2SOn = False
	B2SOnALT = False
	tempC = 0
	on error resume next
	Set objShell = CreateObject("WScript.Shell")
	objShell.RegRead(directory & "ForceDisableB2S")
	If Err.number <> 0 Then
		PopupMessage = "This latest version of Controller.vbs stores its settings in the registry. To adjust the values, you must use VP 10.2 (or newer) and setup your configuration in the DOF section of the -Keys, Nudge and DOF- dialog of Visual Pinball."
		objShell.RegWrite directory & "ForceDisableB2S",0, "REG_DWORD"
		objShell.RegWrite directory & "DOFContactors",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFKnocker",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFChimes",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFBell",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFGear",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFShaker",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFFlippers",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFTargets",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFDropTargets",2, "REG_DWORD"
		MsgBox PopupMessage
	End If
	tempC = objShell.RegRead(directory & "ForceDisableB2S")
	DOFeffects(1)=objShell.RegRead(directory & "DOFContactors")
	DOFeffects(2)=objShell.RegRead(directory & "DOFKnocker")
	DOFeffects(3)=objShell.RegRead(directory & "DOFChimes")
	DOFeffects(4)=objShell.RegRead(directory & "DOFBell")
	DOFeffects(5)=objShell.RegRead(directory & "DOFGear")
	DOFeffects(6)=objShell.RegRead(directory & "DOFShaker")
	DOFeffects(7)=objShell.RegRead(directory & "DOFFlippers")
	DOFeffects(8)=objShell.RegRead(directory & "DOFTargets")
	DOFeffects(9)=objShell.RegRead(directory & "DOFDropTargets")
	Set objShell = nothing

	If TableType = "PROC" or TableType = "VPMALT" Then
		If TableType = "PROC" Then
			Set Controller = CreateObject("VPROC.Controller")
			If Err Then MsgBox "Can't load PROC"
		Else
			LoadVPinMAME
		End If		
		If tempC = 0 Then
			On Error Resume Next
			If Controller is Nothing Then
				Err.Clear
			Else
				Set B2SController = CreateObject("B2S.Server")
				If B2SController is Nothing Then
					Err.Clear
				Else
					B2SController.B2SName = B2ScGameName
					B2SController.Run()
					On Error Goto 0
					B2SOn = True
					B2SOnALT = True
				End If
			End If
		End If
	Else
		If tempC = 0 Then
			On Error Resume Next
			Set Controller = CreateObject("B2S.Server")
			If Controller is Nothing Then
				Err.Clear
				If TableType = "VPM" Then 
					LoadVPinMAME
				End If
			Else
				Controller.B2SName = cGameName
				If TableType = "EM" Then
					Controller.Run()
				End If
				On Error Goto 0
				B2SOn = True
			End If
		Else
			If TableType = "VPM" Then 
				LoadVPinMAME
			End If
		End If
		Set DOFConfig=Nothing
		Set FileObj=Nothing
	End If
End sub

Function SoundFX (Sound, Effect)
	If ((Effect = 0 And B2SOn) Or DOFeffects(Effect)=1) Then
		SoundFX = ""
	Else
		SoundFX = Sound
	End If
End Function

Function SoundFXDOF (Sound, DOFevent, State, Effect)
	If DOFeffects(Effect)=1 Then
		SoundFXDOF = ""
		DOF DOFevent, State
	ElseIf DOFeffects(Effect)=2 Then
		SoundFXDOF = Sound
		DOF DOFevent, State
	Else
		SoundFXDOF = Sound
	End If
End Function

Function SoundFXDOFALT (Sound, DOFevent, State, Effect)
	If DOFeffects(Effect)=1 Then
		SoundFXDOFALT = ""
		DOFALT DOFevent, State
	ElseIf DOFeffects(Effect)=2 Then
		SoundFXDOFALT = Sound
		DOFALT DOFevent, State
	Else
		SoundFXDOFALT = Sound
	End If
End Function


Sub DOF(DOFevent, State)
	If B2SOn Then
		If State = 2 Then
			Controller.B2SSetData DOFevent, 1:Controller.B2SSetData DOFevent, 0
		Else
			Controller.B2SSetData DOFevent, State
		End If
	End If
End Sub

Sub DOFALT(DOFevent, State)
	If B2SOnALT Then
		If State = 2 Then
			B2SController.B2SSetData DOFevent, 1:B2SController.B2SSetData DOFevent, 0
		Else
			B2SController.B2SSetData DOFevent, State
		End If
	End If
End Sub




'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   TABLE INITS & MATHS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  


	Sub Table1_Init()
		ChangeBall(ChooseBall)
		DMD_Init
		LoadEM
		InitPup
		resetbackglass
		Dim i
		Randomize
		Loadhs
		bAttractMode = False
		bOnTheFirstBall = False
		bBallInPlungerLane = False
		bBallSaverActive = False
		bBallSaverReady = False
		bMultiBallMode = False
		bGameInPlay = False
		bAutoPlunger = False
		bMusicOn = True
		BallsOnPlayfield = 0
		BallsInHole = 0
		LastSwitchHit = ""
		Tilt = 0
		TiltSensitivity = 6
		Tilted = False
		bBonusHeld = False
		bJustStarted = True
		GiOff
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":4, ""FS"":1 }"
		StartAttractMode
	End Sub


	'********************
	' MATHS
	'********************

	Function RndNum(min,max)
	 RndNum = Int(Rnd()*(max-min+1))+min     ' Sets a random number between min AND max
	End Function


		Dim BIP
		BIP = 0







'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   KEYS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  


	Sub Table1_KeyDown(ByVal Keycode)

		If keycode = PlungerKey Then
			if havedof = 0 then
			PlaySoundAt "fx_plungerpull", Plunger
			end if
			Plunger.Pullback
		End If

		If hsbModeActive = True Then
			EnterHighScoreKey(keycode)
			elseif bGameInPlay Then
			If keycode = LeftTiltKey Then Nudge 90, 6:if havedof = 0 then PlaySound "Fx_nudge" end if:CheckTilt
			If keycode = RightTiltKey Then Nudge 270, 6:if havedof = 0 then PlaySound "Fx_nudge" end if:CheckTilt
			If keycode = CenterTiltKey Then Nudge 0, 7:if havedof = 0 then PlaySound "Fx_nudge" end if:CheckTilt
			If NOT Tilted Then
			If keycode = LeftFlipperKey Then
				LeftFlipper.RotateToEnd
				LeftFlipper1.RotateToEnd
				PlaySound SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), 0, .67, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
				ldown = 1
				checkdown
				If bSkillshotReady = False Then
				RotateLaneLightsLeft
				RotateebLights
				End If
			End If
			If keycode = RightFlipperKey Then 
				RightFlipper.RotateToEnd
				PlaySound SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), 0, .67, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
				rdown = 1
				checkdown
				If bSkillshotReady = False Then
				RotateLaneLightsRight
				RotateebLights
				End If
			End If

			If keycode = StartGameKey Then
				If((PlayersPlayingGame <MaxPlayers) AND(bOnTheFirstBall = True) ) Then
						PlayersPlayingGame = PlayersPlayingGame + 1
						'PuPlayer.playlistplayex pCallouts,"playerone-L" & currentlep(CurrentPlayer),"",100,1
						'chilloutthemusic

						If PlayersPlayingGame = 2 Then
							PuPlayer.LabelSet pBackglass,"Play2","PLAYER 2",1,"{'mt':2,'color':16777215, 'size': 1.5, 'xpos': 93.3, 'xalign': 0}"
							pUpdateScores
						PuPlayer.playlistplayex pCallouts,"playertwo-L" & currentlep(CurrentPlayer),"",100,1
						chilloutthemusic
						End If

						If PlayersPlayingGame = 3 Then
							PuPlayer.LabelSet pBackglass,"Play3","PLAYER 3",1,"{'mt':2,'color':16777215, 'size': 1.5, 'xpos': 93.3, 'xalign': 0}"
							pUpdateScores
						PuPlayer.playlistplayex pCallouts,"playerthree-L" & currentlep(CurrentPlayer),"",100,1
						chilloutthemusic
						End If

						If PlayersPlayingGame = 4 Then
							PuPlayer.LabelSet pBackglass,"Play4","PLAYER 4",1,"{'mt':2,'color':16777215, 'size': 1.5, 'xpos': 93.3, 'xalign': 0}"
							pUpdateScores	
						PuPlayer.playlistplayex pCallouts,"playerfour-L" & currentlep(CurrentPlayer),"",100,1
						chilloutthemusic
						End If

						TotalGamesPlayed = TotalGamesPlayed + 1
						savegp
				End If
			End If
			End If
			Else
			If NOT Tilted Then

				If keycode = LeftFlipperKey Then helptime.enabled = true:DMDintroloop:introtime = 0
				If keycode = RightFlipperKey Then helptime.enabled = true:DMDintroloop:introtime = 0
					If keycode = StartGameKey Then
						If movingball = 0 then
							If(BallsOnPlayfield = 0) Then
								ResetForNewGame()
							End If
						Else
							pNote "Please Wait for","Balls to Reset"
						End If
					End If
			End If
			End If 

	'****************
	' Testing Keys  
	'****************
'	if keycode = "3" then
'		'addscore 18000000
'		bumps(CurrentPlayer) = bumps(CurrentPlayer) + 180
'		BumperRewards
'		CheckHighscore
'	End If
'
'	if keycode = "3" then
'		lm1.State = 1
'		lm2.State = 1
'		lm3.State = 1
'		lm5.State = 1
'		CheckMONSTER
'		skipscene
'	End If
	if keycode = "3" then
		addscore 1000000
	End If
	End Sub


	Sub Table1_KeyUp(ByVal keycode)
	

		If keycode = PlungerKey Then
			if havedof = 0 then PlaySoundAt "fx_plunger", Plunger end if
			Plunger.Fire
		End If

		' Table specific

		If bGameInPLay and hsbModeActive <> True Then


			If keycode = LeftFlipperKey Then
				ldown = 0
				LeftFlipper.RotateToStart
				LeftFlipper1.RotateToStart
				PlaySound SoundFXDOF("fx_flipperdown", 101, DOFOff,DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
			End If
			If keycode = RightFlipperKey Then
				rdown = 0
				RightFlipper.RotateToStart
				PlaySound SoundFXDOF("fx_flipperdown", 102, DOFOff,DOFFlippers), 0, 1, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
			End If
		Else
			If keycode = LeftFlipperKey Then helptime.enabled = false
			If keycode = RightFlipperKey Then helptime.enabled = false
		End If

	End Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   SHADOWS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  






		'*****************************************
		'	ninuzzu's	FLIPPER SHADOWS
		'*****************************************

		sub FlipperTimer_Timer()
			FlipperLSh.RotZ = LeftFlipper.currentangle
			FlipperLSh1.RotZ = LeftFlipper1.currentangle
			FlipperRSh.RotZ = RightFlipper.currentangle

		End Sub

		'*****************************************
		'	ninuzzu's	BALL SHADOW
		'*****************************************
'*********** BALL SHADOW and GLOW BALL *********************************
Dim BallShadow
BallShadow = Array (BallShadow1, BallShadow2, BallShadow3, BallShadow4, BallShadow5, BallShadow6, BallShadow7, BallShadow8)
Const anglecompensate = 15

Sub BallShadowUpdate_timer()
    Dim BOT, b
    BOT = GetBalls

	IF GlowBall Then
		For b = 1 to 9
			If GlowBall and Glowing(b).state = 1 Then Glowing(b).state = 0 End If
		Next
	End If

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
        If BOT(b).X < Table1.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 10
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 10
        End If
        ballShadow(b).Y = BOT(b).Y + 20
        If BOT(b).Z > 120 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
		If GlowBall and b <10 Then
			If Glowing(b).state = 0 Then Glowing(b).state = 1 end if
			Glowing(b).BulbHaloHeight = BOT(b).z + 26
			Glowing(b).x = BOT(b).x : Glowing(b).y = BOT(b).y + anglecompensate
		End If

    Next
End Sub

'*** change ball appearance ***

Sub ChangeBall(ballnr)
	Dim BOT, ii, col
	Table1.BallDecalMode = CustomBallLogoMode(ballnr)
	Table1.BallFrontDecal = CustomBallDecal(ballnr)
	Table1.DefaultBulbIntensityScale = CustomBulbIntensity(ballnr)
	Table1.BallImage = CustomBallImage(ballnr)
	GlowBall = CustomBallGlow(ballnr)
	For ii = 0 to 9
		col = RGB(red3(ballnr), green3(ballnr), Blue3(ballnr))
		Glowing(ii).color = col : Glowing(ii).colorfull = col 
	Next
End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   TILT
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  


	'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

	Sub CheckTilt                                    'Called when table is nudged
		Tilt = Tilt + TiltSensitivity                'Add to tilt count
		TiltDecreaseTimer.Enabled = True
		If(Tilt> TiltSensitivity) AND(Tilt <15) Then 'show a warning
			GiOff
			vpmtimer.addtimer 600, "lightsbackon '"
			'pNote "CAREFUL!","MOUTHBREATHER"
			PuPlayer.playlistplayex pBackglass,"videoscenes","Tilt_warning.mp4",videovol,1
				'PlaySound "buzz"
			'PuPlayer.playlistplayex pBackglass,"videotilt","",100,1
			DOF 131, DOFPulse
			DOF 311, DOFPulse  'DOF MX - Tilt Warning
		End if
		If Tilt> 15 Then 'If more that 15 then TILT the table
			GiOff
			vpmtimer.addtimer 1200, "lightsbackon '"
			Tilted = True
			'pNote "TILT","MOUTHBREATHER"
			PuPlayer.playlistplayex pBackglass,"videoscenes","Tilt.mp4",videovol,1
				'PlaySound "powerdownn"
			'PuPlayer.playlistplayex pBackglass,"videotilt","",100,4
			DOF 310, DOFPulse   'DOF MX - TILT
			DOF 127, DOFOff   'DOF - Beacon - OFF
			DisableTable True
			tilttableclear.enabled = true
			TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
		End If
	End Sub

	Sub lightsbackon
		GiOn
	End Sub

	Dim tilttime:tilttime = 0

	sub tilttableclear_timer
		tilttime = tilttime + 1
		Select Case tilttime
			Case 10
				tableclearing
		End Select
	End Sub

	Sub tableclearing

	End Sub

	Sub posttiltreset

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
			LightSeqTilt.Play SeqAllOff
			LeftFlipper.RotateToStart
			RightFlipper.RotateToStart
			LeftSlingshot.Disabled = 1
			RightSlingshot.Disabled = 1
			PuPlayer.playresume 4
			PuPlayer.playlistplayex pAudio,"audiomodes","clear.mp3",100,1
			PuPlayer.playlistplayex pMusic,"audioclear","clear.mp3",100, 1
		Else
			GiOn
			LightSeqTilt.StopPlay
			LeftSlingshot.Disabled = 0
			RightSlingshot.Disabled = 0
		End If
	End Sub

	Sub TiltRecoveryTimer_Timer()
		If(BallsOnPlayfield = 0) Then
			EndOfBall()
			TiltRecoveryTimer.Enabled = False
		End If
	End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   SOUND FUNCTIONS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  


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

	Sub GameTimer_Timer
		RollingUpdate
	End Sub



	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	'-> Supporting Ball Sound Functions
	'->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->
	Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
		Vol = Csng(BallVel(ball) ^2 / 2000)
	End Function

	Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
		Dim tmp
		tmp = ball.x * 2 / table1.width-1
		If tmp > 0 Then
			Pan = Csng(tmp ^10)
		Else
			Pan = Csng(-((- tmp) ^10) )
		End If
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

		Const tnob = 5 ' total number of balls
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
				If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
					rolling(b) = True
					PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
				Else
					If rolling(b) = True Then
						StopSound("fx_ballrolling" & b)
						rolling(b) = False
					End If
				End If
			Next
		End Sub

Const lob = 0   'number of locked balls

Sub RollingUpdate()
    Dim BOT, b, ballpitch
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball
    For b = lob to UBound(BOT)
        If BallVel(BOT(b) )> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b) )
            Else
                ballpitch = Pitch(BOT(b) ) * 50
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, ballpitch, 1, 0
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

		'**********************
		' Ball Collision Sound
		'**********************

		Sub OnBallBallCollision(ball1, ball2, velocity)
			PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
		End Sub

		'**************************************
		' Explanation of the collision routine
		'**************************************

		' The collision is built in VP.
		' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
		' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
		' depending of the speed of the collision.


		Sub Pins_Hit (idx)
			PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
		End Sub

		Sub Targets_Hit (idx)
			PlaySound "fx_target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
		End Sub

		Sub Metals_Thin_Hit (idx)
			PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		End Sub

		Sub Metals_Medium_Hit (idx)
			PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		End Sub

		Sub Metals2_Hit (idx)
			PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		End Sub

		Sub Gates_Hit (idx)
			PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		End Sub

		Sub Spinner_Spin
			PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
		End Sub

		Sub Rubbers_Hit(idx)
			dim finalspeed
			finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
			If finalspeed > 20 then 
				PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
			End if
			If finalspeed >= 6 AND finalspeed <= 20 then
				RandomSoundRubber()
			End If
		End Sub

		Sub Posts_Hit(idx)
			dim finalspeed
			finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
			If finalspeed > 16 then 
				PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
			End if
			If finalspeed >= 6 AND finalspeed <= 16 then
				RandomSoundRubber()
			End If
		End Sub

		Sub RandomSoundRubber()
			Select Case Int(Rnd*3)+1
				Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
				Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
				Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
			End Select
		End Sub

		Sub LeftFlipper_Collide(parm)
			RandomSoundFlipper()
		End Sub

		Sub RightFlipper_Collide(parm)
			RandomSoundFlipper()
		End Sub

		Sub RandomSoundFlipper()
			'Select Case Int(Rnd*3)+1
				'Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
				'Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
				'Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
			'End Select
		End Sub




'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   START GAME, END GANE
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

'
	Sub ResetForNewGame()
		Dim i
		bGameInPLay = True
		StopAttractMode
		GiOn
		TotalGamesPlayed = TotalGamesPlayed + 1
		savegp
		CurrentPlayer = 1
		PlayersPlayingGame = 1
		bOnTheFirstBall = True
		For i = 1 To MaxPlayers
			Score(i) = 0
			BonusPoints(i) = 0
			BonusHeldPoints(i) = 0
			BonusMultiplier(i) = 1
			BallsRemaining(i) = 3
			ExtraBallsAwards(i) = 0
		Next
		Tilt = 0
		Game_Init()
		vpmtimer.addtimer 1500, "FirstBall '"
	End Sub


	Sub EndOfGame()
		tavern3t.isdropped = 1
		tavern2t.isdropped = 1
		tavern1t.isdropped = 1
		keyt3.isdropped = 1
		keyt2.isdropped = 1	
		keyt1.isdropped = 1
		keylock.Kick 180, 15
		tavernlock.Kick 180, 15
		keepkicker.Kick 180, 15
		PuPlayer.playlistplayex pMusic,"audioclear","clear.mp3",100, 1
		'pNote "GAME OVER","PLAY AGAIN"
		PuPlayer.playlistplayex pBackglass,"videogameover","",100,1
		PuPlayer.playlistplayex pCallouts,"gameover-L" & currentlep(CurrentPlayer),"",100,1
		chilloutthemusic
		StartAttractMode
		'debug.print "End Of Game"
		introposition = 0
		bGameInPLay = False
		' just ended your game then play the end of game tune
		bJustStarted = False
		Dim i
		GiOff
	End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   ULTRADMD SCRIPTING
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

	Dim UltraDMD

	' DMD using UltraDMD calls

	Sub DMD(background, toptext, bottomtext, duration)
		If turnonultradmd = 0 then exit sub
		UltraDMD.DisplayScene00 background, toptext, 15, bottomtext, 15, 14, duration, 14
		UltraDMDTimer.Enabled = 1 'to show the score after the animation/message
	End Sub

	Sub DMDScore
	End Sub

	sub dmdscoretime
		If turnonultradmd = 0 then exit sub
		UltraDMD.SetScoreboardBackgroundImage "scoreboard-background.jpg", 15, 7
		UltraDMD.DisplayScoreboard PlayersPlayingGame, CurrentPlayer, Score(1), Score(2), Score(3), Score(4), "Player " & CurrentPlayer, "Ball " & Balls
	end sub

	Sub DMDScoreNow
		If turnonultradmd = 0 then exit sub
		DMDFlush
		DMDScore
	End Sub

	Sub DMDFLush
		If turnonultradmd = 0 then exit sub
		UltraDMDTimer.Enabled = 0
		UltraDMD.CancelRendering
	End Sub

	Sub DMDScrollCredits(background, text, duration)
		If turnonultradmd = 0 then exit sub
		UltraDMD.ScrollingCredits background, text, 15, 14, duration, 14
	End Sub

	Sub DMDId(id, background, toptext, bottomtext, duration)
		If turnonultradmd = 0 then exit sub
		UltraDMD.DisplayScene00ExwithID id, False, background, toptext, 15, 0, bottomtext, 15, 0, 14, duration, 14
	End Sub

	Sub DMDMod(id, toptext, bottomtext, duration)
		If turnonultradmd = 0 then exit sub
		UltraDMD.ModifyScene00Ex id, toptext, bottomtext, duration
	End Sub

	Sub UltraDMDTimer_Timer 'used for the attrackmode and the instant info.
		If turnonultradmd = 0 then exit sub
		If bInstantInfo Then
			InstantInfo
		ElseIf bAttractMode Then
		ElseIf NOT UltraDMD.IsRendering Then
			DMDScoreNow
		ElseIf bromconfig Then
			romconfig
		End If
	End Sub

	Sub DMD_Init
		If turnonultradmd = 0 then exit sub
		Set UltraDMD = CreateObject("UltraDMD.DMDObject")
		If UltraDMD is Nothing Then
			MsgBox "No UltraDMD found.  This table will NOT run without it."
			Exit Sub
		End If

		UltraDMD.Init
		If turnonultradmd = 0 then exit sub
		If Not UltraDMD.GetMajorVersion = 1 Then
			MsgBox "Incompatible Version of UltraDMD found."
			Exit Sub
		End If

		If UltraDMD.GetMinorVersion <1 Then
		If turnonultradmd = 0 then exit sub
			MsgBox "Incompatible Version of UltraDMD found. Please update to version 1.1 or newer."
			Exit Sub
		End If

		Dim fso:Set fso = CreateObject("Scripting.FileSystemObject")
		Dim curDir:curDir = fso.GetAbsolutePathName(".")

		Dim DirName
		DirName = curDir& "\" &TableName& ".UltraDMD"

		If Not fso.FolderExists(DirName) Then _
				Msgbox "UltraDMD userfiles directory '" & DirName & "' does not exist." & CHR(13) & "No graphic images will be displayed on the DMD"
		UltraDMD.SetProjectFolder DirName

		' wait for the animation to end
		While UltraDMD.IsRendering = True
		WEnd

	End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   Pinup Active Backglass
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

'**************************
'   PinUp Player Config
'   Change HasPuP = True if using PinUp Player Videos
'**************************

	Dim HasPup:HasPuP = True

	Dim PuPlayer

	Const pTopper=0
	Const pDMD=1
	Const pBackglass=2
	Const pPlayfield=3
	Const pMusic=4
	Const pAudio=7
	Const pCallouts=8

	Sub chilloutthemusic
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":10 }"
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":50 }"
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 7, ""FN"":11, ""VL"":10 }"
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 8, ""FN"":11, ""VL"":130 }"
		vpmtimer.addtimer 3200, "turnitbackup'"
	End Sub

	Sub turnitbackup
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(videovol)&" }"
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":"&(soundtrackvol)&" }"
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 7, ""FN"":11, ""VL"":"&(sfxvol)&" }"
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 8, ""FN"":11, ""VL"":130 }"
	End Sub

Sub InitPup	
	if HasPuP Then
	on error resume next
	Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay") 
	on error goto 0
	if not IsObject(PuPlayer) then HasPuP = False
	end If

	if HasPuP Then

	PuPlayer.Init pBackglass,cGameName
	PuPlayer.Init pMusic,cGameName
	PuPlayer.Init pAudio,cGameName
	PuPlayer.Init pCallouts,cGameName
	If toppervideo = 1 Then
	PuPlayer.Init pTopper,cGameName
	End If

	PuPlayer.SetScreenex pBackglass,0,0,0,0,0       'Set PuPlayer DMD TO Always ON    <screen number> , xpos, ypos, width, height, POPUP
	PuPlayer.SetScreenex pAudio,0,0,0,0,2
	PuPlayer.hide pAudio
	PuPlayer.SetScreenex pMusic,0,0,0,0,2
	PuPlayer.hide pMusic
	PuPlayer.SetScreenex pCallouts,0,0,0,0,2
	PuPlayer.hide pCallouts


	PuPlayer.playlistadd pAudio,"sfx", 1 , 0
	PuPlayer.playlistadd pMusic,"audioattract", 1 , 0
	PuPlayer.playlistadd pAudio,"audioevents", 1 , 0
	PuPlayer.playlistadd pMusic,"audiobg", 1 , 0
	PuPlayer.playlistadd pMusic,"audiobgrock", 1 , 0
	PuPlayer.playlistadd pMusic,"mbaudio", 1 , 0
	PuPlayer.playlistadd pCallouts,"audiocallouts", 1 , 0
	PuPlayer.playlistadd pBackglass,"backglass", 1 , 0
	PuPlayer.playlistadd pBackglass,"PuPOverlays", 1 , 0
	PuPlayer.playlistadd pBackglass,"videoattract", 1 , 0
	PuPlayer.playlistadd pBackglass,"videoscenes", 1 , 0
        PuPlayer.playlistadd pCallouts,"ballsaved-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"ballsaved-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"ballsaved-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"ballsaved-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"cheers-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"cheers-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"cheers-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"cheers-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"defeated-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"defeated-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"defeated-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"defeated-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"doublejackpot-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"doublejackpot-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"doublejackpot-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"doublejackpot-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"extraball-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"extraball-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"extraball-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"extraball-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"extraballislit-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"extraballislit-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"extraballislit-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"extraballislit-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"gameover-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"gameover-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"gameover-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"gameover-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"getthekey-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"getthekey-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"getthekey-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"getthekey-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"highscore-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"highscore-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"highscore-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"highscore-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"jackpot-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"jackpot-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"jackpot-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"jackpot-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"keyfound-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"keyfound-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"keyfound-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"keyfound-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"kickbackactivated-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"kickbackactivated-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"kickbackactivated-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"kickbackactivated-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"maxtablescore-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"maxtablescore-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"maxtablescore-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"maxtablescore-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"nays-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"nays-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"nays-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"nays-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"needthekeyfirst-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"needthekeyfirst-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"needthekeyfirst-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"needthekeyfirst-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"notpowerupyet-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"notpowerupyet-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"notpowerupyet-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"notpowerupyet-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"onemorepieceofgoldinthekeep-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"onemorepieceofgoldinthekeep-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"onemorepieceofgoldinthekeep-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"onemorepieceofgoldinthekeep-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"playerfour-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"playerfour-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"playerfour-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"playerfour-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"playerone-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"playerone-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"playerone-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"playerone-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"playerthree-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"playerthree-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"playerthree-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"playerthree-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"playertwo-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"playertwo-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"playertwo-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"playertwo-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"poweruptherainbowmachine-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"poweruptherainbowmachine-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"poweruptherainbowmachine-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"poweruptherainbowmachine-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"rainbowmachineisalmoston-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"rainbowmachineisalmoston-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"rainbowmachineisalmoston-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"rainbowmachineison-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"rainbowmachineison-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"rainbowmachineison-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"rainbowmachineison-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"replay-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"replay-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"replay-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"scoreupgradeavailable-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"scoreupgradeavailable-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"scoreupgradeavailable-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"scoreupgradeavailable-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"shootagain-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"shootagain-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"shootagain-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"skillshot-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"skillshot-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"skillshot-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"skillshot-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"stealthegold-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"stealthegold-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"stealthegold-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"stealthegold-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel2-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel2-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel2-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel2-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel3-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel3-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel3-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel3-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel4-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel4-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel4-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel4-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel5-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel5-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel5-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel5-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel6-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel6-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel6-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel6-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel7-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel7-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel7-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"tablescorelevel7-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"triplejackpot-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"triplejackpot-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"triplejackpot-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"welcometotheemeraldIsle-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"welcometotheemeraldIsle-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"welcometotheemeraldIsle-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"welcometotheemeraldIsle-L4", 1 , 0
        PuPlayer.playlistadd pCallouts,"youstolethegold-L1", 1 , 0
        PuPlayer.playlistadd pCallouts,"youstolethegold-L2", 1 , 0
        PuPlayer.playlistadd pCallouts,"youstolethegold-L3", 1 , 0
        PuPlayer.playlistadd pCallouts,"youstolethegold-L4", 1 , 0


	If toppervideo = 1 Then
 	PuPlayer.playlistadd pTopper,"topper", 1 , 0
	End If

	'Set Background video on DMD
		PuPlayer.playlistplayex pBackglass,"backglass","backglass.jpg",0,1
		If hardmode = 1 Then
		PuPlayer.playlistplayex pBackglass,"PuPOverlays","hard.png",0,1
		Else
		PuPlayer.playlistplayex pBackglass,"PuPOverlays","overlay.png",0,1
		end If
		PuPlayer.playlistplayex pBackglass,"videoscenes","Holding Screen_02.mp4",0,1
		PuPlayer.SetBackground pBackglass,1	

	End if

	If toppervideo = 1 Then
		PuPlayer.playlistplayex pTopper,"topper","topper.mp4",1,2
		PuPlayer.SetBackground pTopper,1	
	End If


	PuPlayer.LabelInit pBackglass

	'Setup Pages.  Note if you use fonts they must be in FONTS folder of the pupVideos\tablename\FONTS
	'syntax - PuPlayer.LabelNew <screen# or pDMD>,<Labelname>,<fontName>,<size%>,<colour>,<rotation>,<xAlign>,<yAlign>,<xpos>,<ypos>,<PageNum>,<visible>

	'Page 1 (default score display)
	PuPlayer.LabelNew pBackglass,"Play1",typefont,			3,colorone  ,0,0,1,23,81,1,0
	PuPlayer.LabelNew pBackglass,"Play1score",typefont,		3,colorone  ,0,0,1,31,81,1,0
	PuPlayer.LabelNew pBackglass,"Play2",typefont,			3,colorone  ,0,0,1,23,85,1,0
	PuPlayer.LabelNew pBackglass,"Play2score",typefont,		3,colorone  ,0,0,1,31,85,1,0
	PuPlayer.LabelNew pBackglass,"Play3",typefont,			3,colorone  ,0,0,1,23,89,1,0
	PuPlayer.LabelNew pBackglass,"Play3score",typefont,		3,colorone  ,0,0,1,31,89,1,0
	PuPlayer.LabelNew pBackglass,"Play4",typefont,			3,colorone  ,0,0,1,23,93,1,0
	PuPlayer.LabelNew pBackglass,"Play4score",typefont,		3,colorone  ,0,0,1,31,93,1,0
	PuPlayer.LabelNew pBackglass,"curscore2",numberfont,	7,2370617	,0,1,1,50,87,1,1
	PuPlayer.LabelNew pBackglass,"curscore",numberfont,		7,12971496	,0,1,1,50,87,1,1
	PuPlayer.LabelNew pBackglass,"Ball",typefont,			2,colorone 	,0,1,1,50,81,1,1
	PuPlayer.LabelNew pBackglass,"curplayer",typefont,		3,colorone	,0,1,1,50,93,1,1
	PuPlayer.LabelNew pBackglass,"hs",numberfont,			3,colorone 	,0,1,1,65,93,1,1
	PuPlayer.LabelNew pBackglass,"king",numberfont,			3,colorone 	,0,1,1,65,93,1,1
	PuPlayer.LabelNew pBackglass,"gp",numberfont,			3,colorone 	,0,0,1,33,89,1,1
	PuPlayer.LabelNew pBackglass,"notetitle",numberfont,	3,16777215  ,0,1,1,50,12,1,1
	PuPlayer.LabelNew pBackglass,"notecopy",typefont,		2,16777215 	,0,1,1,50,15,1,1
	PuPlayer.LabelNew pBackglass,"titlebg",numberfont,		9,2370617  	,0,1,1,50,50,1,1
	PuPlayer.LabelNew pBackglass,"title",numberfont,		9,12971496 	,0,1,1,50,50,1,1
	PuPlayer.LabelNew pBackglass,"titlebg2",numberfont,		6,2370617  	,0,1,1,50,50,1,1
	PuPlayer.LabelNew pBackglass,"title2",numberfont,		6,12971496 	,0,1,1,50,50,1,1
	PuPlayer.LabelNew pBackglass,"lefttimer2",numberfont,	7,16777215  ,0,1,1,12,88,1,1
	PuPlayer.LabelNew pBackglass,"lefttimer",numberfont,	7,16777215  ,0,1,1,12,88,1,1
	PuPlayer.LabelNew pBackglass,"righttimer2",numberfont,	8,colorone  ,0,1,1,89,87,1,1
	PuPlayer.LabelNew pBackglass,"righttimer",numberfont,	8,26316  ,0,1,1,89,87,1,1
	PuPlayer.LabelNew pBackglass,"high1name",numberfont,	5,26316  ,0,1,1,28,47,1,1
	PuPlayer.LabelNew pBackglass,"high1score",numberfont,	5,26316  ,0,1,1,42,47,1,1
	PuPlayer.LabelNew pBackglass,"high2name",numberfont,	5,26316  ,0,1,1,28,54,1,1
	PuPlayer.LabelNew pBackglass,"high2score",numberfont,	5,26316  ,0,1,1,42,54,1,1
	PuPlayer.LabelNew pBackglass,"high3name",numberfont,	5,26316  ,0,1,1,28,61,1,1
	PuPlayer.LabelNew pBackglass,"high3score",numberfont,	5,26316  ,0,1,1,42,61,1,1
	PuPlayer.LabelNew pBackglass,"high4name",numberfont,	5,26316  ,0,1,1,28,68,1,1
	PuPlayer.LabelNew pBackglass,"high4score",numberfont,	5,26316  ,0,1,1,42,68,1,1
	PuPlayer.LabelNew pBackglass,"high5name",numberfont,	5,26316  ,0,1,1,56,47,1,1
	PuPlayer.LabelNew pBackglass,"high5score",numberfont,	5,26316  ,0,1,1,70,47,1,1
	PuPlayer.LabelNew pBackglass,"high6name",numberfont,	5,26316  ,0,1,1,56,54,1,1
	PuPlayer.LabelNew pBackglass,"high6score",numberfont,	5,26316  ,0,1,1,70,54,1,1
	PuPlayer.LabelNew pBackglass,"high7name",numberfont,	5,26316  ,0,1,1,56,61,1,1
	PuPlayer.LabelNew pBackglass,"high7score",numberfont,	5,26316  ,0,1,1,70,61,1,1
	PuPlayer.LabelNew pBackglass,"high8name",numberfont,	5,26316  ,0,1,1,56,68,1,1
	PuPlayer.LabelNew pBackglass,"high8score",numberfont,	5,26316  ,0,1,1,70,68,1,1
	PuPlayer.LabelNew pBackglass,"HighScore",numberfont,	6,26316	,0,1,1,50,50,1,1
	PuPlayer.LabelNew pBackglass,"HighScoreL1",numberfont,	8,26316	,0,0,1,45,60,1,1
	PuPlayer.LabelNew pBackglass,"HighScoreL2",numberfont,	8,26316	,0,0,1,49,60,1,1
	PuPlayer.LabelNew pBackglass,"HighScoreL3",numberfont,	8,26316	,0,0,1,53,60,1,1
	PuPlayer.LabelNew pBackglass,"HighScoreL4",numberfont,	5,26316	,0,1,1,50,65,1,1
End Sub

	Sub ruleshelperon
		rulestime.enabled = 1
	End Sub

	Sub ruleshelperoff
		rulestime.enabled = 0
	End Sub

	Dim rulesposition
	rulesposition = 0

	Sub rulestime_timer
		If turnoffrules = 1 then exit sub end if
		rulesposition = rulesposition + 1
		Select Case rulesposition
		Case 1
'			PuPlayer.LabelSet pBackglass,"notetitle","Find Barb",1,""
'			PuPlayer.LabelSet pBackglass,"notecopy","Hit the B A R B lane targets to light Barb Lock.",1,""
		Case 8
'			PuPlayer.LabelSet pBackglass,"notetitle","Save Will",1,""
'			PuPlayer.LabelSet pBackglass,"notecopy","Take down the keep out barricade, then bash the targets 15 times. \r enter the upside and escape with Will.",1,""
		Case 15
'			PuPlayer.LabelSet pBackglass,"notetitle","Escape the Bad Men",1,""
'			PuPlayer.LabelSet pBackglass,"notecopy","Hit the Bad Men targets on the left and lock balls to escape the Bad Men.",1,""
		Case 24
'			PuPlayer.LabelSet pBackglass,"notetitle","Gather the Adventuring Party",1,""
'			PuPlayer.LabelSet pBackglass,"notecopy","Every shot on the table corresponds to a party member in the middle. \r Collect them all and hit castle byers for multiball.",1,""
		Case 32
'			PuPlayer.LabelSet pBackglass,"notetitle","Visit the A.V. Club",1,""
'			PuPlayer.LabelSet pBackglass,"notecopy","The A.V. Club Is where you'll find fun modes. Shoot the ramps to power up the battery. \r Enter the A.V. Club to get a mode started. Finish 2 to light extra ball.",1,""
		Case 40
'			PuPlayer.LabelSet pBackglass,"notetitle","Collect Eggos for Mystery Roll",1,""
'			PuPlayer.LabelSet pBackglass,"notecopy","The mystery is super valuable. Collect waffles to light a mystery roll in castle byers.",1,""
		Case 48
'			PuPlayer.LabelSet pBackglass,"notetitle","Need Another Kickback?",1,""
'			PuPlayer.LabelSet pBackglass,"notecopy","If your kickback is empty then fill up your lane light to reactivate it!",1,""
		Case 56
			rulesposition = 0
	End Select
	End Sub



	Sub resetbackglass
	Loadhs
	PuPlayer.LabelShowPage pBackglass,1,0,""
	PuPlayer.playlistplayex pBackglass,"videoscenes","Holding Screen_02.mp4",0,1
	'PuPlayer.playlistplayex pBackglass,"scene","layout.png",0,1
	PuPlayer.SetBackground pBackglass,1
	PuPlayer.LabelSet pBackglass,"hstitle","HIGH SCORE",1,""
	dim hsscorelbl
	Dim hsscorept
	IF HighScore(0) > HighScore(4) Then
		hsscorelbl = HighScoreName(0)
		hsscorept = FormatNumber(HighScore(0),0)
	Else
		hsscorelbl = HighScoreName(4)
		hsscorept = FormatNumber(HighScore(4),0)
	end if
	PuPlayer.LabelSet pBackglass,"hs","" & hsscorept,1,"{'mt':2,'color':12971496, 'size': 2, 'xpos': 78.3, 'xalign': 1, 'ypos': 90.3, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"king",hsscorelbl,1,"{'mt':2,'color':12971496, 'size': 2, 'xpos': 68.3, 'xalign': 1, 'ypos': 90.3, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"gptitle","GAMES",1,""
	PuPlayer.LabelSet pBackglass,"gp","" & FormatNumber(TotalGamesPlayed,0),1,"{'mt':2,'color':12971496, 'size': 2, 'xpos': 34.1, 'xalign': 1, 'ypos': 88.6, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"Ball","00",1,"{'mt':2,'color':2370617, 'size': 1.7, 'xpos': 51.8, 'xalign': 1, 'ypos': 79.8, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"lefttimer2","1-0",1,"{'mt':2,'color':12971496, 'size': 4.8, 'xpos': 12.5, 'xalign': 1, 'ypos': 89.4, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"lefttimer","1-0",1,"{'mt':2,'color':26316, 'size': 4.8, 'xpos': 12.5, 'xalign': 1, 'ypos': 89.2, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"righttimer2","" & goldstole(CurrentPlayer),1,"{'mt':2,'color':12971496, 'size': 4.8, 'xpos': 87.8, 'xalign': 1, 'ypos': 89.4, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"righttimer","" & goldstole(CurrentPlayer),1,"{'mt':2,'color':2370617, 'size': 4.8, 'xpos': 87.8, 'xalign': 1, 'ypos': 89.2, 'yalign': 1}"
	PuPlayer.playlistplayex pBackglass,"videoscenes","Holding Screen_02.mp4",0,1
	End Sub


	Dim titlepos
	titlepos = 0
	titletimer.enabled = 0
	dim title
	title = ""
	dim subtitle
	subtitle = ""

	Sub titletimer_timer
		titlepos = titlepos + 1
		Select Case titlepos
			Case 1
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':2565927, 'size': 0, 'xpos': 50, 'xalign': 1, 'ypos': 36.3, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 0, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':2565927, 'size': 0, 'xpos': 50, 'xalign': 1, 'ypos': 45.3, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 0, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 1}"
			Case 2
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':5066061, 'size': 0.6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 0.6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':5066061, 'size': 0.4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 0.4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 3
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':7960953, 'size': 1.2, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 1.2, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':7960953, 'size': 0.8, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 0.8, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 4
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':9671571, 'size': 1.8, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 1.8, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':9671571, 'size': 1.2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 1.2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 5
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':11842740, 'size': 2.4, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 2.4, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':11842740, 'size': 1.6, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 1.6, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 6
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':13224393, 'size': 3, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 3, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':13224393, 'size': 2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 7
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':14671839, 'size': 3.6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 3.6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':14671839, 'size': 2.4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 2.4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 8
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':15790320, 'size': 4.2, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 4.2, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':15790320, 'size': 2.8, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 2.8, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 9
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':16316664, 'size': 4.8, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 4.8, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':16316664, 'size': 3.2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 3.2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 10
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':16777215, 'size': 5.4, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 5.4, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':16777215, 'size': 3.6, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 3.6, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 11
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':16777215, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 6, 'xpos': 50.3, 'xalign': 1, 'ypos': 45.3, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':16777215, 'size': 4, 'xpos': 50, 'xalign': 1, 'ypos': 54, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 4, 'xpos': 50.3, 'xalign': 1, 'ypos': 54.3, 'yalign': 1}"
			Case 150
				PuPlayer.LabelSet pBackglass,"title","",1,"{'mt':2,'color':16777215, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg","",1,"{'mt':2,'color':0, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2","",1,"{'mt':2,'color':16777215, 'size': 4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2","",1,"{'mt':2,'color':0, 'size': 4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				titlepos = 0
				titletimer.enabled = 0
		End Select
	End Sub

	
	Sub pNote(msgText,msg2text)
		On Error Resume Next
		title = msgText
		subtitle = msg2text
		If titlepos = 0 Then
			titletimer.enabled = 1
		Else
			titlepos = 0
			titletimer.enabled = 1
			PuPlayer.LabelSet pBackglass,"title","",1,"{'mt':2,'color':16777215, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg","",1,"{'mt':2,'color':0, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"title2","",1,"{'mt':2,'color':16777215, 'size': 4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg2","",1,"{'mt':2,'color':0, 'size': 4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
		End If
	End Sub




	Sub currentplayerbackglass

	End Sub


	Sub scorepost_timer
		pupdatescoresnow
	end Sub

	sub pUpdateScores
	end Sub

	Sub pUpdateScoresnow
		If turnonultradmd = 1 then
			dmdscoretime
		end if
		On Error Resume Next
		PuPlayer.LabelSet pBackglass,"curscore2",FormatNumber(Score(CurrentPlayer),0),1,"{'mt':2,'color':2370617, 'size': 5, 'xpos': 50.3, 'xalign': 1, 'ypos': 87.3, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"curscore",FormatNumber(Score(CurrentPlayer),0),1,"{'mt':2,'color':12971496, 'size': 5, 'xpos': 50, 'xalign': 1, 'ypos': 87, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"curplayer","PLAYER 0" & CurrentPlayer,1,"{'mt':2,'color':12971496, 'size': 1.5, 'xpos': 50.3, 'xalign': 1, 'ypos': 94, 'yalign': 1}"
		If CurrentPlayer = 1 Then
			PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(CurrentPlayer),0),1,"{'mt':2,'color':12971496, 'size': 1.2, 'xpos': 29.3, 'xalign': 2, 'ypos': 80.4, 'yalign': 0 }"
			PuPlayer.LabelSet pBackglass,"Play1","PLAYER 01",1,"{'mt':2,'color':12971496 }"
			'make other scores red (inactive)
			If PlayersPlayingGame = 2 Then
				PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':26316, 'size': 1.2, 'xpos': 29.3, 'xalign': 2, 'ypos': 83.4, 'yalign': 0 }"
				PuPlayer.LabelSet pBackglass,"Play2","PLAYER 02",1,"{'mt':2,'color':26316, 'size': 1.2, 'xpos': 18.9, 'xalign': 0, 'ypos': 83.4, 'yalign': 0}"
			End If
			If PlayersPlayingGame = 3 Then
				PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':26316 }"
				PuPlayer.LabelSet pBackglass,"Play2","PLAYER 02",1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':26316, 'size': 1.2, 'xpos': 29.3, 'xalign': 2, 'ypos': 86.4, 'yalign': 0 }"
				PuPlayer.LabelSet pBackglass,"Play3","PLAYER 03",1,"{'mt':2,'color':26316, 'size': 1.2, 'xpos': 18.9, 'xalign': 0, 'ypos': 86.4, 'yalign': 0}"
			End If
			If PlayersPlayingGame = 4 Then
				PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':26316 }"
				PuPlayer.LabelSet pBackglass,"Play2","PLAYER 02",1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play3","PLAYER 03",1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play4score","" & FormatNumber(Score(4),0),1,"{'mt':2,'color':26316, 'size': 1.2, 'xpos': 29.3, 'xalign': 2, 'ypos': 89.2, 'yalign': 0 }"
				PuPlayer.LabelSet pBackglass,"Play4","PLAYER 04",1,"{'mt':2,'color':26316, 'size': 1.2, 'xpos': 18.9, 'xalign': 0, 'ypos': 89.2, 'yalign': 0}"
			End If
		End If
		If CurrentPlayer = 2 Then
			PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(CurrentPlayer),0),1,"{'mt':2,'color':12971496 }"
			PuPlayer.LabelSet pBackglass,"Play2","PLAYER 02",1,"{'mt':2,'color':12971496 }"
			If PlayersPlayingGame = 2 Then
				PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':26316 }"
				PuPlayer.LabelSet pBackglass,"Play1","PLAYER 01",1,"{'mt':2,'color':26316}"
			End If
			If PlayersPlayingGame = 3 Then
				PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play1","PLAYER 01",1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play3","PLAYER 03",1,"{'mt':2,'color':26316}"
			End If
			If PlayersPlayingGame = 4 Then
				PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play1","PLAYER 01",1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play3","PLAYER 03",1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play4score","" & FormatNumber(Score(4),0),1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play4","PLAYER 04",1,"{'mt':2,'color':26316}"
			End If
		End If
		If CurrentPlayer = 3 Then
			PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(Score(CurrentPlayer),0),1,"{'mt':2,'color':12971496}"
			PuPlayer.LabelSet pBackglass,"Play3","PLAYER 03",1,"{'mt':2,'color':12971496 }"
			If PlayersPlayingGame = 3 Then
				PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':26316 }"
				PuPlayer.LabelSet pBackglass,"Play1","PLAYER 01",1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play2","PLAYER 02",1,"{'mt':2,'color':26316}"
			End If
			If PlayersPlayingGame = 4 Then
				PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play1","PLAYER 01",1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play2","PLAYER 02",1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play4score","" & FormatNumber(Score(4),0),1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play4","PLAYER 04",1,"{'mt':2,'color':26316}"
			End If
		End If
		If CurrentPlayer = 4 Then
			PuPlayer.LabelSet pBackglass,"Play4score","" & FormatNumber(Score(CurrentPlayer),0),1,"{'mt':2,'color':12971496}"
			PuPlayer.LabelSet pBackglass,"Play4","PLAYER 04",1,"{'mt':2,'color':12971496 }"
			If PlayersPlayingGame = 4 Then
				PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play1","PLAYER 01",1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play2","PLAYER 02",1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':26316}"
				PuPlayer.LabelSet pBackglass,"Play3","PLAYER 03",1,"{'mt':2,'color':26316}"
			End If
		End If
	PuPlayer.LabelSet pBackglass,"Ball","0"  &  bpgcurrent - BallsRemaining(CurrentPlayer) + 1 ,1,""
	PuPlayer.LabelSet pBackglass,"lefttimer2",(bigpoints\10000) + 1 & "-" & point1\1000,1,"{'mt':2,'color':12971496, 'size': 4.8, 'xpos': 12.5, 'xalign': 1, 'ypos': 89.4, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"lefttimer",(bigpoints\10000) + 1 & "-" & point1\1000,1,"{'mt':2,'color':26316, 'size': 4.8, 'xpos': 12.5, 'xalign': 1, 'ypos': 89.2, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"righttimer2","" & goldstole(CurrentPlayer),1,"{'mt':2,'color':12971496, 'size': 4.8, 'xpos': 87.8, 'xalign': 1, 'ypos': 89.4, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"righttimer","" & goldstole(CurrentPlayer),1,"{'mt':2,'color':2370617, 'size': 4.8, 'xpos': 87.8, 'xalign': 1, 'ypos': 89.2, 'yalign': 1}"

end Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  High Scores
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

	' load em up


	Dim hschecker:hschecker = 0

	Sub Loadhs
		Dim x
		x = LoadValue(TableName, "HighScore1")
		If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 1300000 End If

		x = LoadValue(TableName, "HighScore1Name")
		If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "SCO" End If

		x = LoadValue(TableName, "HighScore2")
		If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 1200000 End If

		x = LoadValue(TableName, "HighScore2Name")
		If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "TTY" End If

		x = LoadValue(TableName, "HighScore3")
		If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 1100000 End If

		x = LoadValue(TableName, "HighScore3Name")
		If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "WIC" End If

		x = LoadValue(TableName, "HighScore4")
		If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 1000000 End If

		x = LoadValue(TableName, "HighScore4Name")
		If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "NM1" End If

		x = LoadValue(TableName, "HighScore5")
		If(x <> "") Then HighScore(4) = CDbl(x) Else HighScore(4) = 1300000 End If

		x = LoadValue(TableName, "HighScore5Name")
		If(x <> "") Then HighScoreName(4) = x Else HighScoreName(4) = " I " End If

		x = LoadValue(TableName, "HighScore6")
		If(x <> "") then HighScore(5) = CDbl(x) Else HighScore(5) = 1200000 End If

		x = LoadValue(TableName, "HighScore6Name")
		If(x <> "") then HighScoreName(5) = x Else HighScoreName(5) = "GOT" End If

		x = LoadValue(TableName, "HighScore7")
		If(x <> "") then HighScore(6) = CDbl(x) Else HighScore(6) = 1100000 End If

		x = LoadValue(TableName, "HighScore7Name")
		If(x <> "") then HighScoreName(6) = x Else HighScoreName(6) = "THE" End If

		x = LoadValue(TableName, "HighScore8")
		If(x <> "") then HighScore(7) = CDbl(x) Else HighScore(7) = 1000000 End If

		x = LoadValue(TableName, "HighScore8Name")
		If(x <> "") then HighScoreName(7) = x Else HighScoreName(7) = "GLD" End If

		x = LoadValue(TableName, "Credits")
		If(x <> "") then Credits = CInt(x) Else Credits = 0 End If

		x = LoadValue(TableName, "TotalGamesPlayed")
		If(x <> "") then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 End If

		If hschecker = 0 Then
		checkorder
		End If
	End Sub

	Dim hs3,hs2,hs1,hs0,hsn3,hsn2,hsn1,hsn0
	Dim hs4,hs5,hs6,hs7,hsn4,hsn5,hsn6,hsn7


	Sub checkorder
		hschecker = 1
		'regular
		hs3 = HighScore(3)
		hs2 = HighScore(2)
		hs1 = HighScore(1)
		hs0 = HighScore(0)
		hsn3 = HighScoreName(3)
		hsn2 = HighScoreName(2)
		hsn1 = HighScoreName(1)
		hsn0 = HighScoreName(0)

		If hs3 > hs0 Then
			HighScore(0) = hs3
			HighScoreName(0) = hsn3	
			HighScore(1) = hs0
			HighScoreName(1) = hsn0	
			HighScore(2) = hs1
			HighScoreName(2) = hsn1	
			HighScore(3) = hs2
			HighScoreName(3) = hsn2
		ElseIf hs3 > hs1 Then
			HighScore(0) = hs0
			HighScoreName(0) = hsn0	
			HighScore(1) = hs3
			HighScoreName(1) = hsn3	
			HighScore(2) = hs1
			HighScoreName(2) = hsn1	
			HighScore(3) = hs2
			HighScoreName(3) = hsn2
		ElseIf hs3 > hs2 Then
			HighScore(0) = hs0
			HighScoreName(0) = hsn0	
			HighScore(1) = hs1
			HighScoreName(1) = hsn1	
			HighScore(2) = hs3
			HighScoreName(2) = hsn3	
			HighScore(3) = hs2
			HighScoreName(3) = hsn2
		ElseIf hs3 < hs2 Then
			HighScore(0) = hs0
			HighScoreName(0) = hsn0	
			HighScore(1) = hs1
			HighScoreName(1) = hsn1	
			HighScore(2) = hs2
			HighScoreName(2) = hsn2	
			HighScore(3) = hs3
			HighScoreName(3) = hsn3
		End If

		'hardmode
		hs4 = HighScore(4)
		hs5 = HighScore(5)
		hs6 = HighScore(6)
		hs7 = HighScore(7)
		hsn4 = HighScoreName(4)
		hsn5 = HighScoreName(5)
		hsn6 = HighScoreName(6)
		hsn7 = HighScoreName(7)

		If hs7 > hs4 Then
			HighScore(4) = hs7
			HighScoreName(4) = hsn7	
			HighScore(5) = hs4
			HighScoreName(5) = hsn4	
			HighScore(6) = hs5
			HighScoreName(6) = hsn5	
			HighScore(7) = hs6
			HighScoreName(7) = hsn6
		ElseIf hs7 > hs5 Then
			HighScore(4) = hs4
			HighScoreName(4) = hsn4	
			HighScore(5) = hs7
			HighScoreName(5) = hsn7	
			HighScore(6) = hs5
			HighScoreName(6) = hsn5	
			HighScore(7) = hs6
			HighScoreName(7) = hsn6
		ElseIf hs7 > hs6 Then
			HighScore(4) = hs4
			HighScoreName(4) = hsn4	
			HighScore(5) = hs5
			HighScoreName(5) = hsn5	
			HighScore(6) = hs7
			HighScoreName(6) = hsn7	
			HighScore(7) = hs6
			HighScoreName(7) = hsn6
		ElseIf hs7 < hs6 Then
			HighScore(4) = hs4
			HighScoreName(4) = hsn4	
			HighScore(5) = hs5
			HighScoreName(5) = hsn5	
			HighScore(6) = hs6
			HighScoreName(6) = hsn6	
			HighScore(7) = hs7
			HighScoreName(7) = hsn7
		End If
		savehs
	End Sub


	Sub Savehs
		SaveValue TableName, "HighScore1", HighScore(0)
		SaveValue TableName, "HighScore1Name", HighScoreName(0)
		SaveValue TableName, "HighScore2", HighScore(1)
		SaveValue TableName, "HighScore2Name", HighScoreName(1)
		SaveValue TableName, "HighScore3", HighScore(2)
		SaveValue TableName, "HighScore3Name", HighScoreName(2)
		SaveValue TableName, "HighScore4", HighScore(3)
		SaveValue TableName, "HighScore4Name", HighScoreName(3)
		'hardmode
		SaveValue TableName, "HighScore5", HighScore(4)
		SaveValue TableName, "HighScore5Name", HighScoreName(4)
		SaveValue TableName, "HighScore6", HighScore(5)
		SaveValue TableName, "HighScore6Name", HighScoreName(5)
		SaveValue TableName, "HighScore7", HighScore(6)
		SaveValue TableName, "HighScore7Name", HighScoreName(6)
		SaveValue TableName, "HighScore8", HighScore(7)
		SaveValue TableName, "HighScore8Name", HighScoreName(7)
	End Sub


	Sub Savegp
		SaveValue TableName, "TotalGamesPlayed", TotalGamesPlayed
		vpmtimer.addtimer 1000, "Loadhs'"
	End Sub


	' Initials

	Dim hsbModeActive:hsbModeActive = False
	Dim hsEnteredName
	Dim hsEnteredDigits(3)
	Dim hsCurrentDigit
	Dim hsValidLetters
	Dim hsCurrentLetter
	Dim hsLetterFlash

	' Check the scores to see if you got one

	Sub CheckHighscore()
		Dim tmp
		tmp = Score(CurrentPlayer)
		if hardmode = 0 Then
			If tmp > HighScore(3) Then
				AwardSpecial
				vpmtimer.addtimer 2000, "PlaySound ""vo_contratulationsgreatscore"" '"
				HighScore(3) = tmp
				'enter player's name
				HighScoreEntryInit()
				DOF 403, DOFPulse   'DOF MX - Hi Score
			Else
				EndOfBallComplete
			End If
		Else
			If tmp > HighScore(7) Then
				AwardSpecial
				vpmtimer.addtimer 2000, "PlaySound ""vo_contratulationsgreatscore"" '"
				HighScore(7) = tmp
				'enter player's name
				HighScoreEntryInit()
				DOF 403, DOFPulse   'DOF MX - Hi Score
			Else
				EndOfBallComplete
			End If
		end if 
	End Sub





	Sub HighScoreEntryInit()
		hsbModeActive = True
		PlaySound "vo_enteryourinitials"

		hsEnteredDigits(1) = "A"
		hsEnteredDigits(2) = " "
		hsEnteredDigits(3) = " "

		hsCurrentDigit = 1

		'pNote "YOU GOT","A HIGH SCORE!"
		PuPlayer.playlistplayex pCallouts,"highscore-L" & currentlep(CurrentPlayer),"",100,1
		chilloutthemusic
		PuPlayer.playlistplayex pBackglass,"videoscenes","High_score.mp4",videovol,1
		PuPlayer.SetLoop 2,1
		'vpmtimer.addtimer 4000, "hshold '"
		Playsound "knocker"
		flasherspop amber,"crazy"
		HighScoreDisplayName()
		HighScorelabels

	End Sub

	Sub hshold
			PuPlayer.playlistplayex pBackglass,"videoscenes","High-Score-book.jpg",videovol,1
			PuPlayer.SetLoop 2,1
			
	End Sub

	' flipper moving around the letters

	Sub EnterHighScoreKey(keycode)
		If keycode = LeftFlipperKey Then
			Playsound "frankhit"
				If hsletter = 0 Then
					hsletter = 26
				Else
					hsLetter = hsLetter - 1
				End If
				HighScoreDisplayName()
		End If

		If keycode = RightFlipperKey Then
			Playsound "frankhit"
				If hsletter = 26 Then
					hsletter = 0
				Else
					hsLetter = hsLetter + 1
				End If
				HighScoreDisplayName()
		End If

		If keycode = StartGameKey or keycode = PlungerKey Then
			PlaySound "success"
				If hsCurrentDigit = 3 Then
					If hsletter = 0 Then
						hsCurrentDigit = hsCurrentDigit -1
					Else
						assignletter
						vpmtimer.addtimer 700, "HighScoreCommitName()'"
						PuPlayer.playlistplayex pBackglass,"videobarb","clear.mov",100,7
					End If
				End If
				If hsCurrentDigit < 3 Then
					If hsletter = 0 Then
						If hsCurrentDigit = 1 Then
						Else
							hsCurrentDigit = hsCurrentDigit -1
						End If
					Else
						assignletter
						hsCurrentDigit = hsCurrentDigit + 1
						HighScoreDisplayName()

					End If
				End If
		End if
	End Sub

	Dim hsletter
	hsletter = 1

	dim hsdigit:hsdigit = 1

	Sub assignletter
		if hscurrentdigit = 1 Then
			hsdigit = 1
		End If
		if hscurrentdigit = 2 Then
			hsdigit = 2
		End If
		if hscurrentdigit = 3 Then
			hsdigit = 3
		End If
		If hsletter = 1 Then 
			hsEnteredDigits(hsdigit) = "A"
		End If
		If hsletter = 2 Then 
			hsEnteredDigits(hsdigit) = "B"
		End If
		If hsletter = 3 Then 
			hsEnteredDigits(hsdigit) = "C"
		End If
		If hsletter = 4 Then 
			hsEnteredDigits(hsdigit) = "D"
		End If
		If hsletter = 5 Then 
			hsEnteredDigits(hsdigit) = "E"
		End If
		If hsletter = 6 Then 
			hsEnteredDigits(hsdigit) = "F"
		End If
		If hsletter = 7 Then 
			hsEnteredDigits(hsdigit) = "G"
		End If
		If hsletter = 8 Then 
			hsEnteredDigits(hsdigit) = "H"
		End If
		If hsletter = 9 Then 
			hsEnteredDigits(hsdigit) = "I"
		End If
		If hsletter = 10 Then 
			hsEnteredDigits(hsdigit) = "J"
		End If
		If hsletter = 11 Then 
			hsEnteredDigits(hsdigit) = "K"
		End If
		If hsletter = 12 Then 
			hsEnteredDigits(hsdigit) = "L"
		End If
		If hsletter = 13 Then 
			hsEnteredDigits(hsdigit) = "M"
		End If
		If hsletter = 14 Then 
			hsEnteredDigits(hsdigit) = "N"
		End If
		If hsletter = 15 Then 
			hsEnteredDigits(hsdigit) = "O"
		End If
		If hsletter = 16 Then 
			hsEnteredDigits(hsdigit) = "P"
		End If
		If hsletter = 17 Then 
			hsEnteredDigits(hsdigit) = "Q"
		End If
		If hsletter = 18 Then 
			hsEnteredDigits(hsdigit) = "R"
		End If
		If hsletter = 19 Then 
			hsEnteredDigits(hsdigit) = "S"
		End If
		If hsletter = 20 Then 
			hsEnteredDigits(hsdigit) = "T"
		End If
		If hsletter = 21 Then 
			hsEnteredDigits(hsdigit) = "U"
		End If
		If hsletter = 22 Then 
			hsEnteredDigits(hsdigit) = "V"
		End If
		If hsletter = 23 Then 
			hsEnteredDigits(hsdigit) = "W"
		End If
		If hsletter = 24 Then 
			hsEnteredDigits(hsdigit) = "X"
		End If
		If hsletter = 25 Then 
			hsEnteredDigits(hsdigit) = "Y"
		End If
		If hsletter = 26 Then 
			hsEnteredDigits(hsdigit) = "Z"
		End If

	End Sub

	Sub HighScorelabels
		'PuPlayer.LabelSet pBackglass,"HighScore","YOU GOT A\rHIGH SCORE!",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL1","A",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL2"," ",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL3"," ",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL4",Score(CurrentPlayer),1,""
		hsletter = 1
	End Sub

	Sub HighScoreDisplayName()

		Select case hsLetter
		Case 0
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","<",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","<",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","<",1,""
		Case 1
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","A",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","A",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","A",1,""
		Case 2
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","B",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","B",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","B",1,""
		Case 3
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","C",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","C",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","C",1,""
		Case 4
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","D",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","D",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","D",1,""
		Case 5
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","E",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","E",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","E",1,""
		Case 6
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","F",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","F",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","F",1,""
		Case 7
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","G",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","G",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","G",1,""
		Case 8
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","H",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","H",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","H",1,""
		Case 9
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","I",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","I",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","I",1,""
		Case 10
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","J",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","J",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","J",1,""
		Case 11
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","K",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","K",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","K",1,""
		Case 12
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","L",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","L",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","L",1,""
		Case 13
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","M",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","M",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","M",1,""
		Case 14
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","N",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","N",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","N",1,""
		Case 15
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","O",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","O",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","O",1,""
		Case 16
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","P",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","P",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","P",1,""
		Case 17
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","Q",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","Q",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","Q",1,""
		Case 18
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","R",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","R",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","R",1,""
		Case 19
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","S",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","S",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","S",1,""
		Case 20
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","T",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","T",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","T",1,""
		Case 21
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","U",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","U",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","U",1,""
		Case 22
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","V",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","V",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","V",1,""
		Case 23
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","W",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","W",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","W",1,""
		Case 24
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","X",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","X",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","X",1,""
		Case 25
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","Y",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","Y",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","Y",1,""
		Case 26
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","Z",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","Z",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","Z",1,""
		End Select
	End Sub

	' post the high score letters


	Sub HighScoreCommitName()
		PuPlayer.playlistplayex pBackglass,"videoscenes","Holding Screen_02.mp4",100,7
		hsEnteredName = hsEnteredDigits(1) & hsEnteredDigits(2) & hsEnteredDigits(3)
		If hardmode = 0 Then
			HighScoreName(3) = hsEnteredName
		Else
			HighScoreName(7) = hsEnteredName
		End if
		checkorder
		EndOfBallComplete()
		PuPlayer.LabelSet pBackglass,"HighScore","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL1","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL2"," ",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL3"," ",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL4"," ",1,""
			PuPlayer.LabelSet pBackglass,"high1name","",1,""
			PuPlayer.LabelSet pBackglass,"high1score","",1,""
			PuPlayer.LabelSet pBackglass,"high2name","",1,""
			PuPlayer.LabelSet pBackglass,"high2score","",1,""
			PuPlayer.LabelSet pBackglass,"high3name","",1,""
			PuPlayer.LabelSet pBackglass,"high3score","",1,""
			PuPlayer.LabelSet pBackglass,"high4name","",1,""
			PuPlayer.LabelSet pBackglass,"high4score","",1,""
			PuPlayer.LabelSet pBackglass,"high5name","",1,""
			PuPlayer.LabelSet pBackglass,"high5score","",1,""
			PuPlayer.LabelSet pBackglass,"high6name","",1,""
			PuPlayer.LabelSet pBackglass,"high6score","",1,""
			PuPlayer.LabelSet pBackglass,"high7name","",1,""
			PuPlayer.LabelSet pBackglass,"high7score","",1,""
			PuPlayer.LabelSet pBackglass,"high8name","",1,""
			PuPlayer.LabelSet pBackglass,"high8score","",1,""
		hsbModeActive = False
	End Sub




'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  ATTRACT MODE
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 


	Dim introposition
	introposition = 0

	Sub DMDintroloop
		PuPlayer.LabelSet pBackglass,"modetitle","",1,"{'mt':2,'color':16777215, 'size': 0, 'xpos': 80.7, 'xalign': 1, 'ypos': 72.6, 'yalign': 0}"
		introtime = 0	
		introposition = introposition + 1
		Select Case introposition
		Case 1
		PuPlayer.LabelSet pBackglass,"HighScore","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL1","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL2","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL3","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL4","",1,""
			PuPlayer.LabelSet pBackglass,"high5name","",1,""
			PuPlayer.LabelSet pBackglass,"high5score","",1,""
			PuPlayer.LabelSet pBackglass,"high6name","",1,""
			PuPlayer.LabelSet pBackglass,"high6score","",1,""
			PuPlayer.LabelSet pBackglass,"high7name","",1,""
			PuPlayer.LabelSet pBackglass,"high7score","",1,""
			PuPlayer.LabelSet pBackglass,"high8name","",1,""
			PuPlayer.LabelSet pBackglass,"high8score","",1,""

			PuPlayer.playlistplayex pBackglass,"videoscenes","Holding Screen_04_ Logo.mp4",videovol,1
			PuPlayer.LabelSet pBackglass,"high1name","",1,""
			PuPlayer.LabelSet pBackglass,"high1score","",1,""
			PuPlayer.LabelSet pBackglass,"high2name","",1,""
			PuPlayer.LabelSet pBackglass,"high2score","",1,""
			PuPlayer.LabelSet pBackglass,"high3name","",1,""
			PuPlayer.LabelSet pBackglass,"high3score","",1,""
			PuPlayer.LabelSet pBackglass,"high4name","",1,""
			PuPlayer.LabelSet pBackglass,"high4score","",1,""
			PuPlayer.LabelSet pBackglass,"high5name","",1,""
			PuPlayer.LabelSet pBackglass,"high5score","",1,""
			PuPlayer.LabelSet pBackglass,"high6name","",1,""
			PuPlayer.LabelSet pBackglass,"high6score","",1,""
			PuPlayer.LabelSet pBackglass,"high7name","",1,""
			PuPlayer.LabelSet pBackglass,"high7score","",1,""
			PuPlayer.LabelSet pBackglass,"high8name","",1,""
			PuPlayer.LabelSet pBackglass,"high8score","",1,""
		Case 2
		loadhs
		PuPlayer.LabelSet pBackglass,"HighScore","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL1","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL2","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL3","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL4","",1,""
			PuPlayer.playlistplayex pBackglass,"videoscenes","High-Score-book.mp4",videovol,1
			PuPlayer.LabelSet pBackglass,"high1name",HighScoreName(0),1,""
			PuPlayer.LabelSet pBackglass,"high1score",FormatNumber(HighScore(0),0),1,""
			PuPlayer.LabelSet pBackglass,"high2name",HighScoreName(1),1,""
			PuPlayer.LabelSet pBackglass,"high2score",FormatNumber(HighScore(1),0),1,""
			PuPlayer.LabelSet pBackglass,"high3name",HighScoreName(2),1,""
			PuPlayer.LabelSet pBackglass,"high3score",FormatNumber(HighScore(2),0),1,""
			PuPlayer.LabelSet pBackglass,"high4name",HighScoreName(3),1,""
			PuPlayer.LabelSet pBackglass,"high4score",FormatNumber(HighScore(3),0),1,""
			PuPlayer.LabelSet pBackglass,"high5name",HighScoreName(4),1,""
			PuPlayer.LabelSet pBackglass,"high5score",FormatNumber(HighScore(4),0),1,""
			PuPlayer.LabelSet pBackglass,"high6name",HighScoreName(5),1,""
			PuPlayer.LabelSet pBackglass,"high6score",FormatNumber(HighScore(5),0),1,""
			PuPlayer.LabelSet pBackglass,"high7name",HighScoreName(6),1,""
			PuPlayer.LabelSet pBackglass,"high7score",FormatNumber(HighScore(6),0),1,""
			PuPlayer.LabelSet pBackglass,"high8name",HighScoreName(7),1,""
			PuPlayer.LabelSet pBackglass,"high8score",FormatNumber(HighScore(7),0),1,""


		Case 3
			PuPlayer.playlistplayex pBackglass,"videoscenes","Holding Screen_01.mp4",videovol,1
		PuPlayer.LabelSet pBackglass,"HighScore","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL1","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL2","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL3","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL4","",1,""
			PuPlayer.LabelSet pBackglass,"high1name","",1,""
			PuPlayer.LabelSet pBackglass,"high1score","",1,""
			PuPlayer.LabelSet pBackglass,"high2name","",1,""
			PuPlayer.LabelSet pBackglass,"high2score","",1,""
			PuPlayer.LabelSet pBackglass,"high3name","",1,""
			PuPlayer.LabelSet pBackglass,"high3score","",1,""
			PuPlayer.LabelSet pBackglass,"high4name","",1,""
			PuPlayer.LabelSet pBackglass,"high4score","",1,""
			PuPlayer.LabelSet pBackglass,"high5name","",1,""
			PuPlayer.LabelSet pBackglass,"high5score","",1,""
			PuPlayer.LabelSet pBackglass,"high6name","",1,""
			PuPlayer.LabelSet pBackglass,"high6score","",1,""
			PuPlayer.LabelSet pBackglass,"high7name","",1,""
			PuPlayer.LabelSet pBackglass,"high7score","",1,""
			PuPlayer.LabelSet pBackglass,"high8name","",1,""
			PuPlayer.LabelSet pBackglass,"high8score","",1,""

		Case 4
			PuPlayer.playlistplayex pBackglass,"videoscenes","Holding Screen_02.mp4",videovol,1
		PuPlayer.LabelSet pBackglass,"HighScore","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL1","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL2","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL3","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL4","",1,""
			PuPlayer.LabelSet pBackglass,"high5name","",1,""
			PuPlayer.LabelSet pBackglass,"high5score","",1,""
			PuPlayer.LabelSet pBackglass,"high6name","",1,""
			PuPlayer.LabelSet pBackglass,"high6score","",1,""
			PuPlayer.LabelSet pBackglass,"high7name","",1,""
			PuPlayer.LabelSet pBackglass,"high7score","",1,""
			PuPlayer.LabelSet pBackglass,"high8name","",1,""
			PuPlayer.LabelSet pBackglass,"high8score","",1,""

		Case 5
			PuPlayer.playlistplayex pBackglass,"videoscenes","rules.mp4",videovol,1
		PuPlayer.LabelSet pBackglass,"HighScore","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL1","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL2","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL3","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL4","",1,""
			PuPlayer.LabelSet pBackglass,"high5name","",1,""
			PuPlayer.LabelSet pBackglass,"high5score","",1,""
			PuPlayer.LabelSet pBackglass,"high6name","",1,""
			PuPlayer.LabelSet pBackglass,"high6score","",1,""
			PuPlayer.LabelSet pBackglass,"high7name","",1,""
			PuPlayer.LabelSet pBackglass,"high7score","",1,""
			PuPlayer.LabelSet pBackglass,"high8name","",1,""
			PuPlayer.LabelSet pBackglass,"high8score","",1,""
			introposition = 0
	End Select
	End Sub


	Dim introtime
	introtime = 0

	Sub intromover_timer
		introtime = introtime + 1
		If introposition = 1 Then
			If introtime = 5 Then
				DMDintroloop
			End If
		End If
		If introposition = 2 Then
			If introtime = 5 Then
				DMDintroloop
			End If
		End If
		If introposition = 3 Then
			If introtime = 5 Then
				DMDintroloop
			End If
		End If
		If introposition = 4 Then
			If introtime = 5 Then
				DMDintroloop
			End If
		End If
		If introposition = 0 Then
			If introtime = 20 Then
				introposition = 0
				DMDintroloop
			End If
		End If
	End Sub


	Sub StartAttractMode()
		If rockmusic = 1 Then
		PuPlayer.playlistplayex pMusic,"audiobgrock","",soundtrackvol,1	
		Else
		PuPlayer.playlistplayex pMusic,"audiobg","",soundtrackvol,1
		end If
		PuPlayer.SetLoop 4,1
		DOF 323, DOFOn   'DOF MX - Attract Mode ON
		bAttractMode = True
		UltraDMDTimer.Enabled = 1
		StartLightSeq
		'ShowTableInfo
		DMDintroloop
		StartRainbow alights
		StartRainbow2 GI
		StartRainbow3 aapron
		DMDattract.Enabled = 1
		intromover.enabled = true
		ruleshelperoff
	End Sub

	Sub StopAttractMode()
		DOF 323, DOFOff   'DOF MX - Attract Mode Off
		bAttractMode = False
		DMDScoreNow
		LightSeqAttract.StopPlay
		LightSeqAttract2.StopPlay
		StopRainbow alights
		StopRainbow2 GI
		ResetAllLightsColor
		DMDattract.Enabled = 0
		intromover.enabled = false
		
	'StopSong
	End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  CINEMATIC SKIPPING
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  


	Dim ldown:ldown = 0
	Dim rdown:rdown = 0

	Sub checkdown
		If ldown + rdown = 2 Then
			skipscene
		End If
	End Sub

	Sub skipscene

	End Sub






'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  LIGHTING / RAINBOW LIGHTS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 


	'********************************************************************************************
	' Only for VPX 10.2 and higher.
	' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
	' When TotalPeriod done, light or flasher will be set to FinalState value where
	' Final State values are:   0=Off, 1=On, 2=Return to previous State
	'********************************************************************************************

	Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState) 'thanks gtxjoe for the first version

		If TypeName(MyLight) = "Light" Then

			If FinalState = 2 Then
				FinalState = MyLight.State 'Keep the current light state
			End If
			MyLight.BlinkInterval = BlinkPeriod
			MyLight.Duration 2, TotalPeriod, FinalState
		ElseIf TypeName(MyLight) = "Flasher" Then

			Dim steps

			' Store all blink information
			steps = Int(TotalPeriod / BlinkPeriod + .5) 'Number of ON/OFF steps to perform
			If FinalState = 2 Then                      'Keep the current flasher state
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
	' 10 colors: red, orange, amber, yellow...
	'******************************************
	' in this table this colors are use to keep track of the progress during the acts and battles

	'colors
	Dim red, orange, amber, yellow, darkgreen, green, blue, darkblue, purple, white, base

	red = 10
	orange = 9
	amber = 8
	yellow = 7
	darkgreen = 6
	green = 5
	blue = 4
	darkblue = 3
	purple = 2
	white = 1
	base = 11

	Sub SetLightColor(n, col, stat)
		Select Case col
			Case red
				n.color = RGB(18, 0, 0)
				n.colorfull = RGB(255, 0, 0)
			Case orange
				n.color = RGB(18, 3, 0)
				n.colorfull = RGB(255, 64, 0)
			Case amber
				n.color = RGB(193, 49, 0)
				n.colorfull = RGB(255, 153, 0)
			Case yellow
				n.color = RGB(18, 18, 0)
				n.colorfull = RGB(255, 255, 0)
			Case darkgreen
				n.color = RGB(0, 8, 0)
				n.colorfull = RGB(0, 64, 0)
			Case green
				n.color = RGB(0, 18, 0)
				n.colorfull = RGB(0, 255, 0)
			Case blue
				n.color = RGB(0, 18, 18)
				n.colorfull = RGB(0, 255, 255)
			Case darkblue
				n.color = RGB(0, 8, 8)
				n.colorfull = RGB(0, 64, 64)
			Case purple
				n.color = RGB(128, 0, 128)
				n.colorfull = RGB(255, 0, 255)
			Case white
				n.color = RGB(255, 252, 224)
				n.colorfull = RGB(193, 91, 0)
			Case base
				n.color = RGB(255, 197, 143)
				n.colorfull = RGB(255, 255, 236)
		End Select
		If stat <> -1 Then
			n.State = 0
			n.State = stat
		End If
	End Sub

	Sub ResetAllLightsColor ' Called at a new game
		SetLightColor toplane1l, green, -1	
		SetLightColor toplane2l, green, -1	
		SetLightColor toplane3l, green, -1

		SetLightColor topbank1l, red, -1
		SetLightColor topbank2l, red, -1
		SetLightColor topbank3l, red, -1

		SetLightColor locollect, amber, -1
		SetLightColor lojackpot, amber, -1

		SetLightColor rocollect, amber, -1
		SetLightColor rojackpot, amber, -1

		SetLightColor tavernstashl, red, -1
		SetLightColor tavern1l, red, -1

		SetLightColor rain1, red, -1
		SetLightColor rain2, orange, -1
		SetLightColor rain3, yellow, -1
		SetLightColor rain4, green, -1
		SetLightColor rain5, blue, -1
		SetLightColor rain6, purple, -1

		SetLightColor findkey, amber, -1
		SetLightColor keyl, amber, -1

		SetLightColor pointsbank, green, -1
		SetLightColor pointsbanka, green, -1
		SetLightColor points10, darkgreen, -1
		SetLightColor points20, darkgreen, -1
		SetLightColor points30, darkgreen, -1
		SetLightColor points40, darkgreen, -1
		SetLightColor points50, darkgreen, -1
		SetLightColor points60, darkgreen, -1
		SetLightColor points70, darkgreen, -1
		SetLightColor points80, darkgreen, -1
		SetLightColor points90, darkgreen, -1

		SetLightColor lep51l, white, -1
		SetLightColor lep52l, amber, -1

		SetLightColor lep41l, white, -1
		SetLightColor lep42l, amber, -1

		SetLightColor lep31l, white, -1
		SetLightColor lep32l, amber, -1

		SetLightColor lep21l, white, -1
		SetLightColor lep22l, amber, -1

		SetLightColor lep11l, white, -1
		SetLightColor lep12l, amber, -1

		SetLightColor twoxl, red, -1
		SetLightColor threexl, red, -1

		SetLightColor score1l, yellow, -1
		SetLightColor score2l, yellow, -1
		SetLightColor score3l, yellow, -1
		SetLightColor score4l, yellow, -1
		SetLightColor score5l, yellow, -1

		SetLightColor ebl, green, -1
		SetLightColor ebr, green, -1

		SetLightColor lane1l, orange, -1
		SetLightColor lane2l, orange, -1
		SetLightColor lane3l, orange, -1
		SetLightColor lane4l, orange, -1

		SetLightColor kickbackll, darkgreen, -1
		SetLightColor kickbackrl, darkgreen, -1

		SetLightColor LightShootAgain, red, -1
		
	End Sub

	Sub UpdateBonusColors
	End Sub

	'*************************
	' Rainbow Changing Lights
	'*************************

	Dim RGBStep, RGBFactor, rRed, rGreen, rBlue, RainbowLights

	Sub StartRainbow(n)
		set RainbowLights = n
		RGBStep = 0
		RGBFactor = 5
		rRed = 255
		rGreen = 0
		rBlue = 0
		RainbowTimer.Enabled = 1
	End Sub

	Dim RGBStep2, RGBFactor2, rRed2, rGreen2, rBlue2, RainbowLights2
	Sub StartRainbow2(n)
		set RainbowLights2 = n
		RGBStep2 = 0
		RGBFactor2 = 5
		rRed2 = 255
		rGreen2 = 0
		rBlue2 = 0
		RainbowTimer1.Enabled = 1
	End Sub


	Dim RGBStep3, RGBFactor3, rRed3, rGreen3, rBlue3, RainbowLights3
	Sub StartRainbow3(n)
		set RainbowLights3 = n
		RGBStep3 = 0
		RGBFactor3 = 5
		rRed3 = 255
		rGreen3 = 0
		rBlue3 = 0
		RainbowTimer2.Enabled = 1
	End Sub

	Sub StopRainbow(n)
		Dim obj
		RainbowTimer.Enabled = 0
		RainbowTimer.Enabled = 0
			For each obj in RainbowLights
				SetLightColor obj, "white", 0
			Next
	End Sub

	Sub StopRainbow2(n)
		Dim obj
		RainbowTimer1.Enabled = 0
			For each obj in RainbowLights2
				SetLightColor obj, "white", 0
				obj.state = 1
				obj.Intensity = 12
			Next
	End Sub

	Sub RainbowTimer_Timer 'rainbow led light color changing
		Dim obj
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
			For each obj in RainbowLights
				obj.color = RGB(rRed \ 10, rGreen \ 10, rBlue \ 10)
				obj.colorfull = RGB(rRed, rGreen, rBlue)
			Next
	End Sub

	Sub RainbowTimer1_Timer 'rainbow led light color changing
		Dim obj
		Select Case RGBStep2
			Case 0 'Green
				rGreen2 = rGreen2 + RGBFactor2
				If rGreen2 > 255 then
					rGreen2 = 255
					RGBStep2 = 1
				End If
			Case 1 'Red
				rRed2 = rRed2 - RGBFactor2
				If rRed2 < 0 then
					rRed2 = 0
					RGBStep2 = 2
				End If
			Case 2 'Blue
				rBlue2 = rBlue2 + RGBFactor2
				If rBlue2 > 255 then
					rBlue2 = 255
					RGBStep2 = 3
				End If
			Case 3 'Green
				rGreen2 = rGreen2 - RGBFactor2
				If rGreen2 < 0 then
					rGreen2 = 0
					RGBStep2 = 4
				End If
			Case 4 'Red
				rRed2 = rRed2 + RGBFactor2
				If rRed2 > 255 then
					rRed2 = 255
					RGBStep2 = 5
				End If
			Case 5 'Blue
				rBlue2 = rBlue2 - RGBFactor2
				If rBlue2 < 0 then
					rBlue2 = 0
					RGBStep2 = 0
				End If
		End Select
			For each obj in RainbowLights2
				obj.color = RGB(rRed2 \ 10, rGreen2 \ 10, rBlue2 \ 10)
				obj.colorfull = RGB(rRed2, rGreen2, rBlue2)
			Next
	End Sub


	Sub RainbowTimer2_Timer 'rainbow led light color changing
		Dim obj
		Select Case RGBStep3
			Case 0 'Green
				rGreen3 = rGreen3 + RGBFactor3
				If rGreen3 > 255 then
					rGreen3 = 255
					RGBStep3 = 1
				End If
			Case 1 'Red
				rRed3 = rRed3 - RGBFactor3
				If rRed3 < 0 then
					rRed3 = 0
					RGBStep3 = 2
				End If
			Case 2 'Blue
				rBlue3 = rBlue3 + RGBFactor3
				If rBlue3 > 255 then
					rBlue3 = 255
					RGBStep3 = 3
				End If
			Case 3 'Green
				rGreen3 = rGreen3 - RGBFactor3
				If rGreen3 < 0 then
					rGreen3 = 0
					RGBStep3 = 4
				End If
			Case 4 'Red
				rRed3 = rRed3 + RGBFactor3
				If rRed3 > 255 then
					rRed3 = 255
					RGBStep3 = 5
				End If
			Case 5 'Blue
				rBlue3 = rBlue3 - RGBFactor3
				If rBlue3 < 0 then
					rBlue3 = 0
					RGBStep3 = 0
				End If
		End Select
			For each obj in RainbowLights3
				obj.color = RGB(rRed3 \ 10, rGreen3 \ 10, rBlue3 \ 10)
				obj.colorfull = RGB(rRed3, rGreen3, rBlue3)
			Next
	End Sub


	Sub StartLightSeq()
		On Error Resume Next
		Dim a
		For each a in alights
			a.State = 1
		Next
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
		'LightSeqAttract.UpdateInterval = 8
		'LightSeqAttract.Play SeqStripe1VertOn, 25, 3
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
		For each a in GI
			a.State = 1
			a.Intensity = 80
		Next
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 50, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqCircleOutOn, 15, 2
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 25, 1
		LightSeqAttract2.UpdateInterval = 10
		LightSeqAttract2.Play SeqCircleOutOn, 15, 3
		LightSeqAttract2.UpdateInterval = 5
		LightSeqAttract2.Play SeqRightOn, 50, 1
		LightSeqAttract2.UpdateInterval = 5
		LightSeqAttract2.Play SeqLeftOn, 50, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 50, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 50, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 40, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 40, 1
		LightSeqAttract2.UpdateInterval = 10
		LightSeqAttract2.Play SeqRightOn, 30, 1
		LightSeqAttract2.UpdateInterval = 10
		LightSeqAttract2.Play SeqLeftOn, 30, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 15, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 15, 1
		LightSeqAttract2.UpdateInterval = 10
		LightSeqAttract2.Play SeqCircleOutOn, 15, 3
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 25, 1
		LightSeqAttract2.UpdateInterval = 5
		LightSeqAttract2.Play SeqStripe1VertOn, 50, 2
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqCircleOutOn, 15, 2
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqStripe1VertOn, 50, 3
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqCircleOutOn, 15, 2
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqStripe2VertOn, 50, 3
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqStripe1VertOn, 25, 3
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqStripe2VertOn, 25, 3
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 15, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 15, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 15, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 15, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 15, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 15, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 15, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 15, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 15, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 15, 1
	End Sub

	Sub LightSeqAttract_PlayDone()
		StartLightSeq()
	End Sub

	Sub LightSeqTilt_PlayDone()
		LightSeqTilt.Play SeqAllOff
	End Sub

	Sub LightSeqSkillshot_PlayDone()
		LightSeqSkillshot.Play SeqAllOff
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
		For each bulb in GI
			SetLightColor bulb, col, -1
		Next
	End Sub

	Sub GIUpdateTimer_Timer
		Dim tmp, obj
		tmp = Getballs
		If UBound(tmp) <> OldGiState Then
			OldGiState = Ubound(tmp)
			If UBound(tmp) = 3 Then 'we have 4 captive balls on the table (-1 means no balls, 0 is the first ball, 1 is the second..)
				'GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
			Else
				'Gion
			End If
		End If
	End Sub

	Sub GiOn
		DOF 126, DOFOn
		Dim bulb
		For each bulb in GI
			SetLightColor bulb, base, -1
			bulb.State = 1
		Next
	End Sub

	Sub GiOff
		DOF 126, DOFOff
		Dim bulb
		For each bulb in GI
			bulb.State = 0
		Next
	End Sub


	' GI & light sequence effects


	Sub GiEffect(n)
		Select Case n
			Case 0 'all off
				'LightSeqGi.Play SeqAlloff
			Case 1 'all blink
				'LightSeqGi.UpdateInterval = 4
				'LightSeqGi.Play SeqBlinking, , 5, 100
			Case 2 'random
				'LightSeqGi.UpdateInterval = 10
				'LightSeqGi.Play SeqRandom, 5, , 1000
			Case 3 'upon
				'LightSeqGi.UpdateInterval = 4
				'LightSeqGi.Play SeqUpOn, 5, 1
			Case 4 ' left-right-left
				'LightSeqGi.UpdateInterval = 5
				'LightSeqGi.Play SeqLeftOn, 10, 1
				'LightSeqGi.UpdateInterval = 5
				'LightSeqGi.Play SeqRightOn, 10, 1
		End Select
	End Sub

	Sub LightEffect(n)
		Select Case n
			Case 0 ' all off
				'LightSeqInserts.Play SeqAlloff
			Case 1 'all blink
				'LightSeqInserts.UpdateInterval = 4
				'LightSeqInserts.Play SeqBlinking, , 5, 100
			Case 2 'random
				'LightSeqInserts.UpdateInterval = 10
				'LightSeqInserts.Play SeqRandom, 5, , 1000
			Case 3 'upon
				'LightSeqInserts.UpdateInterval = 4
				'LightSeqInserts.Play SeqUpOn, 10, 1
			Case 4 ' left-right-left
				'LightSeqInserts.UpdateInterval = 5
				'LightSeqInserts.Play SeqLeftOn, 10, 1
				'LightSeqInserts.UpdateInterval = 5
				'LightSeqInserts.Play SeqRightOn, 10, 1
			Case 5 'random
				LightSeqbumper.UpdateInterval = 4
				LightSeqbumper.Play SeqBlinking, , 5, 10
			Case 6 'random
				'LightSeqRSling.UpdateInterval = 4
				'LightSeqRSling.Play SeqBlinking, , 5, 6
			Case 7 'random
				'LightSeqLSling.UpdateInterval = 4
				'LightSeqLSling.Play SeqBlinking, , 5, 6
			Case 8 'random
				'LightSeqBack.UpdateInterval = 4
				'LightSeqBack.Play SeqBlinking, , 5, 6
			Case 12 'random
				'LightSeqlr.UpdateInterval = 4
				'LightSeqlr.Play SeqBlinking, , 5, 10
		End Select
	End Sub

	' Flasher Effects using lights

	Dim FEStep, FEffect
	FEStep = 0
	FEffect = 0

	Sub FlashEffect(n)
		Select case n
			Case 0 ' all off
				LightSeqFlasher.Play SeqAlloff
			Case 1 'all blink
				LightSeqFlasher.UpdateInterval = 4
				LightSeqFlasher.Play SeqBlinking, , 5, 100
			Case 2 'random
				LightSeqFlasher.UpdateInterval = 10
				LightSeqFlasher.Play SeqRandom, 5, , 1000
			Case 3 'upon
				LightSeqFlasher.UpdateInterval = 4
				LightSeqFlasher.Play SeqUpOn, 10, 1
			Case 4 ' left-right-left
				LightSeqFlasher.UpdateInterval = 5
				LightSeqFlasher.Play SeqLeftOn, 10, 1
				LightSeqFlasher.UpdateInterval = 5
				LightSeqFlasher.Play SeqRightOn, 10, 1
			Case 5 ' top flashers blink fast
		End Select
	End Sub




'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   SCORING FUNCTIONS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

	Sub AddScore(points)
		If(Tilted = False) Then
			' add the points to the current players score variable
			Score(CurrentPlayer) = Score(CurrentPlayer) + points
			DMDScore
			pUpdateScores
			if Score(CurrentPlayer) > 7000000 Then
				replaycall
			End If
		End if
	End Sub



	Sub replaycall
		gotreplay(CurrentPlayer) = gotreplay(CurrentPlayer) + 1
		Select Case gotreplay(CurrentPlayer)
			Case 1
				PuPlayer.playlistplayex pBackglass,"videoscenes","replay.mp4",videovol,1		
				AwardSpecial	
		End Select
	End Sub

	Sub AddScorePercent(points)
		If(Tilted = False) Then
			' add the points to the current players score variable
			Score(CurrentPlayer) = Score(CurrentPlayer) + playertablescore\points
			DMDScore
			pUpdateScores
		End if
	End Sub


	Dim tablevalue:tablevalue = 0
	Dim playertablescore:playertablescore = 0
	Dim point1:point1 = 1000
	Dim bigpoints:bigpoints = 1

	'calculatescore
	Sub calculatescore
	PuPlayer.LabelSet pBackglass,"lefttimer2",(bigpoints\10000) + 1 & "-" & point1\1000,1,"{'mt':2,'color':12971496, 'size': 4.8, 'xpos': 12.5, 'xalign': 1, 'ypos': 89.4, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"lefttimer",(bigpoints\10000) + 1 & "-" & point1\1000,1,"{'mt':2,'color':26316, 'size': 4.8, 'xpos': 12.5, 'xalign': 1, 'ypos': 89.2, 'yalign': 1}"
		playertablescore = point1 + bigpoints
	End Sub
	
	Sub settablevalue
		If point2(currentplayer) = 0 Then
			tablevalue = tablevalue + 60
		End If
		If point2(currentplayer) = 1 Then
			tablevalue = tablevalue + 20
		End If
		If point2(currentplayer) = 2 Then
			tablevalue = tablevalue + 15
		End If
		If point2(currentplayer) = 3 Then
			tablevalue = tablevalue + 12
		End If
		If point2(currentplayer) = 4 Then
			tablevalue = tablevalue + 10
		End If
		If point2(currentplayer) = 5 Then
			tablevalue = tablevalue + 5
		End If
		If point2(currentplayer) = 6 Then
			tablevalue = tablevalue + 1
		End If
	End Sub

	Dim jackscore
	Sub scorejackpot
		jackscore = (progresslevel(CurrentPlayer) * 100000) * mblevel * (point2(currentplayer) + 1)
		Addscore jackscore
		'pNote "Jackpot",jackscore
	End Sub

	Sub Addtablescore
		If(Tilted = False) Then
			calculatescore
			settablevalue
			If tablevalue > 540 Then
				points10.state = 1
				points20.state = 1
				points30.state = 1
				points40.state = 1
				points50.state = 1
				points60.state = 1
				points70.state = 1
				points80.state = 1
				points90.state = 1
				pointsbank.state = 2
				pointsbanka.state = 2
			End If
			Select Case tablevalue
			Case 60
			point1 = 1000
			Score(CurrentPlayer) = Score(CurrentPlayer) + playertablescore
			points10.state = 1
			Case 120
			Score(CurrentPlayer) = Score(CurrentPlayer) + playertablescore
			points10.state = 1
			points20.state = 1
			point1 = 2000
			Case 180
			Score(CurrentPlayer) = Score(CurrentPlayer) + playertablescore
			points10.state = 1
			points20.state = 1
			points30.state = 1
			point1 = 3000
			Case 240
			Score(CurrentPlayer) = Score(CurrentPlayer) + playertablescore
			points10.state = 1
			points20.state = 1
			points30.state = 1
			points40.state = 1
			point1 = 4000
			Case 300
			Score(CurrentPlayer) = Score(CurrentPlayer) + playertablescore
			points10.state = 1
			points20.state = 1
			points30.state = 1
			points40.state = 1
			points50.state = 1
			point1 = 5000
			Case 360
			Score(CurrentPlayer) = Score(CurrentPlayer) + playertablescore
			points10.state = 1
			points20.state = 1
			points30.state = 1
			points40.state = 1
			points50.state = 1
			points60.state = 1
			point1 = 6000
			Case 420
			Score(CurrentPlayer) = Score(CurrentPlayer) + playertablescore
			points10.state = 1
			points20.state = 1
			points30.state = 1
			points40.state = 1
			points50.state = 1
			points60.state = 1
			points70.state = 1
			point1 = 7000
			Case 480
			Score(CurrentPlayer) = Score(CurrentPlayer) + playertablescore
			points10.state = 1
			points20.state = 1
			points30.state = 1
			points40.state = 1
			points50.state = 1
			points60.state = 1
			points70.state = 1
			points80.state = 1
			point1 = 8000
			Case 540
			Score(CurrentPlayer) = Score(CurrentPlayer) + playertablescore
			points10.state = 1
			points20.state = 1
			points30.state = 1
			points40.state = 1
			points50.state = 1
			points60.state = 1
			points70.state = 1
			points80.state = 1
			points90.state = 1
			pointsbank.state = 2
			pointsbanka.state = 2
			point1 = 9000
chilloutthemusic
			PuPlayer.playlistplayex pCallouts,"scoreupgradeavailable-L" & currentlep(CurrentPlayer),"",100,1	
		PuPlayer.playlistplayex pBackglass,"videoscenes","Score Upgrade Available.mp4",videovol,1
			Case 600
			Score(CurrentPlayer) = Score(CurrentPlayer) + playertablescore
			tablevalue = 9
chilloutthemusic

			End Select
			DMDScore
			pUpdateScores
		End if
	End Sub

	Sub pointsbankt_hit
		if havedof = 0 then
		PlaySoundAt "fx_target", pointsbankt
		End if
			DOF 151, DOFPulse
			DOF 127, DOFPulse
			DOF 336, DOFPulse   'DOF MX - Pointbankhit
		If pointsbank.state = 2 Then
			flasherspop green,"crazy"
			point2(CurrentPlayer) = point2(currentplayer) + 1
			Select Case point2(CurrentPlayer)
				Case 1
chilloutthemusic
					PuPlayer.playlistplayex pCallouts,"tablescorelevel2-L" & currentlep(CurrentPlayer),"",100,1	
		PuPlayer.playlistplayex pBackglass,"videoscenes","Table Score Upgraded.mp4",videovol,1
					score1l.state = 1
					bigpoints = 10000
				Case 2
chilloutthemusic
					PuPlayer.playlistplayex pCallouts,"tablescorelevel3-L" & currentlep(CurrentPlayer),"",100,1	
		PuPlayer.playlistplayex pBackglass,"videoscenes","Table Score Upgraded.mp4",videovol,1
					score1l.state = 1
					score2l.state = 1
					bigpoints = 20000
				Case 3
chilloutthemusic
					PuPlayer.playlistplayex pCallouts,"tablescorelevel4-L" & currentlep(CurrentPlayer),"",100,1	
		PuPlayer.playlistplayex pBackglass,"videoscenes","Table Score Upgraded.mp4",videovol,1
					score1l.state = 1
					score2l.state = 1
					score3l.state = 1
					bigpoints = 30000
					vpmtimer.addtimer 3000, "lightextraball '"
					
				Case 4
chilloutthemusic
					PuPlayer.playlistplayex pCallouts,"tablescorelevel5-L" & currentlep(CurrentPlayer),"",100,1	
		PuPlayer.playlistplayex pBackglass,"videoscenes","Table Score Upgraded.mp4",videovol,1
					DOF 351, DOFPulse   'DOF MX - allrains
					score1l.state = 1
					score2l.state = 1
					score3l.state = 1
					score4l.state = 1
					bigpoints = 40000
				Case 5
chilloutthemusic
					score1l.state = 1
					score2l.state = 1
					score3l.state = 1
					score4l.state = 1
					score5l.state = 1
					bigpoints = 50000
					PuPlayer.playlistplayex pCallouts,"maxtablescore-L" & currentlep(CurrentPlayer),"",100,1
		PuPlayer.playlistplayex pBackglass,"videoscenes","Table Score Upgraded.mp4",videovol,1
			End Select
			resettablepoints
			pointsbank.state = 0
			pointsbanka.state = 0
		Else
			addtablescore
		end If
	End Sub

	Sub resettablepoints
		points10.state = 0
		points20.state = 0
		points30.state = 0
		points40.state = 0
		points50.state = 0
		points60.state = 0
		points70.state = 0
		points80.state = 0
		points90.state = 0
		point1 = 0
		tablevalue = 0
	End Sub
	
	Sub lightextraball
		PuPlayer.playlistplayex pBackglass,"videoscenes","Extra_ball_lit.mp4",videovol,1
		ebl.state = 1 
		eblon(CurrentPlayer) = 1
		PuPlayer.playlistplayex pCallouts,"extraballislit-L" & currentlep(CurrentPlayer),"",100,1
		DOF 115, DOFPulse
	End Sub

	Sub AwardExtraBall()
		If NOT bExtraBallWonThisBall Then
			LightShootAgain.State = 1
			flashflash.Enabled = True
			LightSeqFlasher.UpdateInterval = 150
			LightSeqFlasher.Play SeqRandom, 10, , 10000
			ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
			bExtraBallWonThisBall = True
				DOF 127, DOFPulse
			DOF 111, DOFPulse
			DOF 115, DOFPulse
			DOF 402, DOFPulse   'DOF MX - Extra Ball
			'extraballready(CurrentPlayer) = 0
			If bMultiBallMode = false Then
			PuPlayer.playlistplayex pBackglass,"videoscenes","Extra_ball.mp4",videovol,1
			PuPlayer.playlistplayex pCallouts,"extraball-L" & currentlep(CurrentPlayer),"",100,1
		chilloutthemusic
			End If
		Else
		AddScore 4000000
		END If
	End Sub

	Sub AwardSpecial()
		Credits = Credits + 1
		DOF 140, DOFPulse
		PlaySound SoundFXDOF("knocker",136,DOFPulse,DOFKnocker)	
		DOF 115, DOFPulse
		GiEffect 1
		LightEffect 1
		DOF 400, DOFPulse   'DOF MX - Special
		flasherspop green,"crazy"
	End Sub


	Sub AwardSkillshot()
		Dim i
		DOF 125, DOFPulse
		ResetSkillShotTimer_Timer
		AddScore 100000 * (point2(currentplayer) + 1)
		GiEffect 1
		LightEffect 2
		LightEffect 9
		DOF 115, DOFPulse
		DOF 401, DOFPulse   'DOF MX - Skillshot
	End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   BALL FUNCTIONS & DRAINS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  


	Function Balls
		Dim tmp
		tmp = bpgcurrent - BallsRemaining(CurrentPlayer) + 1
		If tmp> bpgcurrent Then
			Balls = bpgcurrent
		Else
			Balls = tmp
		End If
	End Function


	Sub FirstBall
		ResetForNewPlayerBall()
		CreateNewBall()
	End Sub


	Sub ResetForNewPlayerBall()
		AddScore 0
		BonusPoints(CurrentPlayer) = 0
		bBonusHeld = False
		bExtraBallWonThisBall = False
		ResetNewBallLights()
		ResetNewBallVariables
		bBallSaverReady = True
		bSkillShotReady = True
	End Sub


	Sub CreateNewBall()
		BallRelease.CreateSizedball BallSize / 2
		BallsOnPlayfield = BallsOnPlayfield + 1
		PlaySoundAt SoundFXDOF("fx_Ballrel", 114, DOFPulse, DOFContactors), BallRelease
		BallRelease.Kick 90, 4
		'If BallsOnPlayfield > 1 Then
		'	bMultiBallMode = True
		'		DOF 127, DOFOn    'Beacon ON
		'	bAutoPlunger = True
		'End If
		debug.print "Ball kick" & BallsOnPlayfield
	End Sub


	Sub AddMultiball(nballs)
		'debug.print UBound(BOT)
		mBalls2Eject = mBalls2Eject + nballs
		CreateMultiballTimer.Enabled = True
	End Sub

	Sub CreateMultiballTimer_Timer()
		If bBallInPlungerLane Then
			Exit Sub
		Else
			If BallsOnPlayfield <MaxMultiballs Then
				CreateNewBall()
				mBalls2Eject = mBalls2Eject -1
				If mBalls2Eject = 0 Then 
					Me.Enabled = False
				End If
			Else 
				mBalls2Eject = 0
				Me.Enabled = False
			End If
		End If
	End Sub

	Sub EndOfBall()
		PuPlayer.playlistplayex pAudio,"audiomultiballs","clear.mp3",100,1


		bMultiBallMode = False
		bOnTheFirstBall = False
		If NOT Tilted Then
			vpmtimer.addtimer 500, "EndOfBall2 '"
		Else 
			vpmtimer.addtimer 500, "EndOfBall2 '"
		End If
	End Sub

	
	Sub EndOfBall2()
		Tilted = False
		Tilt = 0
		DisableTable False
		tilttableclear.enabled = False
		tilttime = 0
		If(ExtraBallsAwards(CurrentPlayer) <> 0) Then
			ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1
			If(ExtraBallsAwards(CurrentPlayer) = 0) Then
				LightShootAgain.State = 0
			End If
			LightSeqFlasher.UpdateInterval = 150
			LightSeqFlasher.Play SeqRandom, 10, , 2000

			CreateNewBall()
			ResetForNewPlayerBall
		PuPlayer.playlistplayex pBackglass,"videoscenes","shootagain.mp4",videovol,1
			'pNote "SHOOT AGAIN","SAME PLAYER"
			PuPlayer.playlistplayex pCallouts,"shootagain-L" & currentlep(CurrentPlayer),"",100,1
		chilloutthemusic
		Else
			BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1
			If(BallsRemaining(CurrentPlayer) <= 0) Then
				CheckHighScore()
			Else
				EndOfBallComplete()
			End If
		End If
	End Sub

	Sub EndOfBallComplete()
		ResetNewBallVariables
		Dim NextPlayer
		If(PlayersPlayingGame> 1) Then
			NextPlayer = CurrentPlayer + 1
			If(NextPlayer> PlayersPlayingGame) Then
				NextPlayer = 1
			End If
		Else
			NextPlayer = CurrentPlayer
		End If
		If((BallsRemaining(CurrentPlayer) <= 0) AND(BallsRemaining(NextPlayer) <= 0) ) Then
			EndOfGame()
		Else
			CurrentPlayer = NextPlayer
			AddScore 0
			ResetForNewPlayerBall()
			CreateNewBall()
			If PlayersPlayingGame> 1 Then
				PlaySound "vo_player" &CurrentPlayer
			End If
		End If
	End Sub

	Sub Balldrained
			DOF 362, DOFPulse
		DOF 312, DOFPulse 'DOF MX - Drained
		PuPlayer.playlistplayex pMusic,"audioclear","clear.mp3",100, 1
		PuPlayer.playlistplayex pBackglass,"videodrain","",100,1
		PuPlayer.playlistplayex pBackglass,"videoscenes","Ball_lost.mp4",videovol,1
		level4.enabled = 0
		level5.enabled = 0
	End Sub

	Sub Drain_Hit()
		debug.print "Balls pre math" & BallsOnPlayfield
		Drain.DestroyBall
		BallsOnPlayfield = BallsOnPlayfield - 1
		if havedof = 0 Then
			PlaySoundAt "fx_drain", Drain
		end if 
		If Tilted Then
			StopEndOfBallMode
		End If
		If(bGameInPLay = True) AND(Tilted = False) Then
			If(bBallSaverActive = True) Then
				AddMultiball 1
				bAutoPlunger = True
				If bMultiBallMode = False Then
						PuPlayer.playlistplayex pCallouts,"ballsaved-L" & currentlep(CurrentPlayer),"",100,1
					chilloutthemusic
					'pNote "BALL SAVED",""
					PuPlayer.playlistplayex pBackglass,"videoscenes","Ball_saved.mp4",videovol,1
				End If
			Else
				If ballstashed + keyfound = 2 Then
					If(bMultiBallMode = False) then
						bMultiBallMode = False
						ChangeGi "white"
						vpmtimer.addtimer 1000, "Balldrained '"
						vpmtimer.addtimer 6000, "EndOfBall '"
						PuPlayer.playlistplayex pCallouts,"nays-L" & currentlep(CurrentPlayer),"",100,1
						rainbowgateclosed
						closekickbacks
						closethekeep
						lowerramps
						StopEndOfBallMode
						bAutoPlunger = False
						LightSeqHit.StopPlay
						LightSeqAttract2.StopPlay
						StopRainbow alights
						StopRainbow2 GI
						ResetAllLightsColor
						ChangeBall(0)
					End If
				ElseIf ballstashed + keyfound = 1 Then
						If(bMultiBallMode = True) then
							bMultiBallMode = False
							DOF 127, DOFOff   'DOF - Beacon - OFF
							ChangeGi "white"
						End If
						If(bMultiBallMode = False) then
							bMultiBallMode = False
							ChangeGi "white"
							vpmtimer.addtimer 1000, "Balldrained '"
							vpmtimer.addtimer 6000, "EndOfBall '"
							PuPlayer.playlistplayex pCallouts,"nays-L" & currentlep(CurrentPlayer),"",100,1
							chilloutthemusic
							rainbowgateclosed
							lowerramps
							closekickbacks
							closethekeep
							StopEndOfBallMode
							bAutoPlunger = False
						LightSeqHit.StopPlay
						LightSeqAttract2.StopPlay
						StopRainbow alights
						StopRainbow2 GI
						ResetAllLightsColor
						ChangeBall(0)
						End If
				Elseif ballstashed + keyfound = 0 Then
					If(BallsOnPlayfield = 1) Then
						If(bMultiBallMode = True) then
							mbholder
							DOF 127, DOFOff   'DOF - Beacon - OFF
							endthemultiball
						End If
						bMultiBallMode = False		
						ChangeGi "white"
					End If

					If(BallsOnPlayfield = 0) Then
						bMultiBallMode = False
						ChangeGi "white"
						vpmtimer.addtimer 1000, "Balldrained '"
						vpmtimer.addtimer 6000, "EndOfBall '"
						PuPlayer.playlistplayex pCallouts,"nays-L" & currentlep(CurrentPlayer),"",100,1
		chilloutthemusic
						rainbowgateclosed
						lowerramps
						StopEndOfBallMode
						bAutoPlunger = False
						LightSeqHit.StopPlay
						LightSeqAttract2.StopPlay
						StopRainbow alights
						StopRainbow2 GI
						ResetAllLightsColor
						ChangeBall(0)
						closekickbacks
						closethekeep
					End If
				End If
			End If
		End If
		debug.print "Balls post math" & BallsOnPlayfield
	End Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  BALL SAVE & LAUNCH
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 

	Sub ballsavestarttrigger_hit
		PlaySoundAt SoundFXDOF("fx_gate", 114, DOFPulse, DOFContactors), ballsavestarttrigger
		If(bBallSaverReady = True) AND(15 <> 0) And(bBallSaverActive = False) Then
			if hardmode = 0 Then
			EnableBallSaver BallSaverTime
			end If
		End If
	End Sub

	Sub swPlungerRest_Hit()
		if havedof = 0 Then
		PlaySoundAt "fx_sensor", ActiveBall
		end if 
		bBallInPlungerLane = True
		If bAutoPlunger Then
			'PlungerIM.Strength = 45
			'PlungerIM.AutoFire
			'PlungerIM.Strength = Plunger.MechStrength
			Plunger.AutoPlunger = true
			Plunger.Pullback
			Plunger.Fire
			DOF 148, DOFPulse  'DOF-RGB Or
			DOF 114, DOFPulse	'DOF -Solenoid RSling	
			DOF 115, DOFPulse	'DOF-Strobes
			'bAutoPlunger = False
			'Plunger.AutoPlunger = false
		End If
		DOF 141, DOFOn  'Launch Ball Button Flashing - ON
		DOF 317, DOFOn 'DOF MX - Ball Ready to Launch - ON
		If bSkillShotReady Then
			swPlungerRest.TimerEnabled = 1
			UpdateSkillshot()
		End If
		LastSwitchHit = "swPlungerRest"
	End Sub

	Sub swPlungerRest_UnHit()
		bBallInPlungerLane = False
		swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
		If bSkillShotReady Then
			ResetSkillShotTimer.Enabled = 1
		End If
		DOF 317, DOFOff   'DOF MX - Ball is ready to Launch - OFF
	End Sub


	Sub swPlungerRest_Timer
		'DMD "start-5.wmv", "", "",  6000
		swPlungerRest.TimerEnabled = 0
	End Sub

	Sub EnableBallSaver(seconds)
		bBallSaverActive = True
		bBallSaverReady = False
		BallSaverTimerExpired.Interval = 1000 * seconds
		BallSaverTimerExpired.Enabled = True
		BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
		BallSaverSpeedUpTimer.Enabled = True
		' if you have a ball saver light you might want to turn it on at this point (or make it flash)
		LightShootAgain.BlinkInterval = 160
		LightShootAgain.State = 2
	End Sub

	' The ball saver timer has expired.  Turn it off AND reset the game flag
	'
	Sub BallSaverTimerExpired_Timer()
		'debug.print "Ballsaver ended"
		BallSaverTimerExpired.Enabled = False
		' clear the flag
		Dim waittime
		waittime = 3000
		vpmtimer.addtimer waittime, "ballsavegrace'"
		' if you have a ball saver light then turn it off at this point
		If bExtraBallWonThisBall = True Then
			LightShootAgain.State = 1
		Else
			LightShootAgain.State = 0
		End If
	End Sub

	Sub ballsavegrace
		bBallSaverActive = False
	End Sub

	Sub BallSaverSpeedUpTimer_Timer()
		'debug.print "Ballsaver Speed Up Light"
		BallSaverSpeedUpTimer.Enabled = False
		' Speed up the blinking
		LightShootAgain.BlinkInterval = 80
		LightShootAgain.State = 2
	End Sub




'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  TABLE VARIABLES
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 


	' Game Variables
	Dim LaneBonus
	Dim TargetBonus
	Dim RampBonus
	Dim OrbitBonus
	Dim spinvalue
	Dim barbMultiball
	Dim barbHits(10)
	Dim LookForBarb
	Dim RunMultiball
	Dim RunAway
	Dim RunHits(9)
	Dim BallsInRunLock(4)
	Dim LRHits(10)
	Dim RRHits(7)
	Dim WillMultiball
	Dim WillSuper
	Dim WillSuperReady
	Dim DemoMultiball
	Dim DemoHits
	Dim MonsterFinalBlow
	Dim BarbJackpots
	Dim finalflips
	Dim changetrack
	Dim Saves
	Dim Drains
	Dim energy
	Dim partymultiball
	Dim DGs
	Dim inmode
	Dim keyfound
	Dim ballstashed
	Dim mblevel

	' player specific variables
	Dim barb1on(4)
	Dim point2(4)
	Dim rainbowspins(4)
	Dim bumps(4)
	Dim progresslevel(4)
	Dim currentlep(4)
	Dim lep1done(4)
	Dim lep2done(4)
	Dim lep3done(4)
	Dim lep4done(4)
	Dim lep5done(4)
	Dim kickbackl(4)
	Dim kickbackr(4)
	Dim lane1lon(4)
	Dim lane2lon(4)
	Dim lane3lon(4)
	Dim lane4lon(4)
	Dim topbank1lon(4)
	Dim topbank2lon(4)
	Dim topbank3lon(4)
	Dim lep11lon(4)
	Dim lep21lon(4)
	Dim lep31lon(4)
	Dim lep41lon(4)
	Dim lep51lon(4)
	Dim goldstole(4)
	Dim ebron(4)
	Dim eblon(4)
	Dim gotreplay(4)
	Dim lep4isdone(4)
	Dim lep3isdone(4)
	Dim lep2isdone(4)
	Dim lep1isdone(4)



	

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  GAME STARTING & RESETS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 

	Sub Game_Init() 'called at the start of a new game
		Dim i
		For i = 0 to 4
			point2(i) = 0
			rainbowspins(i) = 0
			bumps(i) = 0
			progresslevel(i) = 0
			currentlep(i) = 0
			lep1done(i) = 0
			lep2done(i) = 0
			lep3done(i) = 0
			lep4done(i) = 0
			lep5done(i) = 0
			kickbackl(i) = 1
			kickbackr(i) = 0
			lane1lon(i) = 0
			lane2lon(i) = 0
			lane3lon(i) = 0
			lane4lon(i) = 0
			topbank1lon(i) = 0
			topbank2lon(i) = 0
			topbank3lon(i) = 0
			lep11lon(i) = 0
			lep21lon(i) = 0
			lep31lon(i) = 0
			lep41lon(i) = 0
			lep51lon(i) = 0
			goldstole(i) = 0
			eblon(i) = 0
			ebron(i) = 0
			gotreplay(i) = 0
			lep4isdone(i) = 0
			lep3isdone(i) = 0
			lep2isdone(i) = 0
			lep1isdone(i) = 0
		Next
		keyfound = 0
		ballstashed = 0
		closethekeep
		lowerramps
		lrflashtime.Enabled = False
		bExtraBallWonThisBall = False
		PuPlayer.LabelSet pBackglass,"high1name","",1,""
		PuPlayer.LabelSet pBackglass,"high1score","",1,""
		PuPlayer.LabelSet pBackglass,"high2name","",1,""
		PuPlayer.LabelSet pBackglass,"high2score","",1,""
		PuPlayer.LabelSet pBackglass,"high3name","",1,""
		PuPlayer.LabelSet pBackglass,"high3score","",1,""
		PuPlayer.LabelSet pBackglass,"high4name","",1,""
		PuPlayer.LabelSet pBackglass,"high4score","",1,""
			PuPlayer.LabelSet pBackglass,"high5name","",1,""
			PuPlayer.LabelSet pBackglass,"high5score","",1,""
			PuPlayer.LabelSet pBackglass,"high6name","",1,""
			PuPlayer.LabelSet pBackglass,"high6score","",1,""
			PuPlayer.LabelSet pBackglass,"high7name","",1,""
			PuPlayer.LabelSet pBackglass,"high7score","",1,""
			PuPlayer.LabelSet pBackglass,"high8name","",1,""
			PuPlayer.LabelSet pBackglass,"high8score","",1,""
		PuPlayer.LabelShowPage pBackglass,1,0,""
		pUpdateScores
		PuPlayer.playlistplayex pBackglass,"videoscenes","Holding Screen_02.mp4",0,1
		PuPlayer.SetBackground pBackglass,1
		PuPlayer.LabelSet pBackglass,"Play1","PLAYER 01",1,"{'mt':2,'color':12971496, 'size': 1.2, 'xpos': 18.9, 'xalign': 0, 'ypos': 80.4, 'yalign': 0}"
		PuPlayer.LabelSet pBackglass,"notetitle","",1,""
		PuPlayer.LabelSet pBackglass,"notecopy","",1,""
		rainbowgateclosed
		ruleshelperon
		resetbackglass
		vpmtimer.addtimer 500, "startervid '"
	End Sub
	
	sub startervid
		PuPlayer.playlistplayex pBackglass,"videoscenes","Emerald_Isle.mp4",videovol,2
	end Sub

	Sub StopEndOfBallMode()              'this sub is called after the last ball is drained
		ResetSkillShotTimer_Timer
	End Sub

	Sub ResetNewBallVariables()          'reset variables for a new ball or player
		If rockmusic = 1 Then
		PuPlayer.playlistplayex pMusic,"audiobgrock","",soundtrackvol,1
		PuPlayer.SetLoop 4,1		
		Else
		PuPlayer.playlistplayex pMusic,"audiobg","",soundtrackvol,1
		PuPlayer.SetLoop 4,1
		End if
	End Sub

	Sub TurnOffPlayfieldLights()
		Dim a
		For each a in alights
			a.State = 0
		Next
	End Sub

	Sub ResetNewBallLights() 'turn on or off the needed lights before a new ball is released
		TurnOffPlayfieldLights()
		currentplayerbackglass
		resettablepoints
		bigpoints = 1
		progressreset
		relightthetable
		PuPlayer.playresume 4
		If PlayersPlayingGame > 1 Then
			If CurrentPlayer = 1 Then
			PuPlayer.playlistplayex pBackglass,"videoscenes","player1.mp4",videovol,1
						PuPlayer.playlistplayex pCallouts,"playerone-L" & currentlep(CurrentPlayer),"",100,1
		chilloutthemusic
			Elseif currentplayer = 2 Then
			PuPlayer.playlistplayex pBackglass,"videoscenes","player2.mp4",videovol,1
						PuPlayer.playlistplayex pCallouts,"playertwo-L" & currentlep(CurrentPlayer),"",100,1
		chilloutthemusic
			Elseif currentplayer = 3 Then
			PuPlayer.playlistplayex pBackglass,"videoscenes","player3.mp4",videovol,1
						PuPlayer.playlistplayex pCallouts,"playerthree-L" & currentlep(CurrentPlayer),"",100,1
		chilloutthemusic
			Elseif currentplayer = 4 Then
			PuPlayer.playlistplayex pBackglass,"videoscenes","player4.mp4",videovol,1
						PuPlayer.playlistplayex pCallouts,"playerfour-L" & currentlep(CurrentPlayer),"",100,1
		chilloutthemusic
			End If
		Else
			'pNote "Ball " & Balls,"Launch Ball"
		'PlaySound "flute"
		PuPlayer.playlistplayex pAudio,"sfx","flute by soughtaftersounds.mp3",sfxvol,1
		End If
		counttherainbow
		if ballstashed = 1 Then
		if havedof = 0 Then
		PlaySoundAt "fx_droptarget", tavern1t
		end if
			tavern3t.isdropped = 0
			tavern2t.isdropped = 1
			tavern1t.isdropped = 1
			tavernstashl.state = 1
		End If
		If keyfound = 1 Then
			openthekeep
		if havedof = 0 Then
		PlaySoundAt "fx_droptarget", tavern1t
		end if
			keyt3.isdropped = 1
			keyt2.isdropped = 1	
			keyt1.isdropped = 1
		End If
		checkifmbready
	End Sub

	Sub relightthetable
		Dim a
		For each a in alights
			a.Intensity = 16
		Next
		If topbank1lon(CurrentPlayer) = 1 Then
			topbank1l.state = 1 
		End If
		If topbank2lon(CurrentPlayer) = 1 Then
			topbank2l.state = 1 
		End If
		If topbank3lon(CurrentPlayer) = 1 Then
			topbank3l.state = 1 
		End If

		If lane1lon(CurrentPlayer) = 1 Then
			lane1l.state = 1
		End If
		If lane2lon(CurrentPlayer) = 1 Then
			lane2l.state = 1
		End If
		If lane3lon(CurrentPlayer) = 1 Then
			lane3l.state = 1
		End If
		If lane4lon(CurrentPlayer) = 1 Then
			lane4l.state = 1
		End If

		If lep4isdone(CurrentPlayer) = 1 Then
			lep42l.state = 1
		End If

		If lep3isdone(CurrentPlayer) = 1 Then
			lep32l.state = 1
		End If

		If lep2isdone(CurrentPlayer) = 1 Then
			lep22l.state = 1
		End If

		If lep1isdone(CurrentPlayer) = 1 Then
			lep12l.state = 1
		End If

		If eblon(CurrentPlayer) = 1 Then
			ebl.state = 1
		End If

		If ebron(CurrentPlayer) = 1 Then
			ebr.state = 1
		End If

		If point2(currentplayer) = 1 Then
			score1l.State = 1
			bigpoints = 10000
		End If
		If point2(currentplayer) = 2 Then
			score1l.State = 1
			score2l.State = 1
			bigpoints = 20000
		End If
		If point2(currentplayer) = 3 Then
			score1l.State = 1
			score2l.State = 1
			score3l.State = 1
			bigpoints = 30000
		End If
		If point2(currentplayer) = 4 Then
			score1l.State = 1
			score2l.State = 1
			score3l.State = 1
			score4l.State = 1
			bigpoints = 40000
		End If
		If point2(currentplayer) = 5 Then
			score1l.State = 1
			score2l.State = 1
			score3l.State = 1
			score4l.State = 1
			score5l.State = 1
			bigpoints = 50000
		End If
		Addtablescore
		If rainbowspins(CurrentPlayer) > 24 Then
				rain1.state = 1
		End If
		If rainbowspins(CurrentPlayer) > 49 Then
				rain2.state = 1
		End If
		If rainbowspins(CurrentPlayer) > 74 Then
				rain3.state = 1
		End If
		If rainbowspins(CurrentPlayer) > 99 Then
				rain4.state = 1
		End If
		If rainbowspins(CurrentPlayer) > 124 Then
				rain5.state = 1
		End If
		If rainbowspins(CurrentPlayer) > 149 Then
				rain6.state = 1
				rainbowgateopen
		End If

		If kickbackl(CurrentPlayer) = 1 Then
			kickbackleftenabled
		End If

		If kickbackr(CurrentPlayer) = 1 Then
			kickbackrightenabled
		End If

		PuPlayer.LabelSet pBackglass,"lefttimer2",bigpoints\10000 & "-" & point1\1000,1,"{'mt':2,'color':12971496, 'size': 4.8, 'xpos': 12.5, 'xalign': 1, 'ypos': 89.4, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"lefttimer",bigpoints\10000 & "-" & point1\1000,1,"{'mt':2,'color':26316, 'size': 4.8, 'xpos': 12.5, 'xalign': 1, 'ypos': 89.2, 'yalign': 1}"
	
		If currentlep(CurrentPlayer) = 0 Then
			setlep
		End If
		If currentlep(CurrentPlayer) = 1 Then
			lep11l.state = 1
			lep11lon(currentplayer) = 1
		elseIf currentlep(CurrentPlayer) = 2 Then
			lep21l.state = 1
			lep21lon(currentplayer) = 1
		elseIf currentlep(CurrentPlayer) = 3 Then
			lep31l.state = 1
			lep31lon(currentplayer) = 1
		elseIf currentlep(CurrentPlayer) = 4 Then
			lep41l.state = 1
			lep41lon(currentplayer) = 1
		elseIf currentlep(CurrentPlayer) = 5 Then
			lep51l.state = 1
			lep51lon(currentplayer) = 1
		End If

		If lep11lon(currentplayer) = 1 Then
			lep11l.state = 1
		End If

		If lep21lon(currentplayer) = 1 Then
			lep21l.state = 1
		End If

		If lep31lon(currentplayer) = 1 Then
			lep31l.state = 1
		End If

		If lep41lon(currentplayer) = 1 Then
			lep41l.state = 1
		End If

		If lep51lon(currentplayer) = 1 Then
			lep51l.state = 1
		End If
		
	End Sub
	

	Sub progressreset 
		if havedof = 0 Then
		PlaySoundAt "fx_droptarget", tavern1t
		end if
		If progresslevel(currentplayer) = 0 Then
			If keyfound = 1 Then
				openthekeep

				keyt3.isdropped = 1
				keyt2.isdropped = 1
				keyt1.isdropped = 1
			Else
				keyt3.isdropped = 0
				keyt2.isdropped = 1	
				keyt1.isdropped = 1
			End If
			if ballstashed = 1 Then
				tavern3t.isdropped = 1
				tavern2t.isdropped = 1
				tavern1t.isdropped = 1
			Else
				tavern3t.isdropped = 0
				tavern2t.isdropped = 1
				tavern1t.isdropped = 1
			End If
			If rainbowspins(CurrentPlayer) < 30 Then
			rainbowspins(CurrentPlayer) = 75
			End If
			taverntimer.enabled = 0
			keytimer.enabled = 0
		End If
		If progresslevel(currentplayer) = 1 Then
			If keyfound = 1 Then
				openthekeep
				keyt3.isdropped = 1
				keyt2.isdropped = 1	
				keyt1.isdropped = 1
			Else
				keyt3.isdropped = 0
				keyt2.isdropped = 1	
				keyt1.isdropped = 1
			End If
			if ballstashed = 1 Then
				tavern3t.isdropped = 1
				tavern2t.isdropped = 1
				tavern1t.isdropped = 1
			Else
				tavern3t.isdropped = 0
				tavern2t.isdropped = 1
				tavern1t.isdropped = 1
			End If
			If rainbowspins(CurrentPlayer) < 30 Then
			rainbowspins(CurrentPlayer) = 75
			End If
			taverntimer.enabled = 0
			keytimer.enabled = 0
		End If

		If progresslevel(currentplayer) = 2 Then
			If keyfound = 1 Then
				openthekeep
				keyt3.isdropped = 1
				keyt2.isdropped = 1	
				keyt1.isdropped = 1
			Else
				keyt3.isdropped = 0
				keyt2.isdropped = 0	
				keyt1.isdropped = 1
			End If
			if ballstashed = 1 Then
				tavern3t.isdropped = 1
				tavern2t.isdropped = 1
				tavern1t.isdropped = 1
			Else
				tavern3t.isdropped = 0
				tavern2t.isdropped = 0
				tavern1t.isdropped = 1
			End If
			If rainbowspins(CurrentPlayer) < 30 Then
			rainbowspins(CurrentPlayer) = 0
			End If
			taverntimer.enabled = 0
			keytimer.enabled = 0
		End If

		If progresslevel(currentplayer) = 3 Then
			If keyfound = 1 Then
				openthekeep
				keyt3.isdropped = 1
				keyt2.isdropped = 1	
				keyt1.isdropped = 1
			Else
				keyt3.isdropped = 0
				keyt2.isdropped = 0	
				keyt1.isdropped = 0
			End If
			if ballstashed = 1 Then
				tavern3t.isdropped = 1
				tavern2t.isdropped = 1
				tavern1t.isdropped = 1
			Else
				tavern3t.isdropped = 0
				tavern2t.isdropped = 0
				tavern1t.isdropped = 0
			End If
			If rainbowspins(CurrentPlayer) < 30 Then
			rainbowspins(CurrentPlayer) = 0
			End If
			taverntimer.enabled = 0
			keytimer.enabled = 0
		End If


		If progresslevel(currentplayer) = 4 Then
			If keyfound = 1 Then
				openthekeep
				keyt3.isdropped = 1
				keyt2.isdropped = 1	
				keyt1.isdropped = 1
			Else
				keyt3.isdropped = 0
				keyt2.isdropped = 0	
				keyt1.isdropped = 0
			End If
			if ballstashed = 1 Then
				tavern3t.isdropped = 1
				tavern2t.isdropped = 1
				tavern1t.isdropped = 1
			Else
				tavern3t.isdropped = 0
				tavern2t.isdropped = 0
				tavern1t.isdropped = 0
			End If
			If rainbowspins(CurrentPlayer) < 30 Then
			rainbowspins(CurrentPlayer) = 0
			End If
			level4.enabled = 1
		End If


		If progresslevel(currentplayer) = 5 Then
			If keyfound = 1 Then
				openthekeep
				keyt3.isdropped = 1
				keyt2.isdropped = 1	
				keyt1.isdropped = 1
			Else
				keyt3.isdropped = 0
				keyt2.isdropped = 0	
				keyt1.isdropped = 0
			End If
			if ballstashed = 1 Then
				tavern3t.isdropped = 1
				tavern2t.isdropped = 1
				tavern1t.isdropped = 1
			Else
				tavern3t.isdropped = 0
				tavern2t.isdropped = 0
				tavern1t.isdropped = 0
			End If
			If rainbowspins(CurrentPlayer) < 30 Then
			rainbowspins(CurrentPlayer) = 0
			End If
			level5.enabled = 1
		End If

		counttherainbow
	End Sub

	dim lev4
	Sub level4_Timer
		lev4 = lev4 + 1
		Select case lev4
			Case 1
		if havedof = 0 Then
		PlaySoundAt "fx_droptarget", tavern1t
		end if
				If keyt3.isdropped = 1 Then
					keyt3.isdropped = 0
				Elseif keyt2.isdropped = 1 Then
					keyt2.isdropped = 0
				Elseif keyt1.isdropped = 1 Then
					keyt1.isdropped = 0
				End If
				If tavern3t.isdropped = 1 Then
					tavern3t.isdropped = 0
				Elseif tavern2t.isdropped = 1 Then
					tavern2t.isdropped = 0
				Elseif tavern1t.isdropped = 1 Then
					tavern1t.isdropped = 0
				End If
				lev4 = 0
		End Select	
	End Sub


	dim lev5
	Sub level5_Timer
		lev5 = lev5 + 1
		Select case lev5
			Case 1
		if havedof = 0 Then
		PlaySoundAt "fx_droptarget", tavern1t
		end if
				If keyt3.isdropped = 1 Then
					keyt3.isdropped = 0
				Elseif keyt2.isdropped = 1 Then
					keyt2.isdropped = 0
				Elseif keyt1.isdropped = 1 Then
					keyt1.isdropped = 0
				End If
				If tavern3t.isdropped = 1 Then
					tavern3t.isdropped = 0
				Elseif tavern2t.isdropped = 1 Then
					tavern2t.isdropped = 0
				Elseif tavern1t.isdropped = 1 Then
					tavern1t.isdropped = 0
				End If
				lev5 = 0
		End Select	
	End Sub


	Sub startamultiball
		Dim waittime
		waittime = 7000
		vpmtimer.addtimer waittime, "closeupshop'"
		DOF 403, DOFPulse  'DOF-MX-Multiball
	End Sub
	
	Sub closeupshop

	End Sub

	Sub endamultiball

	End Sub


	Sub UpdateSkillShot() 'Updates the skillshot light
		tavern1l1.State = 2
	End Sub

	Sub SkillshotOff_Hit 'trigger to stop the skillshot due to a weak plunger shot
		If bSkillShotReady Then

			ResetSkillShotTimer_Timer
		End If
	End Sub

	Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
		ResetSkillShotTimer.Enabled = 0
		bSkillShotReady = False
			If tavern1l1.State = 2 then tavern1l1.State = 0
	End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  SECONDARY HIT EVENTS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 

	'**********Sling Shot Animations
	' Rstep and Lstep  are the variables that increment the animation
	'****************
	Dim RStep, Lstep

	Sub RightSlingShot1_slingshot
		FlashLevel2 = 1 : Flasherflash2_Timer
		PlaySoundAt SoundFXDOF("fx_slingshot", 105, DOFPulse, DOFContactors), lane3
		DOF 106, DOFPulse	'DOF OuterRight Flasher Green
		DOF 301, DOFPulse   'DOF MX - Right Slingshot
	End Sub

	Sub LeftSlingShot1_slingshot
		FlashLevel3 = 1 : Flasherflash3_Timer
		PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFContactors), lane2
		DOF 104, DOFPulse	'DOF OuterLeft Flasher Green
		DOF 300, DOFPulse   'DOF MX - Left Slingshot
	End Sub

	Sub RightSlingShot_Slingshot
		FlashLevel2 = 1 : Flasherflash2_Timer
		PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFContactors), lane3
		PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
		DOF 106, DOFPulse	'DOF OuterRight Flasher Green
		DOF 301, DOFPulse   'DOF MX - Right Slingshot
		RSling.Visible = 0
		RSling1.Visible = 1
		sling1.TransZ = -20
		RStep = 0
		RightSlingShot.TimerEnabled = 1

	End Sub

	Sub RightSlingShot_Timer
		Select Case RStep
			Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10:lepright.ObjRotY = lepright.ObjRotY - 20
			Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:lepright.ObjRotY = lepright.ObjRotY + 20
		End Select
		RStep = RStep + 1
	End Sub

	Sub LeftSlingShot_Slingshot
		FlashLevel3 = 1 : Flasherflash3_Timer
		PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFContactors), lane2
		DOF 104, DOFPulse	'DOF OuterLeft Flasher Green
		DOF 300, DOFPulse   'DOF MX - Left Slingshot
		PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
		LSling.Visible = 0
		LSling1.Visible = 1
		sling2.TransZ = -20
		LStep = 0
		LeftSlingShot.TimerEnabled = 1

	End Sub

	Sub LeftSlingShot_Timer
		Select Case LStep
			Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10:lepleft.ObjRotY = lepright.ObjRotY + 20
			Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:lepleft.ObjRotY = lepleft.ObjRotY - 20
		End Select
		LStep = LStep + 1
	End Sub


	Sub gate2_hit
		if havedof = 0 Then
		PlaySoundAt "fx_gate", gate2
		end if
		DOF 160, DOFPulse	'DOF-RGB-IL
		DOF 341, DOFPulse   'DOF MX - gate2
	End Sub

	Sub gate3_hit
		if havedof = 0 Then
		PlaySoundAt "fx_gate", gate2
		end if
		DOF 161, DOFPulse
		DOF 340, DOFPulse   'DOF MX - gate3
	End Sub

	Sub toplane1t_hit
		if havedof = 0 Then
		PlaySoundAt "fx_sensor", toplane1t
		end if
		DOF 160, DOFPulse	'DOF-RGB-IL
		DOF 333, DOFPulse   'DOF MX -toplane1
		PuPlayer.playlistplayex pAudio,"sfx","sheep.wav",sfxvol,1
		If toplane1l.state = 0 Then
		addtablescore
		toplane1l.state = 1
		End If
		checklanes
	End Sub

	Sub toplane2t_hit
		if havedof = 0 Then
		PlaySoundAt "fx_sensor", toplane1t
		end if
		DOF 150, DOFPulse	'DOF-RGB-IR
		DOF 334, DOFPulse   'DOF MX - toplane2
		PlaySoundAt SoundFXDOF("fx_sensor", 103, DOFPulse, DOFContactors), toplane1t
		PuPlayer.playlistplayex pAudio,"sfx","sheep2 by confusion_music.wav",sfxvol,1
		If toplane2l.state = 0 Then
		addtablescore
		toplane2l.state = 1
		End If

		checklanes
	End Sub

	Sub toplane3t_hit
		if havedof = 0 Then
		PlaySoundAt "fx_sensor", toplane1t
		end if
		DOF 148, DOFPulse	'DOF-RGB-OR
		DOF 335, DOFPulse   'DOF MX - toplane3
		PuPlayer.playlistplayex pAudio,"sfx","sheep3 by klankbeeld.wav",sfxvol,1
		If toplane3l.state = 0 Then
		addtablescore
		toplane3l.state = 1
		End If
		checklanes
	End Sub

	Sub checklanes
		If toplane2l.state = 1 Then
			If toplane1l.state + toplane2l.state + toplane3l.state = 3 Then
				addtablescore
				toplane1l.state = 0
				toplane2l.state = 0
				toplane3l.state = 0
				PuPlayer.playlistplayex pBackglass,"videoscenes","Lane_bonus.mp4",videovol,1
			End If
		End If
	End Sub

	Sub topbank1t_hit
		if havedof = 0 Then
		PlaySoundAt "fx_target", topbank1t
		end if
		DOF 150, DOFPulse	'DOF-RGB-IR
		DOF 330, DOFPulse   'DOF MX - topbank
		If topbank1l.state = 0 Then
		addtablescore
		topbank1l.state = 1
		topbank1lon(CurrentPlayer) = 1
		End If
		checkbank
	End Sub

	Sub topbank2t_hit
		if havedof = 0 Then
		PlaySoundAt "fx_target", topbank1t
		end if
		DOF 150, DOFPulse	'DOF-RGB-IR
		DOF 330, DOFPulse   'DOF MX - topbank
		If topbank2l.state = 0 Then
		addtablescore
		topbank2l.state = 1
		topbank2lon(CurrentPlayer) = 1
		End If
		checkbank
	End Sub

	Sub topbank3t_hit
		if havedof = 0 Then
		PlaySoundAt "fx_target", topbank1t
		end if
		DOF 150, DOFPulse	'DOF-RGB-IR
		DOF 330, DOFPulse   'DOF MX - topbank
		If topbank3l.state = 0 Then
		addtablescore
		topbank3l.state = 1
		topbank3lon(CurrentPlayer) = 1
		End If
		checkbank
	End Sub

	Sub checkbank
		If topbank2l.state = 1 Then
			If topbank1l.state + topbank2l.state + topbank3l.state = 3 Then
				addtablescore
				topbank1l.state = 0
				topbank2l.state = 0
				topbank3l.state = 0
				topbank1lon(CurrentPlayer) = 0
				topbank2lon(CurrentPlayer) = 0
				topbank3lon(CurrentPlayer) = 0
				PuPlayer.playlistplayex pCallouts,"cheers-L" & currentlep(CurrentPlayer),"",100,1	
		chilloutthemusic
			End If
		End If
	End Sub


	Sub RotateLaneLightsLeft
		Dim TempState
		TempState = toplane1l.State
		toplane1l.State = toplane2l.State
		toplane2l.State = toplane3l.State
		toplane3l.State = TempState
		Dim lowertempstate
		lowertempstate = lane1l.State
		lane1l.State = lane2l.State
		lane2l.State = lane3l.State
		lane3l.State = lane4l.State
		lane4l.State = lowertempstate
	End Sub


	Sub RotateebLights
		If ebr.state = 1 Then
			ebr.state = 0 
			ebl.state = 1
		ElseIf ebl.state = 1 Then
			ebr.state = 1
			ebl.state = 0
		End If
	End Sub





	Sub RotateLaneLightsRight
		Dim TempState
		TempState = toplane3l.State
		toplane3l.State = toplane2l.State
		toplane2l.State = toplane1l.State
		toplane1l.State = tempstate
		Dim lowertempstate
		lowertempstate = lane4l.State
		lane4l.State = lane3l.State
		lane3l.State = lane2l.State
		lane2l.State = lane1l.State
		lane1l.State = lowertempstate
	End Sub


	
	'// Rainbow spinners
	Sub counttherainbow
		If bMultiBallMode = True Then
			Exit Sub
		End If
		rainbowspins(CurrentPlayer) = rainbowspins(CurrentPlayer) + 1
		AddScorePercent 10
		checkifmbready
		Select Case rainbowspins(CurrentPlayer)
			Case 25
				rain1.state = 1
				LightSeqhit.Play SeqCircleOutOn,10,1,04
				if havedof = 0 Then
				PlaySoundAt "rainbow", rainspin3
				end if
				PuPlayer.playlistplayex pCallouts,"poweruptherainbowmachine-L" & currentlep(CurrentPlayer),"",100,1
		chilloutthemusic
			Case 50
				rain2.state = 1
				LightSeqhit.Play SeqCircleOutOn,10,1,0
				if havedof = 0 Then
				PlaySoundAt "rainbow", rainspin3
				end if
			Case 75
				rain3.state = 1
				LightSeqhit.Play SeqCircleOutOn,10,1,0
				if havedof = 0 Then
				PlaySoundAt "rainbow", rainspin3
				end if
			Case 100
				rain4.state = 1
				LightSeqhit.Play SeqCircleOutOn,10,1,0
				if havedof = 0 Then
				PlaySoundAt "rainbow", rainspin3
				end if
				PuPlayer.playlistplayex pCallouts,"rainbowmachineisalmoston-L" & currentlep(CurrentPlayer),"",100,1
				PuPlayer.playlistplayex pBackglass,"videoscenes","PowerUP RainbowM.mp4",videovol,1
		chilloutthemusic
			Case 125
				rain5.state = 1
				LightSeqhit.Play SeqCircleOutOn,10,1,0
				if havedof = 0 Then
				PlaySoundAt "rainbow", rainspin3
				end if
			Case 150
				rain6.state = 1
				rainbowgateopen
				LightSeqhit.Play SeqCircleOutOn,10,1,0
				if havedof = 0 Then
				PlaySoundAt "rainbow", rainspin3
				end if
				DOF 176, DOFPulse	'DOF-RGB-OR
				DOF 356, DOFPulse   'DOF MX - rainbowopen
				PuPlayer.playlistplayex pCallouts,"rainbowmachineison-L" & currentlep(CurrentPlayer),"",100,1
		PuPlayer.playlistplayex pBackglass,"videoscenes","Rainbow machine.mp4",videovol,1
		chilloutthemusic
		End Select
		If rainbowspins(CurrentPlayer) > 149 Then
				rain6.state = 1
				rainbowgateopen
		End If
	End Sub

	Sub rainbowgateopen
		'PlaySoundAt SoundFXDOF("stone2", 103, DOFPulse, DOFContactors), Target12
		DOF 176, DOFPulse
		DOF 356, DOFPulse   'DOF MX - rainbowopen
		If Target12.isdropped = 0 Then
		Target12.isdropped = 1
		Target1.isdropped = 1
		end if
		If rainbowleft.z = 100 Then
		Else
		rainbowleft.z = 100
		rainbowright.z = 100
		cloudleft.z = 130
		cloudright.z = 130
		End If
	End Sub

	Sub rainbowgateclosed
		Target12.isdropped = 0
		Target1.isdropped = 0
		If rainbowleft.z = 100 Then
			rainbowleft.z = -500
			rainbowright.z = -500
		cloudleft.z = -430
		cloudright.z = -430
		End If
	End Sub

	sub spinlap_hit
		flasherspop green,"center"
	end Sub

	Sub rainspin1_Spin
		'FlashLevel4 = 1 : Flasherflash4_Timer
		DOF 171, DOFPulse	'DOF-RGB-OL
		DOF 354, DOFPulse   'DOF MX - rain1-back
		if havedof = 0 Then
			PlaySoundAt "fx_spinner", rainspin1
		end if

		If Not Tilted Then
			counttherainbow
		End If
	End Sub

	Sub rainspin2_Spin
		DOF 172, DOFPulse	'DOF-RGB-CL
		if havedof = 0 Then
			PlaySoundAt "fx_spinner", rainspin1
		end if
		If Not Tilted Then
			counttherainbow
		End If
	End Sub
	Sub rainspin3_Spin
				DOF 173, DOFPulse	'DOF-RGB-C
		if havedof = 0 Then
			PlaySoundAt "fx_spinner", rainspin1
		end if
		If Not Tilted Then
			counttherainbow
		End If
	End Sub
	Sub rainspin4_Spin
		DOF 174, DOFPulse	'DOF-RGB-CR
		if havedof = 0 Then
			PlaySoundAt "fx_spinner", rainspin1
		end if
		If Not Tilted Then
			counttherainbow
		End If
	End Sub
	Sub rainspin5_Spin
		'FlashLevel5 = 1 : Flasherflash5_Timer
		DOF 175, DOFPulse	'DOF-RGB-OR
		DOF 355, DOFPulse   'DOF MX - rain5-backr
		if havedof = 0 Then
			PlaySoundAt "fx_spinner", rainspin1
		end if
		If Not Tilted Then
			counttherainbow
		End If
	End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  KICKBACKS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 

'kickbackl(CurrentPlayer), kickbackr(CurrentPlayer)

	Sub closekickbacks
		kickbacklg.open = False
		kickbackll.state = 0
		kickbackrg.open = False
		kickbackrl.state = 0
	End Sub

	Sub kickbackleftenabled
		kickbacklg.open = True
		kickbackll.state = 1
		kickbackl(CurrentPlayer) = 1
	End Sub

	Sub kickbackleftdisabled
		kickbacklg.open = False
		kickbackll.state = 0
		kickbackl(CurrentPlayer) = 0
	End Sub

	Sub kickbackrightenabled
		kickbackrg.open = True
		kickbackrl.state = 1
		kickbackr(CurrentPlayer) = 1
	End Sub

	Sub kickbackrightdisabled
		kickbackrg.open = False
		kickbackrl.state = 0
		kickbackr(CurrentPlayer) = 0
	End Sub

	Sub kickbacklk_hit
		PlaySoundAt SoundFXDOF("fx_kicker", 103, DOFPulse, DOFContactors), kickbacklk
		DOF 171, DOFPulse	'DOF-RGB-OL
		DOF 115, DOFPulse   'DOF - Strobe
		DOF 309, DOFPulse   'DOF MX - Kickback
		kickbacklk.Kick 0, 16
		kickbackleftdisabled
		EnableBallSaver 4
	End Sub

	Sub kickbackrk_hit 
		PlaySoundAt SoundFXDOF("fx_kicker", 114, DOFPulse, DOFContactors), kickbackrk
		DOF 175, DOFPulse	'DOF-RGB-OR
		DOF 115, DOFPulse   'DOF - Strobe
		DOF 309, DOFPulse   'DOF MX - Kickback
		kickbackrk.Kick 0, 25
		kickbackrightdisabled
		EnableBallSaver 4
	End Sub

	'LANES

	Sub lane1_hit
		if havedof = 0 Then
			PlaySoundAt "fx_sensor", lane1
		end if
		DOF 144, DOFPulse
		DOF 313, DOFPulse   'DOF MX - Left Outer Lan
		Addtablescore
		lane1l.state = 1
		lane1lon(CurrentPlayer) = 1
		checkforkickbacks
	End Sub

	Sub lane2_hit 
		if havedof = 0 Then
			PlaySoundAt "fx_sensor", lane2
		end if
		DOF 146, DOFPulse	'DOF-RGB-IL
		DOF 315, DOFPulse   'DOF MX - Left Inner Lane
		Addtablescore
		lane2l.state = 1
		lane2lon(CurrentPlayer) = 1
		checkforkickbacks
		if ebl.state = 1 Then
			AwardExtraBall
			eblon(CurrentPlayer) = 0
			ebl.state = 0
			ebr.state = 0
		End If
	End Sub

	Sub lane3_hit 
		if havedof = 0 Then
			PlaySoundAt "fx_sensor", lane3
		end if
		DOF 147, DOFPulse	'DOF-RGB-IR
		DOF 316, DOFPulse   'DOF MX - Right Inner Lane
		Addtablescore
		lane3l.state = 1
		lane3lon(CurrentPlayer) = 1
		checkforkickbacks
		if ebr.state = 1 Then
			AwardExtraBall
			eblon(CurrentPlayer) = 0
			ebl.state = 0
			ebr.state = 0
		End If
	End Sub

	Sub lane4_hit 
		if havedof = 0 Then
			PlaySoundAt "fx_sensor", lane4
		end if
		DOF 148, DOFPulse	'DOF-RGB-OR
		DOF 314, DOFPulse   'DOF MX - Right Outer Lane
		Addtablescore
		lane4l.state = 1
		lane4lon(CurrentPlayer) = 1
		checkforkickbacks
	End Sub

	Sub checkforkickbacks
		If lane4l.state + lane3l.state + lane2l.state + lane1l.state = 4 Then
		If hardmode = 0 Then

			If kickbackl(CurrentPlayer) = 0 Then
				kickbackleftenabled
				clearlanelights
		PuPlayer.playlistplayex pCallouts,"kickbackactivated-L" & currentlep(CurrentPlayer),"",100,1
		PuPlayer.playlistplayex pBackglass,"videoscenes","kickback.mp4",videovol,1
		chilloutthemusic
			Else
				If kickbackr(CurrentPlayer) = 0 Then
					kickbackrightenabled
					clearlanelights
		PuPlayer.playlistplayex pCallouts,"kickbackactivated-L" & currentlep(CurrentPlayer),"",100,1
		PuPlayer.playlistplayex pBackglass,"videoscenes","kickback.mp4",videovol,1
		chilloutthemusic
				End If
			End If
		End if
		End If
	End Sub

	Sub clearlanelights
		lane1l.state = 0
		lane2l.state = 0
		lane3l.state = 0
		lane4l.state = 0
		lane1lon(CurrentPlayer) = 0
		lane2lon(CurrentPlayer) = 0
		lane3lon(CurrentPlayer) = 0
		lane4lon(CurrentPlayer) = 0
	End Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  THE KEEP & KEY / BAR LOCKS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'
	Sub keyt1_hit
		flasherspop green,"left"
		strippop base,"bwsweep"
		if havedof = 0 Then
			PlaySoundAt "fx_droptarget", keyt1
		end if
		DOF 104, DOFPulse	'DOF-RGB-OL
		DOF 332, DOFPulse   'DOF MX - keytargets
		DOF 333, DOFPulse   'DOF MX - keytargetsleft
			Addtablescore
		LightSeqhit.Play SeqRightOn,10,1,0
		PuPlayer.playlistplayex pCallouts,"getthekey-L" & currentlep(CurrentPlayer),"",100,1
		PuPlayer.playlistplayex pBackglass,"videoscenes","getkey.mp4",videovol,1
		chilloutthemusic
	End Sub

	Sub keyt2_hit
		flasherspop green,"left"
		strippop base,"bwsweep"
		if havedof = 0 Then
			PlaySoundAt "fx_droptarget", keyt1
		end if
		DOF 104, DOFPulse	'DOF-RGB-OL
		DOF 332, DOFPulse   'DOF MX - keytargets
		DOF 333, DOFPulse   'DOF MX - keytargetsleft
			Addtablescore
		LightSeqhit.Play SeqRightOn,10,1,0
		PuPlayer.playlistplayex pCallouts,"getthekey-L" & currentlep(CurrentPlayer),"",100,1
		PuPlayer.playlistplayex pBackglass,"videoscenes","getkey.mp4",videovol,1
		chilloutthemusic
	End Sub

	Sub keyt3_hit
		strippop base,"bwsweep"
		flasherspop green,"left"
		if havedof = 0 Then
			PlaySoundAt "fx_droptarget", keyt1
		end if
		DOF 104, DOFPulse	'DOF-RGB-OL
		DOF 332, DOFPulse   'DOF MX - keytargets
		DOF 333, DOFPulse   'DOF MX - keytargetsleft
			Addtablescore
		LightSeqhit.Play SeqRightOn,10,1,0
		PuPlayer.playlistplayex pCallouts,"getthekey-L" & currentlep(CurrentPlayer),"",100,1
		chilloutthemusic
		PuPlayer.playlistplayex pBackglass,"videoscenes","getkey.mp4",videovol,1
	End Sub

	Sub tavern1t_hit
		strippop base,"bwsweep"
		flasherspop white,"right"
		if havedof = 0 Then
			PlaySoundAt "fx_droptarget", tavern1t
		end if
		DOF 132, DOFPulse	'DOF-RGB-OR
		DOF 331, DOFPulse   'DOF MX - Taverntargets
			Addtablescore
		If tavern1l1.state = 2 Then
			AwardSkillshot
			tavern1l1.state = 0
			PuPlayer.playlistplayex pCallouts,"skillshot-L" & currentlep(CurrentPlayer),"",100,1
			PuPlayer.playlistplayex pBackglass,"videoscenes","skillshot.mp4",videovol,1	
			chilloutthemusic
		Else
		PuPlayer.playlistplayex pBackglass,"videoscenes","Get_Tavern_Lock.mp4",videovol,1
		End If
		LightSeqhit.Play SeqLeftOn,10,1,0

		PuPlayer.playlistplayex pAudio,"sfx","burp.wav",sfxvol,1
	End Sub

	Sub tavern2t_hit
		strippop base,"bwsweep"
		flasherspop white,"right"
		if havedof = 0 Then
			PlaySoundAt "fx_droptarget", tavern1t
		end if
		DOF 132, DOFPulse	'DOF-RGB-OR
		DOF 331, DOFPulse   'DOF MX - Taverntargets
			Addtablescore
		If tavern1l1.state = 2 Then
			AwardSkillshot
			tavern1l1.state = 0
			PuPlayer.playlistplayex pCallouts,"skillshot-L" & currentlep(CurrentPlayer),"",100,1
			PuPlayer.playlistplayex pBackglass,"videoscenes","skillshot.mp4",videovol,1	
			chilloutthemusic
		Else 
		PuPlayer.playlistplayex pBackglass,"videoscenes","Get_Tavern_Lock.mp4",videovol,1
		End If
		LightSeqhit.Play SeqLeftOn,10,1,0

		PuPlayer.playlistplayex pAudio,"sfx","burp.wav",sfxvol,1
	End Sub

	Sub tavern3t_hit
		strippop base,"bwsweep"
		flasherspop white,"right"
		if havedof = 0 Then
			PlaySoundAt "fx_droptarget", tavern1t
		end if
		DOF 132, DOFPulse	'DOF-RGB-OR
		DOF 331, DOFPulse   'DOF MX - Taverntargets
			Addtablescore
		If tavern1l1.state = 2 Then
			AwardSkillshot
			tavern1l1.state = 0
			PuPlayer.playlistplayex pCallouts,"skillshot-L" & currentlep(CurrentPlayer),"",100,1
			PuPlayer.playlistplayex pBackglass,"videoscenes","skillshot.mp4",videovol,1	
			chilloutthemusic
		Else
		PuPlayer.playlistplayex pBackglass,"videoscenes","Get_Tavern_Lock.mp4",videovol,1
		End If

		PuPlayer.playlistplayex pAudio,"sfx","burp.wav",sfxvol,1
		LightSeqhit.Play SeqLeftOn,10,1,0
	End Sub
	
	Sub gate9_hit
		if havedof = 0 Then
			PlaySoundAt "fx_gate", gate9
		end if
		DOF 115, DOFPulse	'DOF-Strobes
		DOF 161, DOFPulse	'DOF-RGB-IR
		DOF 342, DOFPulse   'DOF MX - gate9-back
		PuPlayer.playlistplayex pCallouts,"needthekeyfirst-L" & currentlep(CurrentPlayer),"",100,1
		PuPlayer.playlistplayex pAudio,"sfx","doorthud by hitrison.wav",sfxvol,1
		PuPlayer.playlistplayex pBackglass,"videoscenes","KeepLocked.mp4",videovol,1
		chilloutthemusic
	End Sub

	Sub gate1_hit
		if havedof = 0 Then
			PlaySoundAt "fx_gate", gate1
		end if
		DOF 115, DOFPulse	'DOF-Strobes	
		DOF 160, DOFPulse	'DOF-RGB-IL
		DOF 343, DOFPulse   'DOF MX - gate1-back
		PuPlayer.playlistplayex pCallouts,"needthekeyfirst-L" & currentlep(CurrentPlayer),"",100,1
		PuPlayer.playlistplayex pAudio,"sfx","doorthud by hitrison.wav",sfxvol,1
		PuPlayer.playlistplayex pBackglass,"videoscenes","KeepLocked.mp4",videovol,1
		chilloutthemusic
	End Sub

	Sub target12_hit

		rainbowcall
	End Sub

	Sub Target1_hit

		rainbowcall
	End Sub

	Sub rainbowcall
		dim rainhitnum
		rainhitnum = RndNum(1,5)
		select case rainhitnum
			case 1
			PuPlayer.playlistplayex pCallouts,"notpowerupyet-L" & currentlep(CurrentPlayer),"",100,1
		chilloutthemusic
		End Select
	End Sub

	Sub closethekeep
		if havedof = 0 Then
			PlaySoundAt "fx_droptarget",  keepleft
		end if

		keepleft.isdropped = 0
		keepright.isdropped = 0
		findkey.state = 0
			castlegate.isdropped = 0
	End Sub

	Sub openthekeep
	'PlaySoundAt SoundFXDOF("fx_droptarget", 103, DOFPulse, DOFContactors), keepleft
		keepleft.isdropped = 1
		keepright.isdropped = 1
		findkey.state = 1
		'PuPlayer.playlistplayex pBackglass,"videoscenes","Steal-the-Gold.mp4",videovol,1
	End Sub

	Sub keylock_hit
		strippop base,"bwsweep"
		flasherspop green,"left"
		PlaySoundAt SoundFXDOF("fx_kicker_enter", 134, DOFPulse, DOFContactors), keylock
		DOF 181, DOFPulse	'DOF-bell
		DOF 115, DOFPulse	'DOF-Strobe
		DOF 125, DOFPulse	'DOF-RGB-Center
		DOF 404, DOFPulse   'DOF MX - keylock
			Addtablescore
		LightSeqhit.Play SeqRightOn,10,3,0
		If bMultiBallMode = true Then
			keyl.state = 1
			keyl1.state = 0
			Dim waittime
			waittime = 20000
			vpmtimer.addtimer waittime, "mbholder'"
			vpmtimer.addtimer 18500, "kickoutlights'"
			mblevel = mblevel + 1
			checkmultimulti
			exit sub
		End If
		PuPlayer.playlistplayex pCallouts,"keyfound-L" & currentlep(CurrentPlayer),"",100,1
		PuPlayer.playlistplayex pBackglass,"videoscenes","keyfound.mp4",videovol,1
		chilloutthemusic
		Addmultiball 1
		openthekeep
		findkey.state = 1
		keyfound = 1
		checkifmbready
	End Sub

	Sub mbholder
		if havedof = 0 Then
			PlaySoundAt "fx_kicker",  keylock
		end if

		keylock.Kick 180, 15
		tavernlock.Kick 180, 15
		tavern1l.state = 0
		keyl.state = 0
		If bMultiBallMode = true then
		keyl1.state = 2
		tavern1l1.state = 2
		End If
		mblevel = 1
		checkmultimulti
	End Sub

	sub kickoutlights
		If bMultiBallMode = true Then
			if tavern1l.state = 1 Then
				flasherspop white,"rightkick"
			end If
			if keyl.state = 1 Then
				flasherspop white,"leftkick"
			end If
		end If
	end Sub

	Sub tavernlock_hit
		strippop base,"bwsweep"
		flasherspop white,"right"
		PlaySoundAt SoundFXDOF("fx_kicker_enter", 119, DOFPulse, DOFContactors), tavernlock
		DOF 181, DOFPulse	'DOF-Bell
		DOF 115, DOFPulse	'DOF-Strobe
		DOF 173, DOFPulse	'DOF MX 
		DOF 405, DOFPulse   'DOF MX - tavernlock
			Addtablescore
		LightSeqhit.Play SeqLeftOn,10,3,0
		If bMultiBallMode = true Then
			tavern1l.state = 1
			tavern1l1.state = 0
			Dim waittime
			waittime = 20000
			vpmtimer.addtimer waittime, "mbholder'"
			vpmtimer.addtimer 18500, "kickoutlights'"
			mblevel = mblevel + 1
			checkmultimulti
			exit sub
		End If
		PuPlayer.playlistplayex pCallouts,"cheers-L" & currentlep(CurrentPlayer),"",100,1
		PuPlayer.playlistplayex pBackglass,"videoscenes","Tavern Ball Locked.mp4",videovol,1
		chilloutthemusic
		AddMultiball 1
		tavernstashl.state = 1
		ballstashed = 1
	End Sub

	Sub checkmultimulti
		Select case mblevel
			case 1
			twoxl.state = 0
			threexl.state = 0	
			case 2
			twoxl.intensity = 30
			twoxl.state = 2
			PuPlayer.playlistplayex pBackglass,"videoscenes","Playfield_Doubled.mp4",videovol,1
			case 3
			threexl.intensity = 30
			threexl.state = 2
			PuPlayer.playlistplayex pBackglass,"videoscenes","Playfield_Tripled.mp4",videovol,1
		End Select
		If mblevel = 3 Then
			If(BallsOnPlayfield = 2) Then
				keylock.Kick 180, 15
				tavernlock.Kick 180, 15
		PlaySoundAt SoundFXDOF("fx_kicker", 114, DOFPulse, DOFContactors), keylock
			End If
		End If
	End Sub

	dim mbco:mbco = 0

	Sub checkifmbready
		If keyfound = 1 then
			If rainbowspins(CurrentPlayer) > 149 Then
				locollect.state = 2
				rocollect.state = 2
				openthekeep
				if mbco = 0 Then
				PuPlayer.playlistplayex pCallouts,"stealthegold-L" & currentlep(CurrentPlayer),"",100,1
				chilloutthemusic
				mbco = 1
				end if 
			End If
		End If
	End Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  MAIN SHOTS - PRIMARY HIT EVENTS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 

	'****************
	'     ORBITS
	'****************




'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  MODES
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 

	


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  BUMPERS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 


	Sub Bumper1_Hit
		FlashLevel1 = 1 : Flasherflash1_Timer
		LightEffect 5
		If NOT Tilted Then
			PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), ActiveBall
			DOF 110, DOFPulse
			DOF 302, DOFPulse   'DOF MX - Bumper 1
			AddScorePercent 3
			countbumps
		End If
	End Sub

	Sub Bumper2_Hit
		FlashLevel6 = 1 : Flasherflash6_Timer
		LightEffect 5
		If NOT Tilted Then
			PlaySoundAt SoundFXDOF("fx_bumper", 109, DOFPulse, DOFContactors), ActiveBall
			DOF 111, DOFPulse
			DOF 303, DOFPulse   'DOF MX - Bumper 2
			AddScorePercent 3
			countbumps
		End If
	End Sub

	Sub Bumper3_Hit
		FlashLevel4 = 1 : Flasherflash4_Timer
		LightEffect 5
		If NOT Tilted Then
			PlaySoundAt SoundFXDOF("fx_bumper", 108, DOFPulse, DOFContactors), ActiveBall
			DOF 112, DOFPulse
			DOF 304, DOFPulse   'DOF MX - Bumper 3
			AddScorePercent 3
			countbumps
		End If
	End Sub

	sub countbumps

		bumps(currentplayer) = bumps(CurrentPlayer) + 1
		Select Case bumps(CurrentPlayer)
			Case 4
			Addtablescore
			bumps(CurrentPlayer) = 0
		End Select
	End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  FLIPPER LANES
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 


	Sub LeftInlane_Hit

		PlaySoundAt "fx_sensor", ActiveBall
		If bMultiBallMode = False Then
		End If
		If Tilted Then Exit Sub
		LaneBonus = LaneBonus + 1
		'll1.State = 1
		'll8.State = 1
		AddScore 50050
		' Do some sound or light effect
		LastSwitchHit = "lane1"
	End Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  LEPRECHAUN CHOOSER & CHECKER
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 

	'dims to use (defined above)
	' progresslevel, currentlep, col - leplights,lep1done,lep2done,lep3done,lep4done,lep5done


	Sub setlep
		LightSeqLeps.UpdateInterval = 5
		LightSeqLeps.Play SeqRandom, 10,,1000


		If progresslevel(CurrentPlayer) > 0 Then
			If currentlep(CurrentPlayer) = 1 Then
				lep12l.state = 1
				lep1isdone(CurrentPlayer) = 1
			elseIf currentlep(CurrentPlayer) = 2 Then
				lep22l.state = 1
				lep2isdone(CurrentPlayer) = 1
			elseIf currentlep(CurrentPlayer) = 3 Then
				lep32l.state = 1
				lep3isdone(CurrentPlayer) = 1
			elseIf currentlep(CurrentPlayer) = 4 Then
				lep42l.state = 1
				lep4isdone(CurrentPlayer) = 1
			End If
			currentlep(CurrentPlayer) = currentlep(CurrentPlayer) + 1
			If currentlep(CurrentPlayer) > 4 Then
				currentlep(CurrentPlayer) = 1
			End If
		End If


		If progresslevel(CurrentPlayer) = 0 Then
			currentlep(CurrentPlayer) = RndNum(1,1)
			If CurrentPlayer = 1 Then
				PuPlayer.playlistplayex pCallouts,"welcometotheemeraldIsle-L" & currentlep(CurrentPlayer),"",100,1
		chilloutthemusic
			End If
			progresslevel(CurrentPlayer) = 1
		End If



		If progresslevel(CurrentPlayer) = 5 Then
			currentlep(CurrentPlayer) = 5
		End If
		

	End Sub




'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  GRAB THE GOLD!
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 
	dim mbstarter:mbstarter = 0
	Sub keepkicker_hit
			mbstarter = 1
			PlaySoundAt SoundFXDOF("fx_droptarget", 103, DOFPulse, DOFContactors), tavern1t
			PuPlayer.playlistplayex pAudio,"sfx","siren by samsterbirdies.wav",sfxvol,1
			PlaySoundAt SoundFXDOF("fx_kicker_enter", 114, DOFPulse, DOFContactors), keepkicker
			Addtablescore
			PuPlayer.playlistplayex pCallouts,"stealthegold-L" & currentlep(CurrentPlayer),"",100,1
			chilloutthemusic
			PuPlayer.playlistplayex pBackglass,"videoscenes","Balls_kicking_out.mp4",videovol,1
				DOF 181, DOFPulse	'DOF-Bell
			DOF 151, DOFPulse   'DOF RGB-all
			DOF 115, DOFPulse   'DOF -Strobes
			DOF 127, DOFOn  'DOF beaconlong
			DOF 413, DOFPulse   'DOF MX - MultiPF
			DOF 414, DOFPulse   'DOF MX - MultiPr
			DOF 415, DOFPulse   'DOF MX - Multipl
		if rockmusic = 1 Then
		PuPlayer.playlistplayex pMusic,"mbaudio","Dropkick Murphys - I'm Shipping Up To Boston ..with lyrics.mp3",videovol + 20,1
		else
		PuPlayer.playlistplayex pMusic,"mbaudio","Shake_That_Little_Foot_-_02_-_Bill_Cheathum.mp3",videovol + 20,1
		end if
			Dim waittimenow
			waittimenow = 7000
			vpmtimer.addtimer waittimenow, "startmb'"
			Dim waittime
			waittime = 7500
			vpmtimer.addtimer waittimenow, "kickbackup'"
			bMultiBallMode = True
			'StartRainbow alights
			StartRainbow2 GI
			LightSeqhit.UpdateInterval = 3
			LightSeqhit.Play SeqWiperRightOn,100,3,0
			LightSeqAttract2.UpdateInterval = 3
			LightSeqAttract2.Play SeqWiperRightOn,100,3,0
			tavern3t.isdropped = 1
			tavern2t.isdropped = 1
			tavern1t.isdropped = 1
			keyt3.isdropped = 1
			keyt2.isdropped = 1	
			keyt1.isdropped = 1
			castlegate.isdropped = 1
		level4.enabled = 0
		level5.enabled = 0
	End Sub

	Sub kickbackup
		keylock.Kick 180, 15
		tavernlock.Kick 180, 15
		keepkicker.Kick 180, 15
	End Sub

	Sub startmb
		mbstarter = 0

		PlaySoundAt SoundFXDOF("fx_kicker", 105, DOFPulse, DOFContactors), keylock
		DOF 115, DOFPulse   'DOF -Strobes
		PuPlayer.playlistplayex pBackglass,"videoscenes","Multiball_holding.mp4",videovol,1
		PuPlayer.SetLoop 2,1
		mblevel = 1
		LightSeqAttract2.StopPlay
		LightSeqhit.StopPlay
		ChangeGi "green"
		tavernstashl.state = 0
		tavern1l1.state = 2
		keyl1.state = 2
		lojackpot1.state = 2
		rojackpot1.state = 2
		keylock.Kick 180, 15
		tavernlock.Kick 180, 15
		keepkicker.Kick 180, 15
		ballstashed = 0
		keyfound = 0
		EnableBallSaver 7
		rainbowspins(CurrentPlayer) = 0
		Dim waittime
		waittime = 500
		raiseramps
		vpmtimer.addtimer waittime, "closethekeep'"
		level4.enabled = 0
		level5.enabled = 0
	End Sub

	Sub raiseramps
		keepleftramp.collidable = 1
		keeprightramp.collidable = 1

		If stairramp.z = 47.5 Then
		Else
		stairramp.z = 47.5
		End If
		if havedof = 0 Then
			PlaySoundAt "fx_motor",  keepjackpot
		end if

	End Sub

	Sub lowerramps
		If stairramp.z = -100 Then
		Else
		stairramp.z = -100
		End If
		keepleftramp.collidable = 0
		keeprightramp.collidable = 0
		if havedof = 0 Then
			PlaySoundAt "fx_motor",  keepjackpot
		end if
	End Sub

	Sub keepjackpot_hit
		strippop base,"bwsweep"
		flasherspop amber,"crazy"
		if havedof = 0 Then
			PlaySoundAt "fx_gate",  keepjackpot
		end if
		PlaySoundAt SoundFXDOF("coin", 105, DOFPulse, DOFContactors), kickbackrk
		ChangeBall(2)
		vpmtimer.addtimer 4000, "ohnocall '"
		If mblevel = 1 Then
			PuPlayer.playlistplayex pCallouts,"jackpot-L" & currentlep(CurrentPlayer),"",100,1
			PuPlayer.playlistplayex pBackglass,"videoscenes","jackpot.mp4",videovol,1
			goldstole(CurrentPlayer) = goldstole(CurrentPlayer) + 1 + progresslevel(CurrentPlayer)
			DOF 151, DOFPulse
			DOF 410, DOFPulse   'DOF MX - Jackpot1
			PuPlayer.LabelSet pBackglass,"righttimer2","" & goldstole(CurrentPlayer),1,"{'mt':2,'color':12971496, 'size': 4.8, 'xpos': 87.8, 'xalign': 1, 'ypos': 89.4, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"righttimer","" & goldstole(CurrentPlayer),1,"{'mt':2,'color':2370617, 'size': 4.8, 'xpos': 87.8, 'xalign': 1, 'ypos': 89.2, 'yalign': 1}"
LightSeqhit.Play SeqDownOn,10,1,0		
End If
		If mblevel = 2 Then
			goldstole(CurrentPlayer) = goldstole(CurrentPlayer) + 2 + progresslevel(CurrentPlayer)
							DOF 151, DOFPulse
			DOF 411, DOFPulse   'DOF MX - Jackpot2
			PuPlayer.LabelSet pBackglass,"righttimer2","" & goldstole(CurrentPlayer),1,"{'mt':2,'color':12971496, 'size': 4.8, 'xpos': 87.8, 'xalign': 1, 'ypos': 89.4, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"righttimer","" & goldstole(CurrentPlayer),1,"{'mt':2,'color':2370617, 'size': 4.8, 'xpos': 87.8, 'xalign': 1, 'ypos': 89.2, 'yalign': 1}"
			PuPlayer.playlistplayex pCallouts,"doublejackpot-L" & currentlep(CurrentPlayer),"",100,1
			PuPlayer.playlistplayex pBackglass,"videoscenes","doublejackpot.mp4",videovol,1
LightSeqhit.Play SeqDownOn,10,2,0
		End If
		If mblevel = 3 Then
					DOF 151, DOFPulse
			DOF 323, DOFPulse   'DOF MX - Jackpot3
			goldstole(CurrentPlayer) = goldstole(CurrentPlayer) + 3 + progresslevel(CurrentPlayer)
			PuPlayer.LabelSet pBackglass,"righttimer2","" & goldstole(CurrentPlayer),1,"{'mt':2,'color':12971496, 'size': 4.8, 'xpos': 87.8, 'xalign': 1, 'ypos': 89.4, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"righttimer","" & goldstole(CurrentPlayer),1,"{'mt':2,'color':2370617, 'size': 4.8, 'xpos': 87.8, 'xalign': 1, 'ypos': 89.2, 'yalign': 1}"
			PuPlayer.playlistplayex pCallouts,"triplejackpot-L" & currentlep(CurrentPlayer),"",100,1
			PuPlayer.playlistplayex pBackglass,"videoscenes","triplejackpot.mp4",videovol,1
LightSeqhit.Play SeqDownOn,10,3,0
		End If
		chilloutthemusic
		scorejackpot
	End Sub

	Sub ohnocall
			PuPlayer.playlistplayex pCallouts,"youstolethegold-L" & currentlep(CurrentPlayer),"",100,1
	End Sub

	Sub endthemultiball
		mbco = 0
		Dim waittime
		If rockmusic = 1 Then
		PuPlayer.playlistplayex pMusic,"audiobgrock","",videovol,1
		PuPlayer.SetLoop 4,1		
		Else
		PuPlayer.playlistplayex pMusic,"audiobg","",videovol,1
		PuPlayer.SetLoop 4,1
		End if
		mblevel = 1
		PuPlayer.playlistplayex pCallouts,"defeated-L" & currentlep(CurrentPlayer),"",100,1
		chilloutthemusic
		rainbowgateclosed
		StopRainbow alights
		StopRainbow2 GI
		GiOn
		tavern1l1.state = 0
		keyl1.state = 0
		lojackpot1.state = 0
		rojackpot1.state = 0
		ChangeGi "white"
		ChangeBall(0)
		bMultiBallMode = False
		ResetAllLightsColor
		lowerramps
		rainbowspins(CurrentPlayer) = 0
		If progresslevel(CurrentPlayer) = 5 Then
		Else
		progresslevel(CurrentPlayer) = progresslevel(CurrentPlayer) + 1
		End If
		setlep
		relightthetable
		waittime = 700
		vpmtimer.addtimer waittime, "putemup'"
			PuPlayer.playlistplayex pBackglass,"videoscenes","Holding Screen_02.mp4",videovol,1
			PuPlayer.SetLoop 2,1
relightthetable
	End Sub

	Sub putemup
		progressreset
	End Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  WIZARD MODE
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 










'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  UTILITY - BALL FINDER
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 
	dim movingball:movingball = 0
	dim looktimer:looktimer = 0
	sub ballfinder_timer
		Dim BOT, b
		BOT = GetBalls
		looktimer = looktimer + 1

		' if no balls then no look
		If UBound(BOT) = -1 Then 
			looktimer = 0
			movingball = 0
		End If

		if hsbModeActive = True Then
			looktimer = 0
		end if 

		If bAttractMode = true Then
			looktimer = 0
		end if
	
		if mbstarter = 1 Then
			looktimer = 0
		end If

		For b = 0 to UBound(BOT)
            If BallVel(BOT(b) ) > 1  Then
				looktimer = 0
				movingball = 1
			Else
				movingball = 0
            End If
		Next

		If ldown = 1 or rdown = 1 Then
			For b = 0 to UBound(BOT)

				if BOT(b).Y > 1558 and BOT(b).Y < 1694 Then			
					if Bot(B).X > 240 and Bot(B).X < 625 then
						debug.print "being trapped"
						looktimer = 0
					end if
				end if

				if BOT(b).Y > 648 and BOT(b).Y < 787 Then			
					if Bot(B).X > 467 and Bot(B).X < 643 then
						debug.print "being trapped"
						looktimer = 0
					end if
				end if

			Next
		end if

			For b = 0 to UBound(BOT)
				if BOT(b).Y > 1615 and BOT(b).Y < 1800 Then			
					if Bot(B).X > 872 and Bot(B).X < 950 then
						debug.print "in launch lane"
						looktimer = 0
					end if
				end if
			Next

		'debug.print UBound(BOT)
		'debug.print BallsOnPlayfield

		debug.print "looktimer" & looktimer
		Select Case looktimer
			case 9:kickerclear
			case 15:ballstuckoption
		end Select

	end Sub

	sub kickerclear
		debug.print "clear the kickers"
		pNote "Searching For","Missing Balls"
		rainbowgateopen
	end sub

	Sub ballstuckoption
		debug.print "ball stuck"
		AddMultiball 1
		pNote "Ball","Added"
	end Sub





'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Flashers
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 






	sub bwbulb1 
		strip1.visible = 1
		f4.visible = 1
		vpmtimer.addtimer 200, "f1off '"
	End Sub
	sub f1off
		strip1.visible = 0
		f4.visible = 0
	end Sub

	sub bwbulb2
		strip3.visible = 1
		f4.visible = 1
		vpmtimer.addtimer 200, "f2off '"
	End Sub
	sub f2off
		strip3.visible = 0
	end Sub


	sub bwbulb4 
		Strip7.visible = 1
		f5.visible = 1
		vpmtimer.addtimer 200, "f4off '"
	End Sub
	sub f4off
		Strip7.visible = 0
	end Sub

	sub bwbulb5 
		strip9.visible = 1
		f5.visible = 1
		vpmtimer.addtimer 200, "f5off '"
	End Sub
	sub f5off
		strip9.visible = 0
		f5.visible = 0
	end Sub




	






	'****************************
	' Flashers - Thanks Flupper
	'****************************

	Dim FlashLevel1, FlashLevel2, FlashLevel3, FlashLevel4, FlashLevel5, FlashLevel6
	Flasherlight1.IntensityScale = 0
	Flasherlight2.IntensityScale = 0
	Flasherlight3.IntensityScale = 0
	Flasherlight4.IntensityScale = 0
	Flasherlight5.IntensityScale = 0
	Flasherlight6.IntensityScale = 0

	'*** lower right rbg flasher ***
	Sub Flasherflash1_Timer()
		On Error Resume Next
		dim flashx3, matdim
		If not Flasherflash1.TimerEnabled Then 
			Flasherflash1.TimerEnabled = True
			Flasherflash1.visible = 1
			Flasherlit1.visible = 1
		End If
		flashx3 = FlashLevel1 * FlashLevel1 * FlashLevel1
		Flasherflash1.opacity = 1500 * flashx3
		Flasherlit1.BlendDisableLighting = 10 * flashx3
		Flasherbase1.BlendDisableLighting =  flashx3
		Flasherlight1.IntensityScale = flashx3
		matdim = Round(10 * FlashLevel1)
		Flasherlit1.material = "domelit" & matdim
		FlashLevel1 = FlashLevel1 * 0.9 - 0.01
		If FlashLevel1 < 0.15 Then
			Flasherlit1.visible = 0
		Else
			Flasherlit1.visible = 1
		end If
		If FlashLevel1 < 0 Then
			Flasherflash1.TimerEnabled = False
			Flasherflash1.visible = 0
		End If
	End Sub'

	'*** middle right rbg flasher ***
	Sub Flasherflash2_Timer()
		On Error Resume Next
		dim flashx3, matdim
		If not Flasherflash2.TimerEnabled Then 
			Flasherflash2.TimerEnabled = True
			Flasherflash2.visible = 1
			Flasherlit2.visible = 1
		End If
		flashx3 = FlashLevel2 * FlashLevel2 * FlashLevel2
		Flasherflash2.opacity = 1500 * flashx3
		Flasherlit2.BlendDisableLighting = 10 * flashx3
		Flasherbase2.BlendDisableLighting =  flashx3
		Flasherlight2.IntensityScale = flashx3
		matdim = Round(10 * FlashLevel2)
		Flasherlit2.material = "domelit" & matdim
		FlashLevel2 = FlashLevel2 * 0.9 - 0.01
		If FlashLevel2 < 0.15 Then
			Flasherlit2.visible = 0
		Else
			Flasherlit2.visible = 1
		end If
		If FlashLevel2 < 0 Then
			Flasherflash2.TimerEnabled = False
			Flasherflash2.visible = 0
		End If
	End Sub

	'*** left kicker rbg flasher ***
	Sub Flasherflash3_Timer()
		On Error Resume Next
		dim flashx3, matdim
		If not Flasherflash3.TimerEnabled Then 
			Flasherflash3.TimerEnabled = True
			Flasherflash3.visible = 1
			Flasherlit3.visible = 1
		End If
		flashx3 = FlashLevel3 * FlashLevel3 * FlashLevel3
		Flasherflash3.opacity = 1500 * flashx3
		Flasherlit3.BlendDisableLighting = 10 * flashx3
		Flasherbase3.BlendDisableLighting =  flashx3
		Flasherlight3.IntensityScale = flashx3
		matdim = Round(10 * FlashLevel3)
		Flasherlit3.material = "domelit" & matdim
		FlashLevel3 = FlashLevel3 * 0.9 - 0.01
		If FlashLevel3 < 0.15 Then
			Flasherlit3.visible = 0
		Else
			Flasherlit3.visible = 1
		end If
		If FlashLevel3 < 0 Then
			Flasherflash3.TimerEnabled = False
			Flasherflash3.visible = 0
		End If
	End Sub

	'*** center rbg flasher ***
	Sub Flasherflash4_Timer()
		On Error Resume Next
		dim flashx3, matdim
		If not Flasherflash4.TimerEnabled Then 
			Flasherflash4.TimerEnabled = True
			Flasherflash4.visible = 1
			Flasherlit4.visible = 1
		End If
		flashx3 = FlashLevel4 * FlashLevel4 * FlashLevel4
		Flasherflash4.opacity = 1500 * flashx3
		Flasherlit4.BlendDisableLighting = 10 * flashx3
		Flasherbase4.BlendDisableLighting =  flashx3
		Flasherlight4.IntensityScale = flashx3
		matdim = Round(10 * FlashLevel4)
		Flasherlit4.material = "domelit" & matdim
		FlashLevel4 = FlashLevel4 * 0.9 - 0.01
		If FlashLevel4 < 0.15 Then
			Flasherlit4.visible = 0
		Else
			Flasherlit4.visible = 1
		end If
		If FlashLevel4 < 0 Then
			Flasherflash4.TimerEnabled = False
			Flasherflash4.visible = 0
		End If
	End Sub

	'*** top right rbg flasher ***
	Sub Flasherflash5_Timer()
		On Error Resume Next
		dim flashx3, matdim
		If not Flasherflash5.TimerEnabled Then 
			Flasherflash5.TimerEnabled = True
			Flasherflash5.visible = 1
			Flasherlit5.visible = 1
		End If
		flashx3 = FlashLevel5 * FlashLevel5 * FlashLevel5
		Flasherflash5.opacity = 1500 * flashx3
		Flasherlit5.BlendDisableLighting = 10 * flashx3
		Flasherbase5.BlendDisableLighting =  flashx3
		Flasherlight5.IntensityScale = flashx3
		matdim = Round(10 * FlashLevel5)
		Flasherlit5.material = "domelit" & matdim
		FlashLevel5 = FlashLevel5 * 0.9 - 0.01
		If FlashLevel5 < 0.15 Then
			Flasherlit5.visible = 0
		Else
			Flasherlit5.visible = 1
		end If
		If FlashLevel5 < 0 Then
			Flasherflash5.TimerEnabled = False
			Flasherflash5.visible = 0
		End If
	End Sub


	'*** top right rbg flasher ***
	Sub Flasherflash6_Timer()
		On Error Resume Next
		dim flashx3, matdim
		If not Flasherflash6.TimerEnabled Then 
			Flasherflash6.TimerEnabled = True
			Flasherflash6.visible = 1
			Flasherlit6.visible = 1
		End If
		flashx3 = FlashLevel6 * FlashLevel6 * FlashLevel6
		Flasherflash6.opacity = 1500 * flashx3
		Flasherlit6.BlendDisableLighting = 10 * flashx3
		Flasherbase6.BlendDisableLighting =  flashx3
		Flasherlight6.IntensityScale = flashx3
		matdim = Round(10 * FlashLevel6)
		Flasherlit6.material = "domelit" & matdim
		FlashLevel6 = FlashLevel6 * 0.9 - 0.01
		If FlashLevel6 < 0.15 Then
			Flasherlit6.visible = 0
		Else
			Flasherlit6.visible = 1
		end If
		If FlashLevel6 < 0 Then
			Flasherflash6.TimerEnabled = False
			Flasherflash6.visible = 0
		End If
	End Sub




	flasherseq.enabled = 0
	dim flasherpos:flasherpos = 0
	dim flasherdir:flasherdir = "left"


	'flasherspop orange,"right"

	Sub flasherspop(n,x)
		On Error Resume Next
		flasherpos = 0
		dim a
		flasherdir = x
		flasherseq.enabled = 1
		for each a in  flasherlights
			SetLightColor a, n, -1
			a.intensity = 300
		Next
	End Sub

	Sub flasherseq_timer
		On Error Resume Next
		flasherpos = flasherpos + 1
		If flasherdir = "left" Then
			select case flasherpos
				case 1
					FlashLevel3 = 1 : Flasherflash3_Timer
				case 2
					FlashLevel1 = 1 : Flasherflash1_Timer
				case 3
					FlashLevel4 = 1 : Flasherflash4_Timer
				case 4
					FlashLevel3 = 1 : Flasherflash3_Timer
				case 5
					FlashLevel4 = 1 : Flasherflash4_Timer
				case 6
					FlashLevel1 = 1 : Flasherflash1_Timer
				case 7
					FlashLevel3 = 1 : Flasherflash3_Timer
				case 8
					FlashLevel4 = 1 : Flasherflash4_Timer
				case 9
					FlashLevel1 = 1 : Flasherflash1_Timer
				case 10
					FlashLevel4 = 1 : Flasherflash4_Timer
				case 11
					FlashLevel3 = 1 : Flasherflash3_Timer
				case 12
					FlashLevel1 = 1 : Flasherflash1_Timer
					flasherseq.enabled = 0
					flasherpos = 0
			end Select
		End If

		If flasherdir = "bwsweep" Then
			select case flasherpos
				case 1
					bwbulb1
				case 2
					bwbulb2
				case 3
					bwbulb3
				case 4
					bwbulb4
				case 5
					bwbulb5
				case 6
					bwbulb4
				case 7
					bwbulb3
				case 8
					bwbulb2
				case 9
					bwbulb1
				case 10
					bwbulb1:bwbulb2:bwbulb3:bwbulb4:bwbulb5
				case 12
					bwbulb1:bwbulb2:bwbulb3:bwbulb4:bwbulb5
					flasherseq.enabled = 0
					flasherpos = 0
			end Select
		End If

		If flasherdir = "right" Then
			select case flasherpos
				case 1
					FlashLevel5 = 1 : Flasherflash5_Timer
				case 2
					FlashLevel2 = 1 : Flasherflash2_Timer
				case 3
					FlashLevel6 = 1 : Flasherflash6_Timer
				case 4
					FlashLevel5 = 1 : Flasherflash5_Timer
				case 5
					FlashLevel2 = 1 : Flasherflash2_Timer
				case 6
					FlashLevel6 = 1 : Flasherflash6_Timer
				case 7
					FlashLevel5 = 1 : Flasherflash5_Timer
				case 8
					FlashLevel2 = 1 : Flasherflash2_Timer
				case 9
					FlashLevel6 = 1 : Flasherflash6_Timer
				case 10
					FlashLevel5 = 1 : Flasherflash5_Timer
				case 11
					FlashLevel2 = 1 : Flasherflash2_Timer
				case 11
					FlashLevel6 = 1 : Flasherflash6_Timer
					flasherseq.enabled = 0
					flasherpos = 0
			end Select
		End If

		If flasherdir = "bottom" Then
			select case flasherpos
				case 1
					FlashLevel3 = 1 : Flasherflash3_Timer
					FlashLevel2 = 1 : Flasherflash2_Timer

				case 4
					FlashLevel3 = 1 : Flasherflash3_Timer
					FlashLevel2 = 1 : Flasherflash2_Timer

				case 7
					FlashLevel3 = 1 : Flasherflash3_Timer
					FlashLevel2 = 1 : Flasherflash2_Timer

				case 10
					FlashLevel3 = 1 : Flasherflash3_Timer
					FlashLevel2 = 1 : Flasherflash2_Timer
					flasherseq.enabled = 0
					flasherpos = 0
			end Select
		End If


		If flasherdir = "center" Then
		dim a
		for each a in  flasherlights
			SetLightColor a, n, -1
			a.intensity = 100
		Next
			select case flasherpos
				case 1
					FlashLevel5 = 1 : Flasherflash5_Timer
					FlashLevel4 = 1 : Flasherflash4_Timer

				case 4
					FlashLevel5 = 1 : Flasherflash5_Timer
					FlashLevel4 = 1 : Flasherflash4_Timer

				case 7
					FlashLevel5 = 1 : Flasherflash5_Timer
					FlashLevel4 = 1 : Flasherflash4_Timer

				case 10
					FlashLevel5 = 1 : Flasherflash5_Timer
					FlashLevel4 = 1 : Flasherflash4_Timer
					flasherseq.enabled = 0
					flasherpos = 0
			end Select
		End If



		If flasherdir = "leftkick" Then
			select case flasherpos
				case 1
					'FlashLevel3 = 1 : Flasherflash3_Timer

				case 4
					FlashLevel3 = 1 : Flasherflash3_Timer

				case 7
					FlashLevel3 = 1 : Flasherflash3_Timer

				case 10
					FlashLevel3 = 1 : Flasherflash3_Timer
					flasherseq.enabled = 0
					flasherpos = 0
			end Select
		End If

		If flasherdir = "rightkick" Then
			select case flasherpos
				case 1
					'FlashLevel3 = 1 : Flasherflash3_Timer

				case 4
					FlashLevel6 = 1 : Flasherflash6_Timer

				case 7
					FlashLevel6 = 1 : Flasherflash6_Timer

				case 10
					FlashLevel6 = 1 : Flasherflash6_Timer
					flasherseq.enabled = 0
					flasherpos = 0
			end Select
		End If

		If flasherdir = "top" Then
			select case flasherpos
				case 1
					FlashLevel1 = 1 : Flasherflash1_Timer
					FlashLevel6 = 1 : Flasherflash6_Timer

				case 4
					FlashLevel1 = 1 : Flasherflash1_Timer
					FlashLevel6 = 1 : Flasherflash6_Timer

				case 7
					FlashLevel1 = 1 : Flasherflash1_Timer
					FlashLevel6 = 1 : Flasherflash6_Timer

				case 10
					FlashLevel1 = 1 : Flasherflash1_Timer
					FlashLevel6 = 1 : Flasherflash6_Timer
					flasherseq.enabled = 0
					flasherpos = 0
			end Select
		End If

		If flasherdir = "crazy" Then
			select case flasherpos
				case 1
					FlashLevel1 = 1 : Flasherflash1_Timer
					FlashLevel3 = 1 : Flasherflash3_Timer
					FlashLevel6 = 1 : Flasherflash6_Timer
				case 2
					FlashLevel2 = 1 : Flasherflash2_Timer
					FlashLevel5 = 1 : Flasherflash5_Timer
				case 3
					FlashLevel5 = 1 : Flasherflash5_Timer
					FlashLevel1 = 1 : Flasherflash1_Timer
				case 4
					FlashLevel4 = 1 : Flasherflash4_Timer
					FlashLevel6 = 1 : Flasherflash6_Timer
				case 5
					FlashLevel3 = 1 : Flasherflash3_Timer
					FlashLevel2 = 1 : Flasherflash2_Timer
				case 6
					FlashLevel1 = 1 : Flasherflash1_Timer
					FlashLevel4 = 1 : Flasherflash4_Timer
				case 7
					FlashLevel2 = 1 : Flasherflash2_Timer
				case 8
					FlashLevel5 = 1 : Flasherflash5_Timer
					FlashLevel6 = 1 : Flasherflash6_Timer
				case 9
					FlashLevel4 = 1 : Flasherflash4_Timer
					FlashLevel2 = 1 : Flasherflash2_Timer
				case 10
					FlashLevel3 = 1 : Flasherflash3_Timer
					FlashLevel5 = 1 : Flasherflash5_Timer
				case 11
					FlashLevel1 = 1 : Flasherflash1_Timer
					FlashLevel3 = 1 : Flasherflash3_Timer
				case 12
					FlashLevel2 = 1 : Flasherflash2_Timer
					FlashLevel5 = 1 : Flasherflash5_Timer
					FlashLevel6 = 1 : Flasherflash6_Timer
				case 13
					FlashLevel5 = 1 : Flasherflash5_Timer
					FlashLevel1 = 1 : Flasherflash1_Timer
				case 14
					FlashLevel4 = 1 : Flasherflash4_Timer
				case 15
					FlashLevel3 = 1 : Flasherflash3_Timer
					FlashLevel2 = 1 : Flasherflash2_Timer
					FlashLevel6 = 1 : Flasherflash6_Timer
					flasherseq.enabled = 0
					flasherpos = 0
			end Select
		End If
	end Sub


	flasherseq1.enabled = 0
	dim flasherpos1:flasherpos1 = 0
	dim flasherdir1:flasherdir1 = "left"


	'flasherspop orange,"right"

	Sub strippop(n,x)
		On Error Resume Next
		flasherpos1 = 0
		flasherdir1 = x
		flasherseq1.enabled = 1
	End Sub		

	Sub flasherseq1_timer
		On Error Resume Next
		flasherpos1 = flasherpos1 + 1
		If flasherdir1 = "bwsweep" Then
			select case flasherpos1
				case 1
					bwbulb1
				case 2
					bwbulb2
				case 3
					bwbulb4
				case 4
					bwbulb5
				case 5
					bwbulb4
				case 6
					bwbulb2
				case 7
					bwbulb1
				case 8
					bwbulb1:bwbulb2:bwbulb3:bwbulb4:bwbulb5
				case 12
					bwbulb1:bwbulb2:bwbulb3:bwbulb4:bwbulb5
					flasherseq1.enabled = 0
					flasherpos1 = 0
			end Select
		End If
	end Sub
		


