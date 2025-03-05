'**************far*******************
' Scarface - Balls and Power v 2.0
'      VP table by JPSalas
'  Graphics & Rules by hassanchop
' Art and models updated by Joe Picaso
' ReMastered in 2025 by MerlinRTP
'*********************************


'**************far*************
'DOF version by Arngrim v 1.1

'DOF mapping

'E107 bumper right
'E108 ball release
'E109 middle yellow target left
'E110 middle yellow target center
'E111 middle yellow target right
'E112 middle yellow targets drop reset
'E113 B of Bank target
'E114 A of Bank target
'E115 N of Bank target
'E115 N of Bank target
'E116 K of Bank target
'E117 Central bank hit
'E118 World is yours VUK
'E118 World is yours VUK
'E119 Car eject
'E120 Left Ramp done
'E121 Center door open
'E122 Platform rotating
'E123 Autoplunger
'E124 Plunger lane exit
'E125 Plunger lane diverter
'E126 Car hit
'E127 Shooter lane switch
'E128 Left outer lane
'E129 Left inner lane
'E130 Right inner lane
'E131 Right outer lane
'E132 Start button light
'E133 Right ramp done
'E134 Knocker
'E135 Lockpost
'E136 Backdoorpost
'E137 Win event

'E201 rollover middle left
'E202 rollover middle right
'E203 rollover left
'E204 rollover right
'E205 rotation lights from platform 1
'E206 rotation lights from platform 2
'E207 rotation lights from platform 3
'E208 rotation lights from platform 4
'E209 rotation lights from platform 5
'E210 flash when skillshot made
'E211 
'E212 
'E213 
'E214 Bumper flash
'E219 left lane light
'E222 Easter egg beacon


'**************far***********

Option Explicit
Randomize
SetLocale 1033			'Forces VBS to use english to stop crashes.

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

Dim pupPackScreenFile
Dim ObjFso
Dim ObjFile

Dim DMDType, bIsMiniGameActive, pDMD

Const myVersion = "1.0.0"


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'User Config Changes - Everything else should be changed via tweak menu
Const ScorbitActive				= 0 	' Is Scorbit Active	

Const sBackgroundMusic = "bgout_scarface_NOE"
'Const sBackgroundMusic = "bgout_scarface_Main"


Dim MessageHeight:MessageHeight = 7.5
'Dim MessageHeight:MessageHeight = 17	' Should be good for VR

'may need to change this based on your resolution
'Dim VR_MessageSize: VR_MessageSize = 2.5	' controls the size of the mode messages and progress in VR DMD
'Dim VR_BonusSize: VR_BonusSize = 2.5  ' controls the size of the multiplier values in VR DMD
'Dim VR_BallValue: VR_BallValue = 1.5	' controls the size of the ball value message in VR DMD
'Dim VR_Score:VR_Score = 4.5			' controls the size of the main score in VR DMD

Dim VR_MessageSize: VR_MessageSize = 6	' controls the size of the mode messages and progress in VR DMD
Dim VR_BonusSize: VR_BonusSize = 4.5  ' controls the size of the multiplier values in VR DMD
Dim VR_BallValue: VR_BallValue = 3	' controls the size of the ball value message in VR DMD
Dim VR_Score:VR_Score = 9			' controls the size of the main score in VR DMD
     '


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX











'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'***********************************************************************
' Do not change anything below this line unless you know what the fuck you are doing

Const     ScorbitShowClaimQR	= 1 	' If Scorbit is active this will show a QR Code  on ball 1 that allows player to claim the active player from the app
Const     ScorbitUploadLog		= 0 	' Store local log and upload after the game is over 
Const     ScorbitAlternateUUID  = 0 	' Force Alternate UUID from Windows Machine and saves it in VPX Users directory (C:\Visual Pinball\User\ScorbitUUID.dat)


'/////////////////////////////////////////////////////////////////////
'VR Users UnComment the line below 
'MessageHeight = 17

'----- Music Options -----
Dim fMusicVolume ' 0.1 Default			'Music volume. 0 = no music, 1 = full volume
Dim fAttractVolume : fAttractVolume = 0.3			'Attract mode music volume. 0 = no music, 1 = full volume
Dim fIntroVolume : fIntroVolume = 0.008			' Intro Music Volume
Dim fDuckfactor				'Duck music to 50% (multiplicative with fMusicVolume) during video
Dim nVideoVolume : nVideoVolume = 90
Dim fCalloutVolume ' 0.9 Default	' Volume for Voice Callouts


Const bNoMusic = False


Dim BallHandlingQueue : Set BallHandlingQueue = New vpwQueueManager
Dim EOBQueue : Set EOBQueue = New vpwQueueManager
Dim DMDQueue : Set DMDQueue = New vpwQueueManager
Dim HSQueue : Set HSQueue = New vpwQueueManager
Dim AudioQueue : Set AudioQueue = New vpwQueueManager
Dim GeneralPupQueue: Set GeneralPupQueue = New vpwQueueManager

Dim bMultiBallMode
Dim bGameStarted
Dim bFreeplay
Dim bMiniGameActive
Dim bMiniGamePlayed
Dim bGameOver
Dim bMiniGameAllowed
Dim bSupressMainEvents
Dim FlamingoCounter 
Dim bSkipTargetReset
Dim bOnTheFirstBallScorbit
Dim GameModeStrTmp
Dim bOnTheFirstBall
Dim EBCounter
Dim nTiltLevel 
Dim EOBTime
Dim bSupressEndEventMessage

Dim bSupressSafety
Dim bRightFlipperHeld,bLeftFlipperHeld

Dim MiniGameLoops(4,2)
Const EjectBallValue = 40
Const RDTValue = 600

Const Bonus_Spinners = 100
Const Bonus_BankTargets = 500
Const Bonus_Car = 300


'****************************************************************
'	VR Stuff
'****************************************************************
'///////////////////////---- VR Room ----////////////////////////
Dim VRRoomChoice : VRRoomChoice = 3				'0 - VR Room Off, 1 - Cab Only, 2 - Minimal, 3 - Mega
Dim VRTest : VRTest = False
Dim FlipperChoice: FlipperChoice = 0
Dim RailChoice: RailChoice = False
Dim BallChoice: BallChoice = 0
Dim SidewallChoice: SidewallChoice = 0
Dim TrustOpt:TrustOpt = 1
Dim OutlaneOpt:OutlaneOpt = 0

'//////////////F12 Menu//////////////
' Called when options are tweaked by the player. 
' - 0: game has started, good time to load options and adjust accordingly
' - 1: an option has changed
' - 2: options have been reseted
' - 3: player closed the tweak UI, good time to update staticly prerendered parts
' Table1.Option arguments are: 
' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional arry of literal strings
Dim dspTriggered : dspTriggered = False
Sub Table1_OptionEvent(ByVal eventId)
	If eventId = 1 And Not dspTriggered Then dspTriggered = True : DisableStaticPreRendering = True : End If

    ' VRRoom
	VRRoomChoice = Table1.Option("VR Room", 0, 3, 1, 3, 0, Array("OFF","Cab", "Minimal", "Mega"))
	LoadVRRoom


    ' Sound volumes
    VolumeDial = Table1.Option("Mech Volume", 0, 1, 0.01, 0.8, 1)
    BallRollVolume = Table1.Option("Ball Roll Volume", 0, 1, 0.01, 0.8, 1)
	RampRollVolume = Table1.Option("Ramp Roll Volume", 0, 1, 0.01, 0.5, 1)
	fCalloutVolume = Table1.Option("Callout Volume - Default 90%", 0, 1, 0.01, 0.9, 1)
	fMusicVolume = Table1.Option("Music Volume - Default 50%", 0, 1, 0.001, 0.5, 1)
	SetMusicVolumes 

	'Balls Per Game
	nBallsPerGame = Table1.Option("Balls Per Game", 0, 2, 1, 0, 0, Array("3 (Default)", "4", "5"))
	SetBallsPerGame nBallsPerGame

	'Freeplay
	bFreeplay = Table1.Option("Freeplay On", 0, 1, 1, 0, 0, Array("False (Default)", "True"))
	'SetFreeplay bFreePlay

	FlipperChoice = Table1.Option("Flippers", 0, 5, 1, 0, 0, Array("White-White/Red", "Red-White", "Black-White","Purple-Red", "White-Black","White-Red"))
	SetFlippers FlipperChoice

	bMiniGameActive = Table1.Option("Mini-Game Active", 0, 1, 1, 1, 0, Array("False", "True (Default)"))

	RailChoice = Table1.Option("Rails Visible", 0, 1, 1, 1, 0, Array("False", "True (Default)"))
	SetRails RailChoice
	LoadVRRoom

	TrustOpt = Table1.Option("Trust Post", 0, 1, 1, 1, 0, Array("False", "True (Default)"))
	SetTrustPost TrustOpt


	OutlaneOpt = Table1.Option("Outlanes", 0, 2, 1, 0, 0, Array("Both (Default)", "Inside", "None"))
	SetOutlanes OutlaneOpt

	SidewallChoice = Table1.Option("Sidewall Art", 0, 2, 1, 0, 0, Array("Sidewall", "PinCab", "Black"))
	SetSidewall SidewallChoice

	'BallChoice = Table1.Option("Ball Choice", 0, 2, 1, 0, 0, Array("Scarface_D", "Scratches", "Scratches4Medium"))
	'SetBallDecal BallChoice

	If EventID = 3 Then SetMusicVolumes 
	If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If
End Sub


'***************MINI GAME ***************************** AOR
' Do not change these values
Dim PupScreenMiniGame' = 5 ' 'DO NOT CHANGE THIS VALUE was 2 on terrifier
Dim iMiniGameCnt(4) 'reset to zero on game init, adds one each time player plays minigame (limits to 1 minigame per Player, per Game)
'****************************************************** AOR

Sub SetFlippers(Opt)
	Select Case Opt
		Case 0:
			LfLogo.image = "FLWhiteWhite"
			RfLogo.image = "FLWhiteWhite"
		Case 1:
			LfLogo.image = "FLredwhite"
			RfLogo.image = "FLredwhite"
		Case 2:
			LfLogo.image = "FLblackwhite"
			RfLogo.image = "FLblackwhite"
		Case 3:
			LfLogo.image = "FLpurplered"
			RfLogo.image = "FLpurplered"
		Case 4:
			LfLogo.image = "FLwhiteblack"
			RfLogo.image = "FLwhiteblack"
		Case 5:
			LfLogo.image = "FLwhitered"
			RfLogo.image = "FLwhitered"
	End Select
End Sub

Sub SetRails(Opt)
	Select Case Opt
		Case 0:
			Ramp15.Visible = 0
			Ramp16.Visible = 0
			Primitive13.visible= 0
		Case 1:
			Ramp15.Visible = 1
			Ramp16.Visible = 1
			Primitive13.visible=1
	End Select
End Sub

Sub SetTrustPost(Opt)
	Select Case Opt
		Case 0:
			zCol_Rubber_Post002.collidable = 0
			pin1.Visible = 0
			Primitive1.visible= 0
		Case 1:
			zCol_Rubber_Post002.collidable = 1
			pin1.Visible = 1
			Primitive1.visible=1
	End Select
End Sub

Sub SetOutlanes(Opt)
	Select Case Opt
		Case 0: 'both
			zCol_Rubber_Post004.collidable = 1
			zCol_Rubber_Post034.collidable = 1
			Primitive005.visible = 1
			Primitive008.visible = 1
			pin002.visible = 1
			pin003.visible = 1

			zCol_Rubber_Post003.collidable = 1
			zCol_Rubber_Post013.collidable = 1
			pin001.visible = 1
			pin6.visible = 1
			Primitive004.visible = 1
			Primitive11.visible = 1

		Case 1: ' inside only
			zCol_Rubber_Post004.collidable = 0
			zCol_Rubber_Post034.collidable = 0
			Primitive005.visible = 0
			Primitive008.visible = 0
			pin002.visible = 0
			pin003.visible = 0

			zCol_Rubber_Post003.collidable = 1
			zCol_Rubber_Post013.collidable = 1
			pin001.visible = 1
			pin6.visible = 1
			Primitive004.visible = 1
			Primitive11.visible = 1

		Case 2 ' none
			zCol_Rubber_Post004.collidable = 0
			zCol_Rubber_Post034.collidable = 0
			Primitive005.visible = 0
			Primitive008.visible = 0
			pin002.visible = 0
			pin003.visible = 0

			zCol_Rubber_Post003.collidable = 0
			zCol_Rubber_Post013.collidable = 0
			pin001.visible = 0
			pin6.visible = 0
			Primitive004.visible = 0
			Primitive11.visible = 0
	End Select
End Sub

Sub SetSidewall(Opt)
	Select Case Opt
		Case 0:
			PinCab_Blades.visible = 0
			Wall055.sidevisible = 1
			Wall056.sidevisible = 1
			Wall055.Image = "SidewallLeft"
			Wall056.Image = "SidewallRight"
		Case 1:
			Wall055.sidevisible = 0
			Wall056.sidevisible = 0
			PinCab_Blades.Image = "Sidewalls S"
			PinCab_Blades.visible = 1
		Case 2:
			PinCab_Blades.visible = 0
			Wall055.sidevisible = 0
			Wall056.sidevisible = 0
	End Select
End Sub

Sub SetBallsPerGame(Opt)
	Select Case Opt
		Case 0: nBallsPerGame = 3
		Case 1:	nBallsPerGame = 4
		Case 2:	nBallsPerGame = 5
		Case 3:	nBallsPerGame = 1
	End Select
End Sub

Sub SetFreePlay(Opt)
	Select Case Opt
		Case 0: bFreePlay = False
		Case 1:	bFreePlay = True
	End Select
End Sub

Sub SetBallDecal(Opt)
' Does not work :-(
	Select Case Opt
		Case 0:
			ChangeBall "Scarface_D"
		Case 1:
			ChangeBall "scratches"
		Case 2:
			ChangeBall "scratches4medium"
	End Select
End Sub

'*******************************************
'  ZMUS - Music
'*******************************************
Dim fCurrentMusicVol : fCurrentMusicVol = 0
Dim sMusicTrack : sMusicTrack = ""
Dim fSongVolume : fSongVolume = 1

Sub SetMusicVolumes
fDuckfactor = .2*fMusicVolume
PlayTheme
End Sub



Sub DuckMusic2
	PlaySound sMusicTrack, -1, 0.4,0,0,0,1,0,0
End Sub

Sub DuckResume
	PlaySound sMusicTrack, -1, fMusicVolume,0,0,0,1,0,0
End Sub

Sub DuckMusic
	PlaySound sMusicTrack, -1, fMusicVolume * fDuckFactor,0,0,0,1,0,0
	fCurrentMusicVol = fMusicVolume * fDuckfactor
end sub

Sub DuckUpdate
	Dim bDuck, fGoalVolume
	bDuck = True


	If Not oCurrentEvent is Nothing Then
		If oCurrentEvent.pupTrigger <> 0 Then
			bDuck = True
		Else
			bDuck = False
		End If
	Else
		bDuck = False
	End If

	If (Not bDuck) And sMusicTrack <> "" Then
		If sMusicTrack = "music-attract" then
			fGoalVolume = fAttractVolume
		Else
			fGoalVolume = fMusicVolume
		end If

		If fCurrentMusicVol < fGoalVolume Then
			' fade in is 6 updates of 17 ms
			fCurrentMusicVol = fCurrentMusicVol + ((fGoalVolume - fDuckfactor) / 6)
			If fCurrentMusicVol >= fGoalVolume Then
				fCurrentMusicVol = fGoalVolume
			End If
			PlaySound sMusicTrack, -1, fCurrentMusicVol,0,0,0,1,0,0
		End If
	End If
end sub

'PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)
Sub SwitchMusic(sTrack)
'debug.print "sTrack:" &sTrack
'debug.print "sMusicTrack:" &sMusicTrack
	If sTrack <> sMusicTrack Then
		'if sMusicTrack <> "" Then StopSound sMusicTrack
		if sMusicTrack <> "" Then StopAllMusic
		sMusicTrack = sTrack
		If "music-attract" = sTrack Then
			PlaySound sTrack, -1, fAttractVolume
			fCurrentMusicVol = fAttractVolume
		Else
			PlaySound sTrack, -1, fMusicVolume,0,0,0,1,0,0
			fCurrentMusicVol = fMusicVolume
		End If
	Else
		'debug.print "Not a New Song"
	End If

	'debug.print "SONG:" &sMusicTrack
End Sub


Sub PlayTheme
if bNoMusic Then
	StopAllMusic
	debug.print "BAILING"
	Exit Sub
End If
''''''


	If EventStarted Or SideEventStarted or EE Then
		' do nothing
	Else
		debug.print "No Events Active"
		SwitchMusic sBackgroundMusic
		Exit Sub
	End If

    If EventStarted Then
        Select Case EventStarted
			Case 1:
				
				SwitchMusic "bgout_scarface_ME1"
				Exit Sub
			Case 2:
				
				SwitchMusic "bgout_scarface_ME2"
				Exit Sub
			Case 3:
				
				SwitchMusic "bgout_scarface_ME3"
				Exit Sub
			Case 4:
				
				SwitchMusic "bgout_scarface_ME4"
				Exit Sub
			Case 5:
				
				SwitchMusic "bgout_scarface_ME5"
				Exit Sub
			Case 6:
				
				SwitchMusic "bgout_scarface_ME6"
				Exit Sub
			Case 7:
				
				SwitchMusic "bgout_scarface_ME7"
				Exit Sub
			Case 8:
				
				SwitchMusic "bgout_scarface_ME8"
				Exit Sub
			Case 9:
				
				SwitchMusic "bgout_scarface_ME9"
				Exit Sub
			Case 10:
				
				SwitchMusic "bgout_scarface_ME10"
				Exit Sub
			Case 11:
				
				SwitchMusic "bgout_scarface_ME11"
				Exit Sub
		End Select
'	Else
'		
'		SwitchMusic "bgout_scarface_NOE"
	End If

    If SideEventStarted Then
        Select Case SideEventNr
			Case 1:
				
				SwitchMusic "bgout_scarface_SEHU"
				Exit Sub
			Case 2:
				
				SwitchMusic "bgout_scarface_SEH"
				Exit Sub
			Case 3:
				
				SwitchMusic "bgout_scarface_MB"
				Exit Sub
			Case 4:
				
				SwitchMusic "bgout_scarface_SEY"
				Exit Sub
			Case 5:
				
				SwitchMusic "bgout_scarface_SEHU"
				Exit Sub
		End Select
	End If

End Sub


Sub StopAllMusic
	debug.print "StopAllMusic Called"
	sMusicTrack = ""
	StopSound "bgout_scarface_NOE"
	StopSound "bgout_scarface_Main"
	StopSound "bgout_scarface_EE"
	StopSound "bgout_scarface_MB"
	StopSound "bgout_scarface_ME1"
	StopSound "bgout_scarface_ME2"
	StopSound "bgout_scarface_ME3"
	StopSound "bgout_scarface_ME4"
	StopSound "bgout_scarface_ME5"
	StopSound "bgout_scarface_ME6"
	StopSound "bgout_scarface_ME7"
	StopSound "bgout_scarface_ME8"
	StopSound "bgout_scarface_ME9"
	StopSound "bgout_scarface_ME10"
	StopSound "bgout_scarface_ME11"
	StopSound "bgout_scarface_SEH"
	StopSound "bgout_scarface_SEHU"
	StopSound "bgout_scarface_SEY"

End Sub



'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
'************************
' Play Musics MP3 version
'************************

Dim SongPlaying

Sub PlaySong2(sng)
    If bMusicOn Then
        If SongPlaying <> sng Then
            PlayMusic "Scarface\" &sng
            SongPlaying = sng
        End If
    End If
End Sub

Sub table1_MusicEnded 'repeats the same song again
    SongPlaying = ""
    PlayTheme
End Sub


Sub PlayTheme2 'select a song depending of the mode
    If SideEventStarted Then
        Select Case SideEventNr
            Case 1:PlaySong "bgout_scarface_SEHU.mp3"
            Case 2:PlaySong "bgout_scarface_SEH.mp3"
            Case 3:PlaySong "bgout_scarface_MB.mp3"
            Case 4:PlaySong "bgout_scarface_SEY.mp3"
            Case 5:PlaySong "bgout_scarface_SEHU.mp3"
        End Select

        Exit Sub
    End If

    If EventStarted Then
        Select Case EventStarted
            Case 1:PlaySong "bgout_scarface_ME1.mp3"
            Case 2:PlaySong "bgout_scarface_ME2.mp3"
            Case 3:PlaySong "bgout_scarface_ME3.mp3"
            Case 4:PlaySong "bgout_scarface_ME4.mp3"
            Case 5:PlaySong "bgout_scarface_ME5.mp3"
            Case 6:PlaySong "bgout_scarface_ME6.mp3"
            Case 7:PlaySong "bgout_scarface_ME7.mp3"
            Case 8:PlaySong "bgout_scarface_ME8.mp3"
            Case 9:PlaySong "bgout_scarface_ME9.mp3"
            Case 10:PlaySong "bgout_scarface_ME10.mp3"
            Case 11:PlaySong "bgout_scarface_ME11.mp3"
        End Select
    Else
        PlaySong sBackgroundMusic&".mp3"
    End If

End Sub

'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

'************************************** B2S additions / Rosve **************************************

Const FullBackglassFX=1  'When set to 0 some backglass effects are skipped. Change to 1 if you have a fast PC.
Const LWMaxBallsOnPF=1   'Some flasher effects are skipped if there are more than LWMaxBallsOnPF in play.
                         ' This setting can be increased on a fast PC.  

' B2S *******************************************************************************
Const cGameName = "scarface"

' some B2S vars
Dim B2SCurrentFrame

' initialize this vars
B2SCurrentFrame = ""

' Mode Durations

Const GetCarTime = 19	' Side 5
Const CarHole8Time = 120	
Const BumperOffTime = 180
Const BankTime = 24		' Side 1
Const StartTWIYTime = 70	' Side 3
Const Event1Time = 30	' Communist
Const Event2Time = 45	' Drug Deal
Const Event3Time = 90	' Sosa Deal
Const Event4Time = 120	' Your Own
Const Event5Time = 120	' Disco Brawl
Const Event6Time = 60	' Frank's Death
Const Event7Time = 120	' Top Climb
Const Event8Time = 60	' Cop Sting
Const Event9Time = 60	' Journalist
Const Event11Time = 300
Const StartHitManTime = 60	' Side 2

' B2S *******************************************************************************

' some constants
Const nBallSavedTime = 20 'in seconds
Const nMaxMultiplier = 10
Dim nBallsPerGame': nBallsPerGame = 3
Const nMaxBalls = 6                  'Max number of balls on the table at the same time
Const bMusicOn = True
Dim Version:Version = "1.0.2"

'Const BallSize = 25  'Ball radius
Const BallMass = 1

Const BallSize = 50				 'Ball diameter in VPX units; must be 50
'Const BallMass = 1				  'Ball mass must be 1
Const tnob = 5					  'Total number of balls the table can hold
Const lob = 0					   'Locked balls


'*******************************************
'  ZOPT: User Options
'*******************************************

Dim VolumeDial : VolumeDial = 0.9           	' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 0.9   	' Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.5 		' Level of ramp rolling volume. Value between 0 and 1

Dim tablewidth
tablewidth = Table1.width
Dim tableheight
tableheight = Table1.height


'********************
' Table Init & Events
'********************

Dim GameStarted
Dim Tilt
Dim Tilted
Dim TiltWarning
Dim AutoPlungerReady
Dim BallsRemaining
Dim BallsOnPlayfield
Dim Special
Dim SpecialIsLit
Dim SpecialAwarded
Dim Match
Dim LastSwitchHit
Dim BallLockEnabled
Dim BallsInLock(4)
Dim BallSaved
Dim ExtraBallIsLit
Dim ExtraBallAwarded
Dim MultiBallMode
Dim Credits
Dim Player
Dim Players
Dim BonusAward


'============= MerlinRTP
Dim nPlayfieldX(4)			' Playfield multiplier for the current player
Dim nBonusX(4)				' Bonus multiplier for the current player
Dim nTimeBallSave		' Time in ms left on the ball save
Dim anScore(4)
Dim nPlayer				' 0 based index of the current player (0 = player 1)
Dim nPlayersInGame
Dim nCurrentPlayerIndex
Dim MainMode(4,12)	     ' Array used to track ART Modes
Dim SideEvent(4,6)	 	     ' Array used to track PaleGirl Modes
Dim nSideProgress
Dim nMainProgress
Dim bInstantInfo
'Dim bBonusFirstPass
Dim DrainTracker(8)
Dim IndexTmp
Dim bHeadMoving

'****************************************
' Real Time updatess using the GameTimer
'****************************************
'used for all the real time updates




Dim asSideProgressMessage
asSideProgressMessage = Array("", "1", "3", "30", "10", "1") 


Dim asMainModeMessages
asMainModeMessages = Array("", "SHOOT KILL RAMP", "HIT ANY 6 DROP TARGETS", "SHOOT 3 MINI LOOPS", "SHOOT 30 SPINNERS", "HIT 30 BUMPERS", "HIT CENTER DROP TARGETS", _
"HIT ANY BANK TARGET", "SHORT PLUNGE INTO CAR", "SHOOT THE CAR", "SHOOT FLASHING SHOTS", "KILL TONY")

Dim asMainModeMessages2
asMainModeMessages2 = Array("", "", "", "", "", "", "SHOOT KILL RAMP", _
"SHOOT RIGHT RAMP", "", "", "", "")

Dim asMainProgressMessage
asMainProgressMessage = Array("", "1", "6", "3", "30", "30", "3", "4", "8", "5", "8", "10") 

Dim asSideEventMessages
asSideEventMessages = Array("", "SHOOT BANK TARGETS", "SHOOT LEFT RAMP", "SHOOT THE RAMPS", "SHOOT RIGHT RAMP", "HIT THE CAR")

Dim asModeNames
asModeNames = Array("", "COMMUNIST", "DRUG DEAL", "SOSA DEAL", "YOUR OWN", "DISCO BRWAL", "FRANK'S DEATH", "TOP CLIMB", "COP STING", "JORNALIST", "MANNY'S DEATH", "SAY HELLO" )

Dim asSideNames
asSideNames = Array("", "BANK RUN", "HITMAN", "MULTIBALL", "YEYO RUN", "GET CAR")



Sub Table1_Init()
	LoadEM

'	SetBallsPerGame 3

	FlBumperFadeTarget(1) = 0
	FlBumperFadeTarget(2) = 0
	FlBumperFadeTarget(3) = 0
	nPlayersInGame = 0
    GameStarted = 0
    Credits = 0
    Player = 0
    Players = 0
    Loadhs
    GiOff
    AllLampsOff
	nPlayer = 0
	bInstantInfo = False
	WallsDown
	AttractTimer.Interval = 2000

	PuPInit

	'Addscore 0

	bGameStarted = False
    ' Init bumpers, slingshots, targets and other VP objects
    LockPost.Isdropped = 1:
	BackDoorPost.Isdropped = 1

    GameTimer.Enabled = 1
    AutoPlunger.Pullback
	If Credits > 0 Then
		DOF 132, DOFOn
	End If
    ' Start Attrack mode
	if Not ScorbitActive Then
		AttractMode_On ' maybe add to startattractmode
	End If

    GiOn
End Sub

Sub Table1_Exit()
	If B2SOn Then
		Controller.Stop
	End If
End Sub

Sub Initialize   ' First Ball
dim i
    ResetLights
   
	' Reset player scores
	for i = 0 to 4
		Score(i) = 0
		anScore(i) = 0
		BallsInLock(i) = 0
	Next

	addscore 0
    


    Score(0) = 0 'used in the EE
    Score(1) = 0
    Score(2) = 0
    Score(3) = 0
    Score(4) = 0
    Bonus = 0
    BallsRemaining = nBallsPerGame ' first ball
    BallsOnPlayfield = 0
    BallLockEnabled = 0
    
    BallsInDesk = 0
    BallSaved = 0
    nBonusX(nPlayer) = 1
    MultiballDelay = 0
    NextBallBusy = 0
    TiltDelay = 0
    Tilted = False
    Tilt = 0
    TiltWarning = 0
    Special = 0
    SpecialIsLit = 0
    SpecialAwarded = 0
    AutoPlungerDelay = 0
    AutoPlungerReady = 0
    MultiBallMode = 0
    ExtraBallIsLit = 0
    ExtraBallAwarded = 0
    GameStarted = 1
	FlamingoCounter = 0
	bRightFlipperHeld = False
	bLeftFlipperHeld = False
	bSkipTargetReset = False
    Goal1 = 2000000
    Goal2 = 6000000
	ClearHighScoreDMD
    Made1(1) = False
    Made1(2) = False
    Made1(3) = False
    Made1(4) = False
    Made2(1) = False
    Made2(2) = False
    Made2(3) = False
    Made2(4) = False
	for i = 1 to 4
		iMiniGameCnt(i) = 0 'AOR, so we can only play minigame once per player, per 
	next 
	bMultiBallMode = False
    AudioQueue.Add "PlayTheme","PlayTheme",80,8000,0,0,0,False
    ' Reset Variables
    InitVariables
    ' Init Modes

    ResetDroptargets
    InitModes

    InitSkillShot
    InitLaneLights
    InitExtraBall
    InitLock
    InitYeyo
    InitBumpers
    InitScarface
    InitKillRamp
    InitGetCar
    InitMiniLoops
    InitTWIY
    InitEvents
    InitEasterEgg
	Init_Game


	if ScorbitActive = 1 And (Scorbit.bNeedsPairing) = False Then 
		Scorbit.StartSession()
		GameModeStrTmp="BL{Red}: Starting Game"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
	bOnTheFirstBallScorbit = True
	pDMDsetPage pScores, "Initialize"
End Sub

Sub StartGame

    Initialize
    AttractMode_Off
    GiOn
	bGameStarted = True
    NextBallDelay = 30
	'HidePupSplashMessages
	ClearPupSplashMessages
	GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",60,3000,0,0,0,False
	AttractTimer.Enabled = 0

End Sub

Sub ResetLights
    AllLampsOff
End Sub

'******************************
'     Game Timer Loop
' used for all the small waits
'******************************

Dim BallRelDelay, BonusDelay, NextBallDelay, TiltDelay
Dim GameOverDelay, MatchDelay, MultiballDelay, AutoPlungerDelay, EjectLockedBallsDelay, EjectRightBallDelay
Dim ComboDelay, NextBallBusy, VukDelay

Dim DbgOn: DbgOn = True
Dim BonusMult
Sub GameTimer_Timer
    Dim x, tmpTimeValue


    ' Check some subs - update realtime variables
    'check the delays

    If NextBallDelay> 0 Then
        NextBallDelay = NextBallDelay - 1
        If NextBallDelay = 0 Then
            NextBall
        End If
    End If

    If TiltDelay> 0 Then
        TiltDelay = TiltDelay - 1
        If TiltDelay = 0 Then
            Tilt = Tilt - 1
            If Tilt> 0 Then
                TiltDelay = 20
            Else
                TiltWarning = 0
            End If
        End If
    End If

    If GameOverDelay> 0 Then
'		if DbgON Then Dbg "GameOver Delay": DbgOn = False
        GameOverDelay = GameOverDelay - 1
        If GameOverDelay = 0 And bGameOver = False Then GameOver
'		DbgOn = True
    End If

    If BonusDelay > 0 Then
		AudioQueue.RemoveAll(True)
		GeneralPupQueue.RemoveAll(True)
		BallHandlingQueue.RemoveAll(True)

		FlamingoTimer.Enabled = 0
		pDMDLabelhide "Flamingo"

		if DbgON Then Dbg "Bonus Delay": DbgOn = False

		BonusDelay = BonusDelay - 1

		if BonusDelay = int(BonusDelay2/4)*3 Then
			PuPlayer.LabelSet pDMD,"SideEventCaption","Side Events "&SideEventsFinished,1, ""
		Elseif BonusDelay = int(BonusDelay2/4)*2 Then
			BonusMult = Bonus * nBonusX(nPlayer)
			Dbg "BONUS AMOUNT: " &Bonus
			if Bonus = 0 Then 
				PuPlayer.LabelSet pDMD,"BonusTotal", "Bonus 0",1, ""
			Else
				PuPlayer.LabelSet pDMD,"BonusTotal", "Bonus " &(FormatScore(Bonus) & " X " & nBonusX(nPlayer)) & " = " & FormatScore(BonusMult) &" ",1, ""
			End If
		Elseif BonusDelay = int(BonusDelay2/4)*1 Then
			AddScore BonusMult
			BonusMult = 0
			PuPlayer.LabelSet pDMD,"BonusScoreValue","Score " &FormatScore(Score(nPlayer)) &" ",1, ""
		End If

        If BonusDelay = 0 Then 

			NextBallDelay = 40
			DbgOn = True
			HideBonusMessages
			ChangeScores
			pdmdsetpage pScores, "BonusEnded"
			pDMDsetHUD 1
			'ResetOverlay
			Bonus = 0
			if Not bOnTheFirstBall And nballspergame <> Balls Then ChangePlayer

		End If
		
    End If

    If MatchDelay> 0 then 'it works with the two digits of the last player's score
'		if DbgON Then  Dbg "Match Delay": DbgOn = False
        MatchDelay = MatchDelay - 1

        If MatchDelay = 0 then	
'			DbgOn = True
'            Match = 10 * (INT(10 * Rnd(1) ) ):If Match <10 Then Match = "00"
'            x = Match
'            If Match = "00" Then x = 0
'            If x = anScore(nPlayer) MOD 100 Then
'                PlaySound SoundFXDOF("fx_knocker",134,DOFPulse,DOFKnocker)
'                Credits = Credits + 1
'				If Credits = 1 then
'				    DOF 132, DOFOn
'				End If
'            End If
            GameOverDelay = 1
        End If
    End If

    If BallSavedDelay> 0 Then
		'if DbgON Then Dbg "BallSaved Delay": DbgOn = False
        BallSavedDelay = BallSavedDelay - 1
        If BallSavedDelay = 100 Then
            BlinkFastBallSaved 'blink fast the last 5 seconds
        End If
        If BallSavedDelay = 0 Then	
			DbgOn = True
            EndBallSaved
        End If
    End If


    If AutoPlungerDelay> 0 Then
		if DbgON Then Dbg "AutoPlunger Delay": DbgOn = False
        AutoPlungerDelay = AutoPlungerDelay - 1
        If AutoPlungerDelay = 0 Then
			DbgOn = True
            AutoPlungerFire
            AutoPlungerReady = 0
        End If
    Else
        If BallInPlunger AND MultiBallMode AND EventStarted <> 8 Then AutoPlungerDelay = 1
    End If

    If MultiballDelay> 0 Then ' Eject the balls to the autoplunger
		if DbgON Then Dbg "Multiball Delay": DbgOn = False
        MultiballDelay = MultiballDelay - 1
        If MultiballDelay = 80 Then MultiBallMode = 1:NewBall:AutoPlungerReady = 1:AutoPlungerDelay = 10:End If
        If MultiballDelay = 60 Then MultiBallMode = 1:NewBall:AutoPlungerReady = 1:AutoPlungerDelay = 10:End If
        If MultiballDelay = 40 Then MultiBallMode = 1:NewBall:AutoPlungerReady = 1:AutoPlungerDelay = 10:End If
        If MultiballDelay = 20 Then MultiBallMode = 1:NewBall:AutoPlungerReady = 1:AutoPlungerDelay = 10:End If
        If MultiballDelay = 0 Then MultiBallMode = 1:NewBall:AutoPlungerReady = 1:AutoPlungerDelay = 10:DbgOn = True:End If
        'MultiballDelay = MultiballDelay - 1
    End If

    ' only for this table

    If PlungerLaneKDelay> 0 Then
        PlungerLaneKDelay = PlungerLaneKDelay - 1
        If PlungerLaneKDelay = 0 Then
			DbgOn = True
            BallsInPlungerLane = BallsInPlungerLane - 1
            PlungerLaneExit
            If BallsInPlungerLane > 0 Then PlungerLaneKDelay = 20
        End If
    End If

    If OpenLockBlockDelay> 0 Then
		if DbgON Then Dbg "OpenLock Delay": DbgOn = False
        OpenLockBlockDelay = OpenLockBlockDelay - 1
        If OpenLockBlockDelay = 0 Then
			DbgOn = True
            OpenLockBlock
        End If
    End If

    If LaneGateDelay> 0 Then
		if DbgON Then Dbg "Lane Gate Delay": DbgOn = False
        LaneGateDelay = LaneGateDelay - 1
        If LaneGateDelay = 0 Then
			DbgOn = True
            LaneGateClose
        End If
    End If

    If CarHoleDelay> 0 Then
		if DbgON Then Dbg "Car Hole Delay": DbgOn = False
        CarHoleDelay = CarHoleDelay - 1
        If CarHoleDelay = 0 Then
			DbgOn = True
            CarHoleExit
        End If
    End If

    If CarHole8Delay> 0 Then
		'if DbgON Then Dbg "CarHole 8 Delay": DbgOn = False
        CarHole8Delay = CarHole8Delay - 1
        If CarHole8Delay = 0 Then
			DbgOn = True
            CarHole8Exit
        End If
    End If

    If bumperoffdelay> 0 Then
		if DbgON Then Dbg "Bumper Off Delay": DbgOn = False
        bumperoffdelay = bumperoffdelay - 1

		if bumperoffdelay Mod 20 = 0 Then 
			PuPlayer.LabelSet pDMD,"SideEventTimerValue"," " & bumperoffdelay/20 &" ",1,""
		End If

        If bumperoffdelay = 0 Then
			DbgOn = True
            InitBumpers
        End If
    End If

    If BankDelay> 0 Then
		'if DbgON Then Dbg "Bank Delay": DbgOn = False
        BankDelay = BankDelay - 1

		if BankDelay Mod 20 = 0 Then 
			PuPlayer.LabelSet pDMD,"SideEventTimerValue"," " & BankDelay/20 &" ",1,""
		End If

        If BankDelay = 0 Then
			DbgOn = True
			PuPlayer.LabelSet pDMD,"SideEventTimerValue","",1,""
            EndBank
        End If
    End If

    If GetCarDelay> 0 Then
		'if DbgON Then Dbg "GetCar Delay": DbgOn = False

        GetCarDelay = GetCarDelay - 1
		if GetCarDelay Mod 20 = 0 Then 
			PuPlayer.LabelSet pDMD,"SideEventTimerValue"," " &GetCarDelay/20 &" ",1,""
		End If

        If GetCarDelay = 0 Then
			DbgOn = True
			PuPlayer.LabelSet pDMD,"SideEventTimerValue","",1,""
            EndGetCar
        End If
    End If

    If StartTWIYDelay> 0 Then
		'if DbgON Then Dbg "TWIY Delay": DbgOn = False
		bMultiBallMode = True
        StartTWIYDelay = StartTWIYDelay - 1
		if StartTWIYDelay Mod 20 = 0 Then 
			PuPlayer.LabelSet pDMD,"SideEventTimerValue"," " &StartTWIYDelay/20 &" ",1,""
		End If


        'If StartTWIYDelay = 0 Then
		'	DbgOn = True
            'StartTWIY
        'End If
    End If

    If Event1Delay> 0 Then
        Event1Delay = Event1Delay - 1
		if Event1Delay Mod 20 = 0 Then 
			PuPlayer.LabelSet pDMD,"MainModeTimerValue"," " &Event1Delay/20 &" ",1,""
		End If
        If Event1Delay = 400 Then Say "doing_a_great_job", "13252527"
        If Event1Delay = 300 Then Say "wastemytime", "13121324461"
        If Event1Delay = 200 Then Say "You_fuckin_hossa", "13172617171"
        If Event1Delay = 0 Then
            EndEvent1
        End If
    End If

    If Event2Delay> 0 Then
        Event2Delay = Event2Delay - 1
		if Event2Delay Mod 20 = 0 Then 
			PuPlayer.LabelSet pDMD,"MainModeTimerValue"," " &Event2Delay/20 &" ",1,""
		End If
        If Event2Delay = 700 Then Say "No_fuckin_way", "1314251"
        If Event2Delay = 300 Then Say "Ok_your_starting_to_piss_me_off", "222514122315421"
        If Event2Delay = 0 Then
            EndEvent2
        End If
    End If

    If Event3Delay> 0 Then
        Event3Delay = Event3Delay - 1
		if Event3Delay Mod 20 = 0 Then 
			PuPlayer.LabelSet pDMD,"MainModeTimerValue"," " &Event3Delay/20 &" ",1,""
		End If
        If Event3Delay = 700 Then Say "Thats_ok", "26351"
        If Event3Delay = 400 Then Say "Ok", "32341"
        If Event3Delay = 300 Then Say "Ok_so_what_you_doing_later", "3223841214131"
        If Event3Delay = 0 Then
            EndEvent3
        End If
    End If

    If Event4Delay> 0 Then
        Event4Delay = Event4Delay - 1
		if Event4Delay Mod 20 = 0 Then 
			PuPlayer.LabelSet pDMD,"MainModeTimerValue"," " &Event4Delay/20 &" ",1,""
		End If
        If Event4Delay = 0 Then
            EndEvent4
        End If
    End If

    If Event5Delay> 0 Then
        Event5Delay = Event5Delay - 1
		if Event5Delay Mod 20 = 0 Then 
			PuPlayer.LabelSet pDMD,"MainModeTimerValue"," " &Event5Delay/20 &" ",1,""
		End If
        If Event5Delay = 700 Then Say "Hello_pussy_cat", "16242424"
        If Event5Delay = 400 Then Say "Hey_there_sweet_cheeks", "2414131314"
        If Event5Delay = 300 Then Say "Cmon_pussy_cat", "16272538"
        If Event5Delay = 0 Then
            EndEvent5
        End If
    End If

    If Event6Delay> 0 Then
        Event6Delay = Event6Delay - 1
		if Event6Delay Mod 20 = 0 Then 
			PuPlayer.LabelSet pDMD,"MainModeTimerValue"," " &Event6Delay/20 &" ",1,""
		End If
        If Event6Delay = 700 Then Say "You_fuckin_kidding_me_man", "2324133413141"
        If Event6Delay = 600 Then Say "Dont_you_fucken_listen_man", "2313432613131336"
        If Event6Delay = 400 Then Say "Fuckin_cock_sucker", "8524252524"
        If Event6Delay = 300 Then Say "For_fuck_sake", "231323241"
        If Event6Delay = 150 Then Say "Fuck_you_chico", "16574539"
        If Event6Delay = 0 Then
            EndEvent6
        End If
    End If

    If Event7Delay> 0 Then
        Event7Delay = Event7Delay - 1
 		if Event7Delay Mod 20 = 0 Then 
			PuPlayer.LabelSet pDMD,"MainModeTimerValue"," " &Event7Delay/20 &" ",1,""
		End If
        If Event7Delay = 400 Then Say "hello1", "391"
        If Event7Delay = 300 Then Say "hello2", "281"
        If Event7Delay = 0 Then
            EndEvent7
        End If
    End If

    If Event8Delay> 0 Then
        Event8Delay = Event8Delay - 1
		if Event8Delay Mod 20 = 0 Then 
			PuPlayer.LabelSet pDMD,"MainModeTimerValue"," " &Event8Delay/20 &" ",1,""
		End If
        If Event8Delay = 400 Then Say "Bring_my_car_dont_fuck_around", "179517972636663925"
        If Event8Delay = 200 Then Say "I_need_my_fuckin_car_man", "162424161476371"

		if Event8Delay = 0 Then EndEvent8
    End If

    If Event9Delay> 0 Then
        Event9Delay = Event9Delay - 1
		if Event9Delay Mod 20 = 0 Then 
			PuPlayer.LabelSet pDMD,"MainModeTimerValue"," " &Event9Delay/20 &" ",1,""
		End If
        If Event9Delay = 700 Then Say "I_piss_in_your_face", "2333131313131313132"
        If Event9Delay = 400 Then Say "Im_Tony_fuckin_Montana", "13362526251"
        If Event9Delay = 300 Then Say "Now_your_fucked", "26361"
        If Event9Delay = 0 Then
            EndEvent9
        End If
    End If

    If StartEvent11Delay> 0 Then
        StartEvent11Delay = StartEvent11Delay - 1
		'if Event11Delay Mod 20 = 0 Then 
		'	PuPlayer.LabelSet pDMD,"MainModeTimerValue",Event11Delay/20,1,""
		'End If
        If StartEvent11Delay = 0 Then
            StartEvent11Pre
        End If
    End If

    If Event11VoiceDelay> 0 Then
        Event11VoiceDelay = Event11VoiceDelay - 1
        If Event11VoiceDelay = 2400 Then Say "You_know_who_your_fuckin_with", "25162424261"
        If Event11VoiceDelay = 2100 Then Say "You_need_an_army_to_take_me", "13151414141413331"
        If Event11VoiceDelay = 1800 Then SayEvent11
        If Event11VoiceDelay = 1500 Then Say "You_picked_the_wrong_guy_to_fuck_with", "23232523233334261"
        If Event11VoiceDelay = 1200 Then Say "Cmon_bring_your_army", "13144814"
        If Event11VoiceDelay = 900 Then SayEvent11
        If Event11VoiceDelay = 600 Then Say "You_think_you_can_fuck_with_me", "2414141423261"
        If Event11VoiceDelay = 300 Then Say "Cmon_come_and_get_me", "12132643233414"
        If Event11VoiceDelay = 0 Then
            SayEvent11
            Event11VoiceDelay = 2400
        End If
    End If

    If EndEvent11Delay> 0 Then
        EndEvent11Delay = EndEvent11Delay - 1
        If EndEvent11Delay = 0 Then
            EndEvent11
        End If
    End If

    If EndWinEvent11Delay> 0 Then
        EndWinEvent11Delay = EndWinEvent11Delay - 1
        If EndWinEvent11Delay = 0 Then
            EndWinEvent11
        End If
    End If

'    If ResetDropDelay> 0 Then
'        ResetDropDelay = ResetDropDelay - 1
'        If ResetDropDelay = 0 Then
'            ResetDroptargets "Drop Delay"
'        End If
'    End If

    If VukDelay> 0 Then
        VukDelay = VukDelay - 1
        If VukDelay = 20 Then
            PlaySound "reload3_shotgun"
        End If
        If VukDelay = 0 Then
            VukEject
        End If
    End If

    If EndEEDelay> 0 Then
        EndEEDelay = EndEEDelay - 1
        If EndEEDelay = 0 Then
            EndEE
        End If
    End If

    If EndEEDelay2> 0 Then
        EndEEDelay2 = EndEEDelay2 - 1
        If EndEEDelay2 = 0 Then
            If BallsOnPlayfield Then
                EndEEDelay2 = 10
            Else
                GameOver_Part2
            End If
        End If
    End If

    If StartEasterEggShootingDelay> 0 Then
        StartEasterEggShootingDelay = StartEasterEggShootingDelay - 1
        If StartEasterEggShootingDelay = 0 And not bGameStarted Then
			HidePupSplashMessages
            StartEasterEggShooting
        End If
    End If

    If StartHitManDelay> 0 Then
        StartHitManDelay = StartHitManDelay - 1
        If StartHitManDelay = 0 Then
            StartHitMan2
        End If
    End If

    If CTDelay> 0 Then
		'if DbgON Then Dbg "CT Delay": DbgOn = False
        CTDelay = CTDelay - 1
        If CTDelay = 90 Then l40.State = 0:l40b.State = 0:l38.State = 1:l38b.State = 1:CurrentCT = 1
        If CTDelay = 60 Then l38.State = 0:l38b.State = 0:l39.State = 1:l39b.State = 1:CurrentCT = 2
        If CTDelay = 30 Then l39.State = 0:l39b.State = 0:l40.State = 1:l40b.State = 1:CurrentCT = 3
        If CTDelay = 0 Then CTDelay = 91
    End If

    If EjectXLockedBallDelay> 0 Then 'eject extra ball
        EjectXLockedBallDelay = EjectXLockedBallDelay - 1
        If EjectXLockedBallDelay = 0 Then
            BallsInLock(nPlayer) = BallsInLock(nPlayer) - 1
            BallsOnPlayfield = BallsOnPlayfield + 1
            LockExit
        End If
    End If

    If EjectBallsInDeskDelay> 0 Then 'eject balls from under the desk
        EjectBallsInDeskDelay = EjectBallsInDeskDelay - 1
        If EjectBallsInDeskDelay = 0 Then
            If BallsInDesk> 0 Then
                BallsInDesk = BallsInDesk - 1
                BallsOnPlayfield = BallsOnPlayfield + 1
                LockExit
            End If
            If BallsInDesk> 0 Then EjectBallsInDeskDelay = EjectBallValue
        End If
    End If
End Sub

'*****
'Drain
'*****
Dim BonusDelay2
'Dim sPupFound
Sub Drain_Hit

	Debug.print "EBD: " &EjectBallsInDeskDelay
    
    Drain.DestroyBall
    BallsOnPlayfield = BallsOnPlayfield -1

	RandomSoundDrain drain ' Fleep
    LastSwitchHit = "drain"

    ' end modes

    If Tilted Then
        If BallsOnPlayfield = 0 Then
            Tilted = false:TiltObjects 0
            LightSeqTilt.StopPlay
            TiltDelay = 0
            TiltWarning = 0
			bMultiBallMode = False
			EOBTime = 3100
            If EE Then
                EndEE
            Else
                BallSaved = 0
                AutoPlungerReady = 0
                MultiBallMode = 0
                'NextBallDelay = 40
            End If
		Else
			Exit Sub
        End If
    End If

    'only for this table





    If EE Then
        If BallSaved Then
            StartScarShooting 1
        Else
            If BallsOnPlayfield = 0 Then
                EndEE
            End If
        End If
        Exit Sub
    End If

   ' If EventStarted = 8 Then
    '    Exit Sub
    'End If

' added in 103W TWITStarted check
	if (EventStarted = 11 or TWIYStarted) And BallSaved Then
	'Dbg "HERE MAN, WHATCHA GONNA DO:" &OldHeadPos
		Select Case OldHeadPos
			Case 1
				GunUp.rotY = -32
				cannon1.createball
				cannon1.kick 140, 16 + RND(1) * 3
			Case 2
				GunUp.rotY = -32
				cannon2.createball
				cannon2.kick 140, 16 + RND(1) * 3
			Case 3
				GunUp.rotY = -4
				cannon3.createball
				cannon3.kick 210, 16 + RND(1) * 3
			Case 4
				GunUp.rotY = 32
				cannon4.createball
				cannon4.kick 210, 16 + RND(1) * 3
			Case 5
				GunUp.rotY = 32
				cannon5.createball
				cannon5.kick 210, 16 + RND(1) * 3
		End Select
		BallsOnPlayfield = BallsOnPlayfield + 1
		PlaySound "machinegun1"
		Exit Sub
	End If

    If BallSaved Then
		'pDMDScrollBig(LabName,msgText,byVal timeSec,mColor) 'timeSec in MS
		'pDMDScrollBig "Splash", "DON'T MOVE", 3, cWhite
		pDMDSplashBig "DON'T MOVE", 3, cWhite
		GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",80,3100,0,0,0,False

		if Scorbit.bSessionActive then
			GameModeStrTmp="BL{green}:Ball Saved"
			Scorbit.SetGameMode(GameModeStrTmp)
		End If

		Pupevent 303
        AutoPlungerReady = 1
        If MultiBallMode = 1 Then
			MultiBallDelay = MultiBallDelay + 20 'add a new multiball
        Else
            EndBallSaved
            If EventStarted <> 0 Then
				dbg "EV:" &EventStarted
                NewBall8
				'EndMainEvent
				'EventStarted = 1
				'PuPlayer.LabelSet pDMD, "MainModeMessage", asMainModeMessages(CurrentEvent), 1, ""
            Else
				'EndMainEvent
                NewBall
            End If

        End If
        Exit Sub
    End If

    If BallsOnPlayfield = 1 And EjectBallsInDeskDelay = 0 Then 'this is the last multiball so turn off multiballs and other effects
        MultiBallMode = 0
		bMultiBallMode = False
		Say "saygoodnight", "251414152424231"
        AutoPlungerReady = 0
        'end multiball modes
        If EventStarted = 11 Then
            EndEvent11
        End If
        EndHitMan
        EndYeyoRun
        EndTWIY
    End If

	If BallsOnPlayfield = 0 And EjectBallsInDeskDelay = 0 Then GeneralPupQueue.Add "DisplayEOB","DisplayEOB",80,EOBTime,0,0,0,False

End Sub

Sub DisplayEOB

	FlamingoTimer.enabled = 0
	pDMDLabelHide "Flamingo" 

    If BallsOnPlayfield = 0 Then ' this is the last ball
		debug.print "Should be end of ball sequence"
		StopAllMusic		' RTP27
        If ExtraBallAwarded Then
			bMultiBallMode = False
			pDMDSplashBig "SHOOT AGAIN", 3, cGold
			GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",80,3100,0,0,0,False

			ExtraballTimer.enabled = 0
			pDMDLabelHide "ExtraballImage"
			GeneralPupQueue.Add "Hide EBImage","pDMDLabelHide ""ExtraballImage"" ",80,500,0,0,0,False

            InitExtraBall
            AutoPlungerReady = 0
            NewBall
        Else
			bSupressEndEventMessage = True
			'body.Visible = 1
			'GunUp.Visible = 0
			bMultiBallMode = False
			TonySitDownTimer.Enabled = 1
			ClearPupSplashMessages
            SkillshotReady = 1
            'StopAllMusic
            GiOff


            EndMainEvent
            CloseLockBlock
            EndGetCar
            EndBank

			if Scorbit.bSessionActive then
				GameModeStrTmp="BL{Red}:Ball " &CurrBall& " Lost"
				Scorbit.SetGameMode(GameModeStrTmp)
			End If


			FindDrain
			if IndexTmp = 999 Then FindDrain

            BonusDelay2 = DrainLength(IndexTmp)/1000 * 20 ' 6 seconds
			
			BonusDelay = int(BonusDelay2)

			Dbg "*****************************"
			Dbg "BonusDelay: " &BonusDelay
			Dbg "*****************************"
			CountEvents


			pDMDsetHUD 0
			pdmdsetpage pBonus, "Bonus"

			pupevent DrainPUp(IndexTmp)

			UpdateMainEventLights
			UpdateSideLights

			if VRRoom > 0 Then
				PuPlayer.LabelSet pDMD,"BonusCaption",nBonusX(nPlayer) &"X Bonus" ,1, ""
			elseif nBonusX(nPlayer) = 1 Then
				PuPlayer.LabelSet pDMD,"BonusCaption","0 Main Modes Completed = " &nBonusX(nPlayer) &"X Bonus" ,1, ""
			Elseif	nBonusX(nPlayer) = 2 Then
				PuPlayer.LabelSet pDMD,"BonusCaption","1 Main Mode Completed = " &nBonusX(nPlayer) &"X Bonus" ,1, ""
			Else
				PuPlayer.LabelSet pDMD,"BonusCaption",nBonusX(nPlayer)-1 &" Main Modes Completed = " &nBonusX(nPlayer) &"X Bonus" ,1, ""
			End If
			PuPlayer.LabelSet pDMD,"MainEventCaption","Main Events "&MainEventsCount,1, ""
        End If
    End If
End Sub

'******************
' New Ball Release
'******************

Function CurrBall
	Dbg "BPG:" &nBallsPerGame
	Dbg "BR:" &BallsRemaining
	CurrBall = nBallsPerGame - BallsRemaining
End Function

Sub NextBall
	Dim i

'	debug.print "NextBall"
'	if nPlayersinGame > 1 Then

'		nPlayer = nPlayer + 1 'otherwise move to the next player
'    If nPlayer> nPlayersInGame Then
'        BallsRemaining = BallsRemaining - 1
'        nPlayer = 1
'    End If

	''if Not bOnTheFirstBall Then ChangePlayer
'	End If


	'InitYeyo
	'InitKillRamp
	'InitBank

	' Reset MiniGame Prep counters
	BonusPoPCount = 0
	EOBTime = 10
	MiniGameLoops(nPlayer,0) = 0
	MiniGameLoops(nPlayer,1) = 0
	bMiniGamePlayed = False
	bMiniGameAllowed = False
	pdmdlabelhide "Flamingo"
	nTiltLevel = 0
	
	
	RaiseDiverterPin False
	bSupressMainEvents = False
	bSupressEndEventMessage = False
	ResetDroptargets
	RaiseBankTargets

	DMDUpdatePlayerName
	DisplayPlayfieldValue
	CheckMainEvents
	UpdateMainEventLights
	UpdateSideLights
	'DisplayBonusValue ' included with checkmainevents

	DMDUpdateBallNumber Balls
	Dbg "Ball Number: " & Balls

    If BallsRemaining = 0 And nPlayer = nPlayersInGame Then
		Debug.print "Balls left is zero: " & Balls
        StopAllMusic
        AllLampsOff
        'MatchDelay = 100   ' Merlin Removed 104J
        MatchDelay = 1
        Exit Sub
    End If

    ' GiEffect1
    GiOn
    If SkillshotReady Then
        StartSkillshot
    End If
    'If Balls = 1 Then Say "Free_advice_dont_fuck_with_me", "14242424247313132222722424"
    'If Balls = 2 Then Say "I_feel_like_I_already_know_you", "1324171223122413"
    'If Balls >= 3 Then Say "Watch_yourself_man", "1324238523133435212314241"

	if Balls = 1 And nPlayer = 1 Then
		i = INT(RND(1) * 3)
		Select case i
			Case 0:Say "Free_advice_dont_fuck_with_me", "14242424247313132222722424"
			Case 1:Say "I_feel_like_I_already_know_you", "1324171223122413"
			Case 2:Say "Watch_yourself_man", "1324238523133435212314241"
		End Select

	End If

	ReloadLockLights
	DMDUpdateAll
    NewBall
End Sub

Sub NewBall
'	debug.print "** New Ball"
	TonySitDown
'	pDMDSetPage pScores, "NewBall"
'	DMDUpdateAll
	'ClearQueues
	RaiseDiverterPin False
    If(BallsOnPlayfield + BallsInLock(nPlayer)) = nMaxBalls Then Exit Sub 'Exit sub if total balls on table is maxed out
    BallRelease.createball
    BallsOnPlayfield = BallsOnPlayfield + 1
    BallRelease.Kick 90, 8
    'PlaySoundat SoundFXDOF("fx_ballrel",108, DOFPulse,DOFContactors), Ballrelease
	RandomSoundBallRelease ballrelease ' Fleep

	if BallsRemaining > 2 Then
     AudioQueue.Add "PlayTheme","PlayTheme",80,4000,0,0,0,False
	Else
		' Delay audio so ball lost video/audio can complete
    AudioQueue.Add "PlayTheme","PlayTheme",80,6000,0,0,0,False
	End If

End Sub

Function Balls
    Dim tmp
    tmp = nBallsPerGame - BallsRemaining + 1
    If tmp> nBallsPerGame Then
        Balls = nBallsPerGame
    Else
        Balls = tmp
    End If
End Function

'***********************
'       BALL SAVE
'***********************

' lights: l1 (this is the Ball Save light)

Dim BallSavedDelay

Sub InitBallSaver
    BallSavedDelay = 0
    BallSaved = 0
    l1.State = 0
End Sub

Sub StartBallSaved(seconds)

    If BallSavedDelay < (20 * seconds) Then
        BallSavedDelay = BallSavedDelay + (20 * seconds) ' times 20 because of the game timer being 50 units, 1000 = 1 sec
Dbg "Ball Save Started: " &BallSavedDelay
    End If
    BallSaved = 1
    l1.BlinkInterval = 100
    l1.State = 2
End Sub

Sub BlinkFastBallSaved
    l1.State = 0
    l1.BlinkInterval = 20
    l1.State = 2
End Sub

Sub EndBallSaved
Dbg "Stopping Ball SAved"
    'AutoPlungerReady = 0
    If MultiBallmode = 1 Then 'turn off the ballsaver timer during multiball when the timer is 0
        If BallSavedDelay = 0 Then
            AutoPlungerReady = 0
            BallSaved = 0
            l1.State = 0
        End If
    Else
        BallSavedDelay = 0
        BallSaved = 0
        l1.State = 0
    End If
End Sub

'************
' Extra ball
'************
' only one extra ball per player
'light 49

Sub GiveExtraBall
    ExtraBallAwarded = 1
    ExtraBallIsLit = 0
	GeneralPupQueue.Add "pupDMDExtrabllMessage","pupDMDExtrabllMessage",80,20,0,0,0,False

	ExtraballTimer.enabled = 1

	'PuPlayer.LabelSet pDMD, "ExtraballImage", "PuPOverlays\\cocainerazor.png",1,"{'mt':2,'width':50, 'height':50,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
	'PuPlayer.LabelSet pDMD, "ExtraballImage", "PuPOverlays\\briefcase.png",1,"{'mt':2,'width':15, 'height':25,'xalign':0,'yalign':0,'ypos':-2,'xpos':85}"
End Sub

Sub pupDMDExtrabllMessage
    PlaySound SoundFXDOF("fx_knocker", 134, DOFPulse, DOFKnocker)
    l49.State = 0
    l1.State = 1
	pDMDSplashBig "EXTRABALL", 3, cGold
	GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",80,3100,0,0,0,False
    Say "Want_to_get_a_drink", "3425233424261"
End Sub

Sub InitExtraBall
    ExtraBallAwarded = 0
    ExtraBallIsLit = 0
    l1.State = 0
    l49.State = 0
End Sub

Sub LitExtraBall
    If ExtraBallAwarded Then Exit Sub 'do not lit the extraball light if already awarded since only one extra ball per ball
    ExtraBallIsLit = 1
    l49.State = 2
    OnlyOpenLockBlock
End Sub

'************
' Special
'************

Sub GiveSpecial
    SpecialAwarded = 1
    SpecialIsLit = 0
    l55.State = 0
    PlaySound SoundFXDOF("fx_knocker", 134, DOFPulse, DOFKnocker)
    Credits = Credits + 1
	If Credits = 1 then
	    DOF 132, DOFOn
	End If
End Sub

Sub LitSpecial
    SpecialIsLit = 1
    l55.State = 2
End Sub

Sub InitSpecial
    SpecialAwarded = 0
    SpecialIsLit = 0
    l55.State = 0
End Sub

'***************
' Add Multiballs
'***************
' Used to add mutiballs to the table

Sub AddMultiballs(nr)
    MultiBallMode = 1
    MultiballDelay = MultiballDelay + nr * 20
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal keycode)

	if keycode = "3" Then testRTP

    If Gamestarted AND NOT Tilted AND EventStarted <> 8 Then
        If keycode = LeftFlipperKey Then
		bLeftFlipperHeld = True
		SolLFlipper 1
			If PuPGameRunning Then 'AOR
					PuPGameInfo= PuPlayer.GameUpdate(PuPMiniGameTitle, 1 , 87 , "")  'w
			End If
		End If 'AOR					


        If keycode = RightFlipperKey Then
		bRightFlipperHeld = True
		SolRFlipper 1
			If PuPGameRunning Then 'AOR
					PuPGameInfo= PuPlayer.GameUpdate(PuPMiniGameTitle, 1 , 83 , "")  's
			End If
		End If

    Else
            If keycode = LeftFlipperKey And BallsOnPlayfield = 0 Then PlaySound "WilliamsBong":TempEgg = TempEgg + "l":CheckStartEgg
            If keycode = RightFlipperKey And BallsOnPlayfield = 0 Then PlaySound "WilliamsBong":TempEgg = TempEgg + "r":CheckStartEgg
    End If


	If keycode = LeftFlipperKey And bRightFlipperHeld And BallsOnPlayfield = 0 Then
		if bonusdelay > 0 Then
			NextBallDelay = 40
			HideBonusMessages
			PuPEvent DrainKill(IndexTmp)
			pdmdsetpage pScores, "BonusEnded"
			pDMDsetHUD 1
			bonusdelay = 2
			Exit Sub
		End If
	End If

	If keycode = RightFlipperKey And bLeftFlipperHeld And BallsOnPlayfield = 0 Then 
		if bonusdelay > 0 Then
			NextBallDelay = 40
			HideBonusMessages
			PuPEvent DrainKill(IndexTmp)
			pdmdsetpage pScores, "BonusEnded"
			pDMDsetHUD 1
			bonusdelay = 2
			Exit Sub
		End If
	End If

    If keycode = AddCreditKey Then
        If credits = 0 Then Say "Ok_lets_make_this_happen", "12361214231"
        credits = credits + 1
		If credits = 1 Then
			DOF 132, DOFOn	
		End If
        PlaySound "fx_Coin"
        If credits> 15 Then credits = 15

        savehs()
    End If

    If keycode = AddCreditKey2 Then
        If credits = 0 Then Say "Ok_lets_make_this_happen", "12361214231"
        credits = credits + 5
		If credits = 5 Then
		    DOF 132, DOFOn
		End If
        PlaySound "fx_Coin"
        If credits> 15 Then credits = 15
 
        savehs()
    End If

    If keycode = StartGameKey Then
        If credits> 0 or bFreeplay Then
		ClearHS
		ClearPupSplashMessages
		pDMDLabelhide "AttractCredits"
		ClearQueues

		' Kill GameOver Video
		if bGameOver Then PupEvent 505:GameStarted = 0:bGameOver = False: ClearQueues

			soundStartButton() ' Fleep

            If nPlayersInGame <4 Then
                If GameStarted = 0 Then
					'debug.print "WTF"
					GeneralPupQueue.Add "Pupevent 320","Pupevent 320",60,5000,0,0,0,False
                    nPlayersInGame = nPlayersInGame + 1
					Dbg "Added a player: " &nPlayersInGame
					GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",85,0,0,0,0,True
					pDMDSplashBig "Player "&nPlayersInGame &" Added", 3, cGold
					GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",80,3100,0,0,0,False
                    nPlayer = 1
                    credits = credits - 1
					If credits = 0 Then 
						DOF 132, DOFOff	
					End If

                    savehs()
                    If nPlayersInGame = 1 AND BallsOnPlayfield = 0 Then StartGame
                    Else
                        If nPlayer = 1 AND BallsRemaining = nBallsPerGame Then
                            nPlayersInGame = nPlayersInGame + 1
							Dbg "Added a player from Else: " &  nPlayersInGame
						Dbg "Added a player: " &nPlayersInGame
						GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",85,0,0,0,0,True
						pDMDSplashBig "Player "&nPlayersInGame &" Added", 3, cGold
						GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",80,3100,0,0,0,False
						DMDUpdatePlayerName
							credits = credits - 1
							If credits = 0 Then 
								DOF 132, DOFOff	
							End If

                            savehs()
                        End If
                End If
            End If
        Else
            'SaySomethingWhileWaiting

        End If
    End If
 
    If keycode = LeftTiltKey Then Nudge 90, 5:SoundNudgeLeft(): Bump
    If keycode = RightTiltKey Then Nudge 270, 5:SoundNudgeRight(): Bump
    If keycode = CenterTiltKey Then Nudge 0, 6:SoundNudgeCenter(): Bump
    If keycode = PlungerKey Then :Plunger.Pullback:SoundPlungerPull():AutoPlungerReady = 0 ' Fleep
    If hsbModeActive Then HighScoreProcessKey(keycode)
    If keycode = 35 Then 
		ResetHS 'H key
			pDMDSplashBig "RESET HIGHSCORES", 2, cPink
			GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",80,2100,0,0,0,False
	End If
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If KeyCode = PlungerKey Then  ' Fleep
		Plunger.Fire
		If BallinPlunger = 1 Then
			SoundPlungerReleaseBall()                        'Plunger release sound when there is a ball in shooter lane
        Else
			SoundPlungerReleaseNoBall()                        'Plunger release sound when there is no ball in shooter lane
        End If
    End If

    If GameStarted = 1 AND NOT Tilted AND EventStarted <> 8 Then
        If keycode = LeftFlipperKey Then 
		bLeftFlipperHeld = False
			SolLFlipper 0
			InstantInfoTimer.Enabled = False
            If bInstantInfo Then
                bInstantInfo = False
            End If
            
			If PuPGameRunning Then 'AOR
				PuPGameInfo= PuPlayer.GameUpdate(PuPMiniGameTitle, 2 , 87 , "")  'w
			End If 'AOR
		End If
        If keycode = RightFlipperKey Then 
		bRightFlipperHeld = False
			SolRFlipper 0
			InstantInfoTimer.Enabled = False
		    If bInstantInfo Then
                bInstantInfo = False
            End If
            
			If PuPGameRunning Then 'AOR
				PuPGameInfo= PuPlayer.GameUpdate(PuPMiniGameTitle, 2 , 83 , "")  's
			End If 'AOR
		End If
    End If
End Sub

Sub InstantInfoTimer_Timer
    InstantInfoTimer.Enabled = False
    If NOT hsbModeActive Then
        bInstantInfo = True
        InstantInfo
    End If
End Sub

Sub InstantInfo

End Sub

'********************
' Special JP Flippers
'********************

Const ReflipAngle = 20

Sub SolLFlipper(Enabled)

'Fleep
        If Enabled Then
                LF.Fire  'leftflipper.rotatetoend
				FlipperActivate LeftFlipper, LFPress::InstantInfoTimer.Enabled = True 
				RotateLightsLeft       
                If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
                        RandomSoundReflipUpLeft LeftFlipper

                Else 
                        SoundFlipperUpAttackLeft LeftFlipper
                        RandomSoundFlipperUpLeft LeftFlipper
                End If                
        Else
				FlipperDeActivate LeftFlipper, LFPress
                LeftFlipper.RotateToStart
                If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
                        RandomSoundFlipperDownLeft LeftFlipper
                End If
                FlipperLeftHitParm = FlipperUpSoundLevel
    End If
End Sub

Sub SolRFlipper(Enabled)

'Fleep
        If Enabled Then
			RF.Fire 'rightflipper.rotatetoend
			FlipperActivate RightFlipper, RFPress::InstantInfoTimer.Enabled = True
			RotateLightsRight
                If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
                        RandomSoundReflipUpRight RightFlipper
                Else 
                        SoundFlipperUpAttackRight RightFlipper
                        RandomSoundFlipperUpRight RightFlipper
                End If
        Else
			FlipperDeActivate RightFlipper, RFPress
            RightFlipper.RotateToStart
				If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
                        RandomSoundFlipperDownRight RightFlipper
                End If        
                FlipperRightHitParm = FlipperUpSoundLevel
        End If


End Sub


Sub LeftFlipper_Collide(parm)
	CheckLiveCatch ActiveBall, LeftFlipper, LFCount, parm
	LF.ReProcessBalls ActiveBall
	LeftFlipperCollide parm 'Fleep
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch ActiveBall, RightFlipper, RFCount, parm
	RF.ReProcessBalls ActiveBall
	RightFlipperCollide parm 'Fleep
End Sub

'*******************************
'    JP's Alpha Ramp Plunger
'     for non-vpm tables
'*******************************

Dim BallinPlunger

Sub swPlunger_Hit 'plunger lane switch
	BallHandlingQueue.Add "BugFix","BugFix",74,2000,0,0,0,False
    NextBallBusy = 1
    BallinPlunger = 1
    LastSwitchHit = "swPlunger"
    If EventStarted = 8 Then AutoPlungerReady = 0
    If AutoPlungerReady Then AutoPlungerDelay = 10
End Sub

Sub swPlunger_UnHit

    BallinPlunger = 0
    AutoPlungerDelay = 0
    AutoPlungerReady = 0
    NextBallBusy = 0
End Sub

Sub BugFix
	pDMDSetPage pScores, "NewBall"
	DMDUpdateAll
End Sub

'Autoplunger

Sub AutoPlungerFire:AutoPlunger.Fire:AutoPlunger.TimerEnabled = 1:End Sub

'Sub AutoPlungerFire:PlaySoundat SoundFXDOF("fx_popper", 123, DOFPulse, DOFContactors), plunger:AutoPlunger.Fire:AutoPlunger.TimerEnabled = 1:End Sub

Sub AutoPlunger_Timer:AutoPlunger.PullBack:AutoPlunger.TimerEnabled = 0:End Sub

'*****************
'      Tilt
'*****************



Sub TiltObjects(Enabled)
    If Enabled Then
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        Bumper1.Force = 1
        Bumper2.Force = 1
        Bumper3.Force = 1
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        Bumper1.Force = 5
        Bumper2.Force = 5
        Bumper3.Force = 5
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
    End If
End Sub

'**********
' Game Over
'**********

Sub GameOver

'B2S gameover 2-10-13 added
'Random ending line


	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Red}:Game Over"
		Scorbit.SetGameMode(GameModeStrTmp)
		StopScorbit
	End If

	pDMDSplashBig "GAME OVER", 3, cGold
	GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",80,3100,0,0,0,False

Dim wiseguy  
 DOF 210, DOFOff
 Wiseguy = CInt(Int((10 * Rnd()) + 1))
        Select Case wiseguy
            Case 1:Say "saygoodnight", "251414152424231" '2-10-13 added
            Case 2:Say "I_tell_you_something_fuck_you_man", "232414239535251"
            Case 3:Say "I_told_you_not_to_fuck_with_me", "2335145336543332151"
            Case 4:Say "Now_your_fucked", "26361"
            Case 5:Say "Ok_your_starting_to_piss_me_off", "222514122315421"
            Case 6:Say "Run_while_you_can_stupid_fuck", "13131335142121431"
            Case 7:Say "You_boring_you_know_that", "153634151"
            Case 8:Say "You_just_fucked_up_man", "24251423341"
            Case 9:Say "You_need_an_army_to_take_me", "13151414141413331"
            Case 10:Say "You_picked_the_wrong_guy_to_fuck_with", "23232523233334261"
        End Select 


Dbg "HERE 1"	
    Dim tmp
    tmp = Score(1)
    If Score(2)> tmp Then tmp = Score(2)
    If Score(3)> tmp Then tmp = Score(3)
    If Score(4)> tmp Then tmp = Score(4)

Dbg "HERE 2"	
    If tmp> HighScore(1) Then 'add 1 credit for beating the highscore
        credits = credits + 1
		If Credits = 1 then
		   DOF 132, DOFOn
		End If
    End If

    If tmp> HighScore(3) Then
Dbg "HERE 3" 
        PlaySound SoundFXDOF("fx_knocker", 134, DOFPulse, DOFKnocker)
        HighScore(3) = tmp
        'enter player's name
        HighScoreEntryInit()
    Else
		Dbg "HERE 4"
        GameOver_Part2
    End If

End Sub

Sub GameOver_Part2
    Savehs
	Dbg "HERE 5 - "&nPlayer &":" &nPlayersInGame

    'GameStarted = 0
    AllLampsOff
    GiOff
    LeftFlipper.RotateToStart
    RightFlipper.RotateToStart


    StopAllMusic
	
	if nPlayer = nPlayersInGame Then
		nPlayersInGame = 0
		Dbg "GameOver 321"
		GeneralPupQueue.Add "Pupevent 321","Pupevent 321",74,3000,0,0,0,False
		GeneralPupQueue.Add "hidepage","hidepage",74,3000,0,0,0,False
		bGameOver = True
	' Game Over Video is 30 seconds
		BallHandlingQueue.Add "GameOverAttract","GameOverAttract",74,33000,0,0,0,False
	Else
		NextBall	
	End If

End Sub

sub hidepage
		if DMDType = 0 Then 
			pDMDsetHUD 0
			pDMDSetPage 88, "GameOver"
		End If
End sub

Sub GameOverAttract
	Dbg "Attract Started"
	AttractMode_On	
	GameStarted = 0
	'bGameOver = False
End Sub

Sub ClearQueues
	Dbg "Clearing Queues"
	debug.print "Clearing Queues"
	BallHandlingQueue.RemoveAll(True)
	GeneralPupQueue.RemoveAll(True)
	AudioQueue.RemoveAll(True)
End Sub

'***********************
' Ramp Helpers & Others
'***********************

Sub RHelp1_Hit()
    WireRampOff ' Exiting Wire Ramp Stop Playing Sound	
End Sub

Sub RHelp1_UnHit
	PlaySoundAt "WireRamp_Stop", RHelp1
End Sub

Sub RHelp2_Hit()
    WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub RHelp2_UnHit
	PlaySoundAt "WireRamp_Stop", RHelp2
End Sub

Sub RHelp3_Hit()
	if RotOn And RotRounds > 2 Then Exit Sub
    RotRounds = 0
    TurnTableOn
    StopSound "Wire Ramp"
	'WireRampOff ' Exiting Wire Ramp Stop Playing Sound

    SetLamp 5, 1, 2, 4
	LampFlasherOn
    LockPost.IsDropped = 0:DOF 135, DOFPulse
    If YeyoStarted Then
        StartRightRampJackpot
    End If
End Sub

'*****************
' Scores AND Bonus
'*****************

Dim Score(4), Bonus, BonusCount, BonusMultiplier, BonusHeld
Dim HighScore(4)
Dim EEHighScore
Dim HighScoreName(4)
Dim JackpotValue, Goal1, Goal2
Dim Made1(4)
Dim Made2(4)
Dim MaxPelicans

Sub AddScore(sumtoadd)
	if PuPGameRunning Then Exit Sub

    If NOT Tilted Then
		'anScore(nplayer) = anScore(nplayer) + sumtoadd*nPlayfieldX(nPlayer)
        Score(nPlayer) = Score(nPlayer) + sumtoadd*nPlayfieldX(nPlayer) ' * (xMultiplier) * SpecialMultiplier
        If EndWinEvent11Delay Then Exit Sub       'do not display the score while animating the end.

	if VRRoom > 0 Then
		PuPlayer.LabelSet pDMD,"CurrScore"," " & FormatScore(Score(nplayer)) & " ",1,"{'mt':2,'size':"& VR_Score &",'xpos':50,'ypos':87.0}"
	Elseif Score(nPlayer) > 999999999 Then 
		PuPlayer.LabelSet pDMD,"CurrScore"," " & FormatScore(Score(nplayer)) & " ",1,"{'mt':2,'size':8,'xpos':50,'ypos':87.0}"
	Elseif nPlayersinGame < 3 Then
		PuPlayer.LabelSet pDMD,"CurrScore"," " & FormatScore(Score(nplayer)) & " ",1,"{'mt':2,'size':10,'xpos':50,'ypos':88.5}"
	Else
		PuPlayer.LabelSet pDMD,"CurrScore"," " & FormatScore(Score(nplayer)) & " ",1,"{'mt':2,'size':9,'xpos':50,'ypos':87.0}"
	End If

        ' Extra Ball check
        If Score(nPlayer)> HighScore(0) Then
            GiveExtraBall()
        End If
        'If Score(nPlayer)> Goal2 Then
        '    If Made2(nPlayer) = False Then Made2(nPlayer) = True:GiveExtraBall():End If
        'End If
    End If
End Sub

Dim BonusPopCount
Sub AddBonus(sumtoadd)
	Dim tmp
	BonusPoPCount = BonusPopCount + 1
	tmp = BonusPopcount mod 12
	Debug.print "Temp:" &tmp
    If NOT Tilted Then
        Bonus = Bonus + sumtoadd
		pDMDLabelMoveVertFade "bonuspop" & BonusPopCount,sumtoadd,1000,cRed,0,18,500
		'Dbg "BONUS UP: " &Bonus
    End If
End Sub

Sub AddBonusMultiplier 'Increment the BonusMultiplier by 1
    If NOT Tilted Then
        nBonusX(nPlayer) = nBonusX(nPlayer) + 1
		if nBonusX(nPlayer) > 10 Then nBonusX(nPlayer) = 10
		DisplayBonusValue
    End If
End Sub

Sub DisplayBonusValue
	if VRRoom > 0 Then
		PuPlayer.LabelSet pDMD,"BMValue",nBonusX(nPlayer) &"X ",1,"{'mt':2,'size':"& VR_BonusSize &", 'color':" & cGold &"}"
	Else
		PuPlayer.LabelSet pDMD,"BMValue",nBonusX(nPlayer) &"X ",1,"{'mt':2, 'color':" & cGold &"}"
	End If

		
End Sub

Sub DisplayPlayfieldValue
	if VRRoom > 0 Then
		PuPlayer.LabelSet pDMD,"PFMValue",nPlayfieldX(nPlayer) &"X ",1,	"{'mt':2,'size':"& VR_BonusSize &", 'color':" & cGold &"}"
	Else
		PuPlayer.LabelSet pDMD,"PFMValue",nPlayfieldX(nPlayer) &"X ",1,"{'mt':2, 'color':" & cGold &"}"
	End If
End Sub

'************
' GI Effects
'************

Sub GiOn
	dim xx
	For each xx in GI:xx.State = 1: Next
    PlaySound "fx_relay"
	DOF 138, DOFOn
End Sub

Sub GiOff
	dim xx
	For each xx in GI:xx.State = 0: Next
    PlaySound "fx_relay"
	DOF 138, DOFOff
End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
    x = LoadValue("scarface", "HighScore1")
    If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 6000000 End If

    x = LoadValue("scarface", "HighScore1Name")
    If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "RTP" End If

    x = LoadValue("scarface", "HighScore2")
    If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 5500000 End If

    x = LoadValue("scarface", "HighScore2Name")
    If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "JOE" End If

    x = LoadValue("scarface", "HighScore3")
    If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 5000000 End If

    x = LoadValue("scarface", "HighScore3Name")
    If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "HAS" End If

    x = LoadValue("scarface", "HighScore4")
    If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 4500000 End If

    x = LoadValue("scarface", "HighScore4Name")
    If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "JPS" End If

    x = LoadValue("scarface", "credits")
    If(x <> "") then Credits = CInt(x) Else Credits = 0 End If

    x = LoadValue("scarface", "Pelicans")
    If(x <> "") then MaxPelicans = CDbl(x) Else MaxPelicans = 10 End If

    x = LoadValue("scarface", "EEScore")
    If(x <> "") then EEHighScore = CDbl(x) Else EEHighScore = 250 End If

End Sub

Sub Savehs
    SaveValue "scarface", "HighScore1", HighScore(0)
    SaveValue "scarface", "HighScore1Name", HighScoreName(0)
    SaveValue "scarface", "HighScore2", HighScore(1)
    SaveValue "scarface", "HighScore2Name", HighScoreName(1)
    SaveValue "scarface", "HighScore3", HighScore(2)
    SaveValue "scarface", "HighScore3Name", HighScoreName(2)
    SaveValue "scarface", "HighScore4", HighScore(3)
    SaveValue "scarface", "HighScore4Name", HighScoreName(3)
    SaveValue "scarface", "Credits", Credits
    SaveValue "scarface", "EEScore", EEHighScore
    SaveValue "scarface", "Pelicans", MaxPelicans

	Dbg "HERE 4B"
End Sub

Sub Reseths
    HighScoreName(0) = "RTP"
    HighScoreName(1) = "JOE"
    HighScoreName(2) = "HAS"
    HighScoreName(3) = "JPS"
    HighScore(0) = 10000000
    HighScore(1) = 9000000
    HighScore(2) = 8000000
    HighScore(3) = 7000000
	Credits = 0
	MaxPelicans = 10
	EEHighScore = 250
    Savehs
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

Sub HighScoreEntryInit()
	Dbg "In High Score"
	'debug.print "In High Score"
    hsbModeActive = True
    hsLetterFlash = 0

    hsEnteredDigits(0) = " "
    hsEnteredDigits(1) = " "
    hsEnteredDigits(2) = " "
    hsCurrentDigit = 0

    hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ'<>*+-/=\^0123456789`" ' ` is back arrow
    hsCurrentLetter = 1
    HighScoreDisplayNameNow()

    HighScoreFlashTimer.Interval = 250
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreProcessKey(keycode)
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
        if(hsCurrentLetter> len(hsValidLetters) ) then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = PlungerKey OR keycode = StartGameKey Then
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
            if(hsCurrentDigit> 0) then
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
    Dim i, TempStr

    TempStr = " >"
    if(hsCurrentDigit> 0)then TempStr = TempStr & hsEnteredDigits(0)
    if(hsCurrentDigit> 1)then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit> 2)then TempStr = TempStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3)then
        if(hsLetterFlash <> 0)then
            TempStr = TempStr & "_"
        else
            TempStr = TempStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit <1)then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit <2)then TempStr = TempStr & hsEnteredDigits(2)

    TempStr = TempStr & "< "
	pDMDHighScore "YOUR NAME:", Mid(TempStr, 2, 5), 9999,""
End Sub

Sub HighScoreFlashTimer_Timer()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = hsLetterFlash + 1
    if(hsLetterFlash = 2) then hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreCommitName()
    HighScoreFlashTimer.Enabled = False
    hsbModeActive = False
    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ") then
        hsEnteredName = "RTP"
    end if

    HighScoreName(3) = hsEnteredName
    SortHighscore
    GameOver_Part2
End Sub

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 0 to 3
        For j = 0 to 2
            If HighScore(j) <HighScore(j + 1) Then
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

Sub CheckMaxPelican
	if MaxPelicans < PuPGameScore/1000 Then MaxPelicans = PuPGameScore/1000
End Sub


Function FormatScore(ByVal Num) 'it returns a string with commas (as in Black's original font)
    dim i
    dim NumString

	if Num=0 then 
		FormatScore="0"
		Exit Function
	End if 

    NumString = CStr(abs(Num))

    For i = Len(NumString)-3 to 1 step -3
        if IsNumeric(mid(NumString, i, 1))then
            NumString = left(NumString, i) & "," & right(NumString, Len(NumString)-i)   ' ANDREW
		   'NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1)) + 48) & right(NumString, Len(NumString)- i)
        end if
    Next
    FormatScore = NumString
End function


' ********************************
'   Table info & Attract Mode
' ********************************


Dim AttractTimerCount

Sub AttractMode_On()
Dim i,Selection
		
	i = INT(RND(1) * 2)

	Select case i
		Case 0:Selection = "Merlin"
		Case 1:Selection = "JoeP"
	End Select

	'debug.print "Attract Mode On"
	pDMDSetPage pScores, "InitGame"
	pDMDsetHUD 1

	AttractTimerCount = 0
	AttractTimer.Enabled = 1
	pDMDsetPage 88, "Attract"

	LightSeqAttract.StopPlay
    SetupAttractMode()
	AudioQueue.Add "Selection",Selection,80,14000,0,0,0,False

End Sub

Sub JoeP
	StopAllMusic
	Say "JoeP", "2414131314363131312131373323"

	AudioQueue.Add "SwitchMusic","SwitchMusic sBackgroundMusic",80,5000,0,0,0,False
	End Sub

Sub Merlin
	StopAllMusic
	Say "Merlin", "332324241363131312131373323242413631313121313633232424132423"

	AudioQueue.Add "SwitchMusic","SwitchMusic sBackgroundMusic",80,8000,0,0,0,False
	End Sub

Sub AttractMode_Off()
	'debug.print "Attract Mode Off"

	pDMDsetPage pScores, "Attract"	
	AudioQueue.RemoveAll(True)
	LightSeqAttract.StopPlay
End Sub

Sub SetupAttractMode()
    LightSeqAttract.Play SeqBlinking, , 5, 150
    LightSeqAttract.UpdateInterval = 25
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
    SetupAttractMode()
End Sub

Sub LightSeqSkillshot_PlayDone()
    LightSeqSkillshot.Play SeqAllOff
End Sub

Sub LightSeqTilt_PlayDone()
    LightSeqTilt.Play SeqAllOff
End Sub

Sub LightSeqEE_PlayDone()
    EasterLights
End Sub


'*******************************************************************
'           >>>        Here starts game code     <<<
'*******************************************************************
' Any target hit sub will follow this:
' - play a sound
' - do some physical movement
' - add a score
' - check some variables/modes this trigger is a member of
' - set the "LastSwicthHit" variable
'*******************************************************************

'Variables

Dim ScoreSkillShot
Dim ScoreBumpers
Dim ScoreSuperBumpers
Dim ScoreCar
Dim ScoreOuterLoop
Dim ScoreRightRamp
Dim ScoreMiniLoop
Dim ScoreCenterHole
Dim ScoreLeftRamp
Dim ScoreBankLights
Dim ScoreDrops
Dim ScoreTWIYJackpot
Dim ScoreLock
Dim ScoreSpinnerTargets
Dim ScoreCollectYeyo
Dim ScoreME1Complete
Dim ScoreME2Part
Dim ScoreME2Complete
Dim ScoreME3Part
Dim ScoreME3Complete
Dim ScoreME4Part
Dim ScoreME4Complete
Dim ScoreME5Part
Dim ScoreME5Complete
Dim ScoreME6Part
Dim ScoreME6Complete
Dim ScoreME7Part
Dim ScoreME7Complete
Dim ScoreME8Skill
Dim ScoreME8Miss
Dim ScoreME8Complete
Dim ScoreME9PArt
Dim ScoreME9Complete
Dim ScoreME10Part
Dim ScoreME10Complete
Dim ScoreME11Jackpot
Dim ScoreME11SuperJackpot
Dim ScoreCompleteLanes
Dim ScoreHitmanJackpot
Dim ScoreHitmanJackpotIncrease
Dim ScoreCarJackpot
Dim ScoreBankRunJackpot
Dim ScoreSlingshots
Dim ScoreYeyoJackpot

Sub InitVariables
    'score variables
    ScoreSkillShot = 25000
    ScoreBumpers = 100
    ScoreSuperBumpers = 1 'this is a multiplier
    ScoreCar = 500
    ScoreOuterLoop = 1000
    ScoreRightRamp = 5000
    ScoreMiniLoop = 150
    ScoreCenterHole = 100
    ScoreLeftRamp = 2000
    ScoreBankLights = 5000
    ScoreDrops = 30
    ScoreTWIYJackpot = 500000
    ScoreLock = 2000
    ScoreSpinnerTargets = 100
    ScoreCollectYeyo = 2000
    ScoreME1Complete = 200000
    ScoreME2Part = 2500
    ScoreME2Complete = 200000
    ScoreME3Part = 2500
    ScoreME3Complete = 400000
    ScoreME4Part = 2500
    ScoreME4Complete = 400000
    ScoreME5Part = 2500
    ScoreME5Complete = 500000
    ScoreME6Part = 2500
    ScoreME6Complete = 400000
    ScoreME7Part = 2500
    ScoreME7Complete = 800000
	ScoreME8Complete = 500000
    ScoreME8Skill = 20000
    ScoreME8Miss = 0
    ScoreME9PArt = 2500
    ScoreME9Complete = 400000
    ScoreME10Part = 50000
    ScoreME10Complete = 1000000
    ScoreME11Jackpot = 200000
    ScoreME11SuperJackpot = 10000000
    ScoreCompleteLanes = 60
    ScoreHitmanJackpot = 300000
    ScoreHitmanJackpotIncrease = 10000
    ScoreCarJackpot = 500000
    ScoreBankRunJackpot = 500000
    ScoreSlingshots = 10
    ScoreYeyoJackpot = 500000

    ' other variables
    BumperHits = 0
    VukDelay = 0
    EjectBallsInDeskDelay = 0
End Sub

'************************
'       Lock hole
'************************
' kicker: lock
' lights 50, 51, 52

Dim EjectXLockedBallDelay, OpenLockBlockDelay, StartTWIYDelay

Sub OpenLockBlock
    If EventStarted = 11 or bMultiballMode Then Exit Sub
    PlaySound SoundFX("fx_solenoidon", DOFContactors)
	'SoundSaucerKick 1, LockBlock
    LockBlock.IsDropped = 1
    lockblock1.IsDropped = 0
    BallLockEnabled = 1
    UpdateLockLights
End Sub

Sub OnlyOpenLockBlock
    If EventStarted = 11 or bMultiballMode Then Exit Sub
    PlaySound SoundFX("fx_solenoidon", DOFContactors)
	'SoundSaucerKick 1, LockBlock
    LockBlock.IsDropped = 1
    lockblock1.IsDropped = 0
End Sub

Sub CloseLockBlock
    PlaySound SoundFX("fx_solenoidoff", DOFContactors)
	'SoundSaucerKick 0, LockBlock
    LockBlock.IsDropped = 0
    lockblock1.IsDropped = 1
End Sub

Sub InitLock
If bMultiballMode Then Exit Sub	
    l50.State = 0
    l51.State = 0
    l52.State = 0
    BallsInLock(nPlayer) = 0
    LockBlock.IsDropped = 0
    LockBlock1.IsDropped = 1
    BallLockEnabled = 0
    OpenLockBlockDelay = 0
End Sub

Sub Lock_Hit
    PlaySoundat "fx_hole_enter",lock
    
dbg " Lock Hit"

    Lock.Destroyball
    BallsInLock(nPlayer) = BallsInLock(nPlayer) + 1
    BallsOnPlayfield = BallsOnPlayfield - 1
    If ExtraBallIsLit Then
        GiveExtraBall
    End If
    If BallLockEnabled = 0 Then 'not in lock mode so eject back the ball and close the gate
        PlaySoundat "fx_solenoidoff",lock
        LockBlock.IsDropped = 0
        lockblock1.IsDropped = 1
        EjectXLockedBallDelay = 40
        Exit Sub
    End If
    If BallLockEnabled AND BallsInLock(nPlayer) = 1 Then '1st ball locked

		pDMDSplashBig "Ball 1 Locked", 3, cWhite
		GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",80,3100,0,0,0,False
        UpdateLockLights
        AddMultiballs 1                         'add one ball to the plunger
		Pupevent 302
		if Scorbit.bSessionActive then
			GameModeStrTmp="BL{orange}:Ball 1 Locked"
			Scorbit.SetGameMode(GameModeStrTmp)
		End If
    End If

    If BallLockEnabled AND BallsInLock(nPlayer) = 2 Then '2nd ball locked

		pDMDSplashBig "Ball 2 Locked", 3, cWhite
		GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",80,3100,0,0,0,False
        UpdateLockLights
        AddMultiballs 1                         'add one ball to the plunger
		Pupevent 302
		if Scorbit.bSessionActive then
			GameModeStrTmp="BL{orange}:Ball 2 Locked"
			Scorbit.SetGameMode(GameModeStrTmp)
		End If
    End If

    If BallLockEnabled AND BallsInLock(nPlayer) = 3 Then '3rd ball locked

		pDMDSplashBig "Multiball", 3, cWhite
		GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",80,3100,0,0,0,False
        UpdateLockLights
        BallLockEnabled = 0:CloseLockBlock	
 
		pupevent 310 ' multiball
		if Scorbit.bSessionActive then
			GameModeStrTmp="BL{orange}:Ball 3 Locked"
			Scorbit.SetGameMode(GameModeStrTmp)
		End If
		StartTWIY
        StartTWIYDelay = StartTWIYTime*20
    End If
End Sub

Sub ReloadLockLights
    Select Case BallsInLock(nPlayer)
        Case 0:l50.State = 0:l51.State = 0:l52.State = 0
        Case 1:l50.State = 1:l51.State = 0:l52.State = 0
        Case 2:l50.State = 1:l51.State = 1:l52.State = 0
        Case 3:l50.State = 1:l51.State = 1:l52.State = 1
    End Select

	if BallsInLock(nPlayer) > 0 Then
		LockBlock.IsDropped = 1
		lockblock1.IsDropped = 0
		BallLockEnabled = 1
	Else
		BallLockEnabled = 0
		CloseLockBlock
	End If
End Sub

Sub UpdateLockLights
    Select Case BallsInLock(nPlayer)
        Case 0:l50.State = 2
        Case 1:l50.State = 1:l51.State = 2
        Case 2:l50.State = 1:l51.State = 1:l52.State = 2
        Case 3:l50.State = 1:l51.State = 1:l52.State = 1
    End Select
End Sub

Sub EjectBallsLocked 'all the locked balls
    If BallsInLock(nPlayer)> 0 Then EjectLockedBallsDelay = 40
End Sub

Sub LockExit            'from the front of the desk
    Dropall "Lock Exit"        'in case they are up

	Dbg " Kicking Ball Out"
	BallhandlingQueue.Add "KickForReal","KickForReal",90,120,0,0,0,False
	BallhandlingQueue.Add "bSupressMainEvents = False","bSupressMainEvents = False",90,500,0,0,0,False
	BallhandlingQueue.Add "ResetDroptargets","ResetDroptargets",90,RDTValue,0,0,0,False
	if EE Then Exit Sub

'    ResetDropDelay = 180 'reset them right after
'	ResetDroptargets "Drop Delay"
	if EventStarted = 0 or EventStarted = 2 or EventStarted = 6 or EventStarted = 8 or EventStarted = 10 Then 
		BallhandlingQueue.Add "ResetDroptargets","ResetDroptargets",90,RDTValue,0,0,0,False
	Else
		BallhandlingQueue.Add "Door.Isdropped = 0","Door.Isdropped = 0",90,RDTValue,0,0,0,False
	End If
	SafetyCounter = 0
	bSupressSafety = 1

	
End Sub

Sub KickForReal
	bSupressMainEvents = True
    Deskin.createball
    Deskin.Kick 188 + RND(3) * 3, 20 + RND(1) * 3
	BallhandlingQueue.Add "bSupressSafety = 0","bSupressSafety = 0",90,200,0,0,0,False
	BallhandlingQueue.Add "CheckDoor","CheckDoor",90,RDTValue,0,0,0,False

End Sub

Sub CheckDoor
if EventStarted <> 0 Then Door.Isdropped = 0
End Sub
'******************
' Helpers & effects
'******************

Dim LaneGateDelay

Sub LaneGateT_Hit
    LaneBlock.IsDropped = 1
	DOF 125, DOFPulse
    LaneGateDelay = 6
	HidePupSplashMessages
	bOnTheFirstBall = False
	'if Not EventStarted = 8 Then Pupevent 304
End Sub

Sub LaneGateClose
    LaneBlock.IsDropped = 0
End Sub

'*************
' Holes & Vuks
'*************

Dim PlungerLaneKDelay, BallsInPlungerLane
Dim CarHoleDelay

Sub plungerlanein_Hit
	WireRampOff
    If EventStarted = 8 Then
        
        BallsOnPlayfield = BallsOnPlayfield - 1
        plungerlanein.destroyball
        PlaySoundat "fx_kicker_enter", plungerlanein
        NewBall8
        Exit Sub
    End If

    If SkillshotReady Then
        StartBallSaved(nBallSavedTime) 'start ball save time if 1st ball
    End If
    PlaySoundat "fx_kicker_enter", plungerlanein
    If SkillshotStarted Then PlaySound "car_not_start":EndSkillshot
    BallsInPlungerLane = BallsInPlungerLane + 1
    
    plungerlanein.destroyball 'RTP
    PlungerLaneKDelay = 20
End Sub

Sub PlungerLaneExit
    If ExtraBallislit OR BallLockEnabled Then 'close the diverter
        CloseLockBlock
        OpenLockBlockDelay = 40
    End If
    plungerlaneout.createball 'RTP
    plungerlaneout.kick 0, 23 + Int(RND(1) * 8)
    PlaySoundat SoundFXDOF("fx_kicker", 124, DOFPulse, DOFContactors), plungerlaneout
End Sub

Sub Vukin_Hit
    PlaySoundat "fx_kicker_enter", vukin
    VukDelay = 30
End Sub

Sub VukEject
    vukin.destroyball
    vukout.createball
	if EventStarted = 5 Then
		vukout.kick 270, 8
	Else
		RotRounds = 0
		vukout.kick 90, 25.25, 1.28
		TurnTableOn
	End If

    PlaySoundat SoundFXDOF("fx_popper", 118, DOFPulse, DOFContactors), vukout
End Sub

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	pDMDSetPage pScores, "DT"
	RS.VelocityCorrect(ActiveBall)
	'PlaySoundat SoundFXDOF("right_slingshot", 104, DOFPulse, DOFContactors) , sling1
	RandomSoundSlingshotRight sling1
	FireBackglass
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1

    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If
    Addscore ScoreSlingshots
    LastSwitchHit = "rightslingshot"
    'modes
    if EventStarted = 0 Then LightNextEvent

End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	pDMDSetPage pScores, "DT"
	LS.VelocityCorrect(ActiveBall)
    'PlaySoundat SoundFXDOF("left_slingshot", 103, DOFPulse, DOFContactors), sling2
	RandomSoundSlingshotLeft sling2
	FireBackglass
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1

    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If
    Addscore ScoreSlingshots
    LastSwitchHit = "leftslingshot"
    'modes
    if EventStarted = 0 Then LightNextEvent

End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

'**********
' Bumpers
'**********

Dim BumperHits, bumperoffdelay
 
Sub Bumper1_Hit
    'PlaySoundat SoundFXDOF("fx_bumper", 105, DOFPulse, DOFContactors), bumper1 
	RandomSoundBumperTop bumper1
	FireBackglass
	Bumper1.TimerEnabled = 1
	DOF 214, DOFPulse
    'score & bonus
    'checkmodes this switch is part of	
    If EE Then
        PlayRandomPunch
        AddScore 100
        Exit Sub
    End If
    If EventStarted = 5 Then
        PlaySound "drums1"
		nMainProgress = nMainProgress + 1
		UpdateDMDMainProgress
        CheckEvent5Bumps
    Else
		'PlaySoundAt"fx_bumper1", Bumper1
		FlBumperFadeTarget(1) = 1       
		Bumper1.timerenabled = True
        'SetLamp 2, 1, 0, 0
        BumperHits = BumperHits + 1
        AddScore ScoreBumpers * ScoreSuperBumpers
        CheckSuperPops
		'Dbg "Bumper 1 HIT"
    End If
    LastSwitchHit = "bumper1"
End Sub

Sub Bumper1_Timer()
	'SetLamp 2, 0, 0, 0
	 FlBumperFadeTarget(1) = 0
	l48.State = 0:
	Bumper1.TimerEnabled = 0
End Sub

Sub Bumper2_Hit
    'PlaySoundat SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), bumper2
	RandomSoundBumperMiddle bumper2
	FireBackglass
	Bumper2.TimerEnabled = 1
	DOF 214, DOFPulse
    'score & bonus
    'checkmodes this switch is part of	
    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If
    If EventStarted = 5 Then
        PlaySound "drums2"
		nMainProgress = nMainProgress + 1
		UpdateDMDMainProgress
        CheckEvent5Bumps
    Else
        'SetLamp 2, 1, 0, 0
		FlBumperFadeTarget(2) = 1        
		Bumper2.timerenabled = True
        BumperHits = BumperHits + 1
        AddScore ScoreBumpers * ScoreSuperBumpers
        CheckSuperPops
		'Dbg "Bumper 2 HIT"
    End If
    LastSwitchHit = "bumper2"
End Sub

Sub Bumper2_Timer()
	'SetLamp 2, 0, 0, 0
	 FlBumperFadeTarget(2) = 0
	l48.State = 0:
	Bumper2.TimerEnabled = 0
End Sub
 
Sub Bumper3_Hit
    'PlaySoundat SoundFXDOF("fx_bumper", 106, DOFPulse, DOFContactors), bumper3
	RandomSoundBumperBottom bumper3
	FireBackglass
	Bumper3.TimerEnabled = 1
	DOF 214, DOFPulse
    'score & bonus
    'checkmodes this switch is part of	
    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If
    If EventStarted = 5 Then
        PlaySound "drums2"
		nMainProgress = nMainProgress + 1
		UpdateDMDMainProgress
        CheckEvent5Bumps
    Else
        'SetLamp 2, 1, 0, 0
		FlBumperFadeTarget(3) = 1      
		Bumper3.timerenabled = True
        BumperHits = BumperHits + 1
        AddScore ScoreBumpers * ScoreSuperBumpers
        CheckSuperPops
		'Dbg "Bumper 3 HIT"
    End If
    LastSwitchHit = "bumper3"
End Sub

Sub Bumper3_Timer()
	'SetLamp 2, 0, 0, 0
	FlBumperFadeTarget(3) = 0
	l48.State = 0:
	Bumper3.TimerEnabled = 0
End Sub

Sub CheckSuperPops
    Dim tmp
    If BumperHits <100 Then
        tmp = INT(RND(1) * 3)
        Select case tmp
            Case 0:PlaySound "punch1"
            Case 1:PlaySound "punch1"
            Case 2:PlaySound "punch1"
        End Select
    Else
    End If

    If BumperHits = 100 Then
        bumpersmalllight1.State = 2
        bumpersmalllight2.State = 2
        bumpersmalllight3.State = 2
        bumperbiglight1.State = 2
        bumperbiglight2.State = 2
        bumperbiglight3.State = 2
        ScoreSuperBumpers = 100  ' multiplier
        bumperoffdelay = 60 * 20 '20 because the gametimer is 50. 1000 is 1 second
    End If
End Sub

Sub InitBumpers
        bumpersmalllight1.State = 1
        bumpersmalllight2.State = 1
        bumpersmalllight3.State = 1
        bumperbiglight1.State = 1
        bumperbiglight2.State = 1
        bumperbiglight3.State = 1
    ScoreSuperBumpers = 1 ' multiplier
    bumperoffdelay = 0
End Sub

'***********
' Rollovers
'***********

Sub lat1_Hit

	if (ballsaved = 1 And BallSavedDelay < 4) Then BallSavedDelay = BallSavedDelay + 3  ' extend ball save timer

	PlaySoundat "fx_sensor", lat1
    DOF 128, DOFOn
    'score & bonus
    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If
    If l7.State = 1 Then
        PlaySound "scpoing4"
    Else
        PlaySound "reload4"
    End If
    AddScore 10 '???
    l7.State = 1
    'checkmodes this switch is part of
    LastSwitchHit = "lat1"
    CheckFlipperLights
End Sub
 
Sub lat1_UnHit
	DOF 128, DOFOff
End Sub
 
Sub lat2_Hit
	PlaySoundat "fx_sensor", lat2
    DOF 129, DOFOn 
    'score & bonus
    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If
    If l7.State = 1 Then
        PlaySound "scpoing4"
    Else
        PlaySound "reload4"
    End If
    AddScore 10 '???
    l8.State = 1
    'checkmodes this switch is part of
    LastSwitchHit = "lat2"
    CheckFlipperLights
End Sub
 
Sub lat2_UnHit
	DOF 129, DOFOff
End Sub
 
Sub lat3_Hit
	PlaySoundat "fx_sensor", lat3
    DOF 130, DOFOn
    'score & bonus
    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If
    If l7.State = 1 Then
        PlaySound "scpoing4"
    Else
        PlaySound "reload4"
    End If
    AddScore 10 '???
    l9.State = 1
    'checkmodes this switch is part of
    LastSwitchHit = "lat3"
    CheckFlipperLights
End Sub
 
Sub lat3_UnHit
	DOF 130, DOFOff
End Sub
 
Sub lat4_Hit

	if (ballsaved = 1 And BallSavedDelay < 4) Then BallSavedDelay = BallSavedDelay + 3  ' extend ball save timer

	PlaySoundat "fx_sensor", lat4
    DOF 131, DOFOn
    'score & bonus
    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If
    If l7.State = 1 Then
        PlaySound "scpoing4"
    Else
        PlaySound "reload4"
    End If
    AddScore 10 '???
    l10.State = 1
    'checkmodes this switch is part of
    LastSwitchHit = "lat4"
    CheckFlipperLights
End Sub
 
Sub lat4_UnHit
	DOF 131, DOFOff
End Sub
 
Sub lat5_Hit
    PlaySoundat "fx_sensor", lat5
	DOF 203, DOFOn
    'score & bonus
    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If
    PlaySound "scpoing10"
    'checkmodes this switch is part of
    If EventStarted = 10 And Event10Hits(1) = 0 And LastSwitchHit <> "lat8" Then
        Event10Hits(1) = 1
        l16.State = 0
        CheckEvent10
    Else
        AddScore 10 '???
    End If
    LastSwitchHit = "lat5"
End Sub
 
Sub lat5_UnHit
	DOF 203, DOFOff
End Sub
 
Sub lat6_Hit
    PlaySoundat "fx_sensor", lat6
	DOF 201, DOFOn
    'score & bonus
    'checkmodes this switch is part of
    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If
	
	if LastSwitchHit = "lat7" Then MiniGameLoops(nPlayer,0) = 1:CheckMiniGamePrep
	
    LastSwitchHit = "lat6"
End Sub
 
Sub lat6_UnHit
	DOF 201, DOFOff
End Sub
 
Sub lat7_Hit
    PlaySoundat "fx_sensor" , lat7
	DOF 202, DOFOn
    'score & bonus
    'checkmodes this switch is part of
    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If

	if LastSwitchHit = "lat6" Then MiniGameLoops(nPlayer,1) = 1:CheckMiniGamePrep 

    LastSwitchHit = "lat7"
End Sub

Sub CheckMiniGamePrep
	if bMiniGameActive = False or bMiniGamePlayed Then Exit Sub
	if 	MiniGameLoops(nPlayer,0) And MiniGameLoops(nPlayer,1) Then
		BackDoorPost.isDropped = 0
		if bMiniGameAllowed = False Then
			'pDMDSplashBig "Mini-Game Available", 3, cOrange
			PuPlayer.LabelSet pDMD, "SplashMG2a", "Mini-Game Available", 1, ""
			PuPlayer.LabelSet pDMD, "SplashMG2b", "Shoot Behind The Desk", 1, ""
			GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",80,3100,0,0,0,False
		End If
		bMiniGameAllowed = True
		FlamingoTimer.Enabled = 1
	End If
End Sub
 
Sub lat7_UnHit
	DOF 202, DOFOff
End Sub
 
Sub lat8_Hit
    PlaySoundat "fx_sensor", lat8
	DOF 204, DOFOn
    'score & bonus
    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If
    PlaySound "Ba_Dum_Tss1"
    'checkmodes this switch is part of
    If EventStarted = 10 And Event10Hits(2) = 0 And LastSwitchHit <> "lat5" Then
        Event10Hits(2) = 1
        l53.State = 0
        CheckEvent10
    Else
        AddScore 10 '???
    End If
    LastSwitchHit = "lat8"
End Sub
 
Sub lat8_UnHit
	DOF 204, DOFOff
End Sub

Sub RightRampTrigger_Hit()
	WireRampOn True	 'Play Plastic Ramp Sound
End Sub

Sub LeftRampTrigger_Hit()
	WireRampOn True	 'Play Plastic Ramp Sound	
End Sub
 
Sub LeftRampDone_Hit

	bBackLightsOn = True
	BallHandlingQueue.Add "bBackLightsOn = False","bBackLightsOn = False",60,1000,0,0,0,False
	SetLamp 3, 0, 1, 10
    'PlaySoundat "Wire Ramp",LeftRampDone:
	WireRampOff
	WireRampOn False ' On Wire Ramp Pay Wire Ramp Sound

	DOF 120, DOFPulse
    'score & bonus
    'checkmodes this switch is part of
    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If
    If EventStarted = 10 And Event10Hits(3) = 0 Then
        AddScore ScoreLeftRamp
        Event10Hits(3) = 1
        l23.State = 0
        CheckEvent10
        Exit Sub
    End If
    If EventStarted = 6 AND Event6Drop Then
        AddScore ScoreLeftRamp
		nMainProgress = nMainProgress + 1
		UpdateDMDMainProgress
        WinEvent6
        Exit Sub
    End If
    If EventStarted = 1 Then
        AddScore ScoreLeftRamp
		nMainProgress = nMainProgress + 1
		UpdateDMDMainProgress
        WinEvent1
        Exit Sub
    End If
    If TWIYStarted AND LeftRampJackpotEnabled Then
        If RightRampJackpotEnabled = 0 Then
            l25.State = 2
            l42.State = 2
        End If
        StopLeftRampJackpot
        GiveTWIYJackpot
        Exit Sub
    End If
    If HitManStarted Then
        AddScore ScoreHitmanJackpot
		WinSideEvent
        PlaySound "chananJPk"
    Else
        AddScore ScoreLeftRamp
        PlaySound "gun2"
        KillRampHits = KillRampHits + 1
        CheckKillRamp
    End if
    LastSwitchHit = "leftrampdone"
End Sub
 
Sub RightRampDone_Hit
	LampFlasherOff
	WireRampOff
	WireRampOn False ' On Wire Ramp Pay Wire Ramp Sound
    TurnTableOff 'turn off the rotating platform
    'checkmodes this switch is part of
    LastSwitchHit = "RightRampDone"
End Sub

'***********
' Targets
'***********

Sub rott1_Hit
    STHit 1
    ' score
    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If
    AddScore ScoreSpinnerTargets
    PlaySound "scpoing9"
    addbonus Bonus_Spinners
    'checkmodes this switch is part of
    LastSwitchHit = "rott1"
    If EventStarted = 4 Then
		nMainProgress = nMainProgress + 1
		UpdateDMDMainProgress
        CheckrottHits
    Else
        CheckYeyo
    End If
End Sub

Sub rott1o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub rott2_Hit
    STHit 2
    ' score
    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If
    AddScore ScoreSpinnerTargets
    PlaySound "scpoing9"
    addbonus Bonus_Spinners
    'checkmodes this switch is part of
    LastSwitchHit = "rott2"
    If EventStarted = 4 Then
		nMainProgress = nMainProgress + 1
		UpdateDMDMainProgress
        CheckrottHits
    Else
        CheckYeyo
    End If
End Sub

Sub rott2o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub rott3_Hit
    STHit 3
    ' score
    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If
    AddScore ScoreSpinnerTargets
    PlaySound "scpoing9"
    addbonus Bonus_Spinners
    'checkmodes this switch is part of
    LastSwitchHit = "rott3"
    If EventStarted = 4 Then
		nMainProgress = nMainProgress + 1
		UpdateDMDMainProgress
        CheckrottHits
    Else
        CheckYeyo
    End If
End Sub

Sub rott3o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub rott4_Hit
	STHit 4
    ' score
    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If
    AddScore ScoreSpinnerTargets
    PlaySound "scpoing9"
    addbonus Bonus_Spinners
    'checkmodes this switch is part of
    LastSwitchHit = "rott4"
    If EventStarted = 4 Then
		nMainProgress = nMainProgress + 1
		UpdateDMDMainProgress
        CheckrottHits
    Else
        CheckYeyo
    End If
End Sub

Sub rott4o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub rott5_Hit
	STHit 5
    ' score
    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If
    AddScore ScoreSpinnerTargets
    PlaySound "scpoing9"
    addbonus Bonus_Spinners
    'checkmodes this switch is part of
    LastSwitchHit = "rott5"
    If EventStarted = 4 Then
		nMainProgress = nMainProgress + 1
		UpdateDMDMainProgress
        CheckrottHits
    Else
        CheckYeyo
    End If
End Sub

Sub rott5o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub rott6_Hit
	STHit 6
    ' score
    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If
    AddScore ScoreSpinnerTargets
    PlaySound "scpoing9"
    addbonus Bonus_Spinners
    'checkmodes this switch is part of
    LastSwitchHit = "rott6"
    If EventStarted = 4 Then
		nMainProgress = nMainProgress + 1
		UpdateDMDMainProgress
        CheckrottHits
    Else
        CheckYeyo
    End If
End Sub

Sub rott6o_Hit
	TargetBouncer ActiveBall, 1
End Sub

Sub rott7_Hit
	STHit 7
    ' score
    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If
    AddScore ScoreSpinnerTargets
    PlaySound "scpoing9"
    addbonus Bonus_Spinners
    'checkmodes this switch is part of
    LastSwitchHit = "rott7"
    If EventStarted = 4 Then
		nMainProgress = nMainProgress + 1
		UpdateDMDMainProgress
        CheckrottHits
    Else
        CheckYeyo
    End If
End Sub

Sub rott7o_Hit
	TargetBouncer ActiveBall, 1
End Sub

'*****************************************************************************
'Bank Targets 
'*****************************************************************************

'lt1a.Isdropped = 1:lt2a.Isdropped = 1:lt3a.Isdropped = 1:lt4a.Isdropped = 1

Sub lt1_Hit
	DTHit 4
End Sub

Sub lt1_Timer
	RandomSoundDropTargetReset lt1p
	DTRaise 4
	lt1.timerenabled = 0
End Sub

Sub lt2_Hit
	DTHit 5
End Sub

Sub lt2_Timer
	RandomSoundDropTargetReset lt2p
	DTRaise 5
	lt2.timerenabled = 0:End Sub

Sub lt3_Hit
	DTHit 6
End Sub

Sub lt3_Timer:
	RandomSoundDropTargetReset lt3p
	DTRaise 6
	lt3.timerenabled = 0
End Sub

Sub lt4_Hit
	DTHit 7
End Sub

Sub lt4_Timer
	RandomSoundDropTargetReset lt4p
	DTRaise 7
	lt4.timerenabled = 0
End Sub

Sub RaiseBankTargets
	RandomSoundDropTargetReset lt1p
	DTRaise 4
	l11.State = 0
	BankTargets(0) = 0
    BankTargets(1) = 0


	'RandomSoundDropTargetReset lt2p
	DTRaise 5
	l12.State = 0
    BankTargets(2) = 0


	'RandomSoundDropTargetReset lt3p
	DTRaise 6
	l13.State = 0
    BankTargets(3) = 0


	'RandomSoundDropTargetReset lt4p
	DTRaise 7
	l14.State = 0
    BankTargets(4) = 0
End Sub

'*************
' Car Side
'*************

Sub rt1_Hit
    PlaySoundat SoundFX("fx_target",DOFContactors), CarHole
    SetLamp 4, 0, 1, 8
    ' score
    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If
    If EventStarted = 10 And Event10Hits(5) = 0 Then
        AddScore ScoreCar
        Event10Hits(5) = 1
        l17.State = 0
        CheckEvent10
        Exit Sub
    End If
    If GetCarStarted Then
        CarHits = 0
        setlamp 1, 0, 1, 10
		Flash1 True
		BallHandlingQueue.Add "Flash1 False","Flash1 False",60,2000,0,0,0,False
        AddScore ScoreCarJackpot
		WinSideEvent
        LightSeqWinEvent.Play SeqRandom, 60, , 1000
        EndGetCar
        Exit Sub
    End If
    'checkmodes this switch is part of
    AddScore ScoreCar
    LastSwitchHit = "rt1"
    CarHits = CarHits + 1
	if EventStarted = 9 Then 
		nMainProgress = nMainProgress + 1
		UpdateDMDMainProgress
	End If
    PlaySound "car_acell":DOF 126, DOFPulse
    CheckCarHits
	addbonus Bonus_Car
End Sub

'*****************************************************************************
'Drop Targets  Center
'*****************************************************************************

Dim DroppedTargets(3), CurrentCT, CTDelay
Dim ResetDropDelay


Sub ResetDroptargets


if bSkipTargetReset And DroppedTargets(1) And DroppedTargets(2) And DroppedTargets(3) Then debug.print "Skipping Target Reset": Exit Sub

    Door.Isdropped = 0
    CurrentCT = 0
    If EventStarted = 0 Then
        CTDelay = 91
        l38.State = 0
        l39.State = 0
        l40.State = 0
        l38b.State = 0
        l39b.State = 0
        l40b.State = 0
        l37.State = 0
    End If


Dbg " DROP 2 "
	RandomSoundDropTargetReset ct2p
	DTRaise 1
	DTRaise 2
	DTRaise 3
    DroppedTargets(1) = 0
    DroppedTargets(2) = 0
    DroppedTargets(3) = 0

    ResetDropDelay = 0
	bSkipTargetReset = False
End Sub

Sub DropAll(CallerName)

Dbg " DROP ALL TARGETS " & CallerName
    'PlaySoundat SoundFX("fx_droptarget", DOFcontactors), DeskIn
	DOF 112, DOFPulse

	If DroppedTargets(1) = 0 Then
		DTDrop 1
		SoundDropTargetDrop ct1p
		DroppedTargets(1) = 1
	End If
	If DroppedTargets(2) = 0 Then
		DTDrop 2
		SoundDropTargetDrop ct1p
		DroppedTargets(2) = 1
	End If
	If DroppedTargets(3) = 0 Then
		DTDrop 3
		SoundDropTargetDrop ct1p
		DroppedTargets(3) = 1
	End If
    Door.IsDropped = 1
    CTDelay = 0
    If EventStarted = 0 Then
        l38.State = 1
        l39.State = 1
        l40.State = 1
        l38b.State = 1
        l39b.State = 1
        l40b.State = 1
        l37.State = 2
    End If
End Sub

Sub ForceDropAll(CallerName)

Dbg " DROP ALL TARGETS " & CallerName
    'PlaySoundat SoundFX("fx_droptarget", DOFcontactors), DeskIn
	DOF 112, DOFPulse


		DTDrop 1
		SoundDropTargetDrop ct1p
		DroppedTargets(1) = 1


		DTDrop 2
		SoundDropTargetDrop ct1p
		DroppedTargets(2) = 1


		DTDrop 3
		SoundDropTargetDrop ct1p
		DroppedTargets(3) = 1

    Door.IsDropped = 1
    CTDelay = 0
    If EventStarted = 0 Then
        l38.State = 1
        l39.State = 1
        l40.State = 1
        l38b.State = 1
        l39b.State = 1
        l40b.State = 1
        l37.State = 2
    End If
End Sub

Sub Door_Hit:PlaySoundat "fx_metalhit", DeskIn:DOF 117, DOFPulse:End Sub


Sub ct1_Hit
	pDMDSetPage pScores, "DT"
	if bSupressMainEvents Then Exit Sub
    DTHit 1
End Sub

Sub ct1_Timer
    ct1.TimerEnabled = 0
	if bSupressMainEvents Then Exit Sub
    RandomSoundDropTargetReset ct1p
	DTRaise 1
	DroppedTargets(1) = 0
End Sub

Sub ct2_Hit
	pDMDSetPage pScores, "DT"
	if bSupressMainEvents Then Exit Sub
    DTHit 2
End Sub

Sub ct2_Timer
    ct2.TimerEnabled = 0
	if bSupressMainEvents Then Exit Sub
    RandomSoundDropTargetReset ct2p
	DTRaise 2
	DroppedTargets(2) = 0
End Sub

Sub ct3_Hit
	pDMDSetPage pScores, "DT"
	if bSupressMainEvents Then Exit Sub
    DTHit 3
End Sub

Sub ct3_Timer
    ct3.TimerEnabled = 0
	if bSupressMainEvents Then Exit Sub
    RandomSoundDropTargetReset ct3p
	DTRaise 3
	DroppedTargets(3) = 0
End Sub

Sub CheckDropTargets(Opt)
	Dbg "Check Drop Targets " & opt
    DroppedTargets(0) = DroppedTargets(1) + DroppedTargets(2) + DroppedTargets(3)
    If DroppedTargets(0) = 3 Then DropAll "CheckDropTargets"
End Sub

Sub CheckDropTargetsBackdoor
	DroppedTargets(0) = 0
    DroppedTargets(0) = DroppedTargets(1) + DroppedTargets(2) + DroppedTargets(3)
	'Debug.print "DT(0): " &DroppedTargets(0)
    If DroppedTargets(0) = 3 Then bSkipTargetReset = True
End Sub

'****************************
' Rotate lights with flippers
'****************************
' lights are l7, l8, l9 and l10

Dim LaneLightsTimes, LaneLightsGoal

Sub InitLaneLights
    LaneLightsTimes = 0
    LaneLightsGoal = 10
End Sub

Sub RotateLightsLeft
    Dim tmp
    tmp = l7.state
    l7.state = l8.state
    l8.state = l9.state
    l9.state = l10.state
    l10.state = tmp
End Sub

Sub RotateLightsRight
    Dim tmp
    tmp = l10.state
    l10.state = l9.state
    l9.state = l8.state
    l8.state = l7.state
    l7.state = tmp
End Sub

Sub CheckFlipperLights
    If l7.State = 1 AND l8.State = 1 AND l9.State = 1 AND l10.State = 1 Then
        LightSeqFlipper.Play SeqBlinking, , 5, 40
        PlaySound "reload5"
        l7.state = 0
        l8.state = 0
        l9.state = 0
        l10.state = 0
        LaneLightsTimes = LaneLightsTimes + 1


		pDMDSplash3Lines  "RESET LANE LIGHTS", LaneLightsGoal-LaneLightsTimes &" MORE TIMES", "LIGHTS EXTRABALL", 3, cWhite
		GeneralPuPQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",40,3100,0,0,0,False

        If LaneLightsTimes = LaneLightsGoal Then 'turn on Extraball Light
            LaneLightsGoal = 40                  ' you need 40 for the next extra ball
            LitExtraBall
        End If
    End If
End Sub

'************
' Turn Table
'************

Sub LampFlashersOn
	Flasher5a.Visible = 1
	Flasher5b.Visible = 1
	Flasher5c.Visible = 1
	Flasher5d.Visible = 1
	Flasher5e.Visible = 1
End Sub

Sub LampFlashersOff
	Flasher5a.Visible = 0
	Flasher5b.Visible = 0
	Flasher5c.Visible = 0
	Flasher5d.Visible = 0
	Flasher5e.Visible = 0
End Sub

Dim RotSpeed, RotDir, RotOn, RotRounds

Sub TurnHelp_Hit
    RotRounds = RotRounds + 1
    If RotOn = 0 Then TurnTableOn
    If RotRounds> 3 Then LockPost.IsDropped = 1:DOF 135, DOFPulse
    TurnHelp.kick 275, 15 + RND(1) * 5
End Sub

Sub TurnHelp1_Hit
    If RotOn = 0 Then
        TurnTableOn
    End If
    TurnHelp1.kick 330, 15 + RND(1) * 5
End Sub

Sub TurnHelp2_Hit
    RotRounds = RotRounds + 1
    If RotOn = 0 Then TurnTableOn
    If RotRounds> 4 Then 
		TurnHelp2.kick 100, 12
	Else
		TurnHelp2.kick 275, 15 + RND(1) * 5
	End If
End Sub

Sub TurnHelp3_Hit
    RotRounds = RotRounds + 1
    If RotOn = 0 Then TurnTableOn
    If RotRounds> 4 Then LockPost.IsDropped = 1:DOF 135, DOFPulse
    TurnHelp3.kick 300, 15 + RND(1) * 5
End Sub

Sub TurnHelp4_Hit
    RotRounds = RotRounds + 1
    If RotOn = 0 Then TurnTableOn
    If RotRounds> 3 Then LockPost.IsDropped = 1:DOF 135, DOFPulse
    TurnHelp4.kick 275, 15 + RND(1) * 5
End Sub

Sub TurnTableON
    RotOn = 1
	LampFlashersOn
    SetLamp 5, 0, 2, 0
    RotSpeed = 0
    RotDir = 1
    Rotation.Enabled = 1:DOF 122, DOFOn
End Sub

Sub TurnTableOff
    RotOn = 0
	LampFlashersOff
    SetLamp 5, 0, 0, 0
    RotDir = -1
End Sub

Sub Rotation_Timer
    RotSpeed = RotSpeed + RotDir
    If RotSpeed >= 35 Then RotSpeed = 35
    If RotSpeed <= 0 Then
       Rotation.Enabled = 0:DOF 122, DOFOff       
    End If
    RotatingPlatform.RotAndTra2 = RotatingPlatform.RotAndTra2 + RotSpeed
End Sub

'**************
' Head Tracking
'**************

Dim HeadPos, OldHeadPos
Dim StartY, FinalY
HeadPos = 3:UpdateHead

Sub UpdateHead
    If ShootingBalls or bHeadMoving then Exit Sub
	Select Case HeadPos
        Case 1:
			Select Case OldHeadPos
				Case 1:
					StartY = -56 : FinalY = -56 : bHeadMoving = False ': HeadTimer.Enabled = 1
				Case 2:
					StartY = -32 : FinalY = -56 : bHeadMoving = True :HeadCountDownTimer.Enabled = 1			
				Case 3:
					StartY = -4 : FinalY = -56 : bHeadMoving = True :HeadCountDownTimer.Enabled = 1
				Case 4:
					StartY = 32 : FinalY = -56 : bHeadMoving = True :HeadCountDownTimer.Enabled = 1
				Case 5:
					StartY = 56 : FinalY = -56 : bHeadMoving = True :HeadCountDownTimer.Enabled = 1
			End Select
			OldHeadPos = 1
        Case 2:
			Select Case OldHeadPos
				Case 1:
					StartY = -56 : FinalY = -32 : bHeadMoving =  True:HeadCountUpTimer.Enabled = 1
				Case 2:
					StartY = -32 : FinalY = -32 : bHeadMoving = False ':HeadCountDownTimer.Enabled = 1			
				Case 3:
					StartY = -4 : FinalY = -32 : bHeadMoving = True :HeadCountDownTimer.Enabled = 1
				Case 4:
					StartY = 32 : FinalY = -32 : bHeadMoving = True :HeadCountDownTimer.Enabled = 1
				Case 5:
					StartY = 56 : FinalY = -32 : bHeadMoving = True :HeadCountDownTimer.Enabled = 1
			End Select
			OldHeadPos = 2
        Case 3:
			Select Case OldHeadPos
				Case 1:
					StartY = -56 : FinalY = -4 : bHeadMoving = True :HeadCountUpTimer.Enabled = 1
				Case 2:
					StartY = -32 : FinalY = -4 : bHeadMoving = True :HeadCountUpTimer.Enabled = 1			
				Case 3:
					StartY = -4 : FinalY = -4 : bHeadMoving = False ':HeadTimer.Enabled = 1
				Case 4:
					StartY = 32 : FinalY = -4 : bHeadMoving = True :HeadCountDownTimer.Enabled = 1
				Case 5:
					StartY = 56 : FinalY = -4 : bHeadMoving = True :HeadCountDownTimer.Enabled = 1
			End Select
			OldHeadPos = 3
        Case 4:
			Select Case OldHeadPos
				Case 1:
					StartY = -56 : FinalY = 32 : bHeadMoving = True :HeadCountUpTimer.Enabled = 1
				Case 2:
					StartY = -32 : FinalY = 32 : bHeadMoving = True :HeadCountUpTimer.Enabled = 1			
				Case 3:
					StartY = -4 : FinalY = 32 : bHeadMoving = True :HeadCountUpTimer.Enabled = 1
				Case 4:
					StartY = 32 : FinalY = 32 : bHeadMoving = False' :HeadTimer.Enabled = 1
				Case 5:
					StartY = 56 : FinalY = 32 : bHeadMoving = True :HeadCountDownTimer.Enabled = 1
			End Select
			OldHeadPos = 4
        Case 5:
			Select Case OldHeadPos
				Case 1:
					StartY = -56 : FinalY = 56 : bHeadMoving = True : HeadCountUpTimer.Enabled = 1
				Case 2:
					StartY = -32 : FinalY = 56 : bHeadMoving = True : HeadCountUpTimer.Enabled = 1			
				Case 3:
					StartY = -4 : FinalY = 56 : bHeadMoving = True : HeadCountUpTimer.Enabled = 1
				Case 4:
					StartY = 32 : FinalY = 56 : bHeadMoving = True :HeadCountUpTimer.Enabled = 1
				Case 5:
					StartY = 56 : FinalY = 56 : bHeadMoving = False ': HeadTimer.Enabled = 1
			End Select
			OldHeadPos = 5
    End Select
End Sub

Sub Head1_Hit:HeadPos = 1:UpdateHead:End Sub
Sub Head2_Hit:HeadPos = 2:UpdateHead:End Sub
Sub Head3_Hit:HeadPos = 3:UpdateHead:End Sub
Sub Head4_Hit:HeadPos = 4:UpdateHead:End Sub
Sub Head5_Hit:HeadPos = 5:UpdateHead:End Sub

'***********************
'Mouth talking animation
'***********************

Dim MouthPos, MouthReps, MouthIntervals, Voice
MouthPos = 0
MouthReps = 0
MouthIntervals = ""
Voice = ""

Sub Say(sound, interval)
'	Dbg "Started Say:" &sound
    StopSound Voice

    MouthPos = 0

    Voice = sound
    MouthReps = LEN(interval)
    MouthIntervals = interval

'Dbg "Reps: " &MouthReps

    If ShootingBalls = 0 Then
        MouthTimer.Enabled = 1
'		Dbg "Mouth On"
    End If

	Playsound Voice,1,fCalloutVolume

'	dbg "Interval: " &MouthIntervals
End Sub

Sub HeadCountUpTimer_Timer()
	if StartY = FinalY Then bHeadMoving = False : HeadCountUpTimer.Enabled = 0 : Exit Sub
	StartY = StartY + 4
	Head.rotY = StartY
	Mouth.rotY = StartY
	if StartY < -32 Then
		GunUp.rotY = -32
	Elseif StartY > 32 Then
		GunUp.rotY = 32
	Else
		GunUp.rotY = StartY
	End If
End Sub

Sub HeadCountDownTimer_Timer()
	if StartY = FinalY Then bHeadMoving = False : HeadCountDownTimer.Enabled = 0 :Exit Sub
	StartY = StartY - 4
	Head.rotY = StartY
	Mouth.rotY = StartY
	if StartY > 32 Then
		GunUp.rotY = 32
	Elseif StartY < -32 Then
		GunUp.rotY = -32
	Else
		GunUp.rotY = StartY
	End If
End Sub


dim nMouth : nMouth = 49
dim CalVal

Sub MouthTimer_Timer
    Dim tmp, tmpinterval
    tmp = HeadPos + MouthPos*5
	CalVal = ABS(MID(MouthIntervals, Len(MouthIntervals) - MouthReps + 1, 1))
    Select Case tmp
        Case 1, 2, 3, 4, 5:MouthTimer.Interval = nMouth * CalVal:mouth.transY = 0
        Case 6:mouth.transY = -5:MouthTimer.Interval = nMouth * CalVal
        Case 7:mouth.transY = -5:MouthTimer.Interval = nMouth * CalVal
        Case 8:mouth.transY = -5:MouthTimer.Interval = nMouth * CalVal
        Case 9:mouth.transY = -5:MouthTimer.Interval = nMouth * CalVal
        Case 10:mouth.transY = -5:MouthTimer.Interval = nMouth * CalVal
    End Select

    MouthPos = ABS(MouthPos -1)
    MouthReps = MouthReps -1
'	dbg "CalVal : " &CalVal
'	dbg "Reps: " &MouthReps
'	dbg "Reps: " &MouthPos
    If MouthReps <= 0 Then
		mouth.transY = 0
        MouthTimer.Enabled = 0
        Updatehead
    End If
End Sub

'*************************
'Say Something stupid test
'*************************
' for example Say "Bring_it_to_me_cockroach", 313211131111
' the higher the number the longer the pause: 1 short delay, 9 long delay
' first number is always with the mouth close,

Sub SaySomethingStupid
    Dim currentsay
    currentsay = INT(81 * RND(1) )
    Select Case currentsay
        Case 0:Say "Boyfriend_fuck_him", "191589159218121813"
        Case 1:Say "Bring_it_to_me_cockroach", "1622111115121"
        Case 2:Say "Bring_my_car_dont_fuck_around", "179517972636663925"
        Case 3:Say "Cmon_bring_your_army", "13144814"
        Case 4:Say "Cmon_come_and_get_me", "12132643233414"
        Case 5:Say "Cmon_pussy_cat", "16272538"
        Case 6:Say "doing_a_great_job", "13252527"
        Case 7:Say "Dont_fuck_with_me", "34433337"
        Case 8:Say "Dont_make_me_spank_you", "3323242413631313121313"
        Case 9:Say "Dont_you_fucken_listen_man", "2313432613131336"
        Case 10:Say "Dont_you_got_something_better_2_do", "122121212121212122123"
        Case 11:Say "Free_advice_dont_fuck_with_me", "14242424247313132222722424"
        Case 12:Say "Fuck_off", "2939"
        Case 13:Say "Fuck_you_chico", "16574539"
        Case 14:Say "Fuckin_cock_sucker", "8524252524"
        Case 15:Say "fuckin_cockroach", "1934242624"
        Case 16:Say "Fuckin_fuck", "29381"
        Case 17:Say "Go_fuck_yourself", "353528"
        Case 18:Say "Have_my_car_set_to_me_ok", "24152414141726"
        Case 19:Say "Hello_pussy_cat", "16242424"
        Case 20:Say "Hello_you_there", "176427"
        Case 21:Say "hello1", "391"
        Case 22:Say "hello2", "281"
        Case 23:Say "hey", "161"
        Case 24:Say "Hey_there_sweet_cheeks", "2414131314"
        Case 25:Say "heyfuckyouman", "162527171"
        Case 26:Say "How_about_we_get_some_ice_cream", "2792131314141413131131"
        Case 27:Say "I_dont_give_a_fuck", "262423141"
        Case 28:Say "I_feel_like_I_already_know_you", "1324171223122413"
        Case 29:Say "I_like_you_your_like_a_tiger", "2713931234131"
        Case 30:Say "I_need_my_fuckin_car_man", "162424161476371"
        Case 31:Say "I_piss_in_your_face", "2333131313131313132"
        Case 32:Say "I_tell_you_something_fuck_you_man", "232414239535251"
        Case 33:Say "I_told_you_not_to_fuck_with_me", "2335145336543332151"
        Case 34:Say "I_want_to_talk_2_George", "16873332362"
        Case 35:Say "Im_fucked", "5417191"
        Case 36:Say "Im_Tony_fuckin_M", "13362526251"
        Case 37:Say "Im_Tony_fuckin_Montana", "13362526251"
        Case 38:Say "Its_obvious_you_got_an_attitude", "131414242224231414141"
        Case 39:Say "My_name_is_Tony_M", "2414133413341"
        Case 40:Say "No_fuckin_way", "1314251"
        Case 41:Say "Now_your_fucked", "26361"
        Case 42:Say "Oh_baby", "44241"
        Case 43:Say "Ok", "32341"
        Case 44:Say "Ok_lets_make_this_happen", "12361214231"
        Case 45:Say "Ok_so_what_you_doing_later", "3223841214131"
        Case 46:Say "Ok_so_why_dont_we_get_together", "322384121413141"
        Case 47:Say "Ok_your_starting_to_piss_me_off", "222514122315421"
        Case 48:Say "Run_while_you_can_stupid_fuck", "13131335142121431"
        Case 49:Say "saygoodnight", "251414152424231"
        Case 50:Say "sayhello2", "1415142926161"
        Case 51:Say "Shit_ass_fuck", "362434"
        Case 52:Say "Thats_ok", "26351"
        Case 53:Say "Tony_would_like_a_word", "131312363628273414141"
        Case 54:Say "Want_to_get_a_drink", "3425233424261"
        Case 55:Say "wastemytime", "13121324461"
        Case 56:Say "Watch_yourself_man", "1324238523133435212314241"
        Case 57:Say "What_are_you_going_to_do_for_me", "1312121312131"
        Case 58:Say "Who_you_think_you_fuckin_with", "1324313131313761"
        Case 59:Say "Why_dont_you_get_fucked", "241414251"
        Case 60:Say "WTF_do_you_want", "27391"
        Case 61:Say "Yeah_its_Tony", "3633371"
        Case 62:Say "Yeah1", "271"
        Case 63:Say "Yeah2", "261"
        Case 64:Say "You_better_not_be_comunist", "12131313253423231413325272"
        Case 65:Say "You_boring_you_know_that", "153634151"
        Case 66:Say "You_fuckin_high_or_what", "131426141"
        Case 67:Say "You_fuckin_hossa", "13172617171"
        Case 68:Say "You_fuckin_kidding_me_man", "2324133413141"
        Case 69:Say "You_fuckin_prick", "232423441"
        Case 70:Say "You_guys_dont_quit", "1423142324453313332324253453433233343252433341534341"
        Case 71:Say "You_just_fucked_up_man", "24251423341"
        Case 72:Say "You_know_what_you_are_soft", "242673333574324242423241"
        Case 73:Say "You_know_what_you_need", "26279965342515351"
        Case 74:Say "You_know_who_your_fuckin_with", "25162424261"
        Case 75:Say "You_need_an_army_to_take_me", "13151414141413331"
        Case 76:Say "You_picked_the_wrong_guy_to_fuck_with", "23232523233334261"
        Case 77:Say "You_think_you_can_fuck_with_me", "2414141423261"
        Case 78:Say "You_want_me_dont_you", "4415151"
        Case 79:Say "For_fuck_sake", "231323241"
        Case 80:Say "Hey_who_the_fuck_r_u", "232424241"

    End Select
End Sub

Sub SaySomethingWhileWaiting
    Dim tmp
    tmp = INT(RND(1) * 6)
    Select Case tmp
        Case 0:Say "Dont_you_got_something_better_2_do", "122121212121212122123"
        Case 1:Say "Fuck_off", "2939"
        Case 2:Say "You_want_me_dont_you", "4415151"
        Case 3:Say "You_better_not_be_comunist", "12131313253423231413325272"
        Case 4:Say "WTF_do_you_want", "27391"
        Case 5:Say "You_guys_dont_quit", "1423142324453313332324253453433233343252433341534341"
    End Select
End Sub

'*******************
' Scarface Shooting
'*******************
' animation and shooting av balls
' 1-disable the head triggers and mouth
' 2-start animation
' 3-shoot balls

Dim ShootingBalls, ScarShootPos, ScarShootBall

Sub InitScarface
    ShootingBalls = 0
    ScarShootPos = 0
    ScarShootBall = 0
End Sub

Sub StartScarShooting(nballs) 'it shoot the ball which started the multiball + multiballs
	'Debug.print "Balls:" &nBalls
    ScarShootBall = ScarShootBall + nballs
    If ShootingBalls = 0 Then
		'debug.print "Shooting is 0"
        ShootingBalls = 1
        ScarShootPos = 0
		Head.rotY = -4
		Mouth.rotY = -4
		GunUp.rotY = -4	
		bHeadMoving = True
        ScarAnim.Enabled = 1
    End If
End Sub

Sub ScarAnim_Timer
    Select Case ScarShootPos
        Case 0:GunUp.rotY = -18 : Head.rotY = -18: Mouth.rotY = -18 'doll.Image = "doll2"
        Case 1:GunUp.rotY = -18 : Head.rotY = -18: Mouth.rotY = -18 'doll.Image = "doll2"
        Case 2:GunUp.rotY = -36 : Head.rotY = -36: Mouth.rotY = -36 'doll.Image = "dollg1"
        Case 3:GunUp.rotY = -36 : Head.rotY = -36: Mouth.rotY = -36 'doll.Image = "dollg1"
        Case 4:GunUp.rotY = -36 : Head.rotY = -36: Mouth.rotY = -36 'doll.Image = "dollg1"
        Case 5:GunUp.rotY = -18 : Head.rotY = -18: Mouth.rotY = -18 'doll.Image = "dollg2"


            If ScarShootBall AND(BallsOnPlayfield + BallsInLock(nPlayer)) <nMaxBalls + 1 Then
                cannon1.createball
                cannon1.kick 140, 16 + RND(1) * 3
                If EE AND ScarShootBall = 1 Then PlaySound "sf_hadouken"
                ScarShootBall = ScarShootBall -1
                BallsOnPlayfield = BallsOnPlayfield + 1
                If BallsOnPlayfield> 1 Then MultiBallMode = 1
                PlaySound "machinegun1"
            End IF
        Case 6:GunUp.rotY = 0 : Head.rotY = 0: Mouth.rotY = 0 'doll.Image = "dollg3"
            If ScarShootBall AND(BallsOnPlayfield + BallsInLock(nPlayer)) <nMaxBalls Then
                cannon2.createball
                cannon2.kick 140, 16 + RND(1) * 3
                If EE AND ScarShootBall = 1 Then PlaySound "sf_hadouken"
                ScarShootBall = ScarShootBall -1
                BallsOnPlayfield = BallsOnPlayfield + 1
                If BallsOnPlayfield> 1 Then MultiBallMode = 1
                PlaySound "machinegun1"
            End IF
        Case 7:GunUp.rotY = 18 : Head.rotY = 18: Mouth.rotY = 18 'doll.Image = "dollg4"
            If ScarShootBall AND(BallsOnPlayfield + BallsInLock(nPlayer)) <nMaxBalls Then
                cannon3.createball
                cannon3.kick 140, 16 + RND(1) * 3
                If EE AND ScarShootBall = 1 Then PlaySound "sf_hadouken"
                ScarShootBall = ScarShootBall -1
                BallsOnPlayfield = BallsOnPlayfield + 1
                If BallsOnPlayfield> 1 Then MultiBallMode = 1
                PlaySound "machinegun1"
            End IF
        Case 8:GunUp.rotY = 24 : Head.rotY = 24: Mouth.rotY = 24 'doll.Image = "dollg5"
            If ScarShootBall AND(BallsOnPlayfield + BallsInLock(nPlayer)) <nMaxBalls Then
                cannon4.createball
                cannon4.kick 210, 16 + RND(1) * 3
                If EE AND ScarShootBall = 1 Then PlaySound "sf_hadouken"
                ScarShootBall = ScarShootBall -1
                BallsOnPlayfield = BallsOnPlayfield + 1
                If BallsOnPlayfield> 1 Then MultiBallMode = 1
                PlaySound "machinegun1"
            End IF
        Case 9:GunUp.rotY = 36 : Head.rotY = 36: Mouth.rotY = 36 'doll.Image = "dollg6"
            If ScarShootBall AND(BallsOnPlayfield + BallsInLock(nPlayer)) <nMaxBalls Then
                cannon5.createball
                cannon5.kick 210, 16 + RND(1) * 3
                If EE AND ScarShootBall = 1 Then PlaySound "sf_hadouken"
                ScarShootBall = ScarShootBall -1
                BallsOnPlayfield = BallsOnPlayfield + 1
                If BallsOnPlayfield> 1 Then MultiBallMode = 1
                PlaySound "machinegun1"
            End IF
        Case 10:GunUp.rotY = 42  : Head.rotY = 42: Mouth.rotY = 42'doll.Image = "dollg7"
        Case 11:GunUp.rotY = 42  : Head.rotY = 42: Mouth.rotY = 42 'doll.Image = "dollg7"
        Case 12:GunUp.rotY = 36 : Head.rotY = 36: Mouth.rotY = 36 'doll.Image = "dollg6"
            If ScarShootBall AND(BallsOnPlayfield + BallsInLock(nPlayer)) <nMaxBalls Then
                cannon5.createball
                cannon5.kick 210, 16 + RND(1) * 3
                If EE AND ScarShootBall = 1 Then PlaySound "sf_hadouken"
                ScarShootBall = ScarShootBall -1
                BallsOnPlayfield = BallsOnPlayfield + 1
                If BallsOnPlayfield> 1 Then MultiBallMode = 1
                PlaySound "machinegun1"
            End IF
        Case 13:GunUp.rotY = 24 : Head.rotY = 24: Mouth.rotY = 24 'doll.Image = "dollg5"
            If ScarShootBall AND(BallsOnPlayfield + BallsInLock(nPlayer)) <nMaxBalls Then
                cannon4.createball
                cannon4.kick 210, 16 + RND(1) * 3
                If EE AND ScarShootBall = 1 Then PlaySound "sf_hadouken"
                ScarShootBall = ScarShootBall -1
                BallsOnPlayfield = BallsOnPlayfield + 1
                If BallsOnPlayfield> 1 Then MultiBallMode = 1
                PlaySound "machinegun1"
            End IF
        Case 14:GunUp.rotY = 18 : Head.rotY = 18: Mouth.rotY = 18 'doll.Image = "dollg4"
            If ScarShootBall AND(BallsOnPlayfield + BallsInLock(nPlayer)) <nMaxBalls Then
                cannon3.createball
                cannon3.kick 140, 16 + RND(1) * 3
                If EE AND ScarShootBall = 1 Then PlaySound "sf_hadouken"
                ScarShootBall = ScarShootBall -1
                BallsOnPlayfield = BallsOnPlayfield + 1
                If BallsOnPlayfield> 1 Then MultiBallMode = 1
                PlaySound "machinegun1"
            End IF
        Case 15:GunUp.rotY = 0  : Head.rotY = 0: Mouth.rotY = 0'doll.Image = "dollg3"
            If ScarShootBall AND(BallsOnPlayfield + BallsInLock(nPlayer)) <nMaxBalls Then
                cannon2.createball
                cannon2.kick 140, 16 + RND(1) * 3
                If EE AND ScarShootBall = 1 Then PlaySound "sf_hadouken"
                ScarShootBall = ScarShootBall -1
                BallsOnPlayfield = BallsOnPlayfield + 1
                If BallsOnPlayfield> 1 Then MultiBallMode = 1
                PlaySound "machinegun1"
            End IF
        Case 16:GunUp.rotY = -18 : Head.rotY = -18: Mouth.rotY = -18 'doll.Image = "dollg2"
            If ScarShootBall AND(BallsOnPlayfield + BallsInLock(nPlayer)) <nMaxBalls + 1 Then
                cannon1.createball
                cannon1.kick 140, 16 + RND(1) * 3
                If EE AND ScarShootBall = 1 Then PlaySound "sf_hadouken"
                ScarShootBall = ScarShootBall -1
                BallsOnPlayfield = BallsOnPlayfield + 1
                If BallsOnPlayfield> 1 Then MultiBallMode = 1
                PlaySound "machinegun1"
            End IF
        Case 17:GunUp.rotY = -36  : Head.rotY = -36: Mouth.rotY = -36'doll.Image = "dollg1"
        Case 18:GunUp.rotY = -36  : Head.rotY = -36: Mouth.rotY = -36'doll.Image = "dollg1"
            If ScarShootBall Then
                ScarShootPos = 4
            End If
        Case 19:
			GunUp.rotY = -36 : Head.rotY = -36: Mouth.rotY = -36 'doll.Image = "dollg1"
        Case 20:
			bHeadMoving = False
			ScarAnim.Enabled = 0
			ShootingBalls = 0

    End Select

    ScarShootPos = ScarShootPos + 1

End Sub

'*************
' TV animation
'*************

Dim TVPos

Sub InitTV
    TVPos = 1
End Sub

Sub TVTimer_Timer
   Select case TVPos
        Case 1:TVScreens.image = "1"
        Case 2:TVScreens.image = "2"
        Case 3:TVScreens.image = "3"
        Case 4:TVScreens.image = "4"
        Case 5:TVScreens.image = "5"
        Case 6:TVScreens.image = "6"
        Case 7:TVScreens.image = "7"
        Case 8:TVScreens.image = "8"
        Case 9:TVScreens.image = "9"
        Case 10:TVScreens.image = "10"
        Case 11:TVScreens.image = "11"
        Case 12:TVScreens.image = "12"
        Case 13:TVScreens.image = "13"
        Case 14:TVScreens.image = "14"
        Case 15:TVScreens.image = "15"
        Case 16:TVScreens.image = "16"
        Case 17:TVScreens.image = "17"
        Case 18:TVScreens.image = "18"
        Case 19:TVScreens.image = "19"
        Case 20:TVScreens.image = "20"
        Case 21:TVScreens.image = "21"
        Case 22:TVScreens.image = "22"
        Case 23:TVScreens.image = "23"
        Case 24:TVScreens.image = "24"
        Case 25:TVScreens.image = "25"
        Case 26:TVScreens.image = "26"

    End Select

if VRRoomChoice = 3 Then
   Select case TVPos
        Case 1:VR_Mega_TVScreens.image = "1"
        Case 2:VR_Mega_TVScreens.image = "2"
        Case 3:VR_Mega_TVScreens.image = "3"
        Case 4:VR_Mega_TVScreens.image = "4"
        Case 5:VR_Mega_TVScreens.image = "5"
        Case 6:VR_Mega_TVScreens.image = "6"
        Case 7:VR_Mega_TVScreens.image = "7"
        Case 8:VR_Mega_TVScreens.image = "8"
        Case 9:VR_Mega_TVScreens.image = "9"
        Case 10:VR_Mega_TVScreens.image = "10"
        Case 11:VR_Mega_TVScreens.image = "11"
        Case 12:VR_Mega_TVScreens.image = "12"
        Case 13:VR_Mega_TVScreens.image = "13"
        Case 14:VR_Mega_TVScreens.image = "14"
        Case 15:VR_Mega_TVScreens.image = "15"
        Case 16:VR_Mega_TVScreens.image = "16"
        Case 17:VR_Mega_TVScreens.image = "17"
        Case 18:VR_Mega_TVScreens.image = "18"
        Case 19:VR_Mega_TVScreens.image = "19"
        Case 20:VR_Mega_TVScreens.image = "20"
        Case 21:VR_Mega_TVScreens.image = "21"
        Case 22:VR_Mega_TVScreens.image = "22"
        Case 23:VR_Mega_TVScreens.image = "23"
        Case 24:VR_Mega_TVScreens.image = "24"
        Case 25:VR_Mega_TVScreens.image = "25"
        Case 26:VR_Mega_TVScreens.image = "26"

    End Select
End If

	'dbg "tvPOS: " &TVPos
    TVPos = TVPos + 1
    If TVPos> 26 Then TVPos = 1
End Sub

Sub Init_Game()
	Dim i,j
	
	Dbg "Starting Game"
	ResetDrainTracker
	pDMDSetPage pScores, "InitGame"
	pDMDSetHUD 1
	ClearHS
	LampFlashersOff
'	anScore(0) = 0
	addscore 0
	nCurrentPlayerIndex = 1
	nSideProgress = 0
	nMainProgress = 0
	bHeadMoving = False
	bGameOver = False
	bOnTheFirstBall = True
	

	For i = 0 to 4
		'anScore(i) = 0
		Score(i) = 0
		nBonusX(i) = 1
		nPlayfieldX(i) = 1.0
		Made1(i) = 0
		Made2(i) = 0

		For j = 0 to 11
			MainMode(i, j) = 0
		Next

		For j = 0 to 6
			SideEvent(i, j) = 0
		Next

		For j = 0 to 1
			MiniGameLoops(i,j) = 0
		Next
	Next


	pDMDLabelHide "MainModeTimerValue"
	pDMDLabelHide "SideEventTimerValue"
	DisplayBonusValue
	DisplayPlayfieldValue

End Sub

'*******************************************************************************
'      <<  MODES  >>
' In any MODE there will be a:
' - list of switches, lights
' - list of variables
' - init sub -init variables and ready to start
' - check sub -check if it has started, and check if it is completed, give award
' - start sub -mostly it setups a variable, mode started, used in the check sub
' - end sub -to turn off the mode. Many modes will end with the ball others will
'            continue until completed or until the end of the game.
'*******************************************************************************

Dim SideEventNr, SideEventStarted, SideEventPrepared, BallsInDesk, EjectBallsInDeskDelay, SideEvents(4,5), SideEventsFinished, SidesCompleted

Sub InitModes
    Dim i,j
    SideEventNr = 0 ' contains the side mode nr
    SideEventStarted = 0
    SideEventPrepared = 0
    l24.State = 0
    l41.State = 0
    SideEventsFinished = 0
    SidesCompleted = 0
	For i = 0 to 4
		For j = 0 To 5
			SideEvents(nPlayer,i) = 0
		Next
	Next

    UpdateSideLights
End Sub

Sub WinSideEvent
	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Green}: "&asSideNames(SideEventNr) &" Won"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If

	pDMDSplashLines asSideNames(SideEventNr), "COMPLETED", 2, cSilver
	GeneralPuPQueue.Add "HideModeComplete","ClearPupSplashMessages",40,2100,0,0,0,False

	select case SideEventNr
		Case 1
			l2.state = 1
			SideEvent(nPlayer,1) = 1
		Case 2
			l3.state = 1
			SideEvent(nPlayer,2) = 1
		Case 4
			l5.state = 1
			SideEvent(nPlayer,4) = 1
		Case 5
			l6.state = 1
			SideEvent(nPlayer,5) = 1
	End Select
	SideEventsFinished = 0
	SideEvent(nPlayer,SideEventNr) = 1
    SideEventsFinished = SideEvent(nPlayer,1) + SideEvent(nPlayer,2)  + SideEvent(nPlayer,4) + SideEvent(nPlayer,5)

	nPlayfieldX(nPlayer) = 1 + SideEventsFinished
	DisplayPlayfieldValue
End Sub

Sub EndSideEvent
	HideSideEventMessages
	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Red}: "&asSideNames(SideEventNr) &" Ended"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
	Dbg "SideEvent " & SideEventNr &" Ended"

    SideEventsFinished = SideEvent(nPlayer,1) + SideEvent(nPlayer,2) + SideEvent(nPlayer,3) + SideEvent(nPlayer,4) + SideEvent(nPlayer,5)

    If SidesCompleted = 0 Then
        If SideEventsFinished = 5 Then
            SidesCompleted = 1
        End If
    End If
    SideEventPrepared = 0
    l24.State = 0
    l41.State = 0
    SideEventStarted = 0
    BackDoorPost.IsDropped = 1:DOF 136, DOFPulse
    SideEventNr = 0
    UpdateSideLights
    PlayTheme
End Sub

Sub PrepareSideEvent 'make ready to start SideEvent
    SideEventPrepared = 1
    UpdateSideLights
    l24.State = 2
    l41.State = 2
    BackDoorPost.IsDropped = 0:DOF 136, DOFPulse
End Sub

Sub UpdateSideLights
    If SideEvent(nPlayer,1) Then
        l2.State = 1
    Else
        l2.State = 0
    End If

    If SideEvent(nPlayer,2) Then
        l3.State = 1
    Else
        l3.State = 0
    End If

    If SideEvent(nPlayer,3) Then
        l4.State = 1
    Else
        l4.State = 0
    End If

    If SideEvent(nPlayer,4) Then
        l5.State = 1
    Else
        l5.State = 0
    End If

    If SideEvent(nPlayer,5) Then
        l6.State = 1
    Else
        l6.State = 0
    End If

    If SideEventPrepared Then
        Select Case SideEventNr
            Case 1:l2.State = 2
            Case 2:l3.State = 2
            Case 3:l4.State = 2
            Case 4:l5.State = 2
            Case 5:l6.State = 2
        End Select
    End If

	SideEventsFinished = 0
    SideEventsFinished = SideEvent(nPlayer,1) + SideEvent(nPlayer,2)  + SideEvent(nPlayer,4) + SideEvent(nPlayer,5)

	nPlayfieldX(nPlayer) = 1 + SideEventsFinished
	DisplayPlayfieldValue
End Sub

Sub BackDoorPost_Hit
    PlaySoundat "fx_metalhit", backdoor
End Sub

Sub BackDoor_Hit
    PlaySoundat "fx_hole_enter", backdoor
    BackDoorPost.IsDropped = 1:DOF 136, DOFPulse
	bSupressMainEvents = True


    BackDoor.DestroyBall
    BallsInDesk = BallsInDesk + 1
    BallsOnPlayfield = BallsOnPlayfield - 1
    l24.State = 0
    l41.State = 0

	CheckDropTargetsBackdoor

    If EventStarted = 11 Then
        If Event11Kill Then
            WinEvent11
            Exit Sub
        Else
            Say "Hey_who_the_fuck_r_u", "232424241"
            EjectBallsInDeskDelay = EjectBallValue 'shoot back the ball
            Exit Sub
        End If
    End If

    If SideEventNr AND SideEventStarted = 0 Then 'a side event is prepared to run so start it
        SideEventStarted = 1

		if Scorbit.bSessionActive then
			GameModeStrTmp="BL{Yellow}: "&asSideNames(SideEventNr) &" Started"
			Scorbit.SetGameMode(GameModeStrTmp)
		End If

		Dbg "SideEvent " & SideEventNr &" Started"
			if VRRoom > 0 Then
				PuPlayer.LabelSet pDMD, "SideEventMessage", asSideEventMessages(SideEventNr), 1, "{'mt':2,'size':"& VR_MessageSize &"}"
			Else
				PuPlayer.LabelSet pDMD, "SideEventMessage", asSideEventMessages(SideEventNr), 1, ""
			End If


			UpdateDMDSideProgress
        Select Case SideEventNr
            Case 1:
				GeneralPupQueue.Add "SE1","PupEvent 370",70,5000,0,0,0,False
				StartBank
            Case 2:
				GeneralPupQueue.Add "SE2","PupEvent 371",70,5000,0,0,0,False
				StartHitMan
            Case 3:
				GeneralPupQueue.Add "SE3","PupEvent 372",70,5000,0,0,0,False
				StartTWIY
            case 4:
				GeneralPupQueue.Add "SE4","PupEvent 373",70,5000,0,0,0,False
				StartYeyoRun
            case 5:
				GeneralPupQueue.Add "SE5","PupEvent 374",70,5000,0,0,0,False
				StartGetCar
        End Select
    Else
		'Debug.print "here1 "
		If ballsonplayfield = 0 And bMiniGameActive And EventStarted = 0 Then 
			if EventStarted = 3 or bMiniGameAllowed = False Then 
				Say "Hey_who_the_fuck_r_u", "232424241"
				EjectBallsInDeskDelay = EjectBallValue 'shoot back the ball
			Else
				StartMiniGame
			End If
		Else
			'debug.print "BP:" &ballsonplayfield
			Say "Hey_who_the_fuck_r_u", "232424241"
			EjectBallsInDeskDelay = EjectBallValue 'shoot back the ball
		End If
    End If

Lastswitchhit = "backdoor"
End Sub

Sub DeskIn_Hit

	if EE Then
		DeskIn.DestroyBall
		BallsInDesk = BallsInDesk + 1
		BallsOnPlayfield = BallsOnPlayfield - 1
		bSkipTargetReset = False
		bSupressMainEvents = True
		LockExit
		Exit Sub
	End If

    PlaySoundat "fx_kicker_enter", deskin
	bSkipTargetReset = False
	bSupressMainEvents = True
 dbg "DESK Hit"
   
    DeskIn.DestroyBall
    BallsInDesk = BallsInDesk + 1
    BallsOnPlayfield = BallsOnPlayfield - 1

    If EventStarted = 11 Then
        DeskHits = DeskHits + 1
		nMainProgress = nMainProgress + 1
		UpdateDMDMainProgress
        SetLamp 1, 0, 1, 8
		Flash1 True
		BallHandlingQueue.Add "Flash1 False","Flash1 False",60,1500,0,0,0,False
        AddScore ScoreME11Jackpot
        If DeskHits = 10 Then
            Door.IsDropped = 0
            NextPartEvent11
        Else
            StartScarShooting BallsInDesk
            BallsInDesk = 0
        End If
        Exit Sub
    End If
    Door.IsDropped = 0

   ' If EventsFinished = 10 AND SidesCompleted Then
    If EventsFinished = 10 Then
        StartEvent11Delay = 30
        Exit Sub
    End If
    l38.state = 0
    l39.state = 0
    l40.state = 0
    l38b.state = 0
    l39b.state = 0
    l40b.state = 0
    l37.state = 0
    StartMainEvent 'start the current event
End Sub

'*********************
' Skillshot - Car Hole
'*********************

'switches: carhole
'lights: rflaher

Dim SkillshotStarted, SkillShotReady, CarHole8Delay

Sub InitSkillShot
    CarHole8Delay = 0
    SkillshotStarted = 0
    SkillShotReady = 1
End Sub

Sub StartSkillshot
    SkillshotStarted = 1                           'Started
    LightSeqPFLights.Play SeqAllOff                'turn off playfield lights
    LightSeqSkillshot.Play SeqBlinking, , 5000, 50 
    SetLamp 4, 1, 0, 0
End Sub

Sub EndSkillshot
    If SkillShotStarted Then
        SkillShotReady = 0
        SkillShotStarted = 0
        LightSeqPFLights.StopPlay
        SetLamp 4, 0, 0, 0
        LightSeqSkillshot.StopPlay
    End If
End Sub

Sub CarHole_Hit
	WireRampOff
    PlaySoundat SoundFXDOF("fx_kicker_enter", 210, DOFPulse, DOFContactors), carhole
    setlamp 1, 0, 1, 10
		Flash1 True
		BallHandlingQueue.Add "Flash1 False","Flash1 False",60,2000,0,0,0,False

    If EventStarted = 8 Then
        AddScore ScoreME8Skill
		nMainProgress = nMainProgress + 1
		UpdateDMDMainProgress
        PlaySound "car_start"
        
        Set aBall = ActiveBall
        aZpos = aBall.Z
        

		if nMainProgress > 7 Then 
			WinEvent8
		Else
			CarHole8Delay = 14
		End If

        Exit Sub
    End If

    If SkillshotStarted Then
        StartBallSaved(nBallSavedTime) 'start ball save time if 1st ball
        AddScore ScoreSkillShot
		DisplaySkillAward
		
    End If
    CarHoleDelay = 20
End Sub

Sub CarHoleExit 'normal skillshot
    EndSkillshot
    PlaySoundat SoundFXDOF("fx_kicker", 119, DOFPulse, DOFContactors), CarHole
    CarHole.Kick 298, 60
End Sub

Sub CarHole8Exit 'exit from the event 8
    CarHole.TimerInterval = 2
    CarHole.TimerEnabled = 1
End Sub

Sub DisplaySkillAward
	Pupevent 304
	pDMDSplashLines  "SKILLSHOT", (FormatScore(ScoreSkillShot)), 3, cWhite
	GeneralPuPQueue.Add "HideSkillAwardSafety","HideSkillAwardSafety",40,3100,0,0,0,False
End Sub

Sub HideSkillAwardSafety
	' Make sure skill award display goes away, sometimes splash doesnt clear properly
	PuPlayer.LabelSet pDMD,"Splash2a","",1,""
	PuPlayer.LabelSet pDMD,"Splash2b","",1,"" 
End Sub

' Car Hole with animation
Dim aBall, aZpos

Sub CarHole_Timer
    aBall.Z = aZpos
    aZpos = aZpos-4
    If aZpos <0 Then
        Me.TimerEnabled = 0
        Me.DestroyBall
        BallsOnPlayfield = BallsOnPlayfield -1
        PlaySound "fx_kicker_enter" ' RTP uncommented
        NewBall8
    End If
End Sub

'********************************
' Left Ramp - kill ramp - Hit Man
' 2 ball multiball
'********************************
'triggers: leftramp
'lights 18,19,20,21,22, 23 and 3

Dim KillRampHits, HitmanStarted, LeftRampJackpotEnabled, StartHitManDelay

Sub InitKillRamp
    KillRampHits = 0
    HitmanStarted = 0
    LeftRampJackpotEnabled = 0
	DOF 216, DOFOff
    l18.State = 0
    l19.State = 0
    l20.State = 0
    l21.State = 0
    l22.State = 0
    l23.State = 0
	l55.state = 0
    l3.State = 0
    StartHitManDelay = 0
End Sub

Sub CheckKillRamp

    Select Case KillRampHits
        Case 1:l18.State = 1
        Case 2:l19.State = 1
        Case 3:l20.State = 1
        Case 4:l21.State = 1
        Case Else
            If EventStarted = 11 Then Exit Sub
            l22.State = 1
            l12.State = 1
            'PrepareHitMan
			if SideEvent(nPlayer,2) = 0 Then
				l3.State = 2
				SideEventNr = 2
				PrepareSideEvent
			Else

			End If

    End Select
End Sub

Sub StartHitMan

	GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",80,3000,0,0,0,False
    l18.State = 0
    l19.State = 0
    l20.State = 0
    l21.State = 0
    l22.State = 0
    l23.State = 0
    l24.State = 0
    l41.State = 0
    PlayTheme

    BallhandlingQueue.Add "Door.Isdropped = 0","Door.Isdropped = 0",90,RDTValue,0,0,0,False
    CTDelay = 0
    l38.State = 1
    l39.State = 1
    l40.State = 1
    l38b.State = 1
    l39b.State = 1
    l40b.State = 1
    StartHitManDelay = 100
End Sub

Sub StartHitMan2
    StartScarShooting BallsInDesk + 1 'shoot the ball/s which started the event + 1 multiball
    BallsinDesk = 0
    StartBallSaved 20                 '20 seconds
    HitmanStarted = 1
    StartLeftRampJackpot              'lights
End Sub

Sub EndHitMan
    If HitmanStarted Then 'hitman was started
        KillRampHits = 0
        HitmanStarted = 0
        LeftRampJackpotEnabled = 0
	StopLeftRampJackpot 'Lights
        l3.State = 1        'hitman done

        BallhandlingQueue.Add "ResetDroptargets","ResetDroptargets",90,RDTValue,0,0,0,False
        EndSideEvent
    End If
End Sub

Sub StartLeftRampJackpot
    LeftRampJackpotEnabled = 1
    LightSeqLeftRamp.Play SeqRandom, 10, 10
End Sub

Sub LightSeqLeftRamp_PlayDone()
    LightSeqLeftRamp.Play SeqRandom, 10, 10
End Sub

Sub StopLeftRampJackpot
    LeftRampJackpotEnabled = 0
    LightSeqLeftRamp.StopPlay
End Sub

'********************
' Spinner - sell Yeyo 10 SPINNER HITS LIGHTS JACKPOT ON RIGHT RAMP
'********************

'switches : rott1, rott2, rott3, rott4, rott5
'lights 43, 44, 45, 46, 47, 48
'YeyoLights: 0 off, 1 blinking, 2 collected

Dim YeyoHits, YeyoLight, YeyoStarted, RightRampJackpotEnabled

Sub InitYeyo
    YeyoHits = 0
    l43.State = 0
    l44.State = 0
    l45.State = 0
    l46.State = 0
    l47.State = 0
    YeyoLight = 0
    l5.state = 0
    YeyoStarted = 0
    RightRampJackpotEnabled = 0
End Sub

Sub CheckYeyo
    YeyoHits = YeyoHits + 1
    If YeyoStarted AND YeyoHits >= 10 Then '10 hits turn on the jackpot on the right ramp 
        YeyoHits = 0
        StartRightRampJackpot
    End If

    If YeyoHits >= 10 Then
        YeyoHits = 0
        YeyoLight = YeyoLight + 1
        Addscore ScoreCollectYeyo
        Select Case YeyoLight
            Case 1:l43.State = 2
            Case 2:l44.State = 2
            Case 3:l45.State = 2
            Case 4:l46.State = 2
            Case 5:l47.State = 2
        End Select
    End If
End Sub

Sub RightRamp_Hit
	bBackLightsOn = True
	BallHandlingQueue.Add "bBackLightsOn = False","bBackLightsOn = False",60,1000,0,0,0,False
    SetLamp 3, 0, 1, 10
    AddScore ScoreRightRamp
    PlaySound "money1"
    If EE Then
        PlayRandomPunch
        AddScore 1
        Exit Sub
    End If
    If EventStarted = 7 Then
        If Event7State = 1 Then
			nMainProgress = nMainProgress + 1
				if VRRoom > 0 Then
					PuPlayer.LabelSet pDMD, "MainModeMessage", asMainModeMessages(CurrentEvent), 1, "{'mt':2,'size':"& VR_MessageSize &"}"
				Else
					PuPlayer.LabelSet pDMD, "MainModeMessage", asMainModeMessages(CurrentEvent), 1, ""
				End If
			L54.State = 0
			UpdateDMDMainProgress
            CheckEvent7
            AddScore ScoreRightRamp
        End If
        Exit Sub
    End If
    If EventStarted = 10 And Event10Hits(7) = 0 Then
        Event10Hits(7) = 1
        l48.State = 0
        CheckEvent10
        AddScore ScoreRightRamp
        Exit Sub
    End If
    If YeyoStarted AND RightRampJackpotEnabled Then
        AddScore ScoreYeyoJackpot
		WinSideEvent
        StopRightRampJackpot
        Exit Sub
    End If
    If TWIYStarted AND RightRampJackpotEnabled Then
        If LeftRampJackpotEnabled = 0 Then
            l25.State = 2
            l42.State = 2
        End If
        StopRightRampJackpot
        GiveTWIYJackpot
        Exit Sub
    End If
    If l43.State = 2 Then
        AddScore ScoreRightRamp
        l43.State = 1
        Exit Sub
    End If

    If l44.State = 2 Then
        AddScore ScoreRightRamp
        l44.State = 1
        Exit Sub
    End If

    If l45.State = 2 Then
        AddScore ScoreRightRamp
        l45.State = 1
        Exit Sub
    End If

    If l46.State = 2 Then
        AddScore ScoreRightRamp
        l46.State = 1
        Exit Sub
    End If

    If l47.State = 2 Then
        AddScore ScoreRightRamp
        l47.State = 1
        PrepareYeyoRun
    End If
End Sub

'**************
'   Yeyo Run
'**************
' 2 multiball
' lights 5, 24, 41

Sub PrepareYeyoRun
    If EventStarted = 11 or SideEvent(nPlayer,4) = 1 Then Exit Sub
    l5.State = 2
    SideEventNr = 4
    PrepareSideEvent
End Sub

Sub StartYeyoRun
    SideEventNr = 4
    l5.state = 2
    YeyoStarted = 1
    StartScarShooting 2 'shoot 2 multiballs
    BallsInDesk = 0
    StartBallSaved 20   '20 seconds
    ' StartRightRampJackpot
    l41.State = 0
    l24.State = 0
    l15.State = 2
    PlayTheme
End Sub

Sub EndYeyoRun
    'If l5.State = 2 Then l5.State = 1
    If YeyoStarted Then
        YeyoHits = 0
        l43.State = 0
        l44.State = 0
        l45.State = 0
        l46.State = 0
        l47.State = 0
        YeyoLight = 0
        l15.State = 0
        YeyoStarted = 0
        StopRightRampJackpot
        EndSideEvent
    End If
End Sub

Sub StartRightRampJackpot
    RightRampJackpotEnabled = 1
    LightSeqRightRamp.Play SeqRandom, 10, 10
End Sub

Sub LightSeqRightRamp_PlayDone()
    LightSeqRightRamp.Play SeqRandom, 10, 10
End Sub

Sub StopRightRampJackpot
    RightRampJackpotEnabled = 0
    LightSeqRightRamp.StopPlay
End Sub

'************
' Get Car
'************
'20 seconds hurry up
' targets: rt1
' lights 17, 6

Dim CarHits, GetCarStarted, GetCarDelay

Sub InitGetCar
    CarHits = 0
    l17.State = 0
    l6.State = 0
    GetCarDelay = 0
    GetCarStarted = 0
End Sub

Sub CheckCarHits
    If EventStarted = 11 Then Exit Sub
    If EventStarted = 9 Then
        SetLamp 1, 0, 1, 5
		Flash1 True
		BallHandlingQueue.Add "Flash1 False","Flash1 False",60,1000,0,0,0,False
        If CarHits = 5 Then
            WinEvent9
        End If
        Exit Sub
    End If
    If CarHits >= 10 And SideEvent(nPlayer,5) = 0 Then 'PrepareGetCar
        CarHits = 0
        l6.State = 2
        SideEventNr = 5
        PrepareSideEvent
    End If
End Sub

Sub StartGetCar
    Say "Run_while_you_can_stupid_fuck", "13131335142121431"
    GetCarDelay = GetCarTime * 20
    l6.state = 2
    GetCarStarted = 1
    PlayTheme
    EjectBallsInDeskDelay = EjectBallValue 'shoot back the ball
    l41.State = 0
    l24.State = 0
    l17.State = 2
    SetLamp 4, 0, 2, -1
    SetLamp 3, 0, 1, -1
	bBackLightsOn = True
End Sub


' RTP not sure this sub is needed
Sub WinGetCar
	bBackLightsOn = False
    If GetCarStarted Then
        l6.State = 1
        l17.State = 0
        GetCarStarted = 0
        GetCarDelay = 0
        SetLamp 3, 0, 0, 0
        SetLamp 4, 0, 0, 0
		WinSideEvent
        EndSideEvent
    End If
End Sub

Sub EndGetCar
	bBackLightsOn = False
    If GetCarStarted Then
        l6.State = 1
        l17.State = 0
        GetCarStarted = 0
        GetCarDelay = 0
        SetLamp 3, 0, 0, 0
        SetLamp 4, 0, 0, 0
        EndSideEvent
    End If
End Sub

'***************
'   Mini Loop
'***************

'triggers: miniloop

Dim MiniLoopHits

Sub InitMiniLoops
    MiniLoopHits = 0
End Sub

Sub MiniLoop_Hit
    Dim tmp
    If EE Then
        Exit Sub
    End If
    If EventStarted = 10 And Event10Hits(8) = 0 Then
        Event10Hits(8) = 1
        l25.State = 0
        l42.State = 0
		CheckEvent10
        Exit Sub
    End If
    If TWIYStarted Then 'enable jackpot on the ramps
        l25.State = 0
        l42.State = 0
	tmp = INT(RND(1) * 5)
        Select case tmp
            Case 0:Say "Yeah1", "271"
            Case 1:Say "Yeah2", "261"
            Case 2:Say "Ok", "32341"
            Case 3:Say "Thats_ok", "26351"
            Case 4:Say "Yeah_its_Tony", "3633371"
        End Select

        If LeftRampJackpotEnabled = 0 AND RightRampJackpotEnabled = 0 Then
            StartLeftRampJackpot
            StartRightRampJackpot
            Exit Sub
        End If
    End If
    If RND(1) <0.5 Then
        PlaySound "ricochet1"
    Else
        PlaySound "ricochet2"
    End If
    ' normal mode
    AddScore ScoreMiniLoop
    If EventStarted = 11 Then Exit Sub
    MiniLoopHits = MiniLoopHits + 1
    CheckMiniLoop
End Sub

Sub CheckMiniLoop
    If EventStarted = 3 Then
		nMainProgress = nMainProgress + 1
		UpdateDMDMainProgress
        If MiniLoopHits > 2 Then
            MiniLoopHits = 0
            WinEvent3
        End If
        Exit Sub
    End If

    If MiniLoopHits >= 3 Then
        MiniLoopHits = 0
        OpenLockBlock 'enable lock
    End If
End Sub

'******************************
' The World if Yours Multiball
'******************************

'triggers: miniloop trigger, Ramp triggers for Jackpot
'lights: l4, l25, l42

Dim TWIYStarted

Sub InitTWIY
    TWIYStarted = 0
    l4.State = 0
End Sub

Sub StartTWIY
    l50.State = 0
    l51.State = 0
    l52.State = 0
    StartBallSaved 70
    SideEventStarted = 1
    SideEventNr = 3
	Say "sayhello2", "1415142926161"
	AudioQueue.Add "PlayTheme","PlayTheme",80,3000,0,0,0,False
    TWIYStarted = 1
    l4.State = 2
    l25.State = 2
    l42.State = 2
    BackDoorPost.IsDropped = 1                  'drop the post to enable the trigger for the mode
	DOF 136, DOFPulse
	body.Visible = 0
	GunUp.Visible = 1
	TonyStandUpTimer.Enabled = 1
    StartScarShooting BallsInLock(nPlayer) + BallsInDesk 'BallsinDesk should be 0, but just in case we eject all the balls
    BallsInLock(nPlayer) = 0                             'empty the 3 locked balls
    BallsInDesk = 0                             'empty the desk balls
End Sub

Sub UpdateTWIYdmd
End Sub

Sub EndTWIY
	WinSideEvent
    If l4.State = 2 Then l4.State = 1
    If TWIYStarted Then
	TWIYStarted = 0
	TonySitDownTimer.Enabled = 1
        l4.State = 1
		if EventStarted <> 3 Then
			' make sure miniloop event isnt running
			l25.State = 0
			l42.State = 0
		End iF
        StopLeftRampJackpot
        StopRightRampJackpot
        EndSideEvent
    End If
End Sub

Sub GiveTWIYJackpot
    AddScore ScoreTWIYJackpot
    ScoreTWIYJackpot = ScoreTWIYJackpot + 50000
    PlaySound "keyboard"
End Sub

'****************
'  Bank Targets
'****************

'targets: lt1, lt2, lt3, lt4
'lights: 11, 12, 13, 14 and l2

Dim BankTargets(4), BankDelay, BankStarted

Sub InitBank
    BankTargets(0) = 0
    BankTargets(1) = 0
    BankTargets(2) = 0
    BankTargets(3) = 0
    BankTargets(4) = 0
    l11.State = 0
    l12.State = 0
    l13.State = 0
    l14.State = 0
    l2.State = 0
    BankDelay = 0
    BankStarted = 0
End Sub

Sub CheckBank
    If EventStarted = 11 Then Exit Sub
    BankTargets(0) = BankTargets(1) + BankTargets(2) + BankTargets(3) + BankTargets(4)
    If BankTargets(0) = 4 Then
        'AddScore ScoreBankLights
		AddBonus ScoreBankLights
		BankTargets(0) = 0
		BallHandlingQueue.Add "RaiseBankTargets","RaiseBankTargets",60,100,0,0,0,False

		if SideEvent(nPlayer,1) = 1 And SideEventNr = 1 Then
			BankStarted = 1
			bBackLightsOn = False
			SetLamp 3, 0, 0, 0
			LightSeqBankRun.StopPlay
			l2.State = 1
			WinSideEvent
			EndSideEvent
		Elseif SideEvent(nPlayer,1) = 1 Then
			bBackLightsOn = False
			SetLamp 3, 0, 0, 0
		Else
			l2.State = 2
			SideEventNr = 1
			PrepareSideEvent
		End If
    End If
End Sub

Sub StartBank
    Say "Run_while_you_can_stupid_fuck", "13131335142121431"
    BankStarted = 1
    BankDelay = BankTime * 20
	BallHandlingQueue.Add "RaiseBankTargets","RaiseBankTargets",60,100,0,0,0,False
    l24.State = 0
    l41.State = 0
    EjectBallsInDeskDelay = EjectBallValue
    PlayTheme
	bBackLightsOn = True
    SetLamp 3, 0, 2, -1
	bBackLightsOn = True
    LightSeqBankRun.Play SeqRandom, 50, , 4000
End Sub

Sub LightSeqBankRun_PlayDone()
    LightSeqBankRun.Play SeqRandom, 50, , 4000
End Sub

Sub EndBank
	if SideEvent(nPlayer,1) = 1 Then Exit Sub
	bBackLightsOn = False

    If BankStarted Then
        BankStarted = 0
		bBackLightsOn = False
        SetLamp 3, 0, 0, 0
        LightSeqBankRun.StopPlay
        InitBank
        l2.State = 1
        EndSideEvent
    End If
End Sub

'*******************
'    Main Events
'*******************
' slingshots increase the actual event if none is started
'lights 26,27,28,29,30,31,32,33,34,35,36
' EventStarted is set to 1 when any event is started

Dim Events(4,11), EventLights, CurrentEvent, EventStarted

Sub InitEvents
    Dim i,j
    EventLights = Array(dummy, l26, l27, l28, l29, l30, l31, l32, l33, l34, l35, l36)


	For i = 0 to 4
		For j = 0 To 11
			Events(i,j) = 0
		Next
    Next

	For j = 1 To 11
		EventLights(j).State = 0
	Next

    CurrentEvent = 1
    EventLights(CurrentEvent).State = 2
    EventStarted = 0
    Event1Delay = 0
    Event2Delay = 0
    Event3Delay = 0
    Event4Delay = 0
    Event5Delay = 0
    Event6Delay = 0
    Event7Delay = 0
    Event8Delay = 0
    Event9Delay = 0
    StartEvent11Delay = 0
    Event11VoiceDelay = 0
    EndEvent11Delay = 0
    EndWinEvent11Delay = 0
    rottHits = 0
    Event5Bumps = 0
    Event11Kill = 0
End Sub

Sub LightNextEvent
    If EventStarted Then Exit Sub     'if any event is started then do not change the lights
'Dbg "Calling Light Next Event"
    If Events(nPlayer,CurrentEvent) = 0 Then 'the event is not finished so you may turn off the light :)
        EventLights(CurrentEvent).State = 0
    End If
    LightNextEvent2
End Sub

Sub LightNextEvent2
    CurrentEvent = CurrentEvent + 1
    If CurrentEvent = 12 Then CurrentEvent = 1
    If CurrentEvent = 11 AND EventsFinished <10 Then
        CurrentEvent = 1
    End If
    If Events(nPlayer,CurrentEvent) = 1 Then
        LightNextEvent2
    Else
        EventLights(CurrentEvent).State = 2
    End If
End Sub

Sub UpdateMainEventLights
	Dim i,j
		For j = 0 To 11
			Select Case Events(nPlayer,j)
				Case 0
					EventLights(j).State = 0
				Case 1
					EventLights(j).State = 1
				Case 2
					EventLights(j).State = 2
			End Select
		Next

	LightNextEvent
End Sub

Sub UpdateDMDMainProgress
	if VRROOM > 0 Then
		PuPlayer.LabelSet pDMD, "MainModeProgress", nMainProgress &"/" & asMainProgressMessage(CurrentEvent), 1, "{'mt':2,'size':"& VR_MessageSize &"}"	
	Else
		PuPlayer.LabelSet pDMD, "MainModeProgress", nMainProgress &"/" & asMainProgressMessage(CurrentEvent), 1, ""
	End If
end Sub


Sub UpdateDMDSideProgress
	if VRROOM > 0 Then
		PuPlayer.LabelSet pDMD, "SideEventProgress", nSideProgress &"/" & asSideProgressMessage(SideEventNr), 1, "{'mt':2,'size':"& VR_MessageSize &"}"	
	Else
		PuPlayer.LabelSet pDMD, "SideEventProgress", nSideProgress &"/" & asSideProgressMessage(SideEventNr), 1, ""
	End If
End Sub

Sub StartMainEvent
    If EventStarted OR EventsFinished = 10 Then ' another event is started so just return the ball
        EjectBallsInDeskDelay = EjectBallValue              'shoot back the ball
    Else

	Dbg "Starting Main Event " &CurrentEvent

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Yellow}:"&asModeNames(CurrentEvent) & " Started"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
			StopAllMusic
			nMainProgress = 0
			UpdateDMDMainProgress
			if VRRoom > 0 Then
				PuPlayer.LabelSet pDMD, "MainModeMessage", asMainModeMessages(CurrentEvent), 1, "{'mt':2,'size':"& VR_MessageSize &"}"
			Else
				PuPlayer.LabelSet pDMD, "MainModeMessage", asMainModeMessages(CurrentEvent), 1, ""
			End If

			Select Case CurrentEvent
				Case 1:
					StartEvent1
					GeneralPupQueue.Add "MainEvent1","MainEvent1",80,2000,0,0,0,False			
				Case 2:
					StartEvent2
					GeneralPupQueue.Add "MainEvent2","MainEvent2",80,2000,0,0,0,False		
				Case 3:
					StartEvent3
					GeneralPupQueue.Add "MainEvent3","MainEvent3",80,2000,0,0,0,False		
				Case 4:
					StartEvent4
					GeneralPupQueue.Add "MainEvent4","MainEvent4",80,2000,0,0,0,False		
				Case 5:
					StartEvent5
					GeneralPupQueue.Add "MainEvent5","MainEvent5",80,2000,0,0,0,False		
				Case 6:
					StartEvent6
					GeneralPupQueue.Add "MainEvent6","MainEvent6",80,2000,0,0,0,False		
				Case 7:
					StartEvent7
					GeneralPupQueue.Add "MainEvent7","MainEvent7",80,2000,0,0,0,False		
				Case 8:
					StartEvent8
					GeneralPupQueue.Add "MainEvent8","MainEvent8",80,2000,0,0,0,False		
				Case 9:
					StartEvent9
					GeneralPupQueue.Add "MainEvent9","MainEvent9",80,2000,0,0,0,False		
				Case 10:
					StartEvent10
					GeneralPupQueue.Add "MainEvent10","MainEvent10",80,2000,0,0,0,False		
				Case 11:
					StartEvent11
					GeneralPupQueue.Add "MainEvent11","MainEvent11",80,2000,0,0,0,False		
			End Select
		End If
End Sub

Sub EndMainEvent 'due to time out
		If EventStarted Then
			HideMainModeMessages

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Red}:"&asModeNames(CurrentEvent) & " Ended"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If

			Select Case CurrentEvent
				Case 1:EndEvent1
				Case 2:EndEvent2
				Case 3:EndEvent3
				Case 4:EndEvent4
				Case 5:EndEvent5
				Case 6:EndEvent6
				case 7:EndEvent7:L54.State = 0
				case 8:EndEvent8
				case 9:EndEvent9
				case 10:EndEvent10
				case 11:EndEvent11
			End Select

		End If
End Sub

Sub MainEvent1
	Pupevent 350
	AudioQueue.Add "PlayTheme","PlayTheme",80,8000,0,0,0,False
End Sub

Sub MainEvent2
	Pupevent 351
	AudioQueue.Add "PlayTheme","PlayTheme",80,9000,0,0,0,False
End Sub

Sub MainEvent3
	Pupevent 352
	AudioQueue.Add "PlayTheme","PlayTheme",80,8000,0,0,0,False
End Sub

Sub MainEvent4
	Pupevent 353
	AudioQueue.Add "PlayTheme","PlayTheme",80,9000,0,0,0,False
End Sub

Sub MainEvent5
	Pupevent 354
	AudioQueue.Add "PlayTheme","PlayTheme",80,8000,0,0,0,False
End Sub

Sub MainEvent6
	Pupevent 355
	AudioQueue.Add "PlayTheme","PlayTheme",80,7000,0,0,0,False
End Sub

Sub MainEvent7
	Pupevent 356
	AudioQueue.Add "PlayTheme","PlayTheme",80,8000,0,0,0,False
End Sub

Sub MainEvent8
	Pupevent 357
	AudioQueue.Add "PlayTheme","PlayTheme",80,7000,0,0,0,False
End Sub

Sub MainEvent9
	Pupevent 358
	AudioQueue.Add "PlayTheme","PlayTheme",80,10000,0,0,0,False
End Sub

Sub MainEvent10
	Pupevent 359
	AudioQueue.Add "PlayTheme","PlayTheme",80,7000,0,0,0,False
End Sub

Sub MainEvent11
	Pupevent 360
	AudioQueue.Add "PlayTheme","PlayTheme",80,10000,0,0,0,False
End Sub



Sub ResetEvents 'make ready the next event after playing an event (winning or not)
	Debug.print "RESET EVENTS " &CurrentEvent
	bSkipTargetReset = False
    If nBonusX(nPlayer) mod 3 = 0 Then LitExtraBall
    EventStarted = 0
    EventLights(CurrentEvent).State = 1
    BallhandlingQueue.Add "ResetDroptargets","ResetDroptargets",90,RDTValue,0,0,0,False
    LightNextEvent
    PlayTheme
End Sub

Function EventsFinished
    Dim i, tmp
    tmp = 0
    For i = 1 to 10
        tmp = tmp + Events(nPlayer,i)
    Next

    EventsFinished = tmp
End Function

Sub WinEventLights
    LightSeqWinEvent.Play SeqRandom, 60, , 1000
    GiOn
    SetLamp 1, 0, 1, 8
		Flash1 True
		BallHandlingQueue.Add "Flash1 False","Flash1 False",60,1500,0,0,0,False
    SetLamp 2, 1, 0, 0

	bBackLightsOn = True
	BallHandlingQueue.Add "bBackLightsOn = False","bBackLightsOn = False",60,1000,0,0,0,False
    SetLamp 3, 0, 1, 8
    SetLamp 4, 0, 1, 8
    SetLamp 5, 0, 1, 8
	LampFlasherOn
	BallHandlingQueue.Add "LampFlasherOff","LampFlasherOff",30,1000,0,0,0,False
End Sub

'these are the lines displayed during the main events
'they are updated whenever the score changes
'otherwise they go in a loop until the event is finished



'****************************
' Event 1 : Shoot kill ramp
'****************************
' triggers: LeftRampDone
' lights: l23

Dim Event1Delay

Sub CheckMainEvents
	nBonusX(nPlayer) = 1
Dim i
	For i = 1 to 10
		if MainMode(nPlayer,i) = 1 Then nBonusX(nPlayer) = nBonusX(nPlayer) + 1
	Next
	DisplayBonusValue
End Sub

Sub StartEvent1
    Say "You_better_not_be_comunist", "12131313253423231413325272"
    EventStarted = 1
    l23.State = 2
	l55.state = 2
    l37.State = 0
    l38.State = 0
    l39.State = 0
    l40.State = 0
    l38b.State = 0
    l39b.State = 0
    l40b.State = 0
    EjectBallsInDeskDelay = EjectBallValue
    Event1Delay = Event1Time * 20 '30 seconds
End Sub

Sub WinEvent1
    DOF 137, DOFPulse    
    Say "fuckin_cockroach", "1934242624"
    WinEventLights
    Event1Delay = 0
    Events(nPlayer,1) = 1 'finished
	MainMode(nplayer,1) = 1
	CheckMainEvents
    AddScore ScoreME1Complete
    l23.State = 0
	l55.state = 0

	DisplayMainComplete

    ResetEvents

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Green}:"&asModeNames(CurrentEvent) & " Completed"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub EndEvent1
    Say "You_know_what_you_are_soft", "242673333574324242423241"
    Event1Delay = 0
    Events(nPlayer,1) = 1 'finished event if it is not finished
    l23.State = 0
	l55.state = 0

	DisplayMainFail

    ResetEvents
End Sub

Sub DisplayMainComplete
	if EventStarted = 0 Then Exit Sub
	pDMDSplashLines asModeNames(EventStarted), "COMPLETED", 2, cGold
	GeneralPuPQueue.Add "HideModeComplete","ClearPupSplashMessages",40,2100,0,0,0,False
End Sub

Sub DisplayMainFail
	if bSupressEndEventMessage or EventStarted = 0 Then Exit Sub
	pDMDSplashLines asModeNames(EventStarted), "FAILED", 2, cRed
	GeneralPuPQueue.Add "HideModeFailed","ClearPupSplashMessages",40,2100,0,0,0,False
End Sub
'************************************
' Event 2 : shoot 5 center drop targets
'************************************
' triggers: ct1, ct2, ct3

Dim CTHits, Event2Delay

Sub StartEvent2
	BallHandlingQueue.Add "RaiseBankTargets","RaiseBankTargets",60,100,0,0,0,False
    Say "I_want_to_talk_2_George", "16873332362"
    EventStarted = 2
	DOF 112, DOFPulse
    l38.State = 2
    l39.State = 2
    l40.State = 2
    l38b.State = 2
    l39b.State = 2
    l40b.State = 2
    CTHits = 0
    EjectBallsInDeskDelay = EjectBallValue
    Event2Delay = Event2Time * 20 '45 seconds
End Sub

Sub WinEvent2
    DOF 137, DOFPulse
    Say "Dont_fuck_with_me", "34433337"
    WinEventLights
    Event2Delay = 0
    Events(nPlayer,2) = 1
	MainMode(nplayer,2) = 1
	CheckMainEvents
    AddScore ScoreME2Complete
    l37.State = 0
    l38.State = 0
    l39.State = 0
    l40.State = 0
    l38b.State = 0
    l39b.State = 0
    l40b.State = 0

	DisplayMainComplete

    ResetEvents
	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Green}:"&asModeNames(CurrentEvent) & " Completed"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub EndEvent2
    Say "You_boring_you_know_that", "153634151"
    Event2Delay = 0
    Events(nPlayer,2) = 1
    AddScore ScoreME2Part
    l37.State = 0
    l38.State = 0
    l39.State = 0
    l40.State = 0
    l38b.State = 0
    l39b.State = 0
    l40b.State = 0

	DisplayMainFail

    ResetEvents
End Sub

'************************************
' Event 3 : shoot 3 center mini loops
'************************************
' 3 loops in 45 seconds

Dim Event3Delay

Sub StartEvent3
    Say "Tony_would_like_a_word", "131312363628273414141"
    EventStarted = 3
	DOF 112, DOFPulse
    InitMiniLoops
    l25.State = 2
    l42.State = 2
    EjectBallsInDeskDelay = EjectBallValue
    Event3Delay = Event3Time * 20 '45 seconds
End Sub

Sub WinEvent3
    DOF 137, DOFPulse
    Say "Fuckin_fuck", "29381"
    WinEventLights
    Event3Delay = 0
    Events(nPlayer,3) = 1
	MainMode(nplayer,3) = 1
	CheckMainEvents
    AddScore ScoreME3Complete
    l25.State = 0
    l42.State = 0

	DisplayMainComplete

    ResetEvents
	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Green}:"&asModeNames(CurrentEvent) & " Completed"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub EndEvent3
    Say "You_fuckin_kidding_me_man", "2324133413141"
    Event3Delay = 0
    Events(nPlayer,3) = 1
    AddScore ScoreME3Part
    l25.State = 0
    l42.State = 0

	DisplayMainFail

    ResetEvents
End Sub

'************************************
' Event 4 : shoot the spinner
'************************************
' 50 hits in 45 seconds

Dim rottHits, Event4Delay

Sub StartEvent4
    Say "I_tell_you_something_fuck_you_man", "232414239535251"

	DOF 112, DOFPulse
    EventStarted = 4
    rottHits = 0
    l16.State = 2
    EjectBallsInDeskDelay = EjectBallValue
    Event4Delay = Event4Time * 20 '45 seconds
End Sub

Sub WinEvent4
    DOF 137, DOFPulse
    Say "How_about_we_get_some_ice_cream", "2792131314141413131131"
    WinEventLights
    Event4Delay = 0
    Events(nPlayer,4) = 1
	MainMode(nplayer,4) = 1
	CheckMainEvents
    AddScore ScoreME4Complete
    rottHits = 0
    l16.State = 0

	DisplayMainComplete

    ResetEvents
	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Green}:"&asModeNames(CurrentEvent) & " Completed"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub EndEvent4
    Say "I_dont_give_a_fuck", "262423141"
    Event4Delay = 0
    Events(nPlayer,4) = 1
    AddScore ScoreME4Part
    rottHits = 0
    l16.State = 0

	DisplayMainFail

    ResetEvents

End Sub

Sub CheckrottHits
    rottHits = rottHits + 1
    If rottHits >= 30 Then
        WinEvent4
    End If
End Sub

'************************************
' Event 5 : shoot 50 bumpers
'************************************

Dim Event5Bumps, Event5Delay

Sub StartEvent5
    Say "You_know_what_you_need", "26279965342515351"
    EventStarted = 5
	RaiseDiverterPin True
    Event5Bumps = 0
    SetLamp 2, 1, 0, 0
    SetLamp 3, 0, 1, -1
	bBackLightsOn = True
        bumpersmalllight1.State = 2
        bumpersmalllight2.State = 2
        bumpersmalllight3.State = 2
        bumperbiglight1.State = 2
        bumperbiglight2.State = 2
        bumperbiglight3.State = 2
    l53.State = 2
	l16.State = 2
    EjectBallsInDeskDelay = EjectBallValue
    Event5Delay = Event5Time * 20 '45 seconds
End Sub

Sub WinEvent5
	bBackLightsOn = False
	RaiseDiverterPin False
    DOF 137, DOFPulse
    Say "Boyfriend_fuck_him", "191589159218121813"
    WinEventLights
    Events(nPlayer,5) = 1
	MainMode(nplayer,5) = 1
	CheckMainEvents
    Event5Delay = 0
    SetLamp 2, 0, 0, 0
	bBackLightsOn = False
    SetLamp 3, 0, 0, 0
    AddScore ScoreME5Complete
        bumpersmalllight1.State = 0
        bumpersmalllight2.State = 0
        bumpersmalllight3.State = 0
        bumperbiglight1.State = 0
        bumperbiglight2.State = 0
        bumperbiglight3.State = 0
    l53.State = 0
	l16.State = 0

	DisplayMainComplete

    ResetEvents
	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Green}:"&asModeNames(CurrentEvent) & " Completed"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub EndEvent5
    Say "Its_obvious_you_got_an_attitude", "131414242224231414141"
	RaiseDiverterPin False
    Events(nPlayer,5) = 1
    Event5Delay = 0
    SetLamp 2, 0, 0, 0
	bBackLightsOn = False
    SetLamp 3, 0, 0, 0
    AddScore ScoreME5Part
        bumpersmalllight1.State = 0
        bumpersmalllight2.State = 0
        bumpersmalllight3.State = 0
        bumperbiglight1.State = 0
        bumperbiglight2.State = 0
        bumperbiglight3.State = 0
    l53.State = 0
	l16.State = 0

	DisplayMainFail

    ResetEvents
End Sub

Sub CheckEvent5Bumps
    Event5Bumps = Event5Bumps + 1
    If Event5Bumps >= 15 Then
        WinEvent5
    End If
End Sub

'*********************************************************
' Event 6 : drop 3 center droptargets and shoot left ramp
'*********************************************************

Dim Event6Drop, Event6Delay

Sub StartEvent6
    Say "You_fuckin_prick", "232423441"
    EventStarted = 6
    'prepare drop targets
	DOF 112, DOFContactors
	' Make sure bank targets reset

    l38.State = 2
    l39.State = 2
    l40.State = 2
    l38b.State = 2
    l39b.State = 2
    l40b.State = 2
	l55.state = 0
    l37.State = 0
    Event6Drop = 0
    EjectBallsInDeskDelay = EjectBallValue
    Event6Delay = Event6Time * 20 '45 seconds

End Sub

Sub RaiseDTs
    PlaySoundat SoundFX("fx_resetdrop", DOFContactors), deskin
    DroppedTargets(0) = 0
    DroppedTargets(1) = 0
    DroppedTargets(2) = 0
    DroppedTargets(3) = 0
	RandomSoundDropTargetReset ct1p
	DTRaise 1
	DTRaise 2
	DTRaise 3
End Sub

Sub WinEvent6
    DOF 137, DOFPulse
    Say "Who_you_think_you_fuckin_with", "1324313131313761"
    WinEventLights
    Events(nPlayer,6) = 1
	MainMode(nplayer,6) = 1
	CheckMainEvents
    Event6Delay = 0
    l23.State = 0
	l55.state = 0

	DisplayMainComplete

    AddScore ScoreME6Complete
    ResetEvents
	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Green}:"&asModeNames(CurrentEvent) & " Completed"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub EndEvent6
    Say "You_just_fucked_up_man", "24251423341"
    Events(nPlayer,6) = 1
    Event6Delay = 0
    l23.State = 0
	l55.state = 0

	DisplayMainFail

    AddScore ScoreME6Part
    ResetEvents
End Sub

Sub CheckEvent6
    DroppedTargets(0) = DroppedTargets(1) + DroppedTargets(2) + DroppedTargets(3)
    If DroppedTargets(0) = 3 Then 'enable the left ramp score
	l23.State = 2
	l55.state = 2
        l38.State = 0
        l39.State = 0
        l40.State = 0
		l38b.State = 0
		l39b.State = 0
		l40b.State = 0
        Event6Drop = 1
        'UpdateEventsDMD
			if VRRoom > 0 Then
				PuPlayer.LabelSet pDMD, "MainModeMessage", asMainModeMessages2(6), 1, "{'mt':2,'size':"& VR_MessageSize &"}"
			Else
				PuPlayer.LabelSet pDMD, "MainModeMessage", asMainModeMessages2(6), 1, ""
			End If
    End If
End Sub

'************************************************
' Event 7: shoot right ramp and left bank targets
'************************************************

Dim Event7State, Event7Hits, Event7Delay

Sub StartEvent7
    Say "What_are_you_going_to_do_for_me", "1312121312131"
    EventStarted = 7 
    Event7Hits = 0
    PrepareBank
    EjectBallsInDeskDelay = EjectBallValue
    Event7Delay = Event7Time * 20 '45 seconds
End Sub

Sub WinEvent7
    DOF 137, DOFPulse
    Say "I_like_you_your_like_a_tiger", "2713931234131"
    WinEventLights
    Events(nPlayer,7) = 1
	MainMode(nplayer,7) = 1
	CheckMainEvents
    Event7Delay = 0
    InitBank
    l48.State = 0

	DisplayMainComplete

    ResetEvents
    AddScore ScoreME7Complete
	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Green}:"&asModeNames(CurrentEvent) & " Completed"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub EndEvent7
    Say "Go_fuck_yourself", "353528"
    Events(nPlayer,7) = 1
    Event7Delay = 0
    InitBank
    l48.State = 0

	DisplayMainFail

    ResetEvents
    AddScore ScoreME7Part
End Sub

Sub CheckEvent7
    Select Case Event7Hits
        Case 0 'bank
            If Event7State = 0 Then
                Event7Hits = 1
                PrepareRRamp
            End If
        Case 1 'Right ramp
            If Event7State = 1 Then
                Event7Hits = 2
                PrepareBank
            End If
        Case 2 'bank
            If Event7State = 0 Then
                Event7Hits = 3
                PrepareRRamp
            End If
        Case 3 'Right ramp win
            If Event7State = 1 Then
                WinEvent7
            End If
    End Select
End Sub

Sub PrepareBank
    Event7State = 0 'bank
    l11.State = 2
    l12.State = 2
    l13.State = 2
    l14.State = 2
    l48.State = 0
End Sub

Sub PrepareRRamp
    Event7State = 1 'right ramp
    l11.State = 0
    l12.State = 0
    l13.State = 0
    l14.State = 0
    l48.State = 2
End Sub

'************************************
' Event 8 : car skillshot
'************************************
Dim Event8Delay

Sub StartEvent8
    Say "Have_my_car_set_to_me_ok", "24152414141726"
    EventStarted = 8
    LeftFlipper.RotateToStart
    RightFlipper.RotateToStart
    l17.State = 2
    SetLamp 4, 1, 0, 0
    Event8Delay = Event8Time * 20 '30 seconds
    BallsInDesk = BallsInDesk - 1
    NewBall8
End Sub

Sub WinEvent8
    WinEventLights
    Events(nPlayer,8) = 1
	MainMode(nplayer,8) = 1
	CheckMainEvents
    ResetEvents
    Event8Delay = 0 'just to be sure it is 0
    l17.State = 0

	DisplayMainComplete

    SetLamp 4, 0, 0, 0
	bSupressMainEvents = False
    AddScore ScoreME8Complete
	CarHoleDelay = 20
	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Green}:"&asModeNames(CurrentEvent) & " Completed"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub EndEvent8
    Say "Bring_it_to_me_cockroach", "1622111115121"
    Events(nPlayer,8) = 1
	MainMode(nplayer,8) = 1
	CheckMainEvents
    Event8Delay = 0 'just to be sure it is 0
    l17.State = 0

	if BallinPlunger Then 
		AutoPlungerReady = 1
		AutoPlungerDelay = 60
	End If

	DisplayMainFail

    SetLamp 4, 0, 0, 0
    ResetEvents
	bSupressMainEvents = False
End Sub

Sub NewBall8
    If EventStarted = 8 Then AutoPlungerReady = 0
    If EventStarted = 8 And Event8Delay = 0 Then EndEvent8
    BallRelease.createball
    BallsOnPlayfield = BallsOnPlayfield + 1
    BallRelease.Kick 90, 8
    'PlaySoundat "fx_ballrel", ballrelease
	RandomSoundBallRelease ballrelease ' Fleep
End Sub

'************************************
' Event 9 : 5 car hits
'************************************
Dim Event9Delay

Sub StartEvent9
    Say "My_name_is_Tony_M", "2414133413341"
    EventStarted = 9
    CarHits = 0
    l17.State = 2
    EjectBallsInDeskDelay = EjectBallValue
    Event9Delay = Event9Time * 20 '45 seconds
End Sub

Sub WinEvent9
    DOF 137, DOFPulse
    Say "I_told_you_not_to_fuck_with_me", "2335145336543332151"
    WinEventLights
    Event9Delay = 0
    Events(nPlayer,9) = 1
	MainMode(nplayer,9) = 1
	CheckMainEvents
    CarHits = 0
    l17.State = 0

	DisplayMainComplete

    AddScore ScoreME9Complete
    ResetEvents
	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Green}:"&asModeNames(CurrentEvent) & " Completed"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub EndEvent9
    Say "Shit_ass_fuck", "362434"
    Event9Delay = 0
    Events(nPlayer,9) = 1
    CarHits = 0
    l17.State = 0

	DisplayMainFail

    AddScore ScoreME9Part
    ResetEvents
End Sub

'************************************
' Event 10 : shoot all shots
'************************************
' not timed. Ends when ball drains

Dim Event10Hits(8)

Sub StartEvent10
    Dim i
	BallHandlingQueue.Add "RaiseBankTargets","RaiseBankTargets",60,100,0,0,0,False
    Say "Ok_so_why_dont_we_get_together", "322384121413141"
    EventStarted = 10
    For i = 0 to 8
        Event10Hits(i) = 0
    Next

    l11.State = 2
    l12.State = 2
    l13.State = 2
    l14.State = 2

    l38.State = 2
    l39.State = 2
    l40.State = 2
    l38b.State = 2
    l39b.State = 2
    l40b.State = 2

    l16.State = 2
    l17.State = 2
    l23.State = 2
    l25.State = 2
    l37.State = 2
    l42.State = 2
    l48.State = 2
    l53.State = 2
    EjectBallsInDeskDelay = EjectBallValue
End Sub

Sub WinEvent10
    DOF 137, DOFPulse
    Say "Yeah_its_Tony", "3633371"
    WinEventLights
    l16.State = 0
    l17.State = 0
    l23.State = 0
    l25.State = 0
    l37.State = 0
    l42.State = 0
    l48.State = 0
    l53.State = 0

	DisplayMainComplete

    Events(nPlayer,10) = 1
	MainMode(nplayer,10) = 1
	CheckMainEvents
    AddScore ScoreME10Complete
    ResetEvents
	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Green}:"&asModeNames(CurrentEvent) & " Completed"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub EndEvent10
	Say "Im_fucked", "5417191"
    l16.State = 0
    l17.State = 0
    l23.State = 0
    l25.State = 0
    l37.State = 0
    l42.State = 0
    l48.State = 0
    l53.State = 0

	DisplayMainFail

    Events(nPlayer,10) = 1
    AddScore ScoreME10Part
    ResetEvents
End Sub

Sub CheckEvent10
    Dim i
    Event10Hits(0) = 0
    For i = 1 to 8
        Event10Hits(0) = Event10Hits(0) + Event10Hits(i)
    Next

	nMainProgress = Event10Hits(0)
	UpdateDMDMainProgress
    Select Case Event10Hits(0)
        Case 1:PlaySound "Drum2"
        Case 2:PlaySound "Drum2"
        Case 3:PlaySound "Drum2"
        Case 4:PlaySound "Drum2"
        Case 5:PlaySound "Drum2"
        Case 6:PlaySound "Drum2"
        Case 7:PlaySound "Drum2"
        Case 8:PlaySound "drumlong":WinEvent10
    End Select
End Sub

'*******************************************
' Event 11 : shoot 10 Desk shots + multiball
'*******************************************

Dim DeskHits, StartEvent11Delay, Event11VoiceDelay, EndEvent11Delay, Event11Kill, EndWinEvent11Delay

Sub StartEvent11Pre
	body.Visible = 0
	GunUp.Visible = 1
	TonyStandUpTimer.Enabled = 1
	BallHandlingQueue.Add "StartEvent11","StartEvent11",90,500,0,0,0,False
End Sub

Sub StartEvent11
    Dim i
    For i = 0 To 11
        EventLights(CurrentEvent).State = 0
    Next
 
    Say "sayhello2", "1415142926161"
    EventStarted = 11
    DropAll "StartEvent11"       ' drop all targets
    BackDoorPost.IsDropped = 1:DOF 136, DOFPulse
    CloseLockBlock 'close the lock if it was open
    l36.State = 2
    l37.State = 2
    l38.State = 0
    l39.State = 0
    l40.State = 0
    l38b.State = 0
    l39b.State = 0
    l40b.State = 0

	bBackLightsOn = True
    SetLamp 3, 0, 2, -1
    DeskHits = 0
    Event11Kill = 0

    StartScarShooting 4
    BallsInDesk = 0
    Event11VoiceDelay = 2100
	BallSaved  = 1
	BallSavedDelay = 800
    EndEvent11Delay = Event11Time * 20 '3 minutes to complete the event
End Sub

Sub WinEvent11
    'simulate tilt to turn all lights and flippers off
	TonySitDownTimer.Enabled = 0
	DOF 137, DOFPulse
    Tilted = True
    DOF 210, DOFOff
    GiOff
    LightSeqTilt.Play SeqAllOff
    TiltObjects 1
    'eject balls under the desk
    EjectBallsInDeskDelay = EjectBallValue
    BallsRemaining = BallsRemaining + 1 'increase one ball to replay the last ball
    PlaySound "tonydeath"
    Event11VoiceDelay = 0
    EndEvent11Delay = 0
    EndWinEvent11Delay = 240
    l37.State = 0
    l25.State = 0
    l42.State = 0
    l24.State = 0
    l41.State = 0

	DisplayMainComplete

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Green}:"&asModeNames(CurrentEvent) & " Completed"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub EndWinEvent11
	bBackLightsOn = False
    SetLamp 3, 0, 0, 0

    BallhandlingQueue.Add "ResetDroptargets","ResetDroptargets",90,RDTValue,0,0,0,False
    InitEvents
    PlayTheme
End Sub

Sub EndEvent11 'executed from the drain when all multiballs drained
	Dbg "Ending EVENT 11"
	TonySitDownTimer.Enabled = 1
	bBackLightsOn = False
    SetLamp 3, 0, 0, 0
    Say "saygoodnight", "251414152424231"
    Event11VoiceDelay = 0
    BallhandlingQueue.Add "ResetDroptargets","ResetDroptargets",90,RDTValue,0,0,0,False
    InitEvents
    l37.State = 0
    l25.State = 0
    l42.State = 0
    l24.State = 0
    l41.State = 0

	DisplayMainFail

    PlayTheme
End Sub

Sub NextPartEvent11
    BallhandlingQueue.Add "ResetDroptargets","ResetDroptargets",90,RDTValue,0,0,0,False
    l25.State = 2
    l42.State = 2
    l24.State = 2
    l41.State = 2
    l37.State = 0
    l38.State = 0
    l39.State = 0
    l40.State = 0
    l38b.State = 0
    l39b.State = 0
    l40b.State = 0
    BackDoorPost.IsDropped = 0:DOF 136, DOFPulse
    Event11Kill = 1
    StartScarShooting BallsInDesk
    BallsInDesk = 0
End Sub

Sub SayEvent11
    Dim tmp
    tmp = INT(6 * RND(1) )
    Select Case tmp
        Case 0:Say "You_know_who_your_fuckin_with", "25162424261"
        Case 1:Say "You_need_an_army_to_take_me", "13151414141413331"
        Case 2:Say "You_picked_the_wrong_guy_to_fuck_with", "23232523233334261"
        Case 3:Say "You_think_you_can_fuck_with_me", "2414141423261"
        Case 4:Say "Cmon_come_and_get_me", "12132643233414"
        Case 5:Say "Cmon_bring_your_army", "13144814"
    End Select
End Sub

'*******************************
' Easter Egg: shoot 100 targets
'*******************************
' 3 ball multiball, ballsave 1 minute, hit 100 targets

Dim EE, TempEgg, EndEEDelay, EndEEDelay2, StartEasterEggShootingDelay

Sub InitEasterEgg
    TempEgg = ""
    EE = 0
    EndEEDelay = 0
    EndEEDelay2 = 0
    StartEasterEggShootingDelay = 0
End Sub

Sub CheckStartEgg
	if BallsOnPlayfield <> 0 Then Exit Sub
    TempEgg = RIGHT(TempEgg, 5)
    If TempEgg = "rlrll" AND Credits> 0 Then
        StartEasterEgg
    End If
End Sub

Sub StartEasterEgg
    AttractMode_Off
	AttractTimer.Enabled = 0
	HidePupSplashMessages

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Pink}:Found The Easter Egg "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
	pDMDSplash3Lines "YOU FOUND" , "", "THE EASTER EGG", 5, cWhite
    'reset everything

    Initialize

    GiOn
    nPlayer = 0

	body.Visible = 0
	GunUp.Visible = 1
	TonyStandUpTimer.Enabled = 1

    StartEasterEggShootingDelay = 50
    StartBallSaved 60
    EndEEDelay = 120 * 20
    SwitchMusic "bgout_scarface_EE"
    EE = 1        'easter egg is active
    l26.State = 0 'turn off the first event light

	bBackLightsOn = True
    SetLamp 3, 0, 1, -1
    CTDelay = 0
    EasterLights
End Sub

Sub EasterLights
    LightSeqEE.Play SeqUpOn, 50, , 1
End Sub

Sub EasterLightsOff
    LightSeqEE.StopPlay
End Sub

Sub StartEasterEggShooting
	'debug.print "EE WTF"
    StartScarShooting 3
    BallsinDesk = 0
End Sub

Sub PlayRandomPunch
    Dim i
    i = INT(RND(1) * 3)
    Select case i
        Case 0:PlaySound "punch1"
        Case 1:PlaySound "punch1"
        Case 2:PlaySound "punch1"
    End Select
End Sub

Sub EndEE
	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Green}:Easter Egg Mode Ended "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
	TonySitDownTimer.Enabled = 1
    GameStarted = 0
	DOF 121, DOFPulse
    AllLampsOff
    GiOff
    LeftFlipper.RotateToStart
    RightFlipper.RotateToStart
    If Score(0) >= 200 Then
        'addcredit
        credits = credits + 1:DOF 132, DOFOn
		pDMDSplashBig "NEW HIGH SCORE", 22, cWhite
		GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",80,22100,0,0,0,False
		DOF 137, DOFPulse
		EEHighScore = Score(0)
        savehs()
    Else
        credits = credits - 1
		pDMDSplashBig "YOU LOSE", 22, cWhite
		GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",80,22100,0,0,0,False
    End If
    EndEEDelay2 = 50
	bBackLightsOn = False
    SetLamp 3, 0, 0, 0
    EasterLightsOff
	DOF 222, DOFOff
End Sub


'******************************************************************
' JP's Fading Lamps for Original tables
' v7.0 for VP9 Only Fading Lights
' Used mostly for flash effects
' Based on PD's Fade Lights System
'******************************************************************
' Syntax:
' SetLamp nr, a, b, c
' a 0 is Off, 1 is On
' b 0 is no blink, 1 is blink, 2 is fast blink
' c number of blinks, -1 is until stopped
' LState(x) final lamp state
' LFade(x)  Fading step
' LBlink(x) nr of blinks
' example:
' setlamp 64, 0, 2, 10
' this is turn off the lamp 64 but first it blinks fast 10 times
' setlamp 64, 1, 2, 10
' this is turn on the lamp 64 but first it blinks fast 10 times
'******************************************************************

Dim LState(200), LFade(200), LBlink(200)

ForceAllLampsOff()
LampTimer.Interval = 40
LampTimer.Enabled = 1

Sub LampTimer_Timer()


	FadeL 3, Light003 'BackWall Bulbs
	FadeLm 4, Light004a 'Car Lights
	FadeL 4, Light004b 'Car Lights

End Sub

Sub AllLampsOff()
    Dim x
    For x = 0 to 200
        SetLamp x, 0, 0, 0
    Next
End Sub

Sub ForceAllLampsOff()
    Dim x
    For x = 0 to 200
        LFade(x) = 8
    Next
End Sub

Sub SetLamp(nr, value, blink, repeat)
    If value> 1 or blink> 2 Then Exit Sub
    If blink = 0 AND value = 0 AND LFade(nr) = 0 Then Exit Sub 'the lamp is already Off and there is no blink so do nothing
    LState(nr) = value
    LBlink(nr) = repeat
    LFade(nr) = value + 2 * blink + 2
    Select Case LFade(nr)
        Case 2:LFade(nr) = 8     'off
        Case 3:LFade(nr) = 11    'on
        Case 4, 5:LFade(nr) = 12 'Blink
        Case 6, 7:LFade(nr) = 16 'Blink fast
    End Select
End Sub

'Lights used 

Sub FadeL(nr, a)
    Select Case LFade(nr)
        'fade to off
        Case 8:a.State = 0:LFade(nr) = 9
        Case 9:'b.State = 1:LFade(nr) = 10
        Case 10:'b.State = 0:LFade(nr) = 0
        'turn on
        Case 11:a.State = 0:a.State = 1:LFade(nr) = 1
        'blink
        Case 12:a.State = 1:LFade(nr) = 13
        Case 13:a.State = 0:LFade(nr) = 14
        Case 14:'b.State = 1:LFade(nr) = 15
        Case 15:'b.State = 0
            LBlink(nr) = LBlink(nr) -1
            If LBlink(nr) = 0 Then
                LFade(nr) = 8 + 3 * LState(nr)
            Else
                LFade(nr) = 12
            End If
        'blink fast
        Case 16:a.State = 1:LFade(nr) = 17
        Case 17:a.state = 0
            LBlink(nr) = LBlink(nr) -1
            If LBlink(nr) = 0 Then
                LFade(nr) = 8 + 3 * LState(nr)
            Else
                LFade(nr) = 16
            End If
    End Select
End Sub

Sub FadeLm(nr, a)
    Select Case LFade(nr)
        'fade to off
        Case 8:a.State = 0
        Case 9:'b.State = 1
        Case 10:'b.State = 0
        'turn on
        Case 11:a.State = 0:a.State = 1
        'blink
        Case 12:a.State = 1
        Case 13:a.State = 0
        Case 14:'b.State = 1
        Case 15:'b.State = 0
        'blink fast
        Case 16:a.State = 1
        Case 17:a.State = 0
    End Select
End Sub


Dim bBackLightsOn
dim backlights(6)

		backLights(0) = 7
		backLights(1) = 7
		backLights(2) = 7
		backLights(3) = 7
		backLights(4) = 7
		backLights(5) = 7

Sub ImageLights_Timer()
	dim obj, idx
	For Each obj in BackwallBulbs
		idx = obj.DepthBias 
		'dbg "IDX:" &idx
		If backlights(idx) > 0 Then
			If backlights(idx) = 8 Then 
				obj.image = "simplelightwhite7" : obj.blenddisablelighting = 2'1
				'Dbg "Image:" &obj.image
			Else 
				backlights(idx) = backlights(idx) - 1 : obj.image = "simplelightwhite" & backlights(idx) : obj.blenddisablelighting = backlights(idx) / 4 - 0.2'/ 7 + 0.3
				'Dbg "Image:" &obj.image
			End If
		Else
			if bBackLightsOn Then
				FireBackglass
				backLights(0) = 7
				backLights(1) = 7
				backLights(2) = 7
				backLights(3) = 7
				backLights(4) = 7
				backLights(5) = 7

				Flasherlight3a.state = 1
				Flasherlight3b.state = 1
				Flasherlight3c.state = 1
				Flasherlight3d.state = 1
				Flasherlight3e.state = 1
				Flasherlight3f.state = 1

			Else
				Flasher3a.visible = 0
				Flasher3b.visible = 0
				Flasher3c.visible = 0
				Flasher3d.visible = 0
				Flasher3e.visible = 0
				Flasher3f.visible = 0

				Flasherlight3a.state = 0
				Flasherlight3b.state = 0
				Flasherlight3c.state = 0
				Flasherlight3d.state = 0
				Flasherlight3e.state = 0
				Flasherlight3f.state = 0
			End If
		End If
	Next

End Sub

Sub LampFlasherOn
	Flasher5a.visible = 1
	Flasher5b.visible = 1
	Flasher5c.visible = 1
	Flasher5d.visible = 1
	Flasher5e.visible = 1

	'light005a.state = 1
	'light005b.state = 1
	light005c.state = 1
	light005d.state = 1
	light005e.state = 1
End Sub

Sub LampFlasherOff
	Flasher5a.visible = 0
	Flasher5b.visible = 0
	Flasher5c.visible = 0
	Flasher5d.visible = 0
	Flasher5e.visible = 0

	light005a.state = 0
	light005b.state = 0
	light005c.state = 0
	light005d.state = 0
	light005e.state = 0
End sub

'******************************
' Diverse Collection Hit Sounds
'******************************

'Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
'Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
'Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
'Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
'Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
'Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
'Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub




Dim objIEDebugWindow
Sub Dbg( myDebugText )
' Uncomment the next line to turn off debugging
Exit Sub

If Not IsObject( objIEDebugWindow ) Then
Set objIEDebugWindow = CreateObject( "InternetExplorer.Application" )
objIEDebugWindow.Navigate "about:blank"
objIEDebugWindow.Visible = True
objIEDebugWindow.ToolBar = False
objIEDebugWindow.Width = 600	
objIEDebugWindow.Height = 900
objIEDebugWindow.Left = 4100
objIEDebugWindow.Top = 100
Do While objIEDebugWindow.Busy
Loop
objIEDebugWindow.Document.Title = "My Debug Window"
objIEDebugWindow.Document.Body.InnerHTML = "<b>Scarface Debug Window -TimeStamp: " & GameTime& "</b></br>"
End If

objIEDebugWindow.Document.Body.InnerHTML = objIEDebugWindow.Document.Body.InnerHTML & myDebugText & " --TimeStamp:<b> " & GameTime & "</b><br>" & vbCrLf
End Sub

Sub Dbg2( myDebugText )
' Uncomment the next line to turn off debugging
Exit Sub

If Not IsObject( objIEDebugWindow ) Then
Set objIEDebugWindow = CreateObject( "InternetExplorer.Application" )
objIEDebugWindow.Navigate "about:blank"
objIEDebugWindow.Visible = True
objIEDebugWindow.ToolBar = False
objIEDebugWindow.Width = 600	
objIEDebugWindow.Height = 900
objIEDebugWindow.Left = 4100
objIEDebugWindow.Top = 100
Do While objIEDebugWindow.Busy
Loop
objIEDebugWindow.Document.Title = "My Debug Window"
objIEDebugWindow.Document.Body.InnerHTML = "<b>Scarface Debug Window -TimeStamp: " & GameTime& "</b></br>"
End If

objIEDebugWindow.Document.Body.InnerHTML = objIEDebugWindow.Document.Body.InnerHTML & myDebugText & " --TimeStamp:<b> " & GameTime & "</b><br>" & vbCrLf
End Sub

'********************* START OF PUPDMD FRAMEWORK v3.0 BETA *************************
'******************************************************************************
'*****   Create a PUPPack within PUPPackEditor for layout config!!!  **********
'******************************************************************************
'
'
'  Quick Steps:
'      1>  create a folder in PUPVideos with Starter_PuPPack.zip and call the folder "yourgame"
'      2>  above set global variable pGameName="yourgame"
'      3>  copy paste the settings section above to top of table script for user changes.
'      4>  on Table you need to create ONE timer only called pupDMDUpdate and set it to 250 ms enabled on startup.
'      5>  go to your table1_init or table first startup function and call PUPINIT function
'      6>  Go to bottom on framework here and setup game to call the appropriate events like pStartGame (call that in your game code where needed)...etc
'      7>  attractmodenext at bottom is setup for you already,  just go to each case and add/remove as many as you want and setup the messages to show.  
'      8>  Have fun and use pDMDDisplay(xxxx)  sub all over where needed.  remember its best to make a bunch of mp4 with text animations... looks the best for sure!
'
'
'Note:  for *Future Pinball* "pupDMDupdate_Timer()" timer needs to be renamed to "pupDMDupdate_expired()"  and then all is good.
'       and for future pinball you need to add the follow lines near top
'Need to use BAM and have com idll enabled.
'				Dim icom : Set icom = xBAM.Get("icom") ' "icom" is name of "icom.dll" in BAM\Plugins dir
'				if icom is Nothing then MSGBOX "Error cannot run without icom.dll plugin"
'				Function CreateObject(className)       
'   					Set CreateObject = icom.CreateObject(className)   
'				End Function
'*******************************************
' ZPUP - PinUp display
'*******************************************
Dim pDMDlastchk: pDMDLastchk= -1    'performance of updates
Dim pDMDCurPage: pDMDCurPage= 0     'default page is empty.
Dim PBackglassCurPage: PBackglassCurPage= 0     'default page is empty.
Dim pInAttract : pInAttract=false   'pAttract mode
Dim pFrameSizeX: pFrameSizeX=1920     'DO NOT CHANGE, this is pupdmd author framesize
Dim pFrameSizeY: pFrameSizeY=1080     'DO NOT CHANGE, this is pupdmd author framesize
Dim pUseFramePos : pUseFramePos=1     'DO NOT CHANGE, this is pupdmd author setting

Dim PuPlayer
' Variables used to setup screens and minigame

Sub ResetOverlay
	PuPlayer.playlistplayex pDMD,"pupoverlays","DMD.png",0,1
End Sub


Sub PuPInit
dbg "Starting PuP"

	Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")   

	'' needed to setup screen info for minigame & pup
	' pupPackScreenFile = PuPlayer.GetRoot & "scarface\ScreenType.txt"
	' Set ObjFso = CreateObject("Scripting.FileSystemObject")
	' Set ObjFile = ObjFso.OpenTextFile(pupPackScreenFile)
	' DMDType = ObjFile.ReadLine

	DMDType = 2

	if DMDType > 1 Then
		bIsMiniGameActive = 1
		PupScreenMiniGame = 5
		pDMD = 5
	Else
		bIsMiniGameActive = 1
		PupScreenMiniGame = 2
		pDMD = 2
	End If


	CheckPupVersion ' ensure we are using the correct version of Pup Player for the Table1
	PuPlayer.B2SInit "", pGameName
	PuPlayer.LabelInit pDMD		
	PuPlayer.LabelInit pBackglass		
	pSetPageLayouts
'	pDMDSetPage pAttract, "pupinit"   'set blank text overlay page.
	pDMDSetPage 1, "pupinit"

	if ScorbitActive Then
		pBackglassSetPage 1
		dbg "Delaying Pup startup"
		BallHandlingQueue.Add "delayStartup","delayStartup",24,2000,0,0,0,False
	Else
		pDMDStartUP
	End If


	if Scorbitactive then 
'		if Scorbit.DoInit(4429, "PupOverlays", Version, "scarface-opdb") then 	' Staging
		if Scorbit.DoInit(4375, "PupOverlays", Version, "scarface-opdb") then 	' Prod
			tmrScorbit.Interval=2000
			tmrScorbit.UserValue = 0
			tmrScorbit.Enabled=True 
			Scorbit.UploadLog = ScorbitUploadLog
		End if 
	End if 

	ResetOverlay ' added 101O
End Sub


Sub delayStartup
	pDMDStartUP                 ' firsttime running for like an startup video..
end sub

Sub pDMDStartUP
'exit sub
dbg "In DMD statup"
	pInAttract = True

	if ScorbitActive Then
		dbg "Calling SCORBIT check pairing"
		BallHandlingQueue.Add "CheckPairing","CheckPairing",24,2000,0,0,0,False
	End If


end Sub 'end DMDStartup


'***********************************************************PinUP Player DMD Helper Functions
Sub DMDUpdateBallNumber(nBallNr)
	if VRROOM > 0 Then
			PuPlayer.LabelSet pDMD,"BallValue","Ball " &nBallNr,1,"{'mt':2,'size':"& VR_BallValue &",'xalign':0,'yalign':0,'ypos':76.6,'xpos':62.25}"
	Else
		PuPlayer.LabelSet pDMD,"BallValue","Ball " &nBallNr,1,"{'mt':2,'size':3,'xalign':0,'yalign':0,'ypos':76.6,'xpos':65}"
	End If
End Sub

Sub DMDClearPlayerName
	pDMDLabelHide "CurrName"
	pDMDLabelhide "Bullet1"
	pDMDLabelhide "Bullet2"
	pDMDLabelhide "Bullet3"
	pDMDLabelhide "Bullet4"
	PuPlayer.LabelSet pDMD,"Position1Score","",1,""
	PuPlayer.LabelSet pDMD,"Position2Score","",1,""
	PuPlayer.LabelSet pDMD,"Position3Score","",1,""
	PuPlayer.LabelSet pDMD,"Position4Score","",1,""
End Sub

Sub DMDUpdatePlayerName
	DMDClearPlayerName

	if VRRoom > 0 Then
		PuPlayer.LabelSet pDMD,"CurrScore"," " & FormatScore(Score(nplayer)) & " ",1,"{'mt':2,'size':"& VR_Score &",'xpos':50,'ypos':87.0}"
	Elseif Score(nPlayer) > 999999999 Then 
		PuPlayer.LabelSet pDMD,"CurrScore"," " & FormatScore(Score(nplayer)) & " ",1,"{'mt':2,'size':8,'xpos':50,'ypos':87.0}"
	Elseif nPlayersinGame < 3 Then
		PuPlayer.LabelSet pDMD,"CurrScore"," " & FormatScore(Score(nplayer)) & " ",1,"{'mt':2,'size':10,'xpos':50,'ypos':88.5}"
	Else
		PuPlayer.LabelSet pDMD,"CurrScore"," " & FormatScore(Score(nplayer)) & " ",1,"{'mt':2,'size':9,'xpos':50,'ypos':87.0}"
	End If


	PuPlayer.LabelSet pDMD,"CurrName","Player " &nPlayer,1,"{'mt':2,'color': "&cSilver&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':79.0}"

	Select Case nPlayer
		Case 1
			
			if nPlayersInGame > 1 Then
				PuPlayer.LabelSet pDMD, "Bullet2", "PuPOverlays\\P2.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':76.5,'xpos':26}"
				PuPlayer.LabelSet pDMD,"Position2Score",FormatScore(Score(2)),1,"{'mt':2,'color': "&cBlack&" ,'xalign':0,'yalign':1,'xpos':29,'ypos':79.1}"  
			End If

			if nPlayersInGame > 2 Then
				PuPlayer.LabelSet pDMD, "Bullet3", "PuPOverlays\\P3.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':90.5,'xpos':26}"
				PuPlayer.LabelSet pDMD,"Position3Score",FormatScore(Score(3)),1,"{'mt':2,'color': "&cBlack&" ,'xalign':0,'yalign':1,'xpos':29,'ypos':94.5}"  
			End If

			if nPlayersInGame > 3 Then
				PuPlayer.LabelSet pDMD, "Bullet4", "PuPOverlays\\P4.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':90.5,'xpos':70.25}"
				PuPlayer.LabelSet pDMD,"Position4Score",FormatScore(Score(4)),1,"{'mt':2,'color': "&cBlack&" ,'xalign':2,'yalign':1,'xpos':71,'ypos':94.5}" 
			End If
		Case 2 

			PuPlayer.LabelSet pDMD, "Bullet1", "PuPOverlays\\P1.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':76.5,'xpos':26}"
			PuPlayer.LabelSet pDMD,"Position1Score",FormatScore(Score(1)),1,"{'mt':2,'color': "&cBlack&" ,'xalign':0,'yalign':1,'xpos':29,'ypos':79.1}"  

			if nPlayersInGame > 2 Then
				PuPlayer.LabelSet pDMD, "Bullet3", "PuPOverlays\\P3.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':90.5,'xpos':26}"
				PuPlayer.LabelSet pDMD,"Position3Score",FormatScore(Score(3)),1,"{'mt':2,'color': "&cBlack&" ,'xalign':0,'yalign':1,'xpos':29,'ypos':94.5}"  
			End If

			if nPlayersInGame > 3 Then
				PuPlayer.LabelSet pDMD, "Bullet4", "PuPOverlays\\P4.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':90.5,'xpos':70.25}"
				PuPlayer.LabelSet pDMD,"Position4Score",FormatScore(Score(4)),1,"{'mt':2,'color': "&cBlack&" ,'xalign':2,'yalign':1,'xpos':71,'ypos':94.5}"
			End If

		Case 3
			PuPlayer.LabelSet pDMD, "Bullet1", "PuPOverlays\\P1.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':76.5,'xpos':26}"
			PuPlayer.LabelSet pDMD,"Position1Score",FormatScore(Score(1)),1,"{'mt':2,'color': "&cBlack&" ,'xalign':0,'yalign':1,'xpos':29,'ypos':79.1}"  

			PuPlayer.LabelSet pDMD, "Bullet2", "PuPOverlays\\P2.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':90.5,'xpos':26}"
			PuPlayer.LabelSet pDMD,"Position2Score",FormatScore(Score(2)),1,"{'mt':2,'color': "&cBlack&" ,'xalign':0,'yalign':1,'xpos':29,'ypos':94.5}" 

			if nPlayersInGame > 3 Then
				PuPlayer.LabelSet pDMD, "Bullet4", "PuPOverlays\\P4.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':90.5,'xpos':70.25}"
				PuPlayer.LabelSet pDMD,"Position4Score",FormatScore(Score(4)),1,"{'mt':2,'color': "&cBlack&" ,'xalign':2,'yalign':1,'xpos':71,'ypos':94.5}"
			End If
		Case 4
			PuPlayer.LabelSet pDMD, "Bullet1", "PuPOverlays\\P1.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':76.5,'xpos':26}"
			PuPlayer.LabelSet pDMD,"Position1Score",FormatScore(Score(1)),1,"{'mt':2,'color': "&cBlack&" ,'xalign':0,'yalign':1,'xpos':29,'ypos':79.1}"  

			PuPlayer.LabelSet pDMD, "Bullet2", "PuPOverlays\\P2.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':90.5,'xpos':26}"
			PuPlayer.LabelSet pDMD,"Position2Score",FormatScore(Score(2)),1,"{'mt':2,'color': "&cBlack&" ,'xalign':0,'yalign':1,'xpos':29,'ypos':94.5}"  

			PuPlayer.LabelSet pDMD, "Bullet3", "PuPOverlays\\P3.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':90.5,'xpos':70.25}"
			PuPlayer.LabelSet pDMD,"Position3Score",FormatScore(Score(3)),1,"{'mt':2,'color': "&cBlack&" ,'xalign':2,'yalign':1,'xpos':71,'ypos':94.5}" 
	End Select
End Sub

Sub DMDUpdateAll
	DMDUpdatePlayerName
	DMDUpdateBallNumber Balls
	DisplayBonusValue
	DisplayPlayfieldValue
End Sub

Sub WallsDown
	DebugWall1.Visible = 0
	DebugWall2.Visible = 0
	DebugWall3.Visible = 0
	DebugWall1.isDropped = 1
	DebugWall2.isDropped = 1
	DebugWall3.isDropped = 1
End Sub


'**************************
'   SCORBIT
'**************************
'==================================================================================================================
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  SCORBIT Interface
' To Use:
' 1) Define a timer tmrScorbit
' 2) Call DoInit at the end of PupInit or in Table Init if you are nto using pup with the appropriate parameters
'     Replace 389 with your TableID from Scorbit 
'     Replace GRWvz-MP37P from your table on OPDB - eg: https://opdb.org/machines/2103
'		if Scorbit.DoInit(389, "PupOverlays", "1.0.0", "GRWvz-MP37P") then 
'			tmrScorbit.Interval=2000
'			tmrScorbit.UserValue = 0
'			tmrScorbit.Enabled=True 
'		End if 
' 3) Customize helper functions below for different events if you want or make your own 
' 4) Call 
'		DoInit - After Pup/Screen is setup (PuPInit)
'		StartSession - When a game starts (ResetForNewGame)
'		StopSession - When the game is over (Table1_Exit, EndOfGame)
'		SendUpdate - called when Score Changes (AddScore)
'			SendUpdate(P1Score, P2Score, P3Score, P4Score, CurrentBall, CurrentPlayer, NumberPlayers)
'			Example:  Scorbit.SendUpdate Score(0), Score(1), Score(2), Score(3), Balls, CurrentPlayer+1, PlayersPlayingGame
'		SetGameMode - When different game events happen like starting a mode, MB etc.  (ScorbitBuildGameModes helper function shows you how)
' 5) Drop the binaries sQRCode.exe and sToken.exe in your Pup Root so we can create session tokens and QRCodes.
'	- Drop QRCode Images (QRCodeS.png, QRcodeB.png) in yur pup PuPOverlays if you want to use those 
' 6) Callbacks 
'		Scorbit_Paired   	- Called when machine is successfully paired.  Hide QRCode and play a sound 
'		Scorbit_PlayerClaimed	- Called when player is claimed.  Hide QRCode, play a sound and display name 
'		ScorbitClaimQR		- Call before/after plunge (swPlungerRest_Hit, swPlungerRest_UnHit)
' 7) Other 
'		Set Pair QR Code	- During Attract
'			if (Scorbit.bNeedsPairing) then 
'				PuPlayer.LabelSet pDMDFull, "ScorbitQR_a", "PuPOverlays\\QRcode.png",1,"{'mt':2,'width':32, 'height':64,'xalign':0,'yalign':0,'ypos':5,'xpos':5}"
'				PuPlayer.LabelSet pDMDFull, "ScorbitQRIcon_a", "PuPOverlays\\QRcodeS.png",1,"{'mt':2,'width':36, 'height':85,'xalign':0,'yalign':0,'ypos':3,'xpos':3,'zback':1}"
'			End if 
'		Set Player Names 	- Wherever it makes sense but I do it here: (pPupdateScores)
'		   if ScorbitActive then 
'			if Scorbit.bSessionActive then
'				PlayerName=Scorbit.GetName(CurrentPlayer+1)
'				if PlayerName="" then PlayerName= "Player " & CurrentPlayer+1 
'			End if 
'		   End if 
'
'
'>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
' TABLE CUSTOMIZATION START HERE 

Sub Scorbit_Paired()								' Scorbit callback when new machine is paired 
dbg2 "Scorbit PAIRED"
	PlaySound "scorbit_login"


	MainBG
	GeneralPupQueue.Add "MainBG","MainBG",25,100,0,0,0,False
	pbackglasslabelhide "ScorbitQR1"
End Sub 

Sub Scorbit_PlayerClaimed(PlayerNum, PlayerName)	' Scorbit callback when QR Is Claimed 
dbg2 "Scorbit LOGIN"
	PlaySound "scorbit_login"
	ScorbitClaimQR(False)
End Sub 


Sub ScorbitClaimQR(bShow)	
dbg2 "In ScorbitClaimQR: " &bShow					'  Show QRCode on first ball for users to claim this position
dbg2 "Session Active: " &Scorbit.bSessionActive
dbg2 "bNeedsPairing:" &Scorbit.bNeedsPairing
	if Scorbit.bSessionActive=False then Exit Sub 
	if ScorbitShowClaimQR=False then Exit Sub
	if Scorbit.bNeedsPairing then exit sub 


dbg2 "BShow: " & bShow
dbg2 "nBall: " &currball
dbg2 "bGameInPlay: " & bGameStarted
dbg2 "GetName: " &Scorbit.GetName(nPlayer)

	if bShow and CurrBall=0 and Scorbit.GetName(nPlayer)="" then 
		if DMDType = 0 Then pdmdsetpage 77, "Scorbit"
		PuPlayer.playlistplayex pBackglass,"PuPOverlays","Scorbit_Claim.png",0,1
		PuPlayer.LabelSet pBackglass, "ScorbitQR2", "PuPOverlays\\QRclaim.png",1,"{'mt':2,'width':19.61, 'height':36,'xalign':0,'yalign':0,'ypos':32,'xpos':74.6}"
	Else 
		dbg2 "Hiding QR claim & Overlay"
		HideScorbit
	End if 
End Sub 

Sub StopScorbit
	Scorbit.StopSession Score(1), Score(2), Score(3), Score(4), nPlayersInGame   ' Stop updateing scores
End Sub


Sub ScorbitBuildGameModes()		' Custom function to build the game modes for better stats 
	dim GameModeStr
	if Scorbit.bSessionActive=False then Exit Sub 
	Dbg2 " Shoulsub sofd be adding Game String:" &GameModeStr
	Scorbit.SetGameMode(GameModeStr)
End Sub 






' END ----------

Sub Scorbit_LOGUpload(state)	' Callback during the log creation process.  0=Creating Log, 1=Uploading Log, 2=Done 
	Select Case state 
		case 0:
			dbg "CREATING LOG"
		case 1:
			dbg "Uploading LOG"
		case 2:
			dbg "LOG Complete"
	End Select 
End Sub 
'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
' TABLE CUSTOMIZATION END HERE - NO NEED TO EDIT BELOW THIS LINE



dim Scorbit : Set Scorbit = New ScorbitIF
' Workaround - Call get a reference to Member Function
Sub tmrScorbit_Timer()								' Timer to send heartbeat 
	Scorbit.DoTimer(tmrScorbit.UserValue)
	tmrScorbit.UserValue=tmrScorbit.UserValue+1
	if tmrScorbit.UserValue>5 then tmrScorbit.UserValue=0
End Sub 
Function ScorbitIF_Callback()
	Scorbit.Callback()
End Function 
Class ScorbitIF

	Public bSessionActive
	Public bNeedsPairing
	Private bUploadLog
	Private bActive
	Private LOGFILE(10000000)
	Private LogIdx

	Private bProduction

	Private TypeLib
	Private MyMac
	Private Serial
	Private MyUUID
	Private TableVersion

	Private SessionUUID
	Private SessionSeq
	Private SessionTimeStart
	Private bRunAsynch
	Private bWaitResp
	Private GameMode
	Private GameModeOrig		' Non escaped version for log
	Private VenueMachineID
	Private CachedPlayerNames(4)
	Private SaveCurrentPlayer

	Public bEnabled
	Private sToken
	Private machineID
	Private dirQRCode
	Private opdbID
	Private wsh

	Private objXmlHttpMain
	Private objXmlHttpMainAsync
	Private fso
	Private Domain

	Public Sub Class_Initialize()
		bActive="false"
		bSessionActive=False
		bEnabled=False 
	End Sub 

	Property Let UploadLog(bValue)
		bUploadLog = bValue
	End Property

	Sub DoTimer(bInterval)	' 2 second interval
		dim holdScores(4)
		dim i
		if bInterval=0 then 
			SendHeartbeat()
		elseif bRunAsynch And bSessionActive = True then ' Game in play (Updated for TNA to resolve stutter in CoopMode)
			Scorbit.SendUpdate Score(1), Score(2), Score(3), Score(4), currBall+1, nPlayer, nPlayersInGame
		End if 
	End Sub 

	Function GetName(PlayerNum)	' Return Parsed Players name  
		if PlayerNum<1 or PlayerNum>4 then 
			GetName=""
		else 
			GetName=CachedPlayerNames(PlayerNum-1)
		End if 
	End Function 

	Function DoInit(MyMachineID, Directory_PupQRCode, Version, opdb)
		dim Nad
		Dim EndPoint
		Dim resultStr 
		Dim UUIDParts 
		Dim UUIDFile

		bProduction=1
'		bProduction=0
		SaveCurrentPlayer=0
		VenueMachineID=""
		bWaitResp=False 
		bRunAsynch=False 
		DoInit=False 
		opdbID=opdb
		dirQrCode=Directory_PupQRCode
		MachineID=MyMachineID
		TableVersion=version
		bNeedsPairing=False
		if bProduction then 
			domain = "api.scorbit.io"
		else 
			domain = "staging.scorbit.io"
			domain = "scorbit-api-staging.herokuapp.com"
		End if 
		Set fso = CreateObject("Scripting.FileSystemObject")
		dim objLocator:Set objLocator = CreateObject("WbemScripting.SWbemLocator")
		Dim objService:Set objService = objLocator.ConnectServer(".", "root\cimv2")
		Set objXmlHttpMain = CreateObject("Msxml2.ServerXMLHTTP")
		Set objXmlHttpMainAsync = CreateObject("Microsoft.XMLHTTP")
		objXmlHttpMain.onreadystatechange = GetRef("ScorbitIF_Callback")
		Set wsh = CreateObject("WScript.Shell")

		' Get Mac for Serial Number 
		dim Nads: set Nads = objService.ExecQuery("Select * from Win32_NetworkAdapter where physicaladapter=true")
		for each Nad in Nads
			if not isnull(Nad.MACAddress) then
				if left(Nad.MACAddress, 6)<>"00090F" then ' Skip over forticlient MAC
dbg2 "Using MAC Addresses:" & Nad.MACAddress & " From Adapter:" & Nad.description   
					MyMac=replace(Nad.MACAddress, ":", "")
					Exit For 
				End if 
			End if 
		Next
		Serial=eval("&H" & mid(MyMac, 5))
		if Serial<0 then Serial=eval("&H" & mid(MyMac, 6))		' Mac Address Overflow Special Case 
		if MyMachineID<>2108 then 			' GOTG did it wrong but MachineID should be added to serial number also
			Serial=Serial+MyMachineID
		End if 
'		Serial=123456
		dbg2 "Serial:" & Serial

		' Get System UUID
		set Nads = objService.ExecQuery("SELECT * FROM Win32_ComputerSystemProduct")
		for each Nad in Nads
			dbg2 "Using UUID:" & Nad.UUID   
			MyUUID=Nad.UUID
			Exit For 
		Next

		if MyUUID="" then 
			MsgBox "SCORBIT - Can get UUID, Disabling."
			Exit Function
		elseif MyUUID="03000200-0400-0500-0006-000700080009" or ScorbitAlternateUUID then
			If fso.FolderExists(UserDirectory) then 
				If fso.FileExists(UserDirectory & "ScorbitUUID.dat") then
					Set UUIDFile = fso.OpenTextFile(UserDirectory & "ScorbitUUID.dat",1)
					MyUUID = UUIDFile.ReadLine()
					UUIDFile.Close
					Set UUIDFile = Nothing
				Else 
					MyUUID=GUID()
					Set UUIDFile=fso.CreateTextFile(UserDirectory & "ScorbitUUID.dat",True)
					UUIDFile.WriteLine MyUUID
					UUIDFile.Close
					Set UUIDFile=Nothing
				End if
			End if 
		End if

		' Clean UUID
		UUIDParts=split(MyUUID, "-")
		MyUUID=LCASE(Hex(eval("&h" & UUIDParts(0))+MyMachineID) & UUIDParts(1) &  UUIDParts(2) &  UUIDParts(3) & UUIDParts(4))		 ' Add MachineID to UUID
		MyUUID=LPad(MyUUID, 32, "0")
'		MyUUID=Replace(MyUUID, "-",  "")
		dbg2 "MyUUID:" & MyUUID 


		' Authenticate and get our token 
		if getStoken() then 
			bEnabled=True 
'			SendHeartbeat
			DoInit=True
		End if 
	End Function 

	Sub Callback()
		Dim ResponseStr
		Dim i 
		Dim Parts
		Dim Parts2
		Dim Parts3
		if bEnabled=False then Exit Sub 

		if bWaitResp and objXmlHttpMain.readystate=4 then 
'			dbg2 "CALLBACK: " & objXmlHttpMain.Status & " " & objXmlHttpMain.readystate
			if objXmlHttpMain.Status=200 and objXmlHttpMain.readystate = 4 then 
				ResponseStr=objXmlHttpMain.responseText
				'debug3 "RESPONSE: " & ResponseStr

				' Parse Name 
				If bSessionActive = True Then
					if CachedPlayerNames(SaveCurrentPlayer-1)="" then  ' Player doesnt have a name
						if instr(1, ResponseStr, "cached_display_name") <> 0 Then	' There are names in the result
							Parts=Split(ResponseStr,",{")							' split it 
							if ubound(Parts)>=SaveCurrentPlayer-1 then 				' Make sure they are enough avail
								if instr(1, Parts(SaveCurrentPlayer-1), "cached_display_name")<>0 then 	' See if mine has a name 
									CachedPlayerNames(SaveCurrentPlayer-1)=GetJSONValue(Parts(SaveCurrentPlayer-1), "cached_display_name")		' Get my name
									CachedPlayerNames(SaveCurrentPlayer-1)=Replace(CachedPlayerNames(SaveCurrentPlayer-1), """", "")
									Scorbit_PlayerClaimed SaveCurrentPlayer, CachedPlayerNames(SaveCurrentPlayer-1)
	'								dbg2 "Player Claim:" & SaveCurrentPlayer & " " & CachedPlayerNames(SaveCurrentPlayer-1)
								End if 
							End if
						End if 
					else												    ' Check for unclaim 
						if instr(1, ResponseStr, """player"":null")<>0 Then	' Someone doesnt have a name
							Parts=Split(ResponseStr,"[")						' split it 
	'dbg2 "Parts:" & Parts(1)
							Parts2=Split(Parts(1),"}")							' split it 
							for i = 0 to Ubound(Parts2)
	'dbg2 "Parts2:" & Parts2(i)
								if instr(1, Parts2(i), """player"":null")<>0 Then
									CachedPlayerNames(i)=""
								End if 
							Next 
						End if 
					End if
				End If

				'Check heartbeat
				HandleHeartbeatResp ResponseStr
			End if 
			bWaitResp=False
		End if 
	End Sub

	Public Sub StartSession()
		if bEnabled=False then Exit Sub 
		dbg2 "Scorbit Start Session" 
		CachedPlayerNames(0)=""
		CachedPlayerNames(1)=""
		CachedPlayerNames(2)=""
		CachedPlayerNames(3)=""
		bRunAsynch=True 
		bActive="true"
		bSessionActive=True
		SessionSeq=0
		SessionUUID=GUID()
		SessionTimeStart=GameTime
		LogIdx=0
		SendUpdate 0, 0, 0, 0, 1, 1, 1
	End Sub

	' Custom method for TNA to work around coop mode stuttering
	Public Sub ForceAsynch(enabled)
		if bEnabled=False then Exit Sub
		if bSessionActive=True then Exit Sub 'Sessions should always control asynch when active
		bRunAsynch=enabled
	End Sub

	Public Sub StopSession(P1Score, P2Score, P3Score, P4Score, NumberPlayers)
		StopSession2 P1Score, P2Score, P3Score, P4Score, NumberPlayers, False
	End Sub 

	Public Sub StopSession2(P1Score, P2Score, P3Score, P4Score, NumberPlayers, bCancel)
		Dim i
		dim objFile
		if bEnabled=False then Exit Sub 
		bRunAsynch=False 'Asynch might have been forced on in TNA to prevent coop mode stutter
		if bSessionActive=False then Exit Sub 
dbg2 "Scorbit Stop Session" 

		bActive="false" 
		SendUpdate P1Score, P2Score, P3Score, P4Score, -1, -1, NumberPlayers
		bSessionActive=False
'		SendHeartbeat

		if bUploadLog and LogIdx<>0 and bCancel=False then 
			dbg2 "Creating Scorbit Log: Size" & LogIdx
			Scorbit_LOGUpload(0)
			Set objFile = fso.CreateTextFile(puplayer.getroot&"\" & cGameName & "\sGameLog.csv")
			For i = 0 to LogIdx-1 
				objFile.Writeline LOGFILE(i)
			Next 
			objFile.Close
			LogIdx=0
			Scorbit_LOGUpload(1)
			pvPostFile "https://" & domain & "/api/session_log/", puplayer.getroot&"\" & cGameName & "\sGameLog.csv", False
			Scorbit_LOGUpload(2)
			on error resume next
			fso.DeleteFile(puplayer.getroot&"\" & cGameName & "\sGameLog.csv")
			on error goto 0
		End if 

	End Sub 

	Public Sub SetGameMode(GameModeStr)
		GameModeOrig=GameModeStr
		GameMode=GameModeStr
		GameMode=Replace(GameMode, ":", "%3a")
		GameMode=Replace(GameMode, ";", "%3b")
		GameMode=Replace(GameMode, " ", "%20")
		GameMode=Replace(GameMode, "{", "%7B")
		GameMode=Replace(GameMode, "}", "%7D")
	End sub 

	Public Sub SendUpdate(P1Score, P2Score, P3Score, P4Score, CurrentBall, nPlayer, NumberPlayers)
		SendUpdateAsynch P1Score, P2Score, P3Score, P4Score, CurrentBall, nPlayer, NumberPlayers, bRunAsynch
	End Sub 

	Public Sub SendUpdateAsynch(P1Score, P2Score, P3Score, P4Score, CurrentBall, nPlayer, NumberPlayers, bAsynch)
		dim i
		Dim PostData
		Dim resultStr
		dim LogScores(4)

		if bUploadLog then 
			if NumberPlayers>=1 then LogScores(0)=P1Score
			if NumberPlayers>=2 then LogScores(1)=P2Score
			if NumberPlayers>=3 then LogScores(2)=P3Score
			if NumberPlayers>=4 then LogScores(3)=P4Score
			LOGFILE(LogIdx)=DateDiff("S", "1/1/1970", Now()) & "," & LogScores(0) & "," & LogScores(1) & "," & LogScores(2) & "," & LogScores(3) & ",,," &  nPlayer & "," & CurrentBall & ",""" & GameModeOrig & """"
			LogIdx=LogIdx+1
		End if


		if bSessionActive=False then Exit Sub 

		if bEnabled=False then Exit Sub 

		if bWaitResp then exit sub ' Drop message until we get our next response 

		SaveCurrentPlayer=nPlayer
		PostData = "session_uuid=" & SessionUUID & "&session_time=" & GameTime-SessionTimeStart+1 & _
					"&session_sequence=" & SessionSeq & "&active=" & bActive

		SessionSeq=SessionSeq+1
		if NumberPlayers > 0 then 
			for i = 0 to NumberPlayers-1
				PostData = PostData & "&current_p" & i+1 & "_score="
				if i <= NumberPlayers-1 then 
					if i = 0 then PostData = PostData & P1Score
					if i = 1 then PostData = PostData & P2Score
					if i = 2 then PostData = PostData & P3Score
					if i = 3 then PostData = PostData & P4Score
				else 
					PostData = PostData & "-1"
				End if 
			Next 
'Dbg2 "Score:" &P1Score &" XXX"
			PostData = PostData & "&current_ball=" & CurrentBall & "&current_player=" & nPlayer
			if GameMode<>"" then PostData=PostData & "&game_modes=" & GameMode

		End if 
		resultStr = PostMsg("https://" & domain, "/api/entry/", PostData, bAsynch)
		'if resultStr<>"" then debug3 "SendUpdate Resp:" & resultStr    			'rtp12
	End Sub 

' PRIVATE BELOW 
	Private Function LPad(StringToPad, Length, CharacterToPad)
	  Dim x : x = 0
	  If Length > Len(StringToPad) Then x = Length - len(StringToPad)
	  LPad = String(x, CharacterToPad) & StringToPad
	End Function

	Private Function GUID()		
		Dim TypeLib
		Set TypeLib = CreateObject("Scriptlet.TypeLib")
		GUID = Mid(TypeLib.Guid, 2, 36)
	End Function

	Private Function GetJSONValue(JSONStr, key)
		dim i 
		Dim tmpStrs,tmpStrs2
		if Instr(1, JSONStr, key)<>0 then 
			tmpStrs=split(JSONStr,",")
			for i = 0 to ubound(tmpStrs)
				if instr(1, tmpStrs(i), key)<>0 then 
					tmpStrs2=split(tmpStrs(i),":")
					GetJSONValue=tmpStrs2(1)
					exit for
				End if 
			Next 
		End if 
	End Function

	Private Sub SendHeartbeat()
		Dim resultStr
		if bEnabled=False then Exit Sub 
		resultStr = GetMsgHdr("https://" & domain, "/api/heartbeat/", "Authorization", "SToken " & sToken)
		
		'Customized for TNA
		If bRunAsynch = False Then 
			dbg2 "Heartbeat Resp:" & resultStr
			HandleHeartbeatResp ResultStr
		End If
	End Sub 

	'TNA custom method
	Private Sub HandleHeartbeatResp(resultStr)
		dim TmpStr
		Dim Command
		Dim rc
		'Dim QRFile:QRFile=puplayer.getroot&"\" & cGameName & "\" & dirQrCode
		Dim QRFile:QRFile=puplayer.getroot & cGameName & "\" & dirQrCode
'dbg2 "QRFile: " &QRFile
		If VenueMachineID="" then
			If resultStr<>"" And Not InStr(resultStr, """machine_id"":" & machineID)=0 Then 'We Paired
				bNeedsPairing=False
				dbg2 "Scorbit: Paired"
				Scorbit_Paired()
			ElseIf resultStr<>"" And Not InStr(resultStr, """unpaired"":true")=0 Then 'We Did not Pair
				dbg2 "Scorbit: NOT Paired"
				bNeedsPairing=True
			Else
				' Error (or not a heartbeat); do nothing
			End If

			TmpStr=GetJSONValue(resultStr, "venuemachine_id")
			if TmpStr<>"" then 
				VenueMachineID=TmpStr
'dbg2 "VenueMachineID=" & VenueMachineID			
				'Command = """" & puplayer.getroot&"\" & cGameName & "\sQRCode.exe"" " & VenueMachineID & " " & opdbID & " """ & QRFile & """"
				Command = """" & puplayer.getroot & cGameName & "\sQRCode.exe"" " & VenueMachineID & " " & opdbID & " """ & QRFile & """"
				rc = wsh.Run(Command, 0, False)
			End if 
		End if
	End Sub

	Private Function getStoken()
		Dim result
		Dim results
'		dim wsh
		Dim tmpUUID:tmpUUID="adc12b19a3504453a7414e722f58736b"
		Dim tmpVendor:tmpVendor="vscorbitron"
		Dim tmpSerial:tmpSerial="999990104"
		'Dim QRFile:QRFile=puplayer.getroot&"\" & cGameName & "\" & dirQrCode
		Dim QRFile:QRFile=puplayer.getroot & cGameName & "\" & dirQrCode
		'Dim sTokenFile:sTokenFile=puplayer.getroot&"\" & cGameName & "\sToken.dat"
		Dim sTokenFile:sTokenFile=puplayer.getroot & cGameName & "\sToken.dat"

		' Set everything up
		tmpUUID=MyUUID
		tmpVendor="vpin"
		tmpSerial=Serial
		
		on error resume next
		fso.DeleteFile(sTokenFile)
		On error goto 0 

		' get sToken and generate QRCode
'		Set wsh = CreateObject("WScript.Shell")
		Dim waitOnReturn: waitOnReturn = True
		Dim windowStyle: windowStyle = 0
		Dim Command 
		Dim rc
		Dim objFileToRead

		'Command = """" & puplayer.getroot&"\" & cGameName & "\sToken.exe"" " & tmpUUID & " " & tmpVendor & " " &  tmpSerial & " " & MachineID & " """ & QRFile & """ """ & sTokenFile & """ " & domain
		Command = """" & puplayer.getroot & cGameName & "\sToken.exe"" " & tmpUUID & " " & tmpVendor & " " &  tmpSerial & " " & MachineID & " """ & QRFile & """ """ & sTokenFile & """ " & domain
dbg2 "RUNNING Command:" & Command
		rc = wsh.Run(Command, windowStyle, waitOnReturn)
dbg2 "Return:" & rc
		if FileExists(puplayer.getroot&"\" & cGameName & "\sToken.dat") and rc=0 then
			Set objFileToRead = fso.OpenTextFile(puplayer.getroot&"\" & cGameName & "\sToken.dat",1)
			result = objFileToRead.ReadLine()
			objFileToRead.Close
			Set objFileToRead = Nothing

			if Instr(1, result, "Invalid timestamp")<> 0 then 
				MsgBox "Scorbit Timestamp Error: Please make sure the time on your system is exact"
				getStoken=False
			elseif Instr(1, result, ":")<>0 then 
				results=split(result, ":")
				sToken=results(1)
				sToken=mid(sToken, 3, len(sToken)-4)
dbg2 "Got TOKEN:" & sToken
				getStoken=True
			Else 
dbg2 "ERROR:" & result
				getStoken=False
			End if 
		else 
dbg2 "ERROR No File:" & rc
		End if 

	End Function 

	private Function FileExists(FilePath)
		If fso.FileExists(FilePath) Then
			FileExists=CBool(1)
		Else
			FileExists=CBool(0)
		End If
	End Function

	Private Function GetMsg(URLBase, endpoint)
		GetMsg = GetMsgHdr(URLBase, endpoint, "", "")
	End Function

	Private Function GetMsgHdr(URLBase, endpoint, Hdr1, Hdr1Val)
		Dim Url
		Url = URLBase + endpoint & "?session_active=" & bActive
'dbg2 "Url:" & Url  & "  Async=" & bRunAsynch
		objXmlHttpMain.open "GET", Url, bRunAsynch
'		objXmlHttpMain.setRequestHeader "Content-Type", "text/xml"
		objXmlHttpMain.setRequestHeader "Cache-Control", "no-cache"
		if Hdr1<> "" then objXmlHttpMain.setRequestHeader Hdr1, Hdr1Val

'		on error resume next
			err.clear
			objXmlHttpMain.send ""
			if err.number=-2147012867 then 
				MsgBox "Multiplayer Server is down (" & err.number & ") " & Err.Description
				bEnabled=False
			elseif err.number <> 0 then 
				debug3 "Server error: (" & err.number & ") " & Err.Description
			End if 
			if bRunAsynch=False then 
dbg2 "Status: " & objXmlHttpMain.status
				If objXmlHttpMain.status = 200 Then
					GetMsgHdr = objXmlHttpMain.responseText
				Else 
					GetMsgHdr=""
				End if 
			Else 
				bWaitResp=True
				GetMsgHdr=""
			End if 
'		On error goto 0

	End Function

	Private Function PostMsg(URLBase, endpoint, PostData, bAsynch)
		Dim Url

		Url = URLBase + endpoint
'dbg2 "PostMSg:" & Url & " " & PostData			'rtp12

		objXmlHttpMain.open "POST",Url, bAsynch
		objXmlHttpMain.setRequestHeader "Content-Type", "application/x-www-form-urlencoded"
		objXmlHttpMain.setRequestHeader "Content-Length", Len(PostData)
		objXmlHttpMain.setRequestHeader "Cache-Control", "no-cache"
		objXmlHttpMain.setRequestHeader "Authorization", "SToken " & sToken
		if bAsynch then bWaitResp=True 

		on error resume next
			objXmlHttpMain.send PostData
			if err.number=-2147012867 then 
				MsgBox "Multiplayer Server is down (" & err.number & ") " & Err.Description
				bEnabled=False
			elseif err.number <> 0 then 
				'debug3 "Multiplayer Server error (" & err.number & ") " & Err.Description
			End if 
			If objXmlHttpMain.status = 200 Then
				PostMsg = objXmlHttpMain.responseText
			else 
				PostMsg="ERROR: " & objXmlHttpMain.status & " >" & objXmlHttpMain.responseText & "<"
			End if 
		On error goto 0
	End Function

	Private Function pvPostFile(sUrl, sFileName, bAsync)
'dbg2 "Posting File " & sUrl & " " & sFileName & " " & bAsync & " File:" & Mid(sFileName, InStrRev(sFileName, "\") + 1)
		Dim STR_BOUNDARY:STR_BOUNDARY  = GUID()
		Dim nFile  
		Dim baBuffer()
		Dim sPostData
		Dim Response

		'--- read file
		Set nFile = fso.GetFile(sFileName)
		With nFile.OpenAsTextStream()
			sPostData = .Read(nFile.Size)
			.Close
		End With


		'--- prepare body
		sPostData = "--" & STR_BOUNDARY & vbCrLf & _
			"Content-Disposition: form-data; name=""uuid""" & vbCrLf & vbCrLf & _
			SessionUUID & vbcrlf & _
			"--" & STR_BOUNDARY & vbCrLf & _
			"Content-Disposition: form-data; name=""log_file""; filename=""" & SessionUUID & ".csv""" & vbCrLf & _
			"Content-Type: application/octet-stream" & vbCrLf & vbCrLf & _
			sPostData & vbCrLf & _
			"--" & STR_BOUNDARY & "--"


		'--- post
		With objXmlHttpMain
			.Open "POST", sUrl, bAsync
			.SetRequestHeader "Content-Type", "multipart/form-data; boundary=" & STR_BOUNDARY
			.SetRequestHeader "Authorization", "SToken " & sToken
			.Send sPostData ' pvToByteArray(sPostData)
			If Not bAsync Then
				Response= .ResponseText
				pvPostFile = Response
dbg2 "Upload Response: " & Response
			End If
		End With

	End Function

	Private Function pvToByteArray(sText)
		pvToByteArray = StrConv(sText, 128)		' vbFromUnicode
	End Function

End Class 

'  END SCORBIT 
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
Sub DelayQRClaim_Timer()
	if bOnTheFirstBall AND BallInPlunger then 
		ScorbitClaimQR(True)
		Dbg2 " Should be calling Show Claim"
	Else
		Dbg2 " BFB: " &bOnTheFirstBall
		Dbg2 " BIPL" & BallInPlunger
	End If
'	 ScorbitClaimQR(True)
'	DelayQRClaim.Enabled=False
End Sub

sub CheckPairing
dbg2 "Inside SCORBIT check pairing"
	if (Scorbit.bNeedsPairing) then 
		dbg2 "Should be displaying pairing info"
		if DMDType = 0 Then pdmdsetpage 77, "Scorbit"
		PuPlayer.playlistplayex pBackglass,"PuPOverlays","Scorbit_Pair.png",0,1
		PuPlayer.LabelSet pBackglass, "ScorbitQR1", "PuPOverlays\\QRcode.png",1,"{'mt':2,'width':19.61, 'height':36,'xalign':0,'yalign':0,'ypos':32,'xpos':74.6}"

		DelayQRClaim.Interval=6000
		DelayQRClaim.Enabled=True
	Else
dbg2 "Already Paired"
		DelayQRClaim.Interval=6000
		DelayQRClaim.Enabled=True
	end if
End sub

Sub HideScorbit
	pDMDsetpage pScores, "Leaving-Scorbit"
	DelayQRClaim.Enabled = False
	if DMDType = 0 Then
		PuPlayer.playlistplayex pBackglass,"PuPOverlays","DMD.png",0,1
	Else
		PuPlayer.playlistplayex pBackglass,"PuPOverlays","blank.png",0,1
		GeneralPupQueue.Add "MainBG","MainBG",25,10,0,0,0,False
	End If
	pBackglasslabelhide "ScorbitQR1"
	pBackglasslabelhide "ScorbitQRIcon1"
	pBackglasslabelhide "ScorbitQR2"
	pBackglasslabelhide "ScorbitQRIcon2"
End Sub


'*******************************************
' ZPUP - PinUp display
'*******************************************
'//////////////////////////////////////////////////////////////////////


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' DO NOT CHANGE ANYTHING IN THIS SECTION
Const     ScorbitClaimSmall		= 1 	' Make Claim QR Code smaller for high res backglass 

	Const cWhite = 	16777215
	Const cRed = 	397512
	Const cGold = 	1604786
	Const cGold2 = 46079
	Const cGreen = 32768
	Const cGrey = 	8421504
	Const cYellow = 65535
	Const cOrange = 33023
	Const cPurple = 16711808
	Const cBlue = 16711680
	Const cLightBlue = 16744448
	Const cBoltYellow = 2148582
	Const cLightGreen = 9747818
	Const cBlack = 0
	Const cPink = 12615935
	Const cSilver = 8421504

Dim pGameName       : pGameName=cGameName

 
Const pFontBold="Bronx Bystreets"  'main score font
'Const pFontGodfather = "The Godfather"
Const pFontGodfather = "The Scarface Free Trial"
Const dmddef="Neue Alte Grotesk Bold"

'Const pDMD = 2   
Const pFullDMD = 5
Const pBackglass = 2
Const pMusic = 4

'pages
Const pDMDBlank = 0
Const pScores = 1
Const pAttract = 2
Const pPrevScores = 3
Const pCredits = 4
Const pSlotMachine = 5
Const pBonus = 6
Const pEvent = 7
Const pHighScore = 8


Sub CheckPupVersion

	Dim strPupVersion
	strPupVersion = PuPlayer.GetVersion
	strPupVersion = Replace(strPupVersion, ".", "")
	strPupVersion = mid (strPupVersion, 1,3)
    strPupVersion = CDbl(strPupVersion)

	If strPupVersion => 150 then
		exit sub
	Else
		msgbox "This table requires PuP Player version 1.5 or greater.  Please update your pup install to play the table", 0
		Table1_Exit	
	End if
End sub

Sub pSetPageLayouts
	Dim i

	pDMDAlwaysPAD		'we pad all text with space before and after for shadow clipping/etc

'labelNew <screen#>, <Labelname>, <fontName>,<size%>,<colour>,<rotation>,<xalign>,<yalign>,<xpos>,<ypos>,<PageNum>,<visible>
'***********************************************************************'
'<screen#>, in standard we’d set this to pDMD ( or 1)
'<Labelname>, your name of the label. keep it short no spaces (like 8 chars) although you can call it anything really. When setting the label you will use this labelname to access the label.
'<fontName> Windows font name, this must be exact match of OS front name. if you are using custom TTF fonts then double check the name of font names.
'<size%>, Height as a percent of display height. 20=20% of screen height.
'<colour>, integer value of windows color.
'<rotation>, degrees in tenths   (900=90 degrees)
'<xAlign>, 0= horizontal left align, 1 = center horizontal, 2= right horizontal
'<yAlign>, 0 = top, 1 = center, 2=bottom vertical alignment
'<xpos>, this should be 0, but if you want to ‘force’ a position you can set this. it is a % of horizontal width. 20=20% of screen width.
'<ypos> same as xpos.
'<PageNum> IMPORTANT… this will assign this label to this ‘page’ or group.
'<visible> initial state of label. visible=1 show, 0 = off.

if DMDType = 0 Then
	pupCreateLabelImage "ScorbitQRicon1","PuPOverlays\\QRcodeS.png",50,30,34,60,77,0
	pupCreateLabelImage "ScorbitQR1","PuPOverlays\\QRcode.png",50,30,34,60,77,0

	pupCreateLabelImage "ScorbitQRicon2","PuPOverlays\\QRcodeB.png",50,30,34,60,77,0
	pupCreateLabelImage "ScorbitQR2","PuPOverlays\\QRclaim.png",50,30,34,60,77,0
Else
	pupCreateLabelImage "ScorbitQRicon1","PuPOverlays\\QRcodeS.png",50,30,34,60,1,0
	pupCreateLabelImage "ScorbitQR1","PuPOverlays\\QRcode.png",50,30,34,60,1,0

	pupCreateLabelImage "ScorbitQRicon2","PuPOverlays\\QRcodeB.png",50,30,34,60,1,0
	pupCreateLabelImage "ScorbitQR2","PuPOverlays\\QRclaim.png",50,30,34,60,1,0
End If

	pupCreateLabelImageDMD "AttractCredits","PuPOverlays\\Credits1.png",0,0,100,100,88,0

	pupCreateLabelImageDMD "ExtraBallImage","PuPOverlays\\briefcase.png",85,-2,15,25,1,0

	pupCreateLabelImage "DMDOverlay", "PuPOverlays\\DMD.png",0,0,100,100,1,0

	pupCreateLabelImageDMD "Flamingo","PuPOverlays\\Flamingo.png",9,7.5,14,14,1,0

	pupCreateLabelImageDMD "Snort","PuPOverlays\\Snort2.png",-100,-100,50,50,1,0
	pupCreateLabelImageDMD "Bullet1","PuPOverlays\\P1.png",0,0,50,50,1,0
	pupCreateLabelImageDMD "Bullet2","PuPOverlays\\P2.png",0,0,50,50,1,0
	pupCreateLabelImageDMD "Bullet3","PuPOverlays\\P3.png",0,0,50,50,1,0
	pupCreateLabelImageDMD "Bullet4","PuPOverlays\\P4.png",0,0,50,50,1,0


	PuPlayer.LabelNew pDMD, "Line1b",pFontBold,			25,cGold	,0,1,1, 50,32,	1,0
	PuPlayer.LabelNew pDMD, "Line2b",pFontBold, 			25,cGold	,0,1,1, 50,55,	1,0

	PuPlayer.LabelNew pDMD,"CurrScore",         dmddef,5,cGold   ,0,1,1, 50,88.5,pScores,0
	PuPlayer.LabelNew pDMD,"CurrName",	             dmddef,5,cSilver   ,0,0,0,50,79.5,1,0
	PuPlayer.LabelNew pDMD,"Position1Score",	             dmddef,5,cBlack   ,0,0,1,33,79.5,1,0
	PuPlayer.LabelNew pDMD,"Position2Name",	             dmddef,5,cBlack   ,0,0,0,20,89,1,0
	PuPlayer.LabelNew pDMD,"Position2Score",	             dmddef,5,cBlack  ,0,0,1,20,94,1,0
	PuPlayer.LabelNew pDMD,"Position3Name",	             dmddef,5,cBlack   ,0,0,0,50,89,1,0
	PuPlayer.LabelNew pDMD,"Position3Score",	             dmddef,5,cBlack   ,0,0,1,50,94,1,0
	PuPlayer.LabelNew pDMD,"Position4Name",	             dmddef,5,cBlack   ,0,0,0,80,89,1,0
	PuPlayer.LabelNew pDMD,"Position4Score",	             dmddef,5,cBlack   ,0,0,1,80,94,1,0

	pDMDLabelSetBorder "CurrScore",cBlack,10,10,1
'	PuPlayer.LabelSet pDMD, "CurrScore", "", 1, _
'		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"

	PuPlayer.LabelSet pDMD, "CurrName", "", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"

	PuPlayer.LabelNew pDMD,"BallValue",	             dmddef,6,cSilver   ,0,1,1,65,77.1,pScores,0
	pDMDLabelSetBorder "BallValue",cBlack,4,4,1

	PuPlayer.LabelNew pDMD,"MainModeTimerValue",	             dmddef,8,cGold   ,0,1,1,3.8,76,1,0
	PuPlayer.LabelSet pDMD, "MainModeTimerValue", "", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"

	PuPlayer.LabelNew pDMD,"SideEventTimerValue",	             dmddef,8,cSilver   ,0,1,1,11.7,80,1,0
	PuPlayer.LabelSet pDMD, "SideEventTimerValue", "", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"

	PuPlayer.LabelNew pDMD,"BMValue",	             dmddef,12,cGold   ,0,1,1,21.75,87.5,1,0
	PuPlayer.LabelSet pDMD, "BMValue", "", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelNew pDMD,"PFMValue",	             dmddef,12,cGold   ,0,1,1,78.75,87.5,1,0
	PuPlayer.LabelSet pDMD, "PFMValue", "", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"


'page 5
	PuPlayer.LabelNew pDMD,"creditCount",	             pFontBold,10,cGold   ,0,1,1, 65,14,5,0
	PuPlayer.LabelNew pDMD,"creditsValue",	             dmddef,11,cGold   ,0,1,1, 80,13,5,0

	pDMDLabelSetBorder "creditCount",cBlack,1,1,1		
	pDMDLabelSetBorder "creditsValue",cBlack,1,1,1		


	' Attract mode: high scores
	PuPlayer.LabelNew pDMD, "Attract1", dmddef, 16, cGold, 0, 1, 1, 50, 30, pAttract, 0
	PuPlayer.LabelNew pDMD, "Attract2", dmddef, 16, cGold, 0, 1, 1, 50, 60, pAttract, 0
	'Add outlines to text labels
	PuPlayer.LabelSet pDMD, "Attract1", "", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "Attract2", "", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"

	' Attract mode: the previous game's scores
	PuPlayer.LabelNew pDMD, "PrevScores", dmddef, 10, cGold, 0, 1, 1, 45, 18, pPrevScores, 0
	PuPlayer.LabelNew pDMD, "PrevCombos", dmddef, 10, cGold, 0, 1, 1, 75, 18, pPrevScores, 0

	PuPlayer.LabelNew pDMD, "PrevP1Label", dmddef, 10, cPurple, 0, 0, 1, 5, 30, pPrevScores, 0
	PuPlayer.LabelNew pDMD, "PrevP1Score", dmddef, 10, cPurple, 0, 1, 1, 45, 30, pPrevScores, 0
	PuPlayer.LabelNew pDMD, "PrevP1Combo", dmddef, 10, cPurple, 0, 1, 1, 75, 30, pPrevScores, 0

	PuPlayer.LabelNew pDMD, "PrevP2Label", dmddef, 10, cYellow, 0, 0, 1, 5, 42, pPrevScores, 0
	PuPlayer.LabelNew pDMD, "PrevP2Score", dmddef, 10, cYellow, 0, 1, 1, 45, 42, pPrevScores, 0
	PuPlayer.LabelNew pDMD, "PrevP2Combo", dmddef, 10, cYellow, 0, 1, 1, 75, 42, pPrevScores, 0

	PuPlayer.LabelNew pDMD, "PrevP3Label", dmddef, 10, cBlue, 0, 0, 1, 5, 54, pPrevScores, 0
	PuPlayer.LabelNew pDMD, "PrevP3Score", dmddef, 10, cBlue, 0, 1, 1, 45, 54, pPrevScores, 0
	PuPlayer.LabelNew pDMD, "PrevP3Combo", dmddef, 10, cBlue, 0, 1, 1, 75, 54, pPrevScores, 0

	PuPlayer.LabelNew pDMD, "PrevP4Label", dmddef, 10, cOrange, 0, 0, 1, 5, 66, pPrevScores, 0
	PuPlayer.LabelNew pDMD, "PrevP4Score", dmddef, 10, cOrange, 0, 1, 1, 45, 66, pPrevScores, 0
	PuPlayer.LabelNew pDMD, "PrevP4Combo", dmddef, 10, cOrange, 0, 1, 1, 75, 66, pPrevScores, 0


	'Add outlines to text labels
	PuPlayer.LabelSet pDMD, "PrevScores", " SCORE ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "PrevCombos", " MAX COMBOS ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "PrevP1Label", " PLAYER 1 ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "PrevP1Score", " 00 ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "PrevP1Combo", " 00 ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "PrevP2Label", " PLAYER 2 ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "PrevP2Score", " 00 ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "PrevP2Combo", " 00 ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "PrevP3Label", " PLAYER 3 ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "PrevP3Score", " 00 ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "PrevP3Combo", " 00 ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "PrevP4Label", " PLAYER 4 ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "PrevP4Score", " 00 ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "PrevP4Combo", " 00 ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"

	' Attract mode: Credits
	PuPlayer.LabelNew pDMD, "Credits", dmddef, 4, cGold,_
		0, 1, 1, 0, 0, pCredits, 1
	PuPlayer.LabelSet pDMD, "Credits", "pngs\\Credits.png", 1, _
		"{'mt':2, 'zback':1, 'width':100, 'height':100, 'yalign':0, 'xalign':0, 'ypos':0, 'xpos':0}"



	' Mode Messaging
	PuPlayer.LabelNew pDMD, "SideEventMessage", dmddef, 10, cSilver, 0, 1, 1, 50, 70, pScores, 0
	PuPlayer.LabelNew pDMD, "SideEventMessage2", dmddef, 10, cSilver, 0, 1, 1, 74, 40, pScores, 0  ' not used


	PuPlayer.LabelNew pDMD, "MainModeMessage2", dmddef, 10, cGold, 0, 1, 1, 28, 40, pScores, 0 

	PuPlayer.LabelNew pDMD, "MainModeMessage", dmddef, 10, cGold, 0, 1, 1, 50, MessageHeight, pScores, 0
	PuPlayer.LabelNew pDMD, "SideEventProgress", dmddef, 10, cSilver, 0, 1, 1, 90, MessageHeight, pScores, 0
	PuPlayer.LabelNew pDMD, "MainModeProgress", dmddef, 10, cGold, 0, 1, 1, 10, MessageHeight, pScores, 0

	'Add outlines to text labels
	PuPlayer.LabelSet pDMD, "MainModeMessage", " ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "MainModeMessage2", " ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "SideEventMessage", " ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "SideEventMessage2", " ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "MainModeProgress", " ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "SideEventProgress", " ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"

	' Splash text

	PuPlayer.LabelNew pDMD, "Splash", dmddef, 18, cWhite, 0, 1, 1, 50, 32, pScores, 0
	PuPlayer.LabelNew pDMD, "Splash2a", dmddef, 12, cGold, 0, 1, 1, 50, 34, pScores, 0
	PuPlayer.LabelNew pDMD, "Splash2b", dmddef, 12, cGold, 0, 1, 1, 50, 50, pScores, 0
	PuPlayer.LabelNew pDMD, "Splash3a", dmddef, 10, cGold, 0, 1, 1, 50, 30, pScores, 0
	PuPlayer.LabelNew pDMD, "Splash3b", dmddef, 10, cGold, 0, 1, 1, 50, 42, pScores, 0
	PuPlayer.LabelNew pDMD, "Splash3c", dmddef, 10, cGold, 0, 1, 1, 50, 54, pScores, 0

	PuPlayer.LabelNew pDMD, "SplashMG2a", dmddef, 12, cPink, 0, 1, 1, 50, 34, pScores, 0
	PuPlayer.LabelNew pDMD, "SplashMG2b", dmddef, 12, cPink, 0, 1, 1, 50, 50, pScores, 0

	PuPlayer.LabelNew pDMD, "Attract2a", dmddef, 12, cGold, 0, 1, 1, 50, 34, 88, 0
	PuPlayer.LabelNew pDMD, "Attract2b", dmddef, 12, cGold, 0, 1, 1, 50, 50, 88, 0
	PuPlayer.LabelSet pDMD, "Attract2a", " ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "Attract2b", " ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"

	'Add outlines to text labels
	PuPlayer.LabelSet pDMD, "Splash", " ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "Splash2a", " ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "Splash2b", " ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "Splash3a", " ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "Splash3b", " ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "Splash3c", " ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "SplashMG2a", " ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "SplashMG2b", " ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"

	' Bonus
	PuPlayer.LabelNew pDMD, "BonusCaption", dmddef, 10, cWhite, 0, 1, 1, 50, 30, pBonus, 0
	PuPlayer.LabelNew pDMD, "MainEventCaption", dmddef, 12, cGold, 0, 1, 1, 50, 44, pBonus, 0
	PuPlayer.LabelNew pDMD, "SideEventCaption", dmddef, 12, cSilver, 0, 1, 1, 50, 54, pBonus, 0

'	PuPlayer.LabelNew pDMD, "BonusPoP0", dmddef, 8, cRed, 0, 1, 1, 25, 60, pScores, 0
'	PuPlayer.LabelNew pDMD, "BonusPoP1", dmddef, 8, cRed, 0, 1, 1, 50, 30, pScores, 0
'	PuPlayer.LabelNew pDMD, "BonusPoP2", dmddef, 8, cRed, 0, 1, 1, 75, 60, pScores, 0
'	PuPlayer.LabelNew pDMD, "BonusPoP3", dmddef, 8, cRed, 0, 1, 1, 20, 30, pScores, 0
'	PuPlayer.LabelNew pDMD, "BonusPoP4", dmddef, 8, cRed, 0, 1, 1, 50, 40, pScores, 0
'	PuPlayer.LabelNew pDMD, "BonusPoP5", dmddef, 8, cRed, 0, 1, 1, 80, 30, pScores, 0
'	PuPlayer.LabelNew pDMD, "BonusPoP6", dmddef, 8, cRed, 0, 1, 1, 35, 50, pScores, 0
'	PuPlayer.LabelNew pDMD, "BonusPoP7", dmddef, 8, cRed, 0, 1, 1, 50, 60, pScores, 0
'	PuPlayer.LabelNew pDMD, "BonusPoP8", dmddef, 8, cRed, 0, 1, 1, 35, 50, pScores, 0

	PuPlayer.LabelNew pDMD, "BonusPoP0", dmddef, 8, cRed, 0, 1, 1, 75, 25, pScores, 0
	PuPlayer.LabelNew pDMD, "BonusPoP1", dmddef, 8, cRed, 0, 1, 1, 25, 60, pScores, 0
	PuPlayer.LabelNew pDMD, "BonusPoP2", dmddef, 8, cRed, 0, 1, 1, 50, 35, pScores, 0
	PuPlayer.LabelNew pDMD, "BonusPoP3", dmddef, 8, cRed, 0, 1, 1, 75, 65, pScores, 0
	PuPlayer.LabelNew pDMD, "BonusPoP4", dmddef, 8, cRed, 0, 1, 1, 20, 35, pScores, 0
	PuPlayer.LabelNew pDMD, "BonusPoP5", dmddef, 8, cRed, 0, 1, 1, 45, 70, pScores, 0
	PuPlayer.LabelNew pDMD, "BonusPoP6", dmddef, 8, cRed, 0, 1, 1, 70, 30, pScores, 0
	PuPlayer.LabelNew pDMD, "BonusPoP7", dmddef, 8, cRed, 0, 1, 1, 15, 50, pScores, 0
	PuPlayer.LabelNew pDMD, "BonusPoP8", dmddef, 8, cRed, 0, 1, 1, 55, 20, pScores, 0
	PuPlayer.LabelNew pDMD, "BonusPoP9", dmddef, 8, cRed, 0, 1, 1, 80, 50, pScores, 0
	PuPlayer.LabelNew pDMD, "BonusPoP10", dmddef, 8, cRed, 0, 1, 1, 25, 25, pScores, 0
	PuPlayer.LabelNew pDMD, "BonusPoP11", dmddef, 8, cRed, 0, 1, 1, 50, 50, pScores, 0



	PuPlayer.LabelNew pDMD, "BonusTotal", dmddef, 12, cRed, 0, 1, 1, 50, 64, pBonus, 0
	PuPlayer.LabelNew pDMD, "BonusScoreValue", dmddef, 12, cWhite, 0, 1, 1, 50, 74, pBonus, 0
	'Add outlines to text labels

	PuPlayer.LabelSet pDMD, "BonusCaption", " ", 0, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "MainEventCaption", " ", 0, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "SideEventCaption", " ", 0, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"


	PuPlayer.LabelSet pDMD, "BonusTotal", " ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "BonusScoreValue", " ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"


	' High Score Labels
	PuPlayer.LabelNew pDMD, "HSMessage", dmddef, 20, cWhite, 0, 1, 1, 50, 40, pScores, 0
	PuPlayer.LabelNew pDMD, "HSInitials", dmddef, 24, cWhite, 0, 1, 1, 50, 62, pScores, 0
 	PuPlayer.LabelSet pDMD, "HSMessage", " ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"
	PuPlayer.LabelSet pDMD, "HSInitials", " ", 1, _
		"{'mt':2,'shadowcolor':0, 'shadowstate': 1, 'xoffset': 5, 'yoffset': 4, 'outline':1}"

End Sub

Sub TestRTP
		pDMDLabelFadePulse "bonusPop1",500,"",25,0
		pDMDLabelMoveVertFade "bonuspop2","10,000",1000,cRed,0,10,500
End Sub

'***********************************************************PinUP Player DMD Helper Functions
' === Border, Shadow ===
	' ========== LABELS (examples) ==========

	'pupCreateLabel(lName, lValue, lFont, lSize, lColor, xpos, ypos, pagenum, lvis)
	'pupCreateLabelImage(lName, lFilename, xpos, ypos, Iwidth, Iheight, pagenum, lvis)

	'pDMDLabelSetBorder(labName,lCol,offsetx,offsety,isVis)
	'pDMDLabelSetShadow(labName,lCol,offsetx,offsety,isVis)

	' === pUseFramePos = 1 (pixels) ===

	'pupCreateLabel "curScore","200,000",dmddef,80,pc_white,960,880,1,1	

	' === pUseFramePos = 0 (%) ===

	'pupCreateLabel "curScore","200,000",dmddef,80,pc_white,50,30,1,1

	' === border, shadow ===

	'pDMDLabelSetBorder "Splash",pc_green,3,3,1
	'pDMDLabelSetShadow "curScore",pc_black,3,3,1

	' === image ===

	'pupCreateLabelImage "cJewel","dmdmisc\\jewel.png",91.25,80.3,15,30,1,1

' === Move and Fade ===

sub pDMDLabelMoveHorzFade(LabName,LabValue,mLen,mColor,pMoveStart,pMoveEnd,mfade)   'pmovestart is -1 = left-off, 0 = current pos, 1 = right-off or can use %
	PuPlayer.LabelSet pDMD,labName,LabValue,0,"{'mt':1,'at':2, 'len':" & (mLen) & ", 'fc':" & mColor & ",'xps':"&pMoveStart&",'xpe' :"&pMoveEnd&", 'tt':2 ,'ad':1, 'af':"&mfade&"}"    
end Sub

sub pDMDLabelMoveVertFade(LabName,LabValue,mLen,mColor,pMoveStart,pMoveEnd,mfade)   'pmovestart is -1 = left-off, 0 = current pos, 1 = right-off  or can use %   
	PuPlayer.LabelSet pDMD,labName,LabValue,0,"{'mt':1,'at':2, 'len':" & (mLen) & ", 'fc':" & mColor & ",'yps':"&pMoveStart&",'ype' :"&pMoveEnd&", 'tt':2 ,'ad':1, 'af':"&mfade&"}"    
end Sub

sub pDMDLabelMoveToFade(LabName,LabValue,mLen,mColor,byVal pStartX,byVal pStartY,byVal pEndX,byVal pEndY,mfade)   'pmovestart is -1 = left-off 0 = current pos 1 = right-off
	if pUseFramePos=1 AND (pStartX+pStartY+pEndx+pendY)>4 Then
		pTranslatePos pStartX,pStartY
		pTranslatePos pEndX,pEndY
	end IF 
	PuPlayer.LabelSet pDMD,labName,LabValue,0,"{'mt':1,'at':2, 'len':" & (mLen) & ", 'fc':" & mColor & ",'xps':"&pStartX&",'xpe' :"&pEndX& ",'yps':"&pStartY&",'ype' :"&pEndY&", 'tt':6 ,'ad':1, 'af':"&mfade&"}"    
end Sub

' === Fade ===

sub pDMDLabelFadeOut(LabName,mLen)   'alpha is 255 max, 0=clear.
	PuPlayer.LabelSet pDMD,labName,"`u`",0,"{'mt':1,'at':5,'astart':255,'aend':0,'len':" & (mLen) & " }"    
end Sub

sub pDMDLabelFadeIn(LabName,mLen)    'alpha is 255 max, 0=clear.
	PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':1,'at':5,'astart':0,'aend':255,'len':" & (mLen) & " }"    
end Sub

sub pDMDLabelFadePulse(LabName,mLen,mColor,pSpeed,isVis)   'alpha is 255 max, 0=clear. alpha start/end and pulsespeed of change
	PuPlayer.LabelSet pDMD,labName,"`u`",isVis,"{'mt':1,'at':6,'astart':70,'aend':255,'len':" & (mLen) & ",'pspeed': "& pSpeed &",'fc':" & mColor & "}" 
end Sub

sub pDMDScreenFadeOut(LabName,mLen)   'alpha is 255 max, 0=clear. 
	PuPlayer.LabelSet pDMD,labName,"`u`",0,"{'mt':1,'at':7,'astart':255,'aend':0,'len':" & (mLen) & " }"    
end Sub

sub pDMDScreenFadeIn(LabName,mLen)    'alpha is 255 max, 0=clear.
	PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':1,'at':7,'astart':0,'aend':255,'len':" & (mLen) & " }"    
end Sub

' === Scroll ===

Sub pDMDScrollBig(LabName,msgText,byVal timeSec,mColor) 'timeSec in MS
	if timeSec<20 Then timeSec=timeSec*1000
	PuPlayer.LabelSet pDMD,LabName,msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':" & (timeSec) & ",'mlen':" & (timeSec*1) & ",'tt':0,'fc':" & mColor & "}"
end sub

Sub pDMDScrollBigV(LabName,msgText,byVal timeSec,mColor) 'timeSec in MS
	if timeSec<20 Then timeSec=timeSec*1000
	PuPlayer.LabelSet pDMD,LabName,msgText,0,"{'mt':1,'at':2,'yps':1,'ype':-1,'len':" & (timeSec) & ",'mlen':" & (timeSec*0.8) & ",'tt':0,'fc':" & mColor & "}"
end sub


sub pDMDLabelSetShadow(labName,lCol,offsetx,offsety,isVis)  ' shadow of text
	dim ST: ST=1 : if isVIS=false Then St=0
	PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'shadowcolor':"& lCol &",'shadowtype': "& ST &", 'xoffset': " & offsetx &", 'yoffset': " & offsety & "}"
end sub

sub pDMDLabelSetOutShadow(labName, lCol,offsetx,offsety,isOutline,isVis)
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'shadowcolor':"&lCol&",'shadowstate': "& isVis &", 'xoffset': " & offsetx &", 'yoffset': " & offsety &", 'outline': "& isOutline &"}"
end sub


sub pDMDLabelSetPos(labName, xpos, ypos)
   PuPlayer.LabelSet pDMD,labName,"",1,"{'mt':2,'xpos':"&xpos& ",'ypos':"&ypos&"}"    
end sub

sub pDMDLabelSetSizeImage(labName, lWidth, lHeight)
   PuPlayer.LabelSet pDMD,labName,"",1,"{'mt':2,'width':"& lWidth & ",'height':"&lHeight&"}" 
end sub

sub pBackglassLabelSetSizeImage(labName, lWidth, lHeight)
   PuPlayer.LabelSet pBackglass,labName,"",1,"{'mt':2,'width':"& lWidth & ",'height':"&lHeight&"}" 
end sub

sub pBackglassLabelSetPos(labName, xpos, ypos)
   PuPlayer.LabelSet pBackglass,labName,"",1,"{'mt':2,'xpos':"&xpos& ",'ypos':"&ypos&"}"    
end sub

Sub pDMDLabelSetBorder(labName,lCol,offsetx,offsety,isVis)   'outline/border around text.
dim ST: ST=2 : if isVIS=false Then St=0
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'shadowcolor':"&lCol&",'shadowtype': "&ST&", 'xoffset': "&offsetx&", 'yoffset': "&offsety&"}"
end sub

Sub pBGLabelSetBorder(labName,lCol,offsetx,offsety,isVis)   'outline/border around text.
dim ST: ST=2 : if isVIS=false Then St=0
   PuPlayer.LabelSet pBackglass,labName,"`u`",1,"{'mt':2,'shadowcolor':"&lCol&",'shadowtype': "&ST&", 'xoffset': "&offsetx&", 'yoffset': "&offsety&"}"
end sub

Sub pupCreateLabelImage(lName, lFilename,xpos, ypos, Iwidth, Iheight, pagenum, lvis)
	PuPlayer.LabelNew pBackglass,lName ,"",50,RGB(100,100,100),0,1,1,1,1,pagenum,lvis
	PuPlayer.LabelSet pBackglass,lName,lFilename,lvis,"{'mt':2,'width':"&IWidth&",'height':"&Iheight&",'xpos':"&xpos&",'ypos':"&ypos&"}"
end Sub

Sub pupCreateLabelImageDMD(lName, lFilename,xpos, ypos, Iwidth, Iheight, pagenum, lvis)
	PuPlayer.LabelNew pDMD,lName ,"",50,RGB(100,100,100),0,1,1,1,1,pagenum,lvis
	PuPlayer.LabelSet pDMD,lName,lFilename,lvis,"{'mt':2,'width':"&IWidth&",'height':"&Iheight&",'xpos':"&xpos&",'ypos':"&ypos&"}"
end Sub

Sub pDMDLabelSet(labName,LabText)
	PuPlayer.LabelSet pDMD, labName, " " & LabText & " ", 1, ""   
end sub

Sub ClearHS
	PuPlayer.LabelSet pDMD, "HSMessage","",0,""
	PuPlayer.LabelSet pDMD, "HSInitials", "",0,""
End Sub

Sub ClearpMsgCombo
	PuPlayer.LabelSet pDMD, "ComboMessage","",0,""
	PuPlayer.LabelSet pDMD, "ComboValue2", "",0,""
End Sub


Sub pDMDLabelHide(labName)
	PuPlayer.LabelSet pDMD,labName," ",0,""
end sub

Sub pDMDLabelShow(labName)
	PuPlayer.LabelSet pDMD,labName," ",1,""
end sub

Sub pBackglassLabelShow(labName)
PuPlayer.LabelSet pBackglass,labName,"",1,""   
end sub

Sub pBackglassLabelHide(labName)
PuPlayer.LabelSet pBackglass,labName,"",0,""   
end sub

Sub pDMDAlwaysPAD  'will pad all text with a space before and after to help with possible text clipping.
     PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 5, ""FN"":46, ""PA"":1 }"    'slow pc mode
end Sub  

Sub pDMDAlwaysPADBG  'will pad all text with a space before and after to help with possible text clipping.
     PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":46, ""PA"":1 }"    'slow pc mode
end Sub  

' Use E as a prefix for event triggers in the Pup Pack Editor
Sub PuPEvent(EventNum)
	PuPlayer.B2SData "E" & EventNum, 1 'send event to puppack driver
End Sub

Dim OldPage
Sub pDMDSetPage(pagenum,caller)
	pDMDCurPage = pagenum
    PuPlayer.LabelShowPage pDMD,pagenum,0,""   'set page to blank 0 page if want off
'	dbg "Caller: " &caller &":" &pagenum
end Sub

Sub pBackglassSetPage(pagenum)    
    PuPlayer.LabelShowPage pBackglass,pagenum,0,""   'set page to blank 0 page if want off
    PBackglassCurPage=pagenum
end Sub


sub pDMDImageMoveTo(LabName, mLen, pStartX, pStartY, pEndX, pEndY)
	Dim sJson
	sJson = "{'mt':1, 'at':2, 'mlen':" & (mLen) & ", 'len':" & (600000) & ", 'xps':" & _
		pStartX & ",'xpe':" & pEndX &  ", 'yps':" & pStartY & ", 'ype':" & pEndY & ", 'tt':2 ,'ad':1}"
    PuPlayer.LabelSet pDMD, labName, "`u`", 1, sJson
end Sub

Sub pDMDSplashLines(msgText,msgText2,timeSec,mColor)'Para uso normal del DMD
PuPlayer.LabelShowPage pDMD,1,timeSec,""
PuPlayer.LabelSet pDMD,"Splash2a",msgText,1,"{'mt':2,'color': " & mColor &" }"  
PuPlayer.LabelSet pDMD,"Splash2b",msgText2,1,"{'mt':2,'color': " & mColor &" }"  
end Sub

Sub pDMDSplash3Lines(msgText,msgText2,msgText3,timeSec,mColor)'Para uso normal del DMD
PuPlayer.LabelShowPage pDMD,1,timeSec,""
PuPlayer.LabelSet pDMD,"Splash3a",msgText,1,"{'mt':2,'color': " & mColor &" }"  
PuPlayer.LabelSet pDMD,"Splash3b",msgText2,1,"{'mt':2,'color': " & mColor &" }"  
PuPlayer.LabelSet pDMD,"Splash3c",msgText3,1,"{'mt':2,'color': " & mColor &" }"  
end Sub

Sub pDMDSplashBig(msgText,timeSec, mColor)
PuPlayer.LabelShowPage pDMD,1,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,1,"{'mt':2,'color': " & mColor &" }"   
end sub

Sub HidePupSplashMessages
	'debug.print "HPSM"
	pDMDLabelHide "Splash"
	pDMDLabelHide "Splash2a"
	pDMDLabelHide "Splash2b"
	pDMDLabelHide "Splash3a"
	pDMDLabelHide "Splash3b"
	pDMDLabelHide "Splash3c"
End Sub

Sub pDMDHighScore(msgText,msgText2,timeSec,mColor)'
'PuPlayer.LabelShowPage pDMD,1,timeSec,""
	PuPlayer.LabelSet pDMD,"HSMessage",msgText,1,""
	PuPlayer.LabelSet pDMD,"HSInitials",msgText2,1,""   
end Sub

Sub ClearHighScoreDMD
'PuPlayer.LabelShowPage pDMD,1,timeSec,""
	PuPlayer.LabelSet pDMD,"HSMessage","",1,""
	PuPlayer.LabelSet pDMD,"HSInitials","",1,""   
End Sub


Sub ClearPupSplashMessages
	'debug.print "CPSM"
	PuPlayer.LabelSet pDMD, "Splash", "",  0, ""
	PuPlayer.LabelSet pDMD, "Splash2a", "",  0, ""
	PuPlayer.LabelSet pDMD, "Splash2b", "",  0, ""
	PuPlayer.LabelSet pDMD, "Splash3a", "",  0, ""
	PuPlayer.LabelSet pDMD, "Splash3b", "",  0, ""
	PuPlayer.LabelSet pDMD, "Splash3c", "",  0, ""
	PuPlayer.LabelSet pDMD, "SplashMG2a", "",  0, ""
	PuPlayer.LabelSet pDMD, "SplashMG2b", "",  0, ""
End Sub

Sub ClearPupAttractMessages
	PuPlayer.LabelSet pDMD, "Attract2a", "",  0, ""
	PuPlayer.LabelSet pDMD, "Attract2b", "",  0, ""
End Sub

Sub ChangeBall(swapimage)

	Dim BOT,b

	BOT = getballs
	if Ubound(BOT) < 1 Then exit Sub
	For b = 0 to Ubound(BOT)
		'BOT(b).image = CustomBallImage(swapimage)
		BOT(b).FrontDecal = swapimage
	Next

End Sub


Sub ClearMainModeTimerMessage
	pDMDLabelHide "MainModeTimerValue"
End Sub
'==============================================================
' Hack to return Narnia ball back in play
Sub TimerNarnia_Timer
	Dim BOT
	BOT = Getballs
'exit sub
    Dim b
	For b = 0 to UBound(BOT)
		if BOT(b).z < -200 Then
			BOT(b).x = 902 : BOT(b).y = 2000  : BOT(b).z = 0
			BOT(b).angmomx= 0 : BOT(b).angmomy= 0 :	BOT(b).angmomz= 0
			BOT(b).velx = 0 : BOT(b).vely = 0 : BOT(b).velz = 0
			KickerAutoPlunge.enabled = True
		end if
	next
end sub


Sub DisplayMainEventStatus
	Dim i
	For i = 1 to 11
		if Events(nPlayer,i) = 1 Then 
			Dbg "Mode " &i &" - Finished"
		Elseif Events(nPlayer,i) = 0 Then
			Dbg "Mode " &i &" - Not Started"
		End If

		if EventStarted = i Then Dbg "Event " &i &" - Running"
	Next

End Sub

Dim MainEventsCount, SideEventsCount
Sub CountEvents
Dim i
MainEventsCount = 0
SideEventsCount = 0

	For i = 1 to 11
		if Events(nPlayer,i) = 1 Then MainEventsCount = MainEventsCount + 1
	Next

	SideEventsCount = SideEventsFinished

End Sub

Sub EventMessageCleanup_Timer()
	if EventStarted = 0 Then HideMainModeMessages
	if SideEventNr = 0 Then HideSideEventMessages
End Sub

Sub HideMainModeMessages
	pDMDLabelHide "MainModeMessage"
	pDMDLabelHide "MainModeMessage2"
	pDMDLabelHide "MainModeTimerValue"
	pDMDLabelHide "MainModeProgress"
End Sub

Sub HideSideEventMessages
	pDMDLabelHide "SideEventMessage"
	pDMDLabelHide "SideEventMessage2"
	pDMDLabelHide "SideEventTimerValue"
	pDMDLabelHide "SideEventProgress"
End Sub

Sub HideBonusMessages
	pDMDLabelHide "BonusCaption"
	pDMDLabelHide "MainEventCaption"
	pDMDLabelHide "SideEventCaption"
	pDMDLabelHide "BonusTotal"
	pDMDLabelHide "BonusScoreValue"
End Sub

Sub pDMDSetHUD(isVis)   'show hide just the pBackGround object (HUD overlay).      
    pDMDLabelVisible "pBackground",isVis
end Sub  

Sub pDMDLabelVisible(labName, isVis)
	PuPlayer.LabelSet pDMD,labName,"`u`",isVis,""   
end sub



'******************************************************
'	ZPHY:  GNEREAL ADVICE ON PHYSICS
'******************************************************
'
' It's advised that flipper corrections, dampeners, and general physics settings should all be updated per these
' examples as all of these improvements work together to provide a realistic physics simulation.
'
' Tutorial videos provided by Bord
' Adding nFozzy roth physics : pt1 rubber dampeners 				https://youtu.be/AXX3aen06FM?si=Xqd-rcaqTlgEd_wx
' Adding nFozzy roth physics : pt2 flipper physics 					https://youtu.be/VSBFuK2RCPE?si=i8ne8Ao2co8rt7fy
' Adding nFozzy roth physics : pt3 other elements 					https://youtu.be/JN8HEJapCvs?si=hvgMOk-ej1BEYjJv
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
' | Force         | 12-15    |
' | Hit Threshold | 1.6-2    |
' | Scatter Angle | 2        |
'
' Slingshots
' | Hit Threshold      | 2    |
' | Slingshot Force    | 3-5  |
' | Slingshot Theshold | 2-3  |
' | Elasticity         | 0.85 |
' | Friction           | 0.8  |
' | Scatter Angle      | 1    |





'******************************************************
'	ZNFF:  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level we'll need the following:
'	1. flippers with specific physics settings
'	2. custom triggers for each flipper (TriggerLF, TriggerRF)
'	3. and, special scripting
'
' TriggerLF and RF should now be 27 vp units from the flippers. In addition, 3 degrees should be added to the end angle
' when creating these triggers.
'
' RF.ReProcessBalls Activeball and LF.ReProcessBalls Activeball must be added the flipper_collide subs.
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
' | EOS Torque         | 0.4            | 0.4                   | 0.375                  | 0.375              |
' | EOS Torque Angle   | 4              | 4                     | 6                      | 6                  |
'

'******************************************************
' Flippers Polarity (Select appropriate sub based on era)
'******************************************************

Dim LF : Set LF = New FlipperPolarity
Dim RF : Set RF = New FlipperPolarity

InitPolarity

'
''*******************************************
'' Late 70's to early 80's
'
'Sub InitPolarity()
'   dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 80
'		x.DebugOn=False ' prints some info in debugger
'
'
'        x.AddPt "Polarity", 0, 0, 0
'        x.AddPt "Polarity", 1, 0.05, - 2.7
'        x.AddPt "Polarity", 2, 0.16, - 2.7
'        x.AddPt "Polarity", 3, 0.22, - 0
'        x.AddPt "Polarity", 4, 0.25, - 0
'        x.AddPt "Polarity", 5, 0.3, - 1
'        x.AddPt "Polarity", 6, 0.4, - 2
'        x.AddPt "Polarity", 7, 0.5, - 2.7
'        x.AddPt "Polarity", 8, 0.65, - 1.8
'        x.AddPt "Polarity", 9, 0.75, - 0.5
'        x.AddPt "Polarity", 10, 0.81, - 0.5
'        x.AddPt "Polarity", 11, 0.88, 0
'        x.AddPt "Polarity", 12, 1.3, 0
'
'		x.AddPt "Velocity", 0, 0, 0.85
'		x.AddPt "Velocity", 1, 0.15, 0.85
'		x.AddPt "Velocity", 2, 0.2, 0.9
'		x.AddPt "Velocity", 3, 0.23, 0.95
'		x.AddPt "Velocity", 4, 0.41, 0.95
'		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
'		x.AddPt "Velocity", 6, 0.62, 1.0
'		x.AddPt "Velocity", 7, 0.702, 0.968
'		x.AddPt "Velocity", 8, 0.95,  0.968
'		x.AddPt "Velocity", 9, 1.03,  0.945
'		x.AddPt "Velocity", 10, 1.5,  0.945
'
'	Next
'
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'    LF.SetObjects "LF", LeftFlipper, TriggerLF
'    RF.SetObjects "RF", RightFlipper, TriggerRF
'End Sub
'
'
'
''*******************************************
'' Mid 80's
'
'Sub InitPolarity()
'   dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 80
'		x.DebugOn=False ' prints some info in debugger
'
'		x.AddPt "Polarity", 0, 0, 0
'		x.AddPt "Polarity", 1, 0.05, - 3.7
'		x.AddPt "Polarity", 2, 0.16, - 3.7
'		x.AddPt "Polarity", 3, 0.22, - 0
'		x.AddPt "Polarity", 4, 0.25, - 0
'		x.AddPt "Polarity", 5, 0.3, - 2
'		x.AddPt "Polarity", 6, 0.4, - 3
'		x.AddPt "Polarity", 7, 0.5, - 3.7
'		x.AddPt "Polarity", 8, 0.65, - 2.3
'		x.AddPt "Polarity", 9, 0.75, - 1.5
'		x.AddPt "Polarity", 10, 0.81, - 1
'		x.AddPt "Polarity", 11, 0.88, 0
'		x.AddPt "Polarity", 12, 1.3, 0
'
'		x.AddPt "Velocity", 0, 0, 0.85
'		x.AddPt "Velocity", 1, 0.15, 0.85
'		x.AddPt "Velocity", 2, 0.2, 0.9
'		x.AddPt "Velocity", 3, 0.23, 0.95
'		x.AddPt "Velocity", 4, 0.41, 0.95
'		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
'		x.AddPt "Velocity", 6, 0.62, 1.0
'		x.AddPt "Velocity", 7, 0.702, 0.968
'		x.AddPt "Velocity", 8, 0.95,  0.968
'		x.AddPt "Velocity", 9, 1.03,  0.945
'		x.AddPt "Velocity", 10, 1.5,  0.945
'
'	Next
'
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'    LF.SetObjects "LF", LeftFlipper, TriggerLF
'    RF.SetObjects "RF", RightFlipper, TriggerRF
'End Sub
'
''*******************************************
''  Late 80's early 90's
'
'Sub InitPolarity()
'	dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 60
'		x.DebugOn=False ' prints some info in debugger
'
'		x.AddPt "Polarity", 0, 0, 0
'		x.AddPt "Polarity", 1, 0.05, - 5
'		x.AddPt "Polarity", 2, 0.16, - 5
'		x.AddPt "Polarity", 3, 0.22, - 0
'		x.AddPt "Polarity", 4, 0.25, - 0
'		x.AddPt "Polarity", 5, 0.3, - 2
'		x.AddPt "Polarity", 6, 0.4, - 3
'		x.AddPt "Polarity", 7, 0.5, - 4.0
'		x.AddPt "Polarity", 8, 0.7, - 3.5
'		x.AddPt "Polarity", 9, 0.75, - 3.0
'		x.AddPt "Polarity", 10, 0.8, - 2.5
'		x.AddPt "Polarity", 11, 0.85, - 2.0
'		x.AddPt "Polarity", 12, 0.9, - 1.5
'		x.AddPt "Polarity", 13, 0.95, - 1.0
'		x.AddPt "Polarity", 14, 1, - 0.5
'		x.AddPt "Polarity", 15, 1.1, 0
'		x.AddPt "Polarity", 16, 1.3, 0
'
'		x.AddPt "Velocity", 0, 0, 0.85
'		x.AddPt "Velocity", 1, 0.15, 0.85
'		x.AddPt "Velocity", 2, 0.2, 0.9
'		x.AddPt "Velocity", 3, 0.23, 0.95
'		x.AddPt "Velocity", 4, 0.41, 0.95
'		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
'		x.AddPt "Velocity", 6, 0.62, 1.0
'		x.AddPt "Velocity", 7, 0.702, 0.968
'		x.AddPt "Velocity", 8, 0.95,  0.968
'		x.AddPt "Velocity", 9, 1.03,  0.945
'		x.AddPt "Velocity", 10, 1.5,  0.945

'	Next
'
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'	LF.SetObjects "LF", LeftFlipper, TriggerLF
'	RF.SetObjects "RF", RightFlipper, TriggerRF
'End Sub

'*******************************************
' Early 90's and after

Sub InitPolarity()
	Dim x, a
	a = Array(LF, RF)
	For Each x In a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
		x.DebugOn=False ' prints some info in debugger

		x.AddPt "Polarity", 0, 0, 0
		x.AddPt "Polarity", 1, 0.05, - 5.5
		x.AddPt "Polarity", 2, 0.16, - 5.5
		x.AddPt "Polarity", 3, 0.20, - 0.75
		x.AddPt "Polarity", 4, 0.25, - 1.25
		x.AddPt "Polarity", 5, 0.3, - 1.75
		x.AddPt "Polarity", 6, 0.4, - 3.5
		x.AddPt "Polarity", 7, 0.5, - 5.25
		x.AddPt "Polarity", 8, 0.7, - 4.0
		x.AddPt "Polarity", 9, 0.75, - 3.5
		x.AddPt "Polarity", 10, 0.8, - 3.0
		x.AddPt "Polarity", 11, 0.85, - 2.5
		x.AddPt "Polarity", 12, 0.9, - 2.0
		x.AddPt "Polarity", 13, 0.95, - 1.5
		x.AddPt "Polarity", 14, 1, - 1.0
		x.AddPt "Polarity", 15, 1.05, -0.5
		x.AddPt "Polarity", 16, 1.1, 0
		x.AddPt "Polarity", 17, 1.3, 0

		x.AddPt "Velocity", 0, 0, 0.85
		x.AddPt "Velocity", 1, 0.23, 0.85
		x.AddPt "Velocity", 2, 0.27, 1
		x.AddPt "Velocity", 3, 0.3, 1
		x.AddPt "Velocity", 4, 0.35, 1
		x.AddPt "Velocity", 5, 0.6, 1 '0.982
		x.AddPt "Velocity", 6, 0.62, 1.0
		x.AddPt "Velocity", 7, 0.702, 0.968
		x.AddPt "Velocity", 8, 0.95,  0.968
		x.AddPt "Velocity", 9, 1.03,  0.945
		x.AddPt "Velocity", 10, 1.5,  0.945

	Next
	
	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
	LF.SetObjects "LF", LeftFlipper, TriggerLF
	RF.SetObjects "RF", RightFlipper, TriggerRF
End Sub

'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

' modified 2023 by nFozzy
' Removed need for 'endpoint' objects
' Added 'createvents' type thing for TriggerLF / TriggerRF triggers.
' Removed AddPt function which complicated setup imo
' made DebugOn do something (prints some stuff in debugger)
'   Otherwise it should function exactly the same as before\
' modified 2024 by rothbauerw
' Added Reprocessballs for flipper collisions (LF.Reprocessballs Activeball and RF.Reprocessballs Activeball must be added to the flipper collide subs
' Improved handling to remove correction for backhand shots when the flipper is raised

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt		'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay		'delay before trigger turns off and polarity is disabled
	Private Flipper, FlipperStart, FlipperEnd, FlipperEndY, LR, PartialFlipCoef, FlipStartAngle
	Private Balls(20), balldata(20)
	Private Name
	
	Dim PolarityIn, PolarityOut
	Dim VelocityIn, VelocityOut
	Dim YcoefIn, YcoefOut
	Public Sub Class_Initialize
		ReDim PolarityIn(0)
		ReDim PolarityOut(0)
		ReDim VelocityIn(0)
		ReDim VelocityOut(0)
		ReDim YcoefIn(0)
		ReDim YcoefOut(0)
		Enabled = True
		TimeDelay = 50
		LR = 1
		Dim x
		For x = 0 To UBound(balls)
			balls(x) = Empty
			Set Balldata(x) = new SpoofBall
		Next
	End Sub
	
	Public Sub SetObjects(aName, aFlipper, aTrigger)
		
		If TypeName(aName) <> "String" Then MsgBox "FlipperPolarity: .SetObjects error: first argument must be a String (And name of Object). Found:" & TypeName(aName) End If
		If TypeName(aFlipper) <> "Flipper" Then MsgBox "FlipperPolarity: .SetObjects error: Second argument must be a flipper. Found:" & TypeName(aFlipper) End If
		If TypeName(aTrigger) <> "Trigger" Then MsgBox "FlipperPolarity: .SetObjects error: third argument must be a trigger. Found:" & TypeName(aTrigger) End If
		If aFlipper.EndAngle > aFlipper.StartAngle Then LR = -1 Else LR = 1 End If
		Name = aName
		Set Flipper = aFlipper
		FlipperStart = aFlipper.x
		FlipperEnd = Flipper.Length * Sin((Flipper.StartAngle / 57.295779513082320876798154814105)) + Flipper.X ' big floats for degree to rad conversion
		FlipperEndY = Flipper.Length * Cos(Flipper.StartAngle / 57.295779513082320876798154814105)*-1 + Flipper.Y
		
		Dim str
		str = "Sub " & aTrigger.name & "_Hit() : " & aName & ".AddBall ActiveBall : End Sub'"
		ExecuteGlobal(str)
		str = "Sub " & aTrigger.name & "_UnHit() : " & aName & ".PolarityCorrect ActiveBall : End Sub'"
		ExecuteGlobal(str)
		
	End Sub
	
	' Legacy: just no op
	Public Property Let EndPoint(aInput)
		
	End Property
	
	Public Sub AddPt(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out)
		Select Case aChooseArray
			Case "Polarity"
				ShuffleArrays PolarityIn, PolarityOut, 1
				PolarityIn(aIDX) = aX
				PolarityOut(aIDX) = aY
				ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity"
				ShuffleArrays VelocityIn, VelocityOut, 1
				VelocityIn(aIDX) = aX
				VelocityOut(aIDX) = aY
				ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef"
				ShuffleArrays YcoefIn, YcoefOut, 1
				YcoefIn(aIDX) = aX
				YcoefOut(aIDX) = aY
				ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
	End Sub
	
	Public Sub AddBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If IsEmpty(balls(x)) Then
				Set balls(x) = aBall
				Exit Sub
			End If
		Next
	End Sub
	
	Private Sub RemoveBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If TypeName(balls(x) ) = "IBall" Then
				If aBall.ID = Balls(x).ID Then
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
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next
	End Property
	
	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				balldata(x).Data = balls(x)
			End If
		Next
		FlipStartAngle = Flipper.currentangle
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub

	Public Sub ReProcessBalls(aBall) 'save data of balls in flipper range
		If FlipperOn() Then
			Dim x
			For x = 0 To UBound(balls)
				If Not IsEmpty(balls(x)) Then
					if balls(x).ID = aBall.ID Then
						If isempty(balldata(x).ID) Then
							balldata(x).Data = balls(x)
						End If
					End If
				End If
			Next
		End If
	End Sub

	'Timer shutoff for polaritycorrect
	Private Function FlipperOn()
		If GameTime < FlipAt+TimeDelay Then
			FlipperOn = True
		End If
	End Function
	
	Public Sub PolarityCorrect(aBall)
		If FlipperOn() Then
			Dim tmp, BallPos, x, IDX, Ycoef, BalltoFlip, BalltoBase, NoCorrection, checkHit
			Ycoef = 1
			
			'y safety Exit
			If aBall.VelY > -8 Then 'ball going down
				RemoveBall aBall
				Exit Sub
			End If
			
			'Find balldata. BallPos = % on Flipper
			For x = 0 To UBound(Balls)
				If aBall.id = BallData(x).id And Not IsEmpty(BallData(x).id) Then
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					BalltoFlip = DistanceFromFlipperAngle(BallData(x).x, BallData(x).y, Flipper, FlipStartAngle)
					If ballpos > 0.65 Then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)								'find safety coefficient 'ycoef' data
				End If
			Next
			
			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				If ballpos > 0.65 Then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)												'find safety coefficient 'ycoef' data
				NoCorrection = 1
			Else
				checkHit = 50 + (20 * BallPos) 

				If BalltoFlip > checkHit or (PartialFlipCoef < 0.5 and BallPos > 0.22) Then
					NoCorrection = 1
				Else
					NoCorrection = 0
				End If
			End If
			
			'Velocity correction
			If Not IsEmpty(VelocityIn(0) ) Then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
				
				'If partialflipcoef < 1 Then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
				
				If Enabled Then aBall.Velx = aBall.Velx*VelCoef
				If Enabled Then aBall.Vely = aBall.Vely*VelCoef
			End If
			
			'Polarity Correction (optional now)
			If Not IsEmpty(PolarityIn(0) ) Then
				Dim AddX
				AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				
				If Enabled and NoCorrection = 0 Then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef*VelCoef)
			End If
			If DebugOn Then debug.print "PolarityCorrect" & " " & Name & " @ " & GameTime & " " & Round(BallPos*100) & "%" & " AddX:" & Round(AddX,2) & " Vel%:" & Round(VelCoef*100)
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	Dim x, aCount
	aCount = 0
	ReDim a(UBound(aArray) )
	For x = 0 To UBound(aArray)		'Shuffle objects in a temp array
		If Not IsEmpty(aArray(x) ) Then
			If IsObject(aArray(x)) Then
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	If offset < 0 Then offset = 0
	ReDim aArray(aCount-1+offset)		'Resize original array
	For x = 0 To aCount-1				'set objects back into original array
		If IsObject(a(x)) Then
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
	BallSpeed = Sqr(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)		'Set up line via two points, no clamping. Input X, output Y
	Dim x, y, b, m
	x = input
	m = (Y2 - Y1) / (X2 - X1)
	b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius
	Public Property Let Data(aBall)
		With aBall
			x = .x
			y = .y
			z = .z
			velx = .velx
			vely = .vely
			velz = .velz
			id = .ID
			mass = .mass
			radius = .radius
		End With
	End Property
	Public Sub Reset()
		x = Empty
		y = Empty
		z = Empty
		velx = Empty
		vely = Empty
		velz = Empty
		id = Empty
		mass = Empty
		radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	Dim y 'Y output
	Dim L 'Line
	'find active line
	Dim ii
	For ii = 1 To UBound(xKeyFrame)
		If xInput <= xKeyFrame(ii) Then
			L = ii
			Exit For
		End If
	Next
	If xInput > xKeyFrame(UBound(xKeyFrame) ) Then L = UBound(xKeyFrame)		'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )
	
	If xInput <= xKeyFrame(LBound(xKeyFrame) ) Then Y = yLvl(LBound(xKeyFrame) )		 'Clamp lower
	If xInput >= xKeyFrame(UBound(xKeyFrame) ) Then Y = yLvl(UBound(xKeyFrame) )		'Clamp upper
	
	LinearEnvelope = Y
End Function

'******************************************************
'  FLIPPER TRICKS
'******************************************************
' To add the flipper tricks you must
'	 - Include a call to FlipperCradleCollision from within OnBallBallCollision subroutine
'	 - Include a call the CheckLiveCatch from the LeftFlipper_Collide and RightFlipper_Collide subroutines
'	 - Include FlipperActivate and FlipperDeactivate in the Flipper solenoid subs

RightFlipper.timerinterval = 1
Rightflipper.timerenabled = True

Sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
End Sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b
	   Dim BOT
	   BOT = GetBalls
	
	If Flipper1.currentangle = Endangle1 And EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'   debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					Exit Sub
				End If
			Next
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				End If
			Next
		End If
	Else
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 Then EOSNudge1 = 0
	End If
End Sub


Dim FCCDamping: FCCDamping = 0.4

Sub FlipperCradleCollision(ball1, ball2, velocity)
	if velocity < 0.7 then exit sub		'filter out gentle collisions
    Dim DoDamping, coef
    DoDamping = false
    'Check left flipper
    If LeftFlipper.currentangle = LFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, LeftFlipper) OR FlipperTrigger(ball2.x, ball2.y, LeftFlipper) Then DoDamping = true
    End If
    'Check right flipper
    If RightFlipper.currentangle = RFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, RightFlipper) OR FlipperTrigger(ball2.x, ball2.y, RightFlipper) Then DoDamping = true
    End If
    If DoDamping Then
		coef = FCCDamping
        ball1.velx = ball1.velx * coef: ball1.vely = ball1.vely * coef: ball1.velz = ball1.velz * coef
        ball2.velx = ball2.velx * coef: ball2.vely = ball2.vely * coef: ball2.velz = ball2.velz * coef
    End If
End Sub
	
'**********************************
' 	ZMAT: General Math Functions
'**********************************
' These get used throughout the script. 

Dim PI
PI = 4 * Atn(1)

Function dSin(degrees)
	dsin = Sin(degrees * Pi / 180)
End Function

Function dCos(degrees)
	dcos = Cos(degrees * Pi / 180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		End If
	ElseIf dx = 0 Then
		If dy = 0 Then
			Atn2 = 0
		Else
			Atn2 = Sgn(dy) * pi / 2
		End If
	End If
End Function

Function ArcCos(x)
	If x = 1 Then
		ArcCos = 0/180*PI
	ElseIf x = -1 Then
		ArcCos = 180/180*PI
	Else
		ArcCos = Atn(-x/Sqr(-x * x + 1)) + 2 * Atn(1)
	End If
End Function

Function max(a,b)
	If a > b Then
		max = a
	Else
		max = b
	End If
End Function

Function min(a,b)
	If a > b Then
		min = b
	Else
		min = a
	End If
End Function

' Used for drop targets
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy) 'Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
	Dim AB, BC, CD, DA
	AB = (bx * py) - (by * px) - (ax * py) + (ay * px) + (ax * by) - (ay * bx)
	BC = (cx * py) - (cy * px) - (bx * py) + (by * px) + (bx * cy) - (by * cx)
	CD = (dx * py) - (dy * px) - (cx * py) + (cy * px) + (cx * dy) - (cy * dx)
	DA = (ax * py) - (ay * px) - (dx * py) + (dy * px) + (dx * ay) - (dy * ax)
	
	If (AB <= 0 And BC <= 0 And CD <= 0 And DA <= 0) Or (AB >= 0 And BC >= 0 And CD >= 0 And DA >= 0) Then
		InRect = True
	Else
		InRect = False
	End If
End Function

Function InRotRect(ballx,bally,px,py,angle,ax,ay,bx,by,cx,cy,dx,dy)
	Dim rax,ray,rbx,rby,rcx,rcy,rdx,rdy
	Dim rotxy
	rotxy = RotPoint(ax,ay,angle)
	rax = rotxy(0) + px
	ray = rotxy(1) + py
	rotxy = RotPoint(bx,by,angle)
	rbx = rotxy(0) + px
	rby = rotxy(1) + py
	rotxy = RotPoint(cx,cy,angle)
	rcx = rotxy(0) + px
	rcy = rotxy(1) + py
	rotxy = RotPoint(dx,dy,angle)
	rdx = rotxy(0) + px
	rdy = rotxy(1) + py
	
	InRotRect = InRect(ballx,bally,rax,ray,rbx,rby,rcx,rcy,rdx,rdy)
End Function

Function RotPoint(x,y,angle)
	Dim rx, ry
	rx = x * dCos(angle) - y * dSin(angle)
	ry = x * dSin(angle) + y * dCos(angle)
	RotPoint = Array(rx,ry)
End Function



'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = Sqr((ax - bx) ^ 2 + (ay - by) ^ 2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) 'Distance between a point and a line where point Is px,py
	DistancePL = Abs((by - ay) * px - (bx - ax) * py + bx * ay - by * ax) / Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI / 180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax)) * 180 / PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle + 90)) + Flipper.x, Sin(Radians(Flipper.currentangle + 90)) + Flipper.y)
End Function

Function DistanceFromFlipperAngle(ballx, bally, Flipper, Angle)
	DistanceFromFlipperAngle = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Angle + 90)) + Flipper.x, Sin(Radians(angle + 90)) + Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle = Abs(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360
	
	If DistanceFromFlipper(ballx,bally,Flipper) < 48 And DiffAngle <= 90 And Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If
End Function

'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

Dim LFPress, RFPress, LFCount, RFCount
Dim LFState, RFState
Dim EOST, EOSA,Frampup, FElasticity,FReturn
Dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 0 '0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1.5 'EM's to late 80's - new recommendation by rothbauerw (previously 1)
Const EOSTnew = 1.2 '90's and later - new recommendation by rothbauerw (previously 0.8)
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode
	Case 0
		SOSRampup = 2.5
	Case 1
		SOSRampup = 6
	Case 2
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'   Const EOSReturn = 0.055  'EM's
'   Const EOSReturn = 0.045  'late 70's to mid 80's
'   Const EOSReturn = 0.035  'mid 80's to early 90's
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
	Flipper.eostorque = EOST * EOSReturn / FReturn
	
	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b, BOT
				BOT = GetBalls
		
		For b = 0 To UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= - 0.4 Then BOT(b).vely =  - 0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState)
	Dim Dir
	Dir = Flipper.startangle / Abs(Flipper.startangle) '-1 for Right Flipper
	
	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup
			Flipper.endangle = FEndAngle - 3 * Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) And FlipperPress = 1 Then
		If FCount = 0 Then FCount = GameTime
		
		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	ElseIf Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 And FlipperPress = 1 Then
		If FState <> 3 Then
			Flipper.eostorque = EOST
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If
	End If
End Sub

Const LiveDistanceMin = 5  'minimum distance In vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114 'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)
Const BaseDampen = 0.55

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
    Dim Dir, LiveDist
    Dir = Flipper.startangle / Abs(Flipper.startangle)    '-1 for Right Flipper
    Dim LiveCatchBounce   'If live catch is not perfect, it won't freeze ball totally
    Dim CatchTime
    CatchTime = GameTime - FCount
    LiveDist = Abs(Flipper.x - ball.x)

    If CatchTime <= LiveCatch And parm > 3 And LiveDist > LiveDistanceMin And LiveDist < LiveDistanceMax Then
        If CatchTime <= LiveCatch * 0.5 Then   'Perfect catch only when catch time happens in the beginning of the window
            LiveCatchBounce = 0
        Else
            LiveCatchBounce = Abs((LiveCatch / 2) - CatchTime)  'Partial catch when catch happens a bit late
        End If
        
        If LiveCatchBounce = 0 And ball.velx * Dir > 0 And LiveDist > 30 Then ball.velx = 0

        If ball.velx * Dir > 0 And LiveDist < 30 Then
            ball.velx = BaseDampen * ball.velx
            ball.vely = BaseDampen * ball.vely
            ball.angmomx = BaseDampen * ball.angmomx
            ball.angmomy = BaseDampen * ball.angmomy
            ball.angmomz = BaseDampen * ball.angmomz
        Elseif LiveDist > 30 Then
            ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
            ball.angmomx = 0
            ball.angmomy = 0
            ball.angmomz = 0
        End If
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf ActiveBall, parm
    End If
End Sub

'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************





'******************************************************
' 	ZDMP:  RUBBER  DAMPENERS
'******************************************************
' These are data mined bounce curves,
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx)
	RubbersD.dampen ActiveBall
	TargetBouncer ActiveBall, 1
End Sub

Sub dSleeves_Hit(idx)
	SleevesD.Dampen ActiveBall
	TargetBouncer ActiveBall, 0.7
End Sub

Dim RubbersD				'frubber
Set RubbersD = New Dampener
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	  'debug, reports In debugger (In vel, out cor); cor bounce curve (linear)

'for best results, try to match in-game velocity as closely as possible to the desired curve
'   RubbersD.addpoint 0, 0, 0.935   'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1		 'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	   'there's clamping so interpolate up to 56 at least

Dim SleevesD	'this is just rubber but cut down to 85%...
Set SleevesD = New Dampener
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	  'debug, reports In debugger (In vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'######################### Adjust these values to increase or lessen the elasticity

Dim FlippersD
Set FlippersD = New Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False
FlippersD.addpoint 0, 0, 1.1
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn   'tbpOut.text
	Public name, Threshold  'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
	End Sub
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Dampen(aBall)
		If threshold Then
			If BallSpeed(aBall) < threshold Then Exit Sub
		End If
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If debugOn Then str = name & " In vel:" & Round(cor.ballvel(aBall.id),2 ) & vbNewLine & "desired cor: " & Round(desiredcor,4) & vbNewLine & _
		"actual cor: " & Round(realCOR,4) & vbNewLine & "ballspeed coef: " & Round(coef, 3) & vbNewLine
		If Print Then Debug.print Round(cor.ballvel(aBall.id),2) & ", " & Round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef
		aBall.vely = aBall.vely * coef
		aBall.velz = aBall.velz * coef
		If debugOn Then TBPout.text = str
	End Sub
	
	Public Sub Dampenf(aBall, parm) 'Rubberizer is handle here
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If Abs(aball.velx) < 2 And aball.vely < 0 And aball.vely >  - 3.75 Then
			aBall.velx = aBall.velx * coef
			aBall.vely = aBall.vely * coef
			aBall.velz = aBall.velz * coef
		End If
	End Sub
	
	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		Dim x
		For x = 0 To UBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x) * aCoef
		Next
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
End Class

'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

Dim cor
Set cor = New CoRTracker

Class CoRTracker
	Public ballvel, ballvelx, ballvely
	
	Private Sub Class_Initialize
		ReDim ballvel(0)
		ReDim ballvelx(0)
		ReDim ballvely(0)
	End Sub
	
	Public Sub Update()	'tracks in-ball-velocity
		Dim str, b, AllBalls, highestID
		allBalls = GetBalls
		
		For Each b In allballs
			If b.id >= HighestID Then highestID = b.id
		Next
		
		If UBound(ballvel) < highestID Then ReDim ballvel(highestID)	'set bounds
		If UBound(ballvelx) < highestID Then ReDim ballvelx(highestID)	'set bounds
		If UBound(ballvely) < highestID Then ReDim ballvely(highestID)	'set bounds
		
		For Each b In allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

'*******************************************
'	ZTIM: Timers
'*******************************************

'The FrameTimer interval should be -1, so executes at the display frame rate
'The frame timer should be used to update anything visual, like some animations, shadows, etc.
'However, a lot of animations will be handled in their respective _animate subroutines.

Dim FrameTime, InitFrameTime
InitFrameTime = 0

FrameTimer.Interval = -1
Sub FrameTimer_Timer() 'The frame timer interval should be -1, so executes at the display frame rate
	FrameTime = GameTime - InitFrameTime
	InitFrameTime = GameTime	'Count frametime
	'Add animation stuff here
	RollingUpdate   		'update rolling sounds
	DoDTAnim				'handle drop target animations
	DoSTAnim				'handle stand up target animations
	BlendTheLighting
End Sub

'The CorTimer interval should be 10. It's sole purpose is to update the Cor calculations
CorTimer.Interval = 10
Sub CorTimer_Timer(): Cor.Update: End Sub
'
'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************

'******************************************************
' 	ZBOU: VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1	  '0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.9	 'Level of bounces. Recommmended value of 0.7-1

Sub TargetBouncer(aBall,defvalue)
	Dim zMultiplier, vel, vratio
	If TargetBouncerEnabled = 1 And aball.z < 30 Then
		'   debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		vel = BallSpeed(aBall)
		If aBall.velx = 0 Then vratio = 1 Else vratio = aBall.vely / aBall.velx
		Select Case Int(Rnd * 6) + 1
			Case 1
				zMultiplier = 0.2 * defvalue
			Case 2
				zMultiplier = 0.25 * defvalue
			Case 3
				zMultiplier = 0.3 * defvalue
			Case 4
				zMultiplier = 0.4 * defvalue
			Case 5
				zMultiplier = 0.45 * defvalue
			Case 6
				zMultiplier = 0.5 * defvalue
		End Select
		aBall.velz = Abs(vel * zMultiplier * TargetBouncerFactor)
		aBall.velx = Sgn(aBall.velx) * Sqr(Abs((vel ^ 2 - aBall.velz ^ 2) / (1 + vratio ^ 2)))
		aBall.vely = aBall.velx * vratio
		'   debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		'   debug.print "conservation check: " & BallSpeed(aBall)/vel
	End If
End Sub

'Add targets or posts to the TargetBounce collection if you want to activate the targetbouncer code from them
Sub TargetBounce_Hit(idx)
	TargetBouncer ActiveBall, 1
End Sub



'******************************************************
'	ZSSC: SLINGSHOT CORRECTION FUNCTIONS by apophis
'******************************************************
' To add these slingshot corrections:
'	 - On the table, add the endpoint primitives that define the two ends of the Slingshot
'	 - Initialize the SlingshotCorrection objects in InitSlingCorrection
'	 - Call the .VelocityCorrect methods from the respective _Slingshot event sub

Dim LS
Set LS = New SlingshotCorrection
Dim RS
Set RS = New SlingshotCorrection

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
	AddSlingsPt 0, 0.00, - 4
	AddSlingsPt 1, 0.45, - 7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4
End Sub

Sub AddSlingsPt(idx, aX, aY)		'debugger wrapper for adjusting flipper script In-game
	Dim a
	a = Array(LS, RS)
	Dim x
	For Each x In a
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
'Function RotPoint(x,y,angle)
'	dim rx, ry
'	rx = x*dCos(angle) - y*dSin(angle)
'	ry = x*dSin(angle) + y*dCos(angle)
'	RotPoint = Array(rx,ry)
'End Function

Class SlingshotCorrection
	Public DebugOn, Enabled
	Private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2
	
	Public ModIn, ModOut
	
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
		Enabled = True
	End Sub
	
	Public Property Let Object(aInput)
		Set Slingshot = aInput
	End Property
	
	Public Property Let EndPoint1(aInput)
		SlingX1 = aInput.x
		SlingY1 = aInput.y
	End Property
	
	Public Property Let EndPoint2(aInput)
		SlingX2 = aInput.x
		SlingY2 = aInput.y
	End Property
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
	
	
	Public Sub VelocityCorrect(aBall)
		Dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then
			XL = SlingX1
			YL = SlingY1
			XR = SlingX2
			YR = SlingY2
		Else
			XL = SlingX2
			YL = SlingY2
			XR = SlingX1
			YR = SlingY1
		End If
		
		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then
			If Abs(XR - XL) > Abs(YR - YL) Then
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If
		
		'Velocity angle correction
		If Not IsEmpty(ModIn(0) ) Then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'   debug.print " BallPos=" & BallPos &" Angle=" & Angle
			'   debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled Then aBall.Velx = RotVxVy(0)
			If Enabled Then aBall.Vely = RotVxVy(1)
			'   debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			'   debug.print " "
		End If
	End Sub
End Class

'******************************************************
' 	ZRDT:  DROP TARGETS by Rothbauerw
'******************************************************
' The Stand Up and Drop Target solutions improve the physics for targets to create more realistic behavior. It allows the ball
' to move through the target enabling the ability to score more than one target with a well placed shot.
' It also handles full target animation, switch handling and deflection on hit. For drop targets there is also a slight lift when
' the drop targets raise, bricking, and popping the ball up if it's over the drop target when it raises.
'
' Add a Timers named DTAnim and STAnim to editor to handle drop & standup target animations, or run them off an always-on 10ms timer (GameTimer)
' DTAnim.interval = 10
' DTAnim.enabled = True

' Sub DTAnim_Timer
' 	DoDTAnim
'	DoSTAnim
' End Sub

' For each drop target, we'll use two wall objects for physics calculations and one primitive for visuals and
' animation. We will not use target objects.  Place your drop target primitive the same as you would a VP drop target.
' The primitive should have it's pivot point centered on the x and y axis and at or just below the playfield
' level on the z axis. Orientation needs to be set using Rotz and bending deflection using Rotx. You'll find a hooded
' target mesh in this table's example. It uses the same texture map as the VP drop targets.
'
' For each stand up target we'll use a vp target, a laid back collidable primitive, and one primitive for visuals and animation.
' The visual primitive should should have it's pivot point centered on the x and y axis and the z should be at or just below the playfield.
' The target should animate backwards using transy.
'
' To create visual target primitives that work with the stand up and drop target code, follow the below instructions:
' (Other methods will work as well, but this is easy for even non-blender users to do)
' 1) Open a new blank table. Delete everything off the table in editor.
' 2) Copy and paste the VP target from your table into this blank table.
' 3) Place the target at x = 0, y = 0  (upper left hand corner) with an orientation of 0 (target facing the front of the table)
' 4) Under the file menu, select Export "OBJ Mesh"
' 5) Go to "https://threejs.org/editor/". Here you can modify the exported obj file. When you export, it exports your target and also 
'    the playfield mesh. You need to delete the playfield mesh here. Under the file menu, chose import, and select the obj you exported
'    from VPX. In the right hand panel, find the Playfield object and click on it and delete. Then use the file menu to Export OBJ.
' 6) In VPX, you can add a primitive and use "Import Mesh" to import the exported obj from the previous step. X,Y,Z scale should be 1.
'    The primitive will use the same target texture as the VP target object. 
'
' * Note, each target must have a unique switch number. If they share a same number, add 100 to additional target with that number.
' For example, three targets with switch 32 would use 32, 132, 232 for their switch numbers.
' The 100 and 200 will be removed when setting the switch value for the target.

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
Dim DT1, DT2, DT3, DT4, DT5, DT6, DT7

'Set array with drop target objects
'
'DropTargetvar = Array(primary, secondary, prim, swtich, animate)
'   primary:	primary target wall to determine drop
'   secondary:  wall used to simulate the ball striking a bent or offset target after the initial Hit
'   prim:	   primitive target used for visuals and animation
'				   IMPORTANT!!!
'				   rotz must be used for orientation
'				   rotx to bend the target back
'				   transz to move it up and down
'				   the pivot point should be in the center of the target on the x, y and at or below the playfield (0) on z
'   switch:	 ROM switch number
'   animate:	Array slot for handling the animation instrucitons, set to 0
'				   Values for animate: 1 - bend target (hit to primary), 2 - drop target (hit to secondary), 3 - brick target (high velocity hit to secondary), -1 - raise target
'   isDropped:  Boolean which determines whether a drop target is dropped. Set to false if they are initially raised, true if initially dropped.
'					Use the function DTDropped(switchid) to check a target's drop status.

Set DT1 = (new DropTarget)(ct1, ct1a, ct1p, 1, 0, False)
Set DT2 = (new DropTarget)(ct2, ct2a, ct2p, 2, 0, False)
Set DT3 = (new DropTarget)(ct3, ct3a, ct3p, 3, 0, False)
Set DT4 = (new DropTarget)(lt1, lt1a, lt1p, 4, 0, False)
Set DT5 = (new DropTarget)(lt2, lt2a, lt2p, 5, 0, False)
Set DT6 = (new DropTarget)(lt3, lt3a, lt3p, 6, 0, False)
Set DT7 = (new DropTarget)(lt4, lt4a, lt4p, 7, 0, False)

Dim DTArray
DTArray = Array(DT1, DT2, DT3, DT4, DT5, DT6, DT7)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 90 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 60 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 10 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30 'velocity at which the target will brick, set to '0' to disable brick
Const DTEnableBrick = 0 'Set to 0 to disable bricking, 1 to enable bricking
Const DTMass = 0.2 'Mass of the Drop Target (between 0 and 1), higher values provide more resistance

'******************************************************
'  DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch) ' RTP
	
	PlayTargetSound
	DTArray(i).animate = DTCheckBrick(ActiveBall,DTArray(i).prim)
	If DTArray(i).animate = 1 Or DTArray(i).animate = 3 Or DTArray(i).animate = 4 Then
		DTBallPhysics ActiveBall, DTArray(i).prim.roty, DTMass
	End If
	DoDTAnim
End Sub

Sub DTRaise(switch)
	Dim i
	i = DTArrayID(switch) 'RTP
	
	DTArray(i).animate =  - 1
	DoDTAnim
End Sub

Sub DTDrop(switch)
	Dim i
	i = DTArrayID(switch) ' RTP
	
	DTArray(i).animate = 1
	DoDTAnim
End Sub

Function DTArrayID(switch)
	Dim i
	For i = 0 To UBound(DTArray)
		'debug.print "I:" &i
		If DTArray(i).sw = switch Then
			DTArrayID = i
					'debug.print "P:" &DTArrayID
			Exit Function
		End If
	Next
End Function

Sub DTBallPhysics(aBall, angle, mass)
	Dim rangle,bangle,calc1, calc2, calc3
	rangle = (angle - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	
	calc1 = cor.BallVel(aball.id) * Cos(bangle - rangle) * (aball.mass - mass) / (aball.mass + mass)
	calc2 = cor.BallVel(aball.id) * Sin(bangle - rangle) * Cos(rangle + 4 * Atn(1) / 2)
	calc3 = cor.BallVel(aball.id) * Sin(bangle - rangle) * Sin(rangle + 4 * Atn(1) / 2)
	
	aBall.velx = calc1 * Cos(rangle) + calc2
	aBall.vely = calc1 * Sin(rangle) + calc3
End Sub

'Check if target is hit on it's face or sides and whether a 'brick' occurred
Function DTCheckBrick(aBall, dtprim)
	Dim bangle, bangleafter, rangle, rangle2, Xintersect, Yintersect, cdist, perpvel, perpvelafter, paravel, paravelafter
	rangle = (dtprim.rotz - 90) * 3.1416 / 180
	rangle2 = dtprim.rotz * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)
	
	Xintersect = (aBall.y - dtprim.y - Tan(bangle) * aball.x + Tan(rangle2) * dtprim.x) / (Tan(rangle2) - Tan(bangle))
	Yintersect = Tan(rangle2) * Xintersect + (dtprim.y - Tan(rangle2) * dtprim.x)
	
	cdist = Distance(dtprim.x, dtprim.y, Xintersect, Yintersect)
	
	perpvel = cor.BallVel(aball.id) * Cos(bangle - rangle)
	paravel = cor.BallVel(aball.id) * Sin(bangle - rangle)
	
	perpvelafter = BallSpeed(aBall) * Cos(bangleafter - rangle)
	paravelafter = BallSpeed(aBall) * Sin(bangleafter - rangle)
	
	If perpvel > 0 And  perpvelafter <= 0 Then
		If DTEnableBrick = 1 And  perpvel > DTBrickVel And DTBrickVel <> 0 And cdist < 8 Then
			DTCheckBrick = 3
		Else
			DTCheckBrick = 1
		End If
	ElseIf perpvel > 0 And ((paravel > 0 And paravelafter > 0) Or (paravel < 0 And paravelafter < 0)) Then
		DTCheckBrick = 4
	Else
		DTCheckBrick = 0
	End If
End Function

Sub DoDTAnim()
	Dim i
	For i = 0 To UBound(DTArray)
		DTArray(i).animate = DTAnimate(DTArray(i).primary,DTArray(i).secondary,DTArray(i).prim,DTArray(i).sw,DTArray(i).animate)
	Next
End Sub

Function DTAnimate(primary, secondary, prim, switch, animate)
	Dim transz, switchid
	Dim animtime, rangle
	
	switchid = switch
	
	Dim ind
	ind = DTArrayID(switchid) 'RTP
	
	rangle = prim.rotz * PI / 180
	
	DTAnimate = animate
	
	If animate = 0 Then
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	ElseIf primary.uservalue = 0 Then
		primary.uservalue = GameTime
	End If
	
	animtime = GameTime - primary.uservalue
	
	If (animate = 1 Or animate = 4) And animtime < DTDropDelay Then
		primary.collidable = 0
		If animate = 1 Then secondary.collidable = 1 Else secondary.collidable = 0
		prim.rotx = DTMaxBend * Cos(rangle)
		prim.roty = DTMaxBend * Sin(rangle)
		DTAnimate = animate
		Exit Function
	ElseIf (animate = 1 Or animate = 4) And animtime > DTDropDelay Then
		primary.collidable = 0
		If animate = 1 Then secondary.collidable = 1 Else secondary.collidable = 1 'If animate = 1 Then secondary.collidable = 1 Else secondary.collidable = 0 'updated by rothbauerw to account for edge case
		prim.rotx = DTMaxBend * Cos(rangle)
		prim.roty = DTMaxBend * Sin(rangle)
		animate = 2
		SoundDropTargetDrop prim
	End If
	
	If animate = 2 Then
		transz = (animtime - DTDropDelay) / DTDropSpeed * DTDropUnits *  - 1
		If prim.transz >  - DTDropUnits  Then
			prim.transz = transz
		End If
		
		prim.rotx = DTMaxBend * Cos(rangle) / 2
		prim.roty = DTMaxBend * Sin(rangle) / 2
		
		If prim.transz <= - DTDropUnits Then
			prim.transz =  - DTDropUnits
			secondary.collidable = 0
			DTArray(ind).isDropped = True 'Mark target as dropped
			'If UsingROM Then
			'	controller.Switch(Switchid mod 100) = 1
			'Else
				DTAction switchid
			'End If
			primary.uservalue = 0
			DTAnimate = 0
			Exit Function
		Else
			DTAnimate = 2
			Exit Function
		End If
	End If
	
	If animate = 3 And animtime < DTDropDelay Then
		primary.collidable = 0
		secondary.collidable = 1
		prim.rotx = DTMaxBend * Cos(rangle)
		prim.roty = DTMaxBend * Sin(rangle)
	ElseIf animate = 3 And animtime > DTDropDelay Then
		primary.collidable = 1
		secondary.collidable = 0
		prim.rotx = 0
		prim.roty = 0
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	End If
	
	If animate =  - 1 Then
		transz = (1 - (animtime) / DTDropUpSpeed) * DTDropUnits *  - 1
		
		If prim.transz =  - DTDropUnits Then
			Dim b
			Dim BOT
			BOT = GetBalls
			
			For b = 0 To UBound(BOT)
				If InRotRect(BOT(b).x,BOT(b).y,prim.x, prim.y, prim.rotz, - 25, - 10,25, - 10,25,25, - 25,25) And BOT(b).z < prim.z + DTDropUnits + 25 Then
					BOT(b).velz = 20
				End If
			Next
		End If
		
		If prim.transz < 0 Then
			prim.transz = transz
		ElseIf transz > 0 Then
			prim.transz = transz
		End If
		
		If prim.transz > DTDropUpUnits Then
			DTAnimate =  - 2
			prim.transz = DTDropUpUnits
			prim.rotx = 0
			prim.roty = 0
			primary.uservalue = GameTime
		End If
		primary.collidable = 0
		secondary.collidable = 1
		DTArray(ind).isDropped = False 'Mark target as not dropped
	End If
	
	If animate =  - 2 And animtime > DTRaiseDelay Then
		prim.transz = (animtime - DTRaiseDelay) / DTDropSpeed * DTDropUnits *  - 1 + DTDropUpUnits
		If prim.transz < 0 Then
			prim.transz = 0
			primary.uservalue = 0
			DTAnimate = 0
			
			primary.collidable = 1
			secondary.collidable = 0
		End If
	End If
End Function

Function DTDropped(switchid)
	Dim ind
	ind = DTArrayID(switchid) ' RTP
	
	DTDropped = DTArray(ind).isDropped
End Function

Sub DTAction(switchid)

if bSupressMainEvents Then Exit Sub
	Select Case switchid
		Case 1
			l38.State = 1
			l38b.State = 1
			DroppedTargets(1) = 1
			' score
			'checkmodes this switch is part of
			If EE Then
				PlayRandomPunch
				ct1.TimerEnabled = 1
				AddScore 1
				Exit Sub
			End If
			If EventStarted = 10 And Event10Hits(6) = 0 Then
				AddScore ScoreDrops
				ct1.TimerEnabled = 1
				l37.State = 0
				l38.State = 0
				l39.State = 0
				l40.State = 0
				l38b.State = 0
				l39b.State = 0
				l40b.State = 0
				Event10Hits(6) = 1
				CheckEvent10
				Exit Sub
			End If
			If EventStarted = 6 Then
				AddScore ScoreDrops
				nMainProgress = nMainProgress + 1
				UpdateDMDMainProgress
				CheckEvent6
				Exit Sub
			End If
			If HitManStarted Then
				AddScore ScoreDrops
				ScoreHitmanJackpot = ScoreHitmanJackpot + ScoreHitmanJackpotIncrease
				ct1.TimerEnabled = 1
				Exit Sub
			End If
			If EventStarted = 2 Then
				ct1.TimerEnabled = 1
				l38.State = 2
				l38b.State = 2
				AddScore ScoreDrops
				CTHits = CTHits + 1
				nMainProgress = nMainProgress + 1
				UpdateDMDMainProgress
				If CTHits = 6 Then WinEvent2
				Exit Sub
			End If

			If EventStarted OR SideEventStarted Then 'if any other event is started  then also raise up again the target, but only score points
				ct1.TimerEnabled = 1
				l38.State = 0
				l38b.State = 0
				AddScore ScoreDrops
				Exit Sub
			End If

			' normal working of the droptargets before en event
			If CurrentCT = 1 Then
				PlaySoundat "shot4", DeskIn
				CTDelay = 0
				CurrentCT = 0
				DropAll "Case 1"
				AddScore ScoreDrops * 3
			Else
				AddScore ScoreDrops
				PlaySoundat "gun6", DeskIn
				If CTDelay> 0 then
					l39.State = 0
					l40.State = 0
					l39b.State = 0
					l40b.State = 0
					CTDelay = 0
					CurrentCT = 0
				End If
				CheckDropTargets "DTActions - 1"
				'Exit Sub ' MerlinRTP 105H
			End If			
		Case 2
			l39.State = 1
			l39b.State = 1
			DroppedTargets(2) = 1
			' score
			If EE Then
				PlayRandomPunch
				ct2.TimerEnabled = 1
				AddScore 1
				Exit Sub
			End If
			'checkmodes this switch is part of
			If EventStarted = 6 Then
				nMainProgress = nMainProgress + 1
				UpdateDMDMainProgress
				AddScore ScoreDrops
				CheckEvent6
				Exit Sub
			End If
			If EventStarted = 10 And Event10Hits(6) = 0 Then
				AddScore ScoreDrops
				ct2.TimerEnabled = 1
				l37.State = 0
				l38.State = 0
				l39.State = 0
				l40.State = 0
				l38b.State = 0
				l39b.State = 0
				l40b.State = 0
				Event10Hits(6) = 1
				CheckEvent10
				Exit Sub
			End If
			If HitManStarted Then
				AddScore ScoreDrops
				ScoreHitmanJackpot = ScoreHitmanJackpot + ScoreHitmanJackpotIncrease
				ct2.TimerEnabled = 1
				Exit Sub
			End If
			If EventStarted = 2 Then 'raise up again the target
				ct2.TimerEnabled = 1
				l39.State = 2
				l39b.State = 2
				AddScore ScoreDrops
				CTHits = CTHits + 1
				nMainProgress = nMainProgress + 1
				UpdateDMDMainProgress
				If CTHits = 6 Then WinEvent2
				Exit Sub
			End If

			If EventStarted OR SideEventStarted Then 'if any other event is started  then also raise up again the target, but only score points
				ct2.TimerEnabled = 1
				l39.State = 0
				l39b.State = 0
				AddScore ScoreDrops
				Exit Sub
			End If

			' normal working of the droptargets before en event
			If CurrentCT = 2 Then
				PlaySoundat "shot4", DeskIn
				CTDelay = 0
				CurrentCT = 0
				DropAll "Case 2"
				AddScore ScoreDrops * 3
			Else
				AddScore ScoreDrops
				PlaySoundAt "gun6", DeskIn
				If CTDelay> 0 then
					l38.State = 0
					l40.State = 0
					l38b.State = 0
					l40b.State = 0
					CTDelay = 0
					CurrentCT = 0
				End If
				CheckDropTargets "Dt-Action - 2"
				'Exit Sub ' MerlinRTP 105H
			End If
			
		Case 3
				l40.State = 1
				l40b.State = 1
				DroppedTargets(3) = 1
				' score
				If EE Then
					PlayRandomPunch
					ct3.TimerEnabled = 1
					AddScore 1
					Exit Sub
				End If
				'checkmodes this switch is part of
				If EventStarted = 6 Then
					AddScore ScoreDrops
					nMainProgress = nMainProgress + 1
					UpdateDMDMainProgress
					CheckEvent6
					Exit Sub
				End If
				If EventStarted = 10 And Event10Hits(6) = 0 Then
					AddScore ScoreDrops
					ct3.TimerEnabled = 1
					l37.State = 0
					l38.State = 0
					l39.State = 0
					l40.State = 0
					l38b.State = 0
					l39b.State = 0
					l40b.State = 0
					Event10Hits(6) = 1
					CheckEvent10
					Exit Sub
				End If
				If HitManStarted Then
					AddScore ScoreDrops
					ScoreHitmanJackpot = ScoreHitmanJackpot + ScoreHitmanJackpotIncrease
					ct3.TimerEnabled = 1
					Exit Sub
				End If
				If EventStarted = 2 Then 'raise up again the target
					ct3.TimerEnabled = 1
					l40.State = 2
					l40b.State = 2
					AddScore ScoreDrops
					CTHits = CTHits + 1
					nMainProgress = nMainProgress + 1
					UpdateDMDMainProgress
					If CTHits = 6 Then WinEvent2
					Exit Sub
				End If

				If EventStarted OR SideEventStarted Then 'if any other event is started  then also raise up again the target, but only score points
					ct3.TimerEnabled = 1
					l40.State = 0
					l40b.State = 0
					AddScore ScoreDrops
					Exit Sub
				End If

				' normal working of the droptargets before en event
				If CurrentCT = 3 Then
					PlaySoundat "shot4", DeskIn
					CTDelay = 0
					CurrentCT = 0
					DropAll "Case 3"
					AddScore ScoreDrops * 3
				Else
					AddScore ScoreDrops
					PlaySoundAt "gun6", DeskIn
					If CTDelay> 0 then
						l38.State = 0
						l39.State = 0
						l38b.State = 0
						l39b.State = 0
						CTDelay = 0
						CurrentCT = 0
					End If
					CheckDropTargets "Dt-Action -3"
					'Exit Sub ' MerlinRTP 105H
				End If

		Case 4
			'lt1.timerenabled = 1
			l11.State = 1
			' score
			If EE Then
				PlayRandomPunch
				AddScore 1
				Exit Sub
			End If
			If BankStarted Then
				LightSeqWinEvent.Play SeqRandom, 60, , 1000
				AddScore ScoreBankRunJackpot
				WinSideEvent
				EndBank
				Exit Sub
			End If
			If EventStarted = 7 AND Event7State = 0 Then
				nMainProgress = nMainProgress + 1
				UpdateDMDMainProgress
				if VRRoom > 0 Then
					PuPlayer.LabelSet pDMD, "MainModeMessage", asMainModeMessages2(CurrentEvent), 1, "{'mt':2,'size':"& VR_MessageSize &"}"
				Else
					PuPlayer.LabelSet pDMD, "MainModeMessage", asMainModeMessages2(CurrentEvent), 1, ""
				End If
				L54.State = 2
				CheckEvent7
				AddScore ScoreDrops
				Exit Sub
			End If
			If EventStarted = 10 And Event10Hits(4) = 0 Then
				Event10Hits(4) = 1
				l11.State = 0
				l12.State = 0
				l13.State = 0
				l14.State = 0
				CheckEvent10
				AddScore ScoreDrops
				Exit Sub
			End If
			AddScore ScoreDrops
			addbonus Bonus_BankTargets
			BankTargets(1) = 1
			PlaySound "ker-chin1"
			CheckBank
			'checkmodes this switch is part of
			LastSwitchHit = "lt1"

		Case 5
			'lt2.timerenabled = 1
			l12.State = 1
			' score
			If EE Then
				PlayRandomPunch
				AddScore 1
				Exit Sub
			End If
			If BankStarted Then
				LightSeqWinEvent.Play SeqRandom, 60, , 1000
				AddScore ScoreBankRunJackpot
				WinSideEvent
				EndBank
				Exit Sub
			End If
			AddScore ScoreDrops
			addbonus Bonus_BankTargets
			If EventStarted = 7 AND Event7State = 0 Then
				nMainProgress = nMainProgress + 1
				if VRRoom > 0 Then
					PuPlayer.LabelSet pDMD, "MainModeMessage", asMainModeMessages2(CurrentEvent), 1, "{'mt':2,'size':"& VR_MessageSize &"}"
				Else
					PuPlayer.LabelSet pDMD, "MainModeMessage", asMainModeMessages2(CurrentEvent), 1, ""
				End If
				L54.State = 2
				UpdateDMDMainProgress
				CheckEvent7
				Exit Sub
			End If
			If EventStarted = 10 And Event10Hits(4) = 0 Then
				Event10Hits(4) = 1
				l11.State = 0
				l12.State = 0
				l13.State = 0
				l14.State = 0
				CheckEvent10
				Exit Sub
			End If
			BankTargets(2) = 1
			AddScore ScoreDrops
			PlaySound "ker-chin1"
			CheckBank
			'checkmodes this switch is part of
			LastSwitchHit = "lt2"
		Case 6
			'lt3.timerenabled = 1
			l13.State = 1
			' score
			If EE Then
				PlayRandomPunch
				AddScore 1
				Exit Sub
			End If
			If BankStarted Then
				LightSeqWinEvent.Play SeqRandom, 60, , 1000
				AddScore ScoreBankRunJackpot
				WinSideEvent
				EndBank
				Exit Sub
			End If
			AddScore ScoreDrops
			addbonus Bonus_BankTargets
			If EventStarted = 7 AND Event7State = 0 Then
				nMainProgress = nMainProgress + 1
				if VRRoom > 0 Then
					PuPlayer.LabelSet pDMD, "MainModeMessage", asMainModeMessages2(CurrentEvent), 1, "{'mt':2,'size':"& VR_MessageSize &"}"
				Else
					PuPlayer.LabelSet pDMD, "MainModeMessage", asMainModeMessages2(CurrentEvent), 1, ""
				End If
				L54.State = 2
				UpdateDMDMainProgress
				CheckEvent7
				Exit Sub
			End If
			If EventStarted = 10 And Event10Hits(4) = 0 Then
				Event10Hits(4) = 1
				l11.State = 0
				l12.State = 0
				l13.State = 0
				l14.State = 0
				CheckEvent10
				Exit Sub
			End If
			BankTargets(3) = 1
			AddScore ScoreDrops
			PlaySound "ker-chin1"
			CheckBank
			'checkmodes this switch is part of
			LastSwitchHit = "lt3"
		Case 7
			'lt4.timerenabled = 1
			l14.State = 1
			' score
			If EE Then
				PlayRandomPunch
				AddScore 1
				Exit Sub
			End If
			If BankStarted Then
				LightSeqWinEvent.Play SeqRandom, 60, , 1000
				AddScore ScoreBankRunJackpot
				WinSideEvent
				EndBank
				Exit Sub
			End If
			AddScore ScoreDrops
			addbonus Bonus_BankTargets
			If EventStarted = 7 AND Event7State = 0 Then
				nMainProgress = nMainProgress + 1
				if VRRoom > 0 Then
					PuPlayer.LabelSet pDMD, "MainModeMessage", asMainModeMessages2(CurrentEvent), 1, "{'mt':2,'size':"& VR_MessageSize &"}"
				Else
					PuPlayer.LabelSet pDMD, "MainModeMessage", asMainModeMessages2(CurrentEvent), 1, ""
				End If
				L54.State = 2
				UpdateDMDMainProgress
				CheckEvent7
				Exit Sub
			End If
			If EventStarted = 10 And Event10Hits(4) = 0 Then
				Event10Hits(4) = 1
				l11.State = 0
				l12.State = 0
				l13.State = 0
				l14.State = 0
				CheckEvent10
				Exit Sub
			End If
			BankTargets(4) = 1
			AddScore ScoreDrops
			PlaySound "ker-chin1"
			CheckBank
			'checkmodes this switch is part of
			LastSwitchHit = "lt4"
	End Select
End Sub

'******************************************************
'****  END DROP TARGETS
'******************************************************

'******************************************************
'	ZRST: STAND-UP TARGETS by Rothbauerw
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
Dim ST1, ST2, ST3, ST4, ST5, ST6, ST7

'Set array with stand-up target objects
'
'StandupTargetvar = Array(primary, prim, swtich)
'   primary:	vp target to determine target hit
'   prim:	   primitive target used for visuals and animation
'				   IMPORTANT!!!
'				   transy must be used to offset the target animation
'   switch:	 ROM switch number
'   animate:	Arrary slot for handling the animation instrucitons, set to 0
'
'You will also need to add a secondary hit object for each stand up (name sw11o, sw12o, and sw13o on the example Table1)
'these are inclined primitives to simulate hitting a bent target and should provide so z velocity on high speed impacts


Set ST1 = (new StandupTarget)(rott1, prott1,1, 0)
Set ST2 = (new StandupTarget)(rott2, prott2,2, 0)
Set ST3 = (new StandupTarget)(rott3, prott3,3, 0)
Set ST4 = (new StandupTarget)(rott4, prott4,4, 0)
Set ST5 = (new StandupTarget)(rott5, prott5,5, 0)
Set ST6 = (new StandupTarget)(rott6, prott6,6, 0)
Set ST7 = (new StandupTarget)(rott7, prott7,7, 0)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
'   STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST1, ST2, ST3, ST4, ST5, ST6, ST7)

'Configure the behavior of Stand-up Targets
Const STAnimStep = 1.5  'vpunits per animation step (control return to Start)
Const STMaxOffset = 9   'max vp units target moves when hit

Const STMass = 0.2	  'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				STAND-UP TARGETS FUNCTIONS
'******************************************************

Sub STHit(switch)
	Dim i
	i = STArrayID(switch)
	
	PlayTargetSound
	STArray(i).animate = STCheckHit(ActiveBall,STArray(i).primary)
	
	If STArray(i).animate <> 0 Then
		DTBallPhysics ActiveBall, STArray(i).primary.orientation, STMass
	End If
	DoSTAnim
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 To UBound(STArray)
		If STArray(i).sw = switch Then
			STArrayID = i
			Exit Function
		End If
	Next
End Function

Function STCheckHit(aBall, target) 'Check if target is hit on it's face
	Dim bangle, bangleafter, rangle, rangle2, perpvel, perpvelafter, paravel, paravelafter
	rangle = (target.orientation - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)
	
	perpvel = cor.BallVel(aball.id) * Cos(bangle - rangle)
	paravel = cor.BallVel(aball.id) * Sin(bangle - rangle)
	
	perpvelafter = BallSpeed(aBall) * Cos(bangleafter - rangle)
	paravelafter = BallSpeed(aBall) * Sin(bangleafter - rangle)
	
	If perpvel > 0 And  perpvelafter <= 0 Then
		STCheckHit = 1
	ElseIf perpvel > 0 And ((paravel > 0 And paravelafter > 0) Or (paravel < 0 And paravelafter < 0)) Then
		STCheckHit = 1
	Else
		STCheckHit = 0
	End If
End Function

Sub DoSTAnim()
	Dim i
	For i = 0 To UBound(STArray)
		STArray(i).animate = STAnimate(STArray(i).primary,STArray(i).prim,STArray(i).sw,STArray(i).animate)
	Next
End Sub

Function STAnimate(primary, prim, switch,  animate)
	Dim animtime
	
	STAnimate = animate
	
	If animate = 0  Then
		primary.uservalue = 0
		STAnimate = 0
		Exit Function
	ElseIf primary.uservalue = 0 Then
		primary.uservalue = GameTime
	End If
	
	animtime = GameTime - primary.uservalue
	
	If animate = 1 Then
		primary.collidable = 0
		prim.transy =  - STMaxOffset

		STAnimate = 2
		Exit Function
	ElseIf animate = 2 Then
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
'****   END STAND-UP TARGETS
'******************************************************

'******************************************************
'	ZPHY   END ---------------------
'******************************************************





'***************************************************************
'* ZVAR - Round robin variations on videos
'***************************************************************
Dim oRoundRobinVariations
Dim oRoundRobinPlays
Dim oRoundRobinEvents
Dim oRoundRobinLengths

Dim DrainPup, DrainKill
DrainPup = Array(401,402,403,404,405,406,407,408,409)
DrainKill = Array(901,902,903,904,905,906,907,908,909)

Dim DrainLength
DrainLength = Array(7005,6772,5372,9805,13172,8339,7705,9105,7272)

Sub InitRoundRobin
	Set oRoundRobinVariations = CreateObject("Scripting.Dictionary")
	Set oRoundRobinPlays = CreateObject("Scripting.Dictionary")
	Set oRoundRobinEvents = CreateObject("Scripting.Dictionary")
	Set oRoundRobinLengths = CreateObject("Scripting.Dictionary")



	oRoundRobinVariations.add "Drain", 9
	oRoundRobinPlays.add "Drain", 0
	oRoundRobinEvents.add "Drain", Array(401,402,403,404,405,406,407,408,409)
	oRoundRobinLengths.add "Drain", Array(7005,6772,5372,9805,13172,8339,7705,9105,7272)


	oRoundRobinVariations.add "BallLocked", 7
	oRoundRobinPlays.add "BallLocked", 0
	oRoundRobinEvents.add "BallLocked", Array(420, 421, 422, 423, 424 , 425, 426)
	oRoundRobinLengths.add "BallLocked", Array(4000, 4000, 4000, 4000, 4000, 4000, 4000)

	oRoundRobinVariations.add "NewGame", 1
	oRoundRobinPlays.add "NewGame", 0
	oRoundRobinEvents.add "NewGame", Array(701)
	oRoundRobinLengths.add "NewGame", Array(9600)



	oRoundRobinVariations.add "GameOver", 6
	oRoundRobinPlays.add "GameOver", 0
	oRoundRobinEvents.add "GameOver", Array(789, 790, 791, 792, 793, 794)
	oRoundRobinLengths.add "GameOver", Array(30205, 13939, 19972, 11305, 14305, 6505)


	oRoundRobinVariations.add "BallSaved", 8
	oRoundRobinPlays.add "BallSaved", 0
	oRoundRobinEvents.add "BallSaved", Array(467, 468, 469, 470, 471, 472, 473, 474)
	oRoundRobinLengths.add "BallSaved", Array(4005, 4005, 4039, 4039, 4039, 4072, 4039, 4912)
End Sub


Function RandomEvent(sEvent)
	Dim nIndex, nPlays, anEvents

	nIndex = IndexTmp
	Dbg "nIndex Event: " &nIndex
	'anEvents = oRoundRobinEvents.Item(sEvent)
	RandomEvent = anEvents(nIndex)
End Function

Function RandomLength(sEvent)
	Dim nIndex, nPlays, anEvents

	nIndex = IndexTmp
	Dbg "nIndex Length: " &nIndex
	'anEvents = oRoundRobinLengths.Item(sEvent)
	RandomLength = anEvents(nIndex)
End Function

Function RoundRobinEvent(sEvent)
	Dim nIndex, nPlays, anEvents

	nPlays = oRoundRobinPlays.Item(sEvent)
	oRoundRobinPlays.Item(sEvent) = nPlays + 1
	nIndex = nPlays Mod oRoundRobinVariations.Item(sEvent)
	anEvents = oRoundRobinEvents.Item(sEvent)
	RoundRobinEvent = anEvents(nIndex)
End Function

Function RoundRobinLength(sEvent)
	Dim nIndex, nPlays, anEvents

	nPlays = oRoundRobinPlays.Item(sEvent) - 1
	nIndex = nPlays Mod oRoundRobinVariations.Item(sEvent)
	anEvents = oRoundRobinLengths.Item(sEvent)
	RoundRobinLength = anEvents(nIndex)
End Function

Function rndZeroArrayCol1d(arrayIn)
    ' send in a 1d array (arrayIn) will return a random col for a column that has a value equal to "zero"
    Dim maxCol: maxCol = UBound(arrayIn) ' get the max number of cols in array 
    Randomize
    Dim col

' check if the array row as all columns with zero values
	Dim i
	dim allOnes: allOnes = True

	for i=0 to maxCol-1
		if arrayIn(i) = 0 Then
			allOnes = False   
		End If
	Next

	If allOnes then ' this array is set to all ones.  return -1
		rndZeroArrayCol1d = 999
		Exit Function 
	End If

	Do While 1
		col =  Int(maxCol*Rnd)
		If arrayIn(col) = 0 or allOnes Then
			Exit Do
		End If
	Loop

	rndZeroArrayCol1d = col ' return values
End Function
'--------------------------------------
Sub FindDrain()
		IndexTmp = rndZeroArrayCol1d(DrainTracker)
	if IndexTmp <> 999 Then	
		DrainTracker(IndexTmp) = 1
	Else
		ResetDrainTracker
		'FindDrain
	End If
End Sub

Sub ResetDrainTracker
Dim i
	For i = 0 to Ubound(DrainTracker)-1
		DrainTracker(i) = 0
	Next

End Sub
' #####################################
' ###### copy script from here on #####
' #####################################

' prepare some global vars to dim/brighten objects when using day-night slider
Dim DayNightAdjust , DNA30, DNA45, DNA90
If NightDay < 10 Then
	DNA30 = 0 : DNA45 = (NightDay-10)/20 : DNA90 = 0 : DayNightAdjust = 0.4
Else
	DNA30 = (NightDay-10)/30 : DNA45 = (NightDay-10)/45 : DNA90 = (NightDay-10)/90 : DayNightAdjust = NightDay/25
End If

Dim FlBumperFadeActual(6)
Dim FlBumperFadeTarget(6)
Dim FlBumperColor(6)
Dim FlBumperTop(6)
Dim FlBumperSmallLight(6)
Dim Flbumperbiglight(6)
Dim FlBumperDisk(6)
Dim FlBumperBase(6)
Dim FlBumperBulb(6)
Dim FlBumperscrews(6)
Dim FlBumperActive(6) 
Dim FlBumperHighlight(6)
Dim cnt : For cnt = 1 to 6 : FlBumperActive(cnt) = False : Next

' colors available are red, white, blue, orange, yellow, green, purple and blacklight

FlInitBumper 1, "purple"
FlInitBumper 2, "purple"
FlInitBumper 3, "purple"

' ### uncomment the statement below to change the color for all bumpers ###
' Dim ind : For ind = 1 to 5 : FlInitBumper ind, "green" : next

Sub FlInitBumper(nr, col)
	FlBumperActive(nr) = True
	' store all objects in an array for use in FlFadeBumper subroutine
	FlBumperFadeActual(nr) = 1 : FlBumperFadeTarget(nr) = 1.1: FlBumperColor(nr) = col
	Set FlBumperTop(nr) = Eval("bumpertop" & nr) : FlBumperTop(nr).material = "bumpertopmat" & nr
	Set FlBumperSmallLight(nr) = Eval("bumpersmalllight" & nr) : Set Flbumperbiglight(nr) = Eval("bumperbiglight" & nr)
	Set FlBumperDisk(nr) = Eval("bumperdisk" & nr) : Set FlBumperBase(nr) = Eval("bumperbase" & nr)
	Set FlBumperBulb(nr) = Eval("bumperbulb" & nr) : FlBumperBulb(nr).material = "bumperbulbmat" & nr
	Set FlBumperscrews(nr) = Eval("bumperscrews" & nr): FlBumperscrews(nr).material = "bumperscrew" & col
	Set FlBumperHighlight(nr) = Eval("bumperhighlight" & nr)
	' set the color for the two VPX lights
	select case col
		Case "red"
			FlBumperSmallLight(nr).color = RGB(255,4,0) : FlBumperSmallLight(nr).colorfull = RGB(255,24,0)
			FlBumperBigLight(nr).color = RGB(255,32,0) : FlBumperBigLight(nr).colorfull = RGB(255,32,0)
			FlBumperHighlight(nr).color = RGB(64,255,0)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.98
			FlBumperSmallLight(nr).TransmissionScale = 0
		Case "blue"
			FlBumperBigLight(nr).color = RGB(32,80,255) : FlBumperBigLight(nr).colorfull = RGB(32,80,255)
			FlBumperSmallLight(nr).color = RGB(0,80,255) : FlBumperSmallLight(nr).colorfull = RGB(0,80,255)
			FlBumperSmallLight(nr).TransmissionScale = 0 : MaterialColor "bumpertopmat" & nr, RGB(8,120,255)
			FlBumperHighlight(nr).color = RGB(255,16,8)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "green"
			FlBumperSmallLight(nr).color = RGB(8,255,8) : FlBumperSmallLight(nr).colorfull = RGB(8,255,8)
			FlBumperBigLight(nr).color = RGB(32,255,32) : FlBumperBigLight(nr).colorfull = RGB(32,255,32)
			FlBumperHighlight(nr).color = RGB(255,32,255) : MaterialColor "bumpertopmat" & nr, RGB(16,255,16) 
			FlBumperSmallLight(nr).TransmissionScale = 0.005
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "orange"
			FlBumperHighlight(nr).color = RGB(255,130,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).color = RGB(255,130,0) : FlBumperSmallLight(nr).colorfull = RGB (255,90,0)
			FlBumperBigLight(nr).color = RGB(255,190,8) : FlBumperBigLight(nr).colorfull = RGB(255,190,8)
		Case "white"
			FlBumperBigLight(nr).color = RGB(255,230,190) : FlBumperBigLight(nr).colorfull = RGB(255,230,190)
			FlBumperHighlight(nr).color = RGB(255,180,100) : 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.99
		Case "blacklight"
			FlBumperBigLight(nr).color = RGB(32,32,255) : FlBumperBigLight(nr).colorfull = RGB(32,32,255)
			FlBumperHighlight(nr).color = RGB(48,8,255) : 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "yellow"
			FlBumperSmallLight(nr).color = RGB(255,230,4) : FlBumperSmallLight(nr).colorfull = RGB(255,230,4)
			FlBumperBigLight(nr).color = RGB(255,240,50) : FlBumperBigLight(nr).colorfull = RGB(255,240,50)
			FlBumperHighlight(nr).color = RGB(255,255,220)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1 
			FlBumperSmallLight(nr).TransmissionScale = 0
		Case "purple"
			FlBumperBigLight(nr).color = RGB(80,32,255) : FlBumperBigLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).color = RGB(80,32,255) : FlBumperSmallLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).TransmissionScale = 0 : 
			FlBumperHighlight(nr).color = RGB(32,64,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
	end select
End Sub

Sub FlFadeBumper(nr, Z)
	FlBumperBase(nr).BlendDisableLighting = 0.5 * DayNightAdjust
'	UpdateMaterial(string, float wrapLighting, float roughness, float glossyImageLerp, float thickness, float edge, float edgeAlpha, float opacity,
'               OLE_COLOR base, OLE_COLOR glossy, OLE_COLOR clearcoat, VARIANT_BOOL isMetal, VARIANT_BOOL opacityActive,
'               float elasticity, float elasticityFalloff, float friction, float scatterAngle) - updates all parameters of a material
	FlBumperDisk(nr).BlendDisableLighting = (0.5 - Z * 0.3 )* DayNightAdjust	

	select case FlBumperColor(nr)

		Case "blue" :
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(38-24*Z,130 - 98*Z,255), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20  + 500 * Z / (0.5 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 5000 * (0.03 * Z +0.97 * Z^3)
			Flbumperbiglight(nr).intensity = 45 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 10000 * (Z^3) / (0.5 + DNA90)

		Case "green"	
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(16 + 16 * sin(Z*3.14),255,16 + 16 * sin(Z*3.14)), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 10 + 150 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 2 * DayNightAdjust + 20 * Z
			FlBumperBulb(nr).BlendDisableLighting = 7 * DayNightAdjust + 6000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 6000 * (Z^3) / (1 + DNA90)
		
		Case "red" 
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 16 - 11*Z + 16 * sin(Z*3.14),0), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 100 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 18 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 20 * DayNightAdjust + 9000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,20 + Z*4,8-Z*8)
		
		Case "orange"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 100 - 22*z  + 16 * sin(Z*3.14),Z*32), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 250 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 2500 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 4000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,100 + Z*50, 0)

		Case "white"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255,230 - 100 * Z, 200 - 150 * Z), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 180 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 5 * DayNightAdjust + 30 * Z
			FlBumperBulb(nr).BlendDisableLighting = 18 * DayNightAdjust + 3000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 14 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z^3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255,255 - 20*Z,255-65*Z) : FlBumperSmallLight(nr).colorfull = RGB(255,255 - 20*Z,255-65*Z)
			MaterialColor "bumpertopmat" & nr, RGB(255,235 - z*36,220 - Z*90)

		Case "blacklight"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 1, RGB(30-27*Z^0.03,30-28*Z^0.01, 255), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 900 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 60 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 30000 * Z^3
			Flbumperbiglight(nr).intensity = 40 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z^3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255-240*(Z^0.1),255 - 240*(Z^0.1),255) : FlBumperSmallLight(nr).colorfull = RGB(255-200*z,255 - 200*Z,255)
			MaterialColor "bumpertopmat" & nr, RGB(255-190*Z,235 - z*180,220 + 35*Z)

		Case "yellow"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 180 + 40*z, 48* Z), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 200 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 40 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 2000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,200, 24 - 24 * z)

		Case "purple" :
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(128-118*Z - 32 * sin(Z*3.14), 32-26*Z ,255), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 15  + 200 * Z / (0.5 + DNA30) 
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 10000 * (0.03 * Z +0.97 * Z^3)
			Flbumperbiglight(nr).intensity = 50 * Z / (1 + DNA45) 
			FlBumperHighlight(nr).opacity = 4000 * (Z^3) / (0.5 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(128-60*Z,32,255)


	end select
End Sub

Sub BumperTimer_Timer
	dim nr
	For nr = 1 to 6
		If FlBumperFadeActual(nr) < FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.8
			If FlBumperFadeActual(nr) > 0.99 Then FlBumperFadeActual(nr) = 1 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
		If FlBumperFadeActual(nr) > FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.4 / (FlBumperFadeActual(nr) + 0.1)
			If FlBumperFadeActual(nr) < 0.01 Then FlBumperFadeActual(nr) = 0 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
	next
End Sub

'' ###################################
'' ###### copy script until here #####
'' ###################################
DIM ODL: ODL = 1.5
Sub BlendTheLighting

	TVScreens.BlendDisableLighting = 2
	VR_Mega_TVScreens.BlendDisableLighting = 2
' Squares
	p002Off.blenddisablelighting = ODL + L2.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p002.blenddisablelighting = 5.0 *  L2.getinplayintensity
	p003Off.blenddisablelighting = ODL + L3.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p003.blenddisablelighting = 5.0 *  L3.getinplayintensity
	p004Off.blenddisablelighting = ODL + L4.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p004.blenddisablelighting = 5.0 *  L4.getinplayintensity
	p005Off.blenddisablelighting = ODL + L5.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p005.blenddisablelighting = 5.0 *  L5.getinplayintensity
	p006Off.blenddisablelighting = ODL + L6.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p006.blenddisablelighting = 5.0 *  L6.getinplayintensity

	p011Off.blenddisablelighting = ODL + L11.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p011.blenddisablelighting = 5.0 *  L11.getinplayintensity
	p012Off.blenddisablelighting = ODL + L12.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p012.blenddisablelighting = 5.0 *  L12.getinplayintensity
	p013Off.blenddisablelighting = ODL + L13.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p013.blenddisablelighting = 5.0 *  L13.getinplayintensity
	p014Off.blenddisablelighting = ODL + L14.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p014.blenddisablelighting = 5.0 *  L14.getinplayintensity

' circles
	p007Off.blenddisablelighting = ODL + L7.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p007.blenddisablelighting = 5.0 *  L7.getinplayintensity
	p008Off.blenddisablelighting = ODL + L8.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p008.blenddisablelighting = 5.0 *  L8.getinplayintensity
	p009Off.blenddisablelighting = ODL + L9.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p009.blenddisablelighting = 5.0 *  L9.getinplayintensity
	p010Off.blenddisablelighting = ODL + L10.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p010.blenddisablelighting = 5.0 *  L10.getinplayintensity

	p015Off.blenddisablelighting = ODL + L15.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p015.blenddisablelighting = 5.0 *  L15.getinplayintensity

	p050Off.blenddisablelighting = ODL + L50.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p050.blenddisablelighting = 5.0 *  L50.getinplayintensity
	p051Off.blenddisablelighting = ODL + L51.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p051.blenddisablelighting = 5.0 *  L51.getinplayintensity
	p052Off.blenddisablelighting = ODL + L52.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p052.blenddisablelighting = 5.0 *  L52.getinplayintensity

'triangles
	p038Off.blenddisablelighting = ODL + L38.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p038.blenddisablelighting = 5.0 *  L38.getinplayintensity
	p039Off.blenddisablelighting = ODL + L39.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p039.blenddisablelighting = 5.0 *  L39.getinplayintensity
	p040Off.blenddisablelighting = ODL + L40.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p040.blenddisablelighting = 5.0 *  L40.getinplayintensity

'arrows
	p016Off.blenddisablelighting = ODL + L16.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p016.blenddisablelighting = 5.0 *  L16.getinplayintensity
	p055Off.blenddisablelighting = ODL + L55.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p055.blenddisablelighting = 5.0 *  L55.getinplayintensity
	p023Off.blenddisablelighting = ODL + L23.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p023.blenddisablelighting = 5.0 *  L23.getinplayintensity
	p037Off.blenddisablelighting = ODL + L37.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p037.blenddisablelighting = 5.0 *  L37.getinplayintensity
	p042Off.blenddisablelighting = ODL + L42.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p042.blenddisablelighting = 5.0 *  L42.getinplayintensity
	p048Off.blenddisablelighting = ODL + L48.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p048.blenddisablelighting = 5.0 *  L48.getinplayintensity
	p053Off.blenddisablelighting = ODL + L53.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p053.blenddisablelighting = 5.0 *  L53.getinplayintensity
	p054Off.blenddisablelighting = ODL + L54.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p054.blenddisablelighting = 5.0 *  L54.getinplayintensity
	p025Off.blenddisablelighting = ODL + L25.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p025.blenddisablelighting = 5.0 *  L25.getinplayintensity
	p017Off.blenddisablelighting = ODL + L17.getinplayintensity ' ..optional    1.5 = how bright when lights are off
	p017.blenddisablelighting = 5.0 *  L17.getinplayintensity

End Sub

'******************************************************
'	ZBRL:  BALL ROLLING AND DROP SOUNDS
'******************************************************

' Be sure to call RollingUpdate in a timer with a 10ms interval see the GameTimer_Timer() sub

Sub RollingTimer_Timer()
    FlipperLSh.Rotz = LeftFlipper.CurrentAngle
    FlipperRSh.Rotz = RightFlipper.CurrentAngle
	LFLogo.Rotz = LeftFlipper.CurrentAngle
    RFLogo.Rotz = RightFlipper.CurrentAngle
End Sub


ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
	Dim i
	For i = 0 To tnob
		rolling(i) = False
	Next
End Sub

Sub RollingUpdate()
	Dim b
	   Dim BOT
	   BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 To tnob - 1
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next
	
	' exit the sub if no balls on the table
	If UBound(BOT) =  - 1 Then Exit Sub
	
	' play the rolling sound for each ball
	For b = 0 To UBound(BOT)
		If BallVel(BOT(b)) > 1 And BOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), - 1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If
		
		' Ball Drop Sounds
		If BOT(b).VelZ <  - 1 And BOT(b).z < 55 And BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If BOT(b).velz >  - 7 Then
					RandomSoundBallBouncePlayfieldSoft BOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard BOT(b)
				End If
			End If
		End If
		
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If
	Next
End Sub

'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************




'******************************************************
' 	ZRRL: RAMP ROLLING SFX
'******************************************************

'Ball tracking ramp SFX 1.0
'   Reqirements:
'		  * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'		  * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'		  * Create a Timer called RampRoll, that is enabled, with a interval of 100
'		  * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'		  * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'		  * To stop tracking ball
'				 * call WireRampOff
'				 * Otherwise, the ball will auto remove if it's below 30 vp units
'

Dim RampMinLoops
RampMinLoops = 4

' RampBalls
' Setup:  Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RampBalls(6,2)
Dim RampBalls(6,2)
'x,0 = ball x,1 = ID, 2 = Protection against ending early (minimum amount of updates)

'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
' Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
' Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
Dim RampType(6)

Sub WireRampOn(input)
	Waddball ActiveBall, input
	RampRollUpdate
End Sub

Sub WireRampOff()
	WRemoveBall ActiveBall.ID
End Sub

' WaddBall (Active Ball, Boolean)
Sub Waddball(input, RampInput) 'This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	Dim x
	For x = 1 To UBound(RampBalls)	'Check, don't add balls twice
		If RampBalls(x, 1) = input.id Then
			If Not IsEmpty(RampBalls(x,1) ) Then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next
	
	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
	For x = 1 To UBound(RampBalls)
		If IsEmpty(RampBalls(x, 1)) Then
			Set RampBalls(x, 0) = input
			RampBalls(x, 1) = input.ID
			RampType(x) = RampInput
			RampBalls(x, 2) = 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			Exit Sub
		End If
		If x = UBound(RampBalls) Then	 'debug
			Debug.print "WireRampOn error, ball queue Is full: " & vbNewLine & _
			RampBalls(0, 0) & vbNewLine & _
			TypeName(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbNewLine & _
			TypeName(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbNewLine & _
			TypeName(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbNewLine & _
			TypeName(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbNewLine & _
			TypeName(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbNewLine & _
			" "
		End If
	Next
End Sub

' WRemoveBall (BallId)
Sub WRemoveBall(ID) 'This subroutine is called from the RampRollUpdate subroutine and is used to remove and stop the ball rolling sounds
	'   Debug.Print "In WRemoveBall() + Remove ball from loop array"
	Dim ballcount
	ballcount = 0
	Dim x
	For x = 1 To UBound(RampBalls)
		If ID = RampBalls(x, 1) Then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		End If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		If Not IsEmpty(Rampballs(x,1)) Then ballcount = ballcount + 1
	Next
	If BallCount = 0 Then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer()
	RampRollUpdate
End Sub

Sub RampRollUpdate()	'Timer update
	Dim x
	For x = 1 To UBound(RampBalls)
		If Not IsEmpty(RampBalls(x,1) ) Then
			If BallVel(RampBalls(x,0) ) > 1 Then ' if ball is moving, play rolling sound
				If RampType(x) Then
					PlaySound("RampLoop" & x), - 1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), - 1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2) = RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
			End If
			If RampBalls(x,0).Z < 30 And RampBalls(x, 2) > RampMinLoops Then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		End If
	Next
	If Not RampBalls(0,0) Then RampRoll.enabled = 0
End Sub

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
Sub tbWR_Timer()	'debug textbox
	Me.text = "on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbNewLine & _
	"1 " & TypeName(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbNewLine & _
	"2 " & TypeName(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbNewLine & _
	"3 " & TypeName(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbNewLine & _
	"4 " & TypeName(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbNewLine & _
	"5 " & TypeName(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbNewLine & _
	"6 " & TypeName(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbNewLine & _
	" "
End Sub

Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
	BallPitch = pSlope(BallVel(ball), 1, - 1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, - 4000, 60, 7000)
End Function

Sub RandomSoundRampStop(obj)
	Select Case Int(rnd*3)
		Case 0: PlaySoundAtVol "wireramp_stop1", obj, 0.2*VolumeDial:PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 1: PlaySoundAtVol "wireramp_stop2", obj, 0.2*VolumeDial:PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 2: PlaySoundAtVol "wireramp_stop3", obj, 0.2*VolumeDial:PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
	End Select
End Sub

'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************

'******************************************************
'                                        KNOCKER
'******************************************************
'SolCallback(6)        = "SolKnocker" 'Change the solenoid number to the correct number for your table.

Sub SolKnocker(Enabled)
        If enabled Then
                KnockerSolenoid 'Add knocker position object
        End If
End Sub


'******************************************************
' 	ZFLE:  FLEEP MECHANICAL SOUNDS
'******************************************************

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.
' Create the following new collections:
'	 Metals (all metal objects, metal walls, metal posts, metal wire guides)
'	 Apron (the apron walls and plunger wall)
'	 Walls (all wood or plastic walls)
'	 Rollovers (wire rollover triggers, star triggers, or button triggers)
'	 Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
'	 Gates (plate gates)
'	 GatesWire (wire gates)
'	 Rubbers (all rubbers including posts, sleeves, pegs, and bands)
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
' Tutorial videos by Apophis
' Audio : Adding Fleep Part 1					https://youtu.be/rG35JVHxtx4?si=zdN9W4cZWEyXbOz_
' Audio : Adding Fleep Part 2					https://youtu.be/dk110pWMxGo?si=2iGMImXXZ0SFKVCh
' Audio : Adding Fleep Part 3					https://youtu.be/ESXWGJZY_EI?si=6D20E2nUM-xAw7xy


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1					  'volume level; range [0, 1]
NudgeLeftSoundLevel = 1				 'volume level; range [0, 1]
NudgeRightSoundLevel = 1				'volume level; range [0, 1]
NudgeCenterSoundLevel = 1			   'volume level; range [0, 1]
StartButtonSoundLevel = 0.1			 'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr   'volume level; range [0, 1]
PlungerPullSoundLevel = 1			   'volume level; range [0, 1]
RollingSoundFactor = 1.1 / 5

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010		'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635		'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0					   'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45					'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel		'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel	   'sound helper; not configurable
SlingshotSoundLevel = 0.95					  'volume level; range [0, 1]
BumperSoundFactor = 4.25						'volume multiplier; must not be zero
KnockerSoundLevel = 1						   'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2		  'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055 / 5			 'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075 / 5			   'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075 / 5			'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025		   'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025		   'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8	  'volume level; range [0, 1]
WallImpactSoundFactor = 0.075				   'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075 / 3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5 / 5			'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10	 'volume multiplier; must not be zero
DTSoundLevel = 0.25				 'volume multiplier; must not be zero
RolloverSoundLevel = 0.25		   'volume level; range [0, 1]
SpinnerSoundLevel = 0.5			 'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor

DrainSoundLevel = 0.8				   'volume level; range [0, 1]
BallReleaseSoundLevel = 1			   'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2	'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015	 'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025 / 5			 'volume multiplier; must not be zero

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
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, - 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
	PlaySound playsoundparams, - 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
	PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
	PlaySound soundname, 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
	PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	PlaySound soundname, 1,min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	PlaySound soundname, 1,min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.y * 2 / tableheight - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioFade = CSng(tmp ^ 10)
	Else
		AudioFade = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.x * 2 / tablewidth - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioPan = CSng(tmp ^ 10)
	Else
		AudioPan = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = CSng(BallVel(ball) ^ 2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = CSng((ball.velz) ^ 2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = Int(Sqr((ball.VelX ^ 2) + (ball.VelY ^ 2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * CSng(BallVel(ball) ^ 3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^ 2 * 15
End Function

Function RndInt(min, max) ' Sets a random number integer between min and max
	RndInt = Int(Rnd() * (max - min + 1) + min)
End Function

Function RndNum(min, max) ' Sets a random number between min and max
	RndNum = Rnd() * (max - min) + min
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////

Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeLeftSoundLevel * VolumeDial, - 0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
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
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd * 11) + 1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd * 7) + 1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd * 10) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd * 8) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
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
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd * 7) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd * 8) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm / 10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm / 10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd * 7) + 1), parm * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////

Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd * 4) + 1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////

Sub Rubbers_Hit(idx)
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 5 Then
		RandomSoundRubberStrong 1
	End If
	If finalspeed <= 5 Then
		RandomSoundRubberWeak()
	End If
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////

Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd * 10) + 1
		Case 1
			PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 2
			PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 3
			PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 4
			PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 5
			PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 6
			PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 7
			PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 8
			PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 9
			PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 10
			PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6 * voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////

Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd * 9) + 1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////

Sub Walls_Hit(idx)
	RandomSoundWall()
End Sub

Sub RandomSoundWall()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 5) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 4) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 3) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////

Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd * 13) + 1), Vol(ActiveBall) * MetalImpactSoundFactor
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
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Bounce_" & Int(Rnd * 2) + 1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////

Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd * 3) + 1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(ActiveBall.id) < 4) And cor.ballvely(ActiveBall.id) > 7 Then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////

Sub RandomSoundFlipperBallGuide()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd * 3) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd * 7) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////

Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 10 Then
		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft ActiveBall
	Else
		RandomSoundTargetHitWeak()
	End If
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////

Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd * 9) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd * 7) + 1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////

Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd * 5) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd * 2) + 1), GateSoundLevel, ActiveBall
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, ActiveBall
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)
	SoundPlayfieldGate
End Sub

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub Arch1_hit()
	If ActiveBall.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If ActiveBall.velx <  - 8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If ActiveBall.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If ActiveBall.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd * 2) + 1), SaucerLockSoundLevel, ActiveBall
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0
			PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1
			PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////

Sub OnBallBallCollision(ball1, ball2, velocity)

	FlipperCradleCollision ball1, ball2, velocity

	Dim snd
	Select Case Int(Rnd * 7) + 1
		Case 1
			snd = "Ball_Collide_1"
		Case 2
			snd = "Ball_Collide_2"
		Case 3
			snd = "Ball_Collide_3"
		Case 4
			snd = "Ball_Collide_4"
		Case 5
			snd = "Ball_Collide_5"
		Case 6
			snd = "Ball_Collide_6"
		Case 7
			snd = "Ball_Collide_7"
	End Select
	
	PlaySound (snd), 0, CSng(velocity) ^ 2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd * 6) + 1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd * 6) + 1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER RELAYS  ////////////////////////////

Const RelayFlashSoundLevel = 0.315  'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05	  'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025 * RelayGISoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025 * RelayGISoundLevel, obj
	End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025 * RelayFlashSoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025 * RelayFlashSoundLevel, obj
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
' 	ZFLD:  FLUPPER DOMES
'******************************************************
' Based on FlupperDoms2.2

' What you need in your table to use these flashers:
' Open this table and your table both in VPX
' Export all the materials domebasemat, Flashermaterial0 - 20 and import them in your table
' Export all textures (images) starting with the name "dome" and "ronddome" and import them into your table with the same names
' Export all textures (images) starting with the name "flasherbloom" and import them into your table with the same names
' Copy a set of 4 objects flasherbase, flasherlit, flasherlight and flasherflash from layer 7 to your table
' If you duplicate the four objects for a new flasher dome, be sure that they all end with the same number (in the 0-20 range)
' Copy the flasherbloom flashers from layer 10 to your table. you will need to make one per flasher dome that you plan to make
' Select the correct flasherbloom texture for each flasherbloom flasher, per flasher dome
' Copy the script below

' Place your flasher base primitive where you want the flasher located on your Table
' Then run InitFlasher in the script with the number of your flasher objects and the color of the flasher.  This will align the flasher object, light object, and
' flasher lit primitive.  It will also assign the appropriate flasher bloom images to the flasher bloom object.
'
' Example: InitFlasher 1, "green"
'
' Color Options: "blue", "green", "red", "purple", "yellow", "white", and "orange"

' You can use the RotateFlasher call to align the Rotz/ObjRotz of the flasher primitives with "handles".  Don't set those values in the editor,
' call the RotateFlasher sub instead (this call will likely crash VP if it's call for the flasher primitives without "handles")
'
' Example: RotateFlasher 1, 180		 'where 1 is the flasher number and 180 is the angle of Z rotation

' For flashing the flasher use in the script: "ObjLevel(1) = 1 : FlasherFlash1_Timer"
' This should also work for flashers with variable flash levels from the rom, just use ObjLevel(1) = xx from the rom (in the range 0-1)
'
' Notes (please read!!):
' - Setting TestFlashers = 1 (below in the ScriptsDirectory) will allow you to see how the flasher objects are aligned (need the targetflasher image imported to your table)
' - The rotation of the primitives with "handles" is done with a script command, not on the primitive itself (see RotateFlasher below)
' - Color of the objects are set in the script, not on the primitive itself
' - Screws are optional to copy and position manually
' - If your table is not named "Table1" then change the name below in the script
' - Every flasher uses its own material (Flashermaterialxx), do not use it for anything else
' - Lighting > Bloom Strength affects how the flashers look, do not set it too high
' - Change RotY and RotX of flasherbase only when having a flasher something other then parallel to the playfield
' - Leave RotX of the flasherflash object to -45; this makes sure that the flash effect is visible in FS and DT
' - If you want to resize a flasher, be sure to resize flasherbase, flasherlit and flasherflash with the same percentage
' - If you think that the flasher effects are too bright, change flasherlightintensity and/or flasherflareintensity below

' Some more notes for users of the v1 flashers and/or JP's fading lights routines:
' - Delete all textures/primitives/script/materials in your table from the v1 flashers and scripts before you start; they don't mix well with v2
' - Remove flupperflash(m) routines if you have them; they do not work with this new script
' - Do not try to mix this v2 script with the JP fading light routine (that is making it too complicated), just use the example script below

' example script for rom based tables (non modulated):

' SolCallback(25)="FlashRed"
'
' Sub FlashRed(flstate)
'	If Flstate Then
'		ObjTargetLevel(1) = 1
'	Else
'		ObjTargetLevel(1) = 0
'	End If
'   FlasherFlash1_Timer
' End Sub

' example script for rom based tables (modulated):

' SolModCallback(25)="FlashRed"
'
' Sub FlashRed(level)
'	ObjTargetLevel(1) = level/255 : FlasherFlash1_Timer
' End Sub

Sub Flash1(Enabled)
	If Enabled Then
		ObjTargetLevel(1) = 1
	Else
		ObjTargetLevel(1) = 0
	End If
	FlasherFlash1_Timer
	Sound_Flash_Relay enabled, Flasherbase1
End Sub

Sub Flash2(Enabled)
	If Enabled Then
		ObjTargetLevel(2) = 1
	Else
		ObjTargetLevel(2) = 0
	End If
	FlasherFlash2_Timer
	Sound_Flash_Relay enabled, Flasherbase2
End Sub

Sub Flash3(Enabled)
	If Enabled Then
		ObjTargetLevel(3) = 1
	Else
		ObjTargetLevel(3) = 0
	End If
	FlasherFlash3_Timer
	Sound_Flash_Relay enabled, Flasherbase3
End Sub

Sub Flash4(Enabled)
	If Enabled Then
		ObjTargetLevel(4) = 1
	Else
		ObjTargetLevel(4) = 0
	End If
	FlasherFlash4_Timer
	Sound_Flash_Relay enabled, Flasherbase1
End Sub

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness

' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object			 ***
Set TableRef = Table1		   ' *** change this, if your table has another name				   ***
FlasherLightIntensity = 0.1	 ' *** lower this, if the VPX lights are too bright (i.e. 0.1)	   ***
FlasherFlareIntensity = 0.3	 ' *** lower this, if the flares are too bright (i.e. 0.1)		   ***
FlasherBloomIntensity = 0.2	 ' *** lower this, if the blooms are too bright (i.e. 0.1)		   ***
FlasherOffBrightness = 0.5	  ' *** brightness of the flasher dome when switched off (range 0-2)  ***
' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20), ObjTargetLevel(20)
'Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height

'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
'InitFlasher 1, "green"
'InitFlasher 2, "red"
'InitFlasher 3, "blue"
'InitFlasher 4, "white"
InitFlasher 1, "red"

' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'   RotateFlasher 1,17
   RotateFlasher 1,0
'   RotateFlasher 3,90
'   RotateFlasher 4,90

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr)
	Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr)
	Set objlight(nr) = Eval("Flasherlight" & nr)
	Set objbloom(nr) = Eval("Flasherbloom" & nr)
	
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ = Atn( (tablewidth / 2 - objbase(nr).x) / (objbase(nr).y - tableheight * 1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ
		objflasher(nr).height = objbase(nr).z + 40
	End If
	
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0
	objlit(nr).visible = 0
	objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX
	objlit(nr).RotY = objbase(nr).RotY
	objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX
	objlit(nr).ObjRotY = objbase(nr).ObjRotY
	objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x
	objlit(nr).y = objbase(nr).y
	objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness
	
	'rothbauerw
	'Adjust the position of the flasher object to align with the flasher base.
	'Comment out these lines if you want to manually adjust the flasher object
	If objbase(nr).roty > 135 Then
		objflasher(nr).y = objbase(nr).y + 50
		objflasher(nr).height = objbase(nr).z + 20
	Else
		objflasher(nr).y = objbase(nr).y + 20
		objflasher(nr).height = objbase(nr).z + 50
	End If
	objflasher(nr).x = objbase(nr).x
	
	'rothbauerw
	'Adjust the position of the light object to align with the flasher base.
	'Comment out these lines if you want to manually adjust the flasher object
	objlight(nr).x = objbase(nr).x
	objlight(nr).y = objbase(nr).y
	objlight(nr).bulbhaloheight = objbase(nr).z - 10
	
	'rothbauerw
	'Assign the appropriate bloom image basked on the location of the flasher base
	'Comment out these lines if you want to manually assign the bloom images
	Dim xthird, ythird
	xthird = tablewidth / 3
	ythird = tableheight / 3
	If objbase(nr).x >= xthird And objbase(nr).x <= xthird * 2 Then
		objbloom(nr).imageA = "flasherbloomCenter"
		objbloom(nr).imageB = "flasherbloomCenter"
	ElseIf objbase(nr).x < xthird And objbase(nr).y < ythird Then
		objbloom(nr).imageA = "flasherbloomUpperLeft"
		objbloom(nr).imageB = "flasherbloomUpperLeft"
	ElseIf  objbase(nr).x > xthird * 2 And objbase(nr).y < ythird Then
		objbloom(nr).imageA = "flasherbloomUpperRight"
		objbloom(nr).imageB = "flasherbloomUpperRight"
	ElseIf objbase(nr).x < xthird And objbase(nr).y < ythird * 2 Then
		objbloom(nr).imageA = "flasherbloomCenterLeft"
		objbloom(nr).imageB = "flasherbloomCenterLeft"
	ElseIf  objbase(nr).x > xthird * 2 And objbase(nr).y < ythird * 2 Then
		objbloom(nr).imageA = "flasherbloomCenterRight"
		objbloom(nr).imageB = "flasherbloomCenterRight"
	ElseIf objbase(nr).x < xthird And objbase(nr).y < ythird * 3 Then
		objbloom(nr).imageA = "flasherbloomLowerLeft"
		objbloom(nr).imageB = "flasherbloomLowerLeft"
	ElseIf  objbase(nr).x > xthird * 2 And objbase(nr).y < ythird * 3 Then
		objbloom(nr).imageA = "flasherbloomLowerRight"
		objbloom(nr).imageB = "flasherbloomLowerRight"
	End If
	
	' set the texture and color of all objects
	Select Case objbase(nr).image
		Case "dome2basewhite"
			objbase(nr).image = "dome2base" & col
			objlit(nr).image = "dome2lit" & col
			
		Case "ronddomebasewhite"
			objbase(nr).image = "ronddomebase" & col
			objlit(nr).image = "ronddomelit" & col
			
		Case "domeearbasewhite"
			objbase(nr).image = "domeearbase" & col
			objlit(nr).image = "domeearlit" & col
	End Select
	If TestFlashers = 0 Then
		objflasher(nr).imageA = "domeflashwhite"
		objflasher(nr).visible = 0
	End If
	Select Case col
		Case "blue"
			objlight(nr).color = RGB(4,120,255)
			objflasher(nr).color = RGB(200,255,255)
			objbloom(nr).color = RGB(4,120,255)
			objlight(nr).intensity = 5000
			
		Case "green"
			objlight(nr).color = RGB(12,255,4)
			objflasher(nr).color = RGB(12,255,4)
			objbloom(nr).color = RGB(12,255,4)
			
		Case "red"
			objlight(nr).color = RGB(255,32,4)
			objflasher(nr).color = RGB(255,32,4)
			objbloom(nr).color = RGB(255,32,4)
			
		Case "purple"
			objlight(nr).color = RGB(230,49,255)
			objflasher(nr).color = RGB(255,64,255)
			objbloom(nr).color = RGB(230,49,255)
			
		Case "yellow"
			objlight(nr).color = RGB(200,173,25)
			objflasher(nr).color = RGB(255,200,50)
			objbloom(nr).color = RGB(200,173,25)
			
		Case "white"
			objlight(nr).color = RGB(255,240,150)
			objflasher(nr).color = RGB(100,86,59)
			objbloom(nr).color = RGB(255,240,150)
			
		Case "orange"
			objlight(nr).color = RGB(255,70,0)
			objflasher(nr).color = RGB(255,70,0)
			objbloom(nr).color = RGB(255,70,0)
	End Select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT And ObjFlasher(nr).RotX =  - 45 Then
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle)
	angle = ((angle + 360 - objbase(nr).ObjRotZ) Mod 180) / 30
	objbase(nr).showframe(angle)
	objlit(nr).showframe(angle)
End Sub

Sub FlashFlasher(nr)
	If Not objflasher(nr).TimerEnabled Then
		objflasher(nr).TimerEnabled = True
		'objflasher(nr).visible = 1
		objbloom(nr).visible = 1
		objlit(nr).visible = 1
	End If
	objflasher(nr).opacity = 1000 * FlasherFlareIntensity * ObjLevel(nr) ^ 2.5
	objbloom(nr).opacity = 100 * FlasherBloomIntensity * ObjLevel(nr) ^ 2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr) ^ 3
	objbase(nr).BlendDisableLighting = FlasherOffBrightness + 10 * ObjLevel(nr) ^ 3
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr) ^ 2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,False,0,0,0,0
	If Round(ObjTargetLevel(nr),1) > Round(ObjLevel(nr),1) Then
		ObjLevel(nr) = ObjLevel(nr) + 0.3
		If ObjLevel(nr) > 1 Then ObjLevel(nr) = 1
	ElseIf Round(ObjTargetLevel(nr),1) < Round(ObjLevel(nr),1) Then
		ObjLevel(nr) = ObjLevel(nr) * 0.85 - 0.01
		If ObjLevel(nr) < 0 Then ObjLevel(nr) = 0
	Else
		ObjLevel(nr) = Round(ObjTargetLevel(nr),1)
		objflasher(nr).TimerEnabled = False
	End If
	'   ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then
		objflasher(nr).TimerEnabled = False
		objflasher(nr).visible = 0
		objbloom(nr).visible = 0
		objlit(nr).visible = 0
	End If
End Sub

Sub FlasherFlash1_Timer()
	FlashFlasher(1)
End Sub
Sub FlasherFlash2_Timer()
	FlashFlasher(2)
End Sub
Sub FlasherFlash3_Timer()
	FlashFlasher(3)
End Sub
Sub FlasherFlash4_Timer()
	FlashFlasher(4)
End Sub

'******************************************************
'******  END FLUPPER DOMES
'******************************************************



sub RTP4

End Sub

Sub PlayerAudio
Dim i
		i = INT(RND(1) * 2)
		i = 1
	Select Case nPlayer
		Case 1
			Select case i
				Case 0:Say "Free_advice_dont_fuck_with_me", "14242424247313132222722424"
				Case 1:Say "Player1_1", "1324171223122413142"
				Case 2:Say "Watch_yourself_man", "1324238523133435212314241"
			End Select
		Case 2
			Select case i
				Case 0:Say "Free_advice_dont_fuck_with_me", "14242424247313132222722424"
				Case 1:Say "Player2_1", "1415314"
				Case 2:Say "Watch_yourself_man", "1324238523133435212314241"
			End Select
		Case 3
			Select case i
				Case 0:Say "Free_advice_dont_fuck_with_me", "14242424247313132222722424"
				Case 1:Say "Player3_1", "141531425"
				Case 2:Say "Watch_yourself_man", "1324238523133435212314241"
			End Select
		Case 4
			Select case i
				Case 0:Say "Free_advice_dont_fuck_with_me", "14242424247313132222722424"
				Case 1:Say "Player4_1", "1415314212174"
				Case 2:Say "Watch_yourself_man", "1324238523133435212314241"
			End Select
	End Select

End Sub



Sub ChangePlayer
	if bGameOver Then Exit Sub
'	StopAllMusic
	ClearQueues
	AutoPlungerReady = 0
	'ClearSnort
	PlaySound "Snort-Sound3"
	AudioQueue.Add "ChangePlayer2","ChangePlayer2",80,400,0,0,0,False
End Sub

Sub ChangePlayer2
	pDMDImageMoveTo "Snort", 2500, 70,24,-100,24
	AudioQueue.Add "PlayerAudio","PlayerAudio",80,2800,0,0,0,False
	AudioQueue.Add "PlayTheme","PlayTheme",70,6300,0,0,0,False
	'AudioQueue.Add "ClearSnort","ClearSnort",70,6500,0,0,0,False
End Sub

Sub ClearSnort
	pdmdlabelhide "Snort"
End Sub

Sub ChangeScores
	nPlayer = nPlayer + 1 'otherwise move to the next player
    If nPlayer> nPlayersInGame Then
		Dbg "Changing Player " &nPlayer &":" &nPlayersInGame
        BallsRemaining = BallsRemaining - 1
        if ballsremaining = 0 And nPlayer -1 = nPlayersInGame Then 
			nPlayer = nPlayer - 1
			Dbg "Leaving"
			Exit Sub
		Else	
			nPlayer = 1
		End If
    End If
	DMDUpdateBallNumber Balls
	DMDClearPlayerName
	DMDUpdatePlayerName
End Sub

Sub TonyStandUp
	Head.z = -50
	Mouth.z = -50

End Sub

Sub TonySitDown
	Head.z = -90
	Mouth.z = -90
End Sub

Sub TonyStandUpTimer_Timer()
	if GunUp.z = -50 Then TonyStandUpTimer.Enabled = 0:  exit sub
	Head.z = Head.z + 4
	Mouth.z = Mouth.z + 4
	GunUp.z = GunUp.z + 4
End Sub

Sub TonySitDownTimer_Timer()
	if GunUp.z = -90 Then TonySitDownTimer.Enabled = 0: GunUp.Visible = 0 : body.Visible = 1: TonySitDown: exit sub
	Head.z = Head.z - 4
	Mouth.z = Mouth.z - 4
	GunUp.z = GunUp.z - 4	
End Sub

Sub RTP2


End Sub

Sub rtp3
StartEvent11Pre
End Sub


'***************************************************************
' ZQUE: VPIN WORKSHOP ADVANCED QUEUING SYSTEM - 1.2.0
'***************************************************************
' WHAT IS IT?
' The VPin Workshop Advanced Queuing System allows table authors
' to put sub routine calls in a queue without creating a bunch
' of timers. There are many use cases for this: queuing sequences
' for light shows and DMD scenes, delaying solenoids until the
' DMD is finished playing all its sequences (such as holding a
' ball in a scoop), managing what actions take priority over
' others (e.g. an extra ball sequence is probably more important
' than a small jackpot), and many more.
'
' This system uses Scripting.Dictionary, a single timer, and the
' GameTime global to keep track of everything in the queue.
' This allows for better stability and a virtually unlimited
' number of items in the queue. It also allows for greater
' versatility, like pre-delays, queue delays, priorities, and
' even modifying items in the queue.
'
' The VPin Workshop Queuing System can replace vpmTimer as a
' proper queue system (each item depends on the previous)
' whereas vpmTimer is a collection of virtual timers that run
' in parallel. It also adds on other advanced functionality.
' However, this queue system does not have ROM support out of
' the box like vpmTimer does.
'
' I recommend reading all the comments before you implement the
' queuing system into your table.
'
' WHAT YOU NEED to use the queuing system:
' 1) Put this VBS file in your scripts folder, or copy / paste
'    the code into your table script (and skip step 2).
' 2) Include this file via Scripting.FileSystemObject, and
'    ExecuteGlobal it.
' 3) Make one or more queues by constructing the vpwQueueManager:
'    Dim queue : Set queue = New vpwQueueManager
' 4) Create (or use) a timer that is always enabled and
'    preferably has an interval of 1 millisecond. Use a
'    higher number for less time precision but less resource
'    use. You only need one timer even if you
'    have multiple queues.
' 5) For each queue you created, call its Tick routine in
'    the timer's *_timer() routine:
'    queue.Tick
' 6) You're done! Refer to the routines in vpwQueueManager to
'    learn how to use the queuing system.
'
' TUTORIAL: https://youtu.be/kpPYgOiUlxQ
'***************************************************************

'===========================================
' vpwQueueManager
' This class manages a queue of
' vpwQueueItems and executes them.
'===========================================
Class vpwQueueManager
	Public qItems ' A dictionary of vpwQueueItems in the queue (do NOT use native Scripting.Dictionary.Add/Remove; use the vpwQueueManager's Add/Remove methods instead!)
	Public preQItems ' A dictionary of vpwQueueItems pending to be added to qItems
	Public debugOn 'Null = no debug. String = activate debug by using this unique label for the queue. REQUIRES baldgeek's error logs.
	
	'----------------------------------------------------------
	' vpwQueueManager.qCurrentItem
	' This contains a string of the key currently active / at
	' the top of the queue. An empty string means no items are
	' active right now.
	' This is an important property; it should be monitored
	' in another timer or routine whenever you Add a queue item
	' with a -1 (indefinite) preDelay or postDelay. Then, for
	' preDelay, ExecuteCurrentItem should be called to run the
	' queue item. And for postDelay, DoNextItem should be
	' called to move to the next item in the queue.
	'
	' For example, let's say you add a queue item with the
	' key "kickTheBall" and an indefinite preDelay. You want
	' to wait until another timer fires before this queue item
	' executes and kicks the ball out of a scoop. In the other
	' timer, you will monitor qCurrentItem. Once it equals
	' "kickTheBall", call ExecuteCurrentItem, which will run
	' the queue item and presumably kick out the ball.
	'
	' WARNING!: If you do not properly execute one of these
	' callback routines on an indefinite delayed item, then
	' the queue will effectively freeze / stop until you do.
	'---------------------------------------------------------
	Public qCurrentItem
	
	Public preDelayTime ' The GameTime the preDelay for the qCurrentItem was started
	Public postDelayTime ' The GameTime the postDelay for the qCurrentItem was started
	
	Private onQueueEmpty ' A string or object to be called every time the queue empties (use the QueueEmpty property to get/set this)
	Private queueWasEmpty ' Boolean to determine if the queue was already empty when firing DoNextItem
	Private preDelayTransfer ' Number of milliseconds of preDelay to transfer over to the next queue item when doNextItem is called
	
	Private Sub Class_Initialize
		Set qItems = CreateObject("Scripting.Dictionary")
		Set preQItems = CreateObject("Scripting.Dictionary")
		qCurrentItem = ""
		onQueueEmpty = ""
		queueWasEmpty = True
		debugOn = Null
		preDelayTransfer = 0
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.Tick
	' This is where all the magic happens! Call this method in
	' your timer's _timer routine to check the queue and
	' execute the necessary methods. We do not iterate over
	' every item in the queue here, which allows for superior
	' performance even if you have hundreds of items in the
	' queue.
	'----------------------------------------------------------
	Public Sub Tick()
		Dim item
		If qItems.Count > 0 Then ' Don't waste precious resources if we have nothing in the queue
			
			' If no items are active, or the currently active item no longer exists, move to the next item in the queue.
			' (This is also a failsafe to ensure the queue continues to work even if an item gets manually deleted from the dictionary).
			If qCurrentItem = "" Or Not qItems.Exists(qCurrentItem) Then
				DoNextItem
			Else ' We are good; do stuff as normal
				Set item = qItems.item(qCurrentItem)
				
				If item.Executed Then
					' If the current item was executed and the post delay passed, go to the next item in the queue
					If item.postDelay >= 0 And GameTime >= (postDelayTime + item.postDelay) Then
						DebugLog qCurrentItem & " - postDelay of " & item.postDelay & " passed."
						DoNextItem
					End If
				Else
					' If the current item expires before it can be executed, go to the next item in the queue
					If item.timeToLive > 0 And GameTime >= (item.queuedOn + item.timeToLive) Then
						DebugLog qCurrentItem & " - expired (Time To live). Moving To the Next queue item."
						DoNextItem
					End If
					
					' If the current item was not executed yet and the pre delay passed, then execute it
					If item.preDelay >= 0 And GameTime >= (preDelayTime + item.preDelay) Then
						DebugLog qCurrentItem & " - preDelay of " & item.preDelay & " passed. Executing callback."
						item.Execute
						preDelayTime = 0
						postDelayTime = GameTime
					End If
				End If
			End If
		End If
		
		' Loop through each item in the pre-queue to find any that is ready to be added
		If preQItems.Count > 0 Then
			Dim k, key
			k = preQItems.Keys
			For Each key In k
				Set item = preQItems.Item(key)
				
				' If a queue item was pre-queued and is ready to be considered as actually in the queue, add it
				If GameTime >= (item.queuedOn + item.preQueueDelay) Then
					DebugLog key & " (preQueue) - preQueueDelay of " & item.preQueueDelay & " passed. Item added To the main queue."
					preQItems.Remove key
					Me.Add key, item.Callback, item.priority, 0, item.preDelay, item.postDelay, item.timeToLive, item.executeNow
				End If
			Next
		End If
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.DoNextItem
	' Goes to the next item in the queue and deletes the
	' currently active one.
	'----------------------------------------------------------
	Public Sub DoNextItem()
		If Not qCurrentItem = "" Then
			If qItems.Exists(qCurrentItem) Then qItems.Remove qCurrentItem ' Remove the current item from the queue if it still exists
			qCurrentItem = ""
		End If
		
		If qItems.Count > 0 Then
			Dim k, key
			Dim nextItem
			Dim nextItemPriority
			Dim item
			nextItemPriority = 0
			nextItem = ""
			
			' Find which item needs to run next based on priority first, queue order second (ignore items with an active preQueueDelay)
			k = qItems.Keys
			For Each key In k
				Set item = qItems.Item(key)
				
				If item.preQueueDelay <= 0 And item.priority > nextItemPriority Then
					nextItem = key
					nextItemPriority = item.priority
				End If
			Next
			
			If qItems.Exists(nextItem) Then
				Set item = qItems.Item(nextItem)
				DebugLog "DoNextItem - checking " & nextItem & " (priority " & item.priority & ")"
				
				' Make sure the item is not expired and not already executed. If it is, remove it and re-call doNextItem
				If (item.timeToLive > 0 And GameTime >= (item.queuedOn + item.timeToLive + preDelayTransfer)) Or item.executed = True Then
					DebugLog "DoNextItem - " & nextItem & " expired (Time To live) Or already executed. Removing And going To the Next item."
					qItems.Remove nextItem
					DoNextItem
					Exit Sub
				End If
				
				'Transfer preDelay time when applicable
				If preDelayTransfer > 0 And item.preDelay > -1 Then
					DebugLog "DoNextItem " & nextItem & " - Transferred remaining postDelay of " & preDelayTransfer & " milliseconds from previously overridden queue item To its preDelay And timeToLive"
					qItems.Item(nextItem).preDelay = item.preDelay + preDelayTransfer
					If item.timeToLive > 0 Then qItems.Item(nextItem).timeToLive = item.timeToLive + preDelayTransfer
					preDelayTransfer = 0
				End If
				
				' Set item as current / active, and execute if it has no pre-delay (otherwise Tick will take care of pre-delay)
				qCurrentItem = nextItem
				If item.preDelay = 0 Then
					DebugLog "DoNextItem - " & nextItem & " Now active. It has no preDelay, so executing callback immediately."
					item.Execute
					preDelayTime = 0
					postDelayTime = GameTime
				Else
					DebugLog "DoNextItem - " & nextItem & " Now active. Waiting For a preDelay of " & item.preDelay & " before executing."
					preDelayTime = GameTime
					postDelayTime = 0
				End If
			End If
		ElseIf queueWasEmpty = False Then
			DebugLog "DoNextItem - Queue Is Now Empty; executing queueEmpty callback."
			CallQueueEmpty() ' Call QueueEmpty if this was the last item in the queue
		End If
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.ExecuteCurrentItem
	' Helper routine that can be used when the current item is
	' on an indefinite preDelay. Call this when you are ready
	' for that item to execute.
	'----------------------------------------------------------
	Public Sub ExecuteCurrentItem()
		If Not qCurrentItem = "" And qItems.Exists(qCurrentItem) Then
			DebugLog "ExecuteCurrentItem - Executing the callback For " & qCurrentItem & "."
			Dim item
			Set item = qItems.Item(qCurrentItem)
			item.Execute
			preDelayTime = 0
			postDelayTime = GameTime
		End If
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.Add
	' REQUIRES Class vpwQueueItem
	'
	' Add an item to the queue.
	'
	' PARAMETERS:
	'
	' key (string) - Unique name for this queue item
	' WARNING: Specifying a key that already exists will
	' overwrite the item in the queue. This is by design. Also
	' note the following behaviors:
	' * Tickers / clocks for tracking delay times will NOT be
	' restarted for this item (but the total duration will be
	' updated. For example, if the old preDelay was 3 seconds
	' and 2 seconds elapsed, but Add was called to update
	' preDelay to 5 seconds, then the queue item will now
	' execute in 3 more seconds (new preDelay - time elapsed)).
	' However, timeToLive WILL be restarted.
	' * Items will maintain their same place in the queue.
	' * If key = qCurrentItem (overwriting the currently active
	' item in the queue) and qCurrentItem already executed
	' the callback (but is waiting for a postDelay), then the
	' current queue item's remaining postDelay will be added to
	' the preDelay of the next item, and this item will be
	' added to the bottom of the queue for re-execution.
	' If you do not want it to re-execute, then add an If
	' guard on your call to the Add method checking
	' "If Not vpwQueueManager.qCurrentItem = key".
	'
	' qCallback (object|string) - An object to be called,
	' or string to be executed globally, when this queue item
	' runs. I highly recommend making sub routines for groups
	' of things that should be executed by the queue so that
	' your qCallback string does not get long, and you can
	' easily organize your callbacks. Also, use double
	' double-quotes when the call itself has quotes in it
	' (VBScript escaping).
	' Example: "playsound ""Plunger"""
	'
	' priority (number) - Items in the queue will be executed
	' in order from highest priority to lowest. Items with the
	' same priority will be executed in order according to
	' when they were added to the queue. Use any number
	' greater than 0. My recommendation is to make a plan for
	' your table on how you will prioritize various types of
	' queue items and what priority number each type should
	' have. Also, you should reserve priority 1 (lowest) to
	' items which should wait until everything else in the
	' queue is done (such as ejecting a ball from a scoop).
	'
	' preQueueDelay (number) - The number of
	' milliseconds before the queue actually considers this
	' item as "in the queue" (pretend you started a timer to
	' add this item into the queue after this delay; this
	' logically works in a similar way; the only difference is
	' timeToLive is still considered even when an item is
	' pre-queued.) Set to 0 to add to the queue immediately.
	' NOTE: this should be less than timeToLive.
	'
	' preDelay (number) - The number of milliseconds before
	' the qCallback executes once this item is active (top)
	' in the queue. Set this to 0 to immediately execute the
	' qCallback when this item becomes active.
	' Set this to -1 to have an indefinite delay until
	' vpwQueueManager.ExecuteCurrentItem is called (see the
	' comment for qCurrentItem for more information).
	' NOTE: this should be less than timeToLive. And, if
	' timeToLive runs out before preDelay runs out, the item
	' will be removed and will not execute.
	'
	' postDelay (number) - After the qCallback executes, the
	' number of milliseconds before moving on to the next item
	' in the queue. Set this to -1 to have an indefinite delay
	' until vpwQueueManager.DoNextItem is called (see the
	' comment for qCurrentItem for more information).
	'
	' timeToLive (number) - After this item is added to the
	' queue, the number of milliseconds before this queue item
	' expires / is removed if the qCallback is not executed by
	' then. Set to 0 to never expire. NOTE: If not 0, this
	' should be greater than preDelay + preQueueDelay or the
	' item will expire before the qCallback is executed.
	' Example use case: Maybe a player scored a jackpot, but
	' it would be awkward / irrelevant to play that jackpot
	' sequence if it hasn't played after a few seconds (e.g.
	' other items in the queue took priority).
	'
	' executeNow (boolean) - Specify true if this item
	' should interrupt the queue and run immediately. This
	' will only happen, however, if the currently active item
	' has a priority less than or equal to the item you are
	' adding. Note this does not bypass preQueueDelay nor
	' preDelay if set.
	' Example: If a player scores an extra ball, you might
	' want that to interrupt everything else going on as it
	' is an important milestone.
	'----------------------------------------------------------
	Public Sub Add(key, qCallback, priority, preQueueDelay, preDelay, postDelay, timeToLive, executeNow)
		DebugLog "Adding queue item " & key
		
		'Construct the item class
		Dim newClass
		Set newClass = New vpwQueueItem
		With newClass
			.Callback = qCallback
			.priority = priority
			.preQueueDelay = preQueueDelay
			.preDelay = preDelay
			.postDelay = postDelay
			.timeToLive = timeToLive
			.executeNow = executeNow
		End With
		
		'If we are attempting to overwrite the current queue item which already executed, take the remaining postDelay and add it to the preDelay of the next item. And set us up to immediately go to the next item while re-adding this item to the queue.
		If preQueueDelay <= 0 And qItems.Exists(key) And qCurrentItem = key Then
			If qItems.Item(key).executed = True Then
				DebugLog key & " (Add) - Attempting To overwrite the current queue item which already executed. Immediately re-queuing this item To the bottom of the queue, transferring the remaining postDelay To the Next item, And going To the Next item."
				If qItems.Item(key).postDelay >= 0 Then
					preDelayTransfer = ((postDelayTime + qItems.Item(key).postDelay) - GameTime)
				End If
				
				'Remove current queue item so we can go to the next item, this can be re-queued to the bottom, and the remaining postDelay transferred to the preDelay of the next item
				qItems.Remove qCurrentItem
				qCurrentItem = ""
			End If
		End If
		
		' Determine execution stuff if this item does not have a pre-queue delay
		If preQueueDelay <= 0 Then
			If executeNow = True Then
				' Make sure this item does not immediately execute if the current item has a higher priority
				If Not qCurrentItem = "" And qItems.Exists(qCurrentItem) Then
					Dim item
					Set item = qItems.Item(qCurrentItem)
					If item.priority <= priority Then
						DebugLog key & " (Add) - Execute Now was Set To True And this item's priority (" & priority & ") Is >= the active item's priority (" & item.priority & " from " & qCurrentItem & "). Making it the current active queue item."
						qCurrentItem = key
						If preDelay = 0 And preDelayTransfer = 0 Then
							DebugLog key & " (Add) - No pre-delay. Executing the callback immediately."
							newClass.Execute
							preDelayTime = 0
							postDelayTime = GameTime
						Else
							DebugLog key & " (Add) - Waiting For a pre-delay of " & (preDelay + preDelayTransfer) & " before executing the callback."
							preDelayTime = GameTime
							postDelayTime = 0
						End If
					Else
						DebugLog key & " (Add) - Execute Now was Set To True, but this item's priority (" & priority & ") Is Not >= the active item's priority (" & item.priority & " from " & qCurrentItem & "). This item will Not be executed Now And will be added To the queue normally."
					End If
				Else
					DebugLog key & " (Add) - Execute Now was Set To True And no item was active In the queue. Making it the current active queue item."
					qCurrentItem = key
					If preDelay = 0 Then
						DebugLog key & " (Add) - No pre-delay. Executing the callback immediately."
						preDelayTransfer = 0 'No preDelay transfer if we are immediately re-executing the same queue item
						newClass.Execute
						preDelayTime = 0
						postDelayTime = GameTime
					Else
						DebugLog key & " (Add) - Waiting For a pre-delay of " & preDelay & " before executing the callback."
						preDelayTime = GameTime
						postDelayTime = 0
					End If
				End If
			End If
			If qItems.Exists(key) Then 'Overwrite existing item in the queue if it exists
				DebugLog key & " (Add) - Already exists In the queue. Updating the item With the new parameters passed In Add."
				Set qItems.Item(key) = newClass
			Else
				DebugLog key & " (Add) - Added To the queue."
				qItems.Add key, newClass
			End If
			queueWasEmpty = False
		Else
			If preQItems.Exists(key) Then 'Overwrite existing item in the preQueue if it exists
				DebugLog key & " (Add) - Already exists In the preQueue. Updating the item With the new parameters passed In Add."
				Set preQItems.Item(key) = newClass
			Else
				DebugLog key & " (Add) - Added To the preQueue."
				preQItems.Add key, newClass
			End If
		End If
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.Remove
	'
	' Removes an item from the queue. It is better to use this
	' than to remove the item from qItems directly as this sub
	' will also call DoNextItem to advance the queue if
	' the item removed was the active item.
	' NOTE: This only removes items from qItems; to remove
	' an item from preQItems, use the standard
	' Scripting.Dictionary Remove method.
	'
	' PARAMETERS:
	'
	' key (string) - Unique name of the queue item to remove.
	'----------------------------------------------------------
	Public Sub Remove(key)
		If qItems.Exists(key) Then
			DebugLog key & " (Remove)"
			qItems.Remove key
			If qCurrentItem = key Or qCurrentItem = "" Then DoNextItem ' Ensure the queue does not get stuck
		End If
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.RemoveAll
	'
	' Removes all items from the queue / clears the queue.
	' It is better to call this sub than to remove all items
	' from qItems directly because this sub cleans up the queue
	' to ensure it continues to work properly.
	'
	' PARAMETERS:
	'
	' preQueue (boolean) - Also clear the pre-queue.
	'----------------------------------------------------------
	Public Sub RemoveAll(preQueue)
		DebugLog "Queue was emptied via RemoveAll."
		
		' Loop through each item in the queue and remove it
		Dim k, key
		k = qItems.Keys
		For Each key In k
			qItems.Remove key
		Next
		qCurrentItem = ""
		
		If queueWasEmpty = False Then CallQueueEmpty() ' Queue is now empty, so call our callback if applicable
		
		If preQueue Then
			k = preQItems.Keys
			For Each key In k
				preQItems.Remove key
			Next
		End If
	End Sub
	
	'----------------------------------------------------------
	' Get vpwQueueManager.QueueEmpty
	' Get the current callback for when the queue is empty.
	'----------------------------------------------------------
	Public Property Get QueueEmpty()
		If IsObject(onQueueEmpty) Then
			Set QueueEmpty = onQueueEmpty
		Else
			QueueEmpty = onQueueEmpty
		End If
	End Property
	
	'----------------------------------------------------------
	' Let vpwQueueManager.QueueEmpty
	' Set the callback to call every time the queue empties.
	' This could be useful for setting a sub routine to be
	' called each time the queue empties for doing things such
	' as ejecting balls from scoops. Unlike using the Add
	' method, this callback is immune from getting removed by
	' higher priority items in the queue and will be called
	' every time the queue is emptied, not just once.
	'
	' PARAMETERS:
	'
	' callback (object|string) - The callback to call every
	' time the queue empties.
	'----------------------------------------------------------
	Public Property Let QueueEmpty(callback)
		If IsObject(callback) Then
			Set onQueueEmpty = callback
		ElseIf VarType(callback) = vbString Then
			onQueueEmpty = callback
		End If
	End Property
	
	'----------------------------------------------------------
	' Get vpwQueueManager.CallQueueEmpty
	' Private method that actually calls the QueueEmpty
	' callback.
	'----------------------------------------------------------
	Private Sub CallQueueEmpty()
		If queueWasEmpty = True Then Exit Sub
		queueWasEmpty = True
		
		If IsObject(onQueueEmpty) Then
			Call onQueueEmpty(0)
		ElseIf VarType(onQueueEmpty) = vbString Then
			If onQueueEmpty > "" Then ExecuteGlobal onQueueEmpty
		End If
	End Sub
	
	'----------------------------------------------------------
	' DebugLog
	' Log something if debugOn is not null.
	' REQUIRES / uses the WriteToLog sub from Baldgeek's
	' error log library.
	'----------------------------------------------------------
	Private Sub DebugLog(message)
		If Not IsNull(debugOn) Then
			WriteToLog "VPW Queue " & debugOn, message
		End If
	End Sub
End Class

'===========================================
' vpwQueueItem
' Represents a single item for the queue
' system. Do NOT use this class directly.
' Instead, use the vpwQueueManager.Add
' routine.

' You can, however, access an individual
' item in the queue via
' vpwQueueManager.qItems and then modify
' its properties while it is still in the
' queue.
'===========================================
Class vpwQueueItem  ' Do not construct this class directly; use vpwQueueManager.Add instead, and vpwQueueManager.qItems.Item(key) to modify an item's properties.
	Public priority ' The item's set priority
	Public timeToLive ' The item's set timeToLive milliseconds requested
	Public preQueueDelay ' The item's pre-queue milliseconds requested
	Public preDelay ' The item's pre delay milliseconds requested
	Public postDelay ' The item's post delay milliseconds requested
	Public executeNow ' Whether the item was set to Execute immediately
	Private qCallback ' The item's callback object or string (use the Callback property on the class to get/set it)
	
	Public executed ' Whether or not this item's qCallback was executed yet
	Public queuedOn ' The game time this item was added to the queue
	Public executedOn ' The game time this item was executed
	
	Private Sub Class_Initialize
		' Defaults
		priority = 0
		timeToLive = 0
		preQueueDelay = 0
		preDelay = 0
		postDelay = 0
		qCallback = ""
		executeNow = False
		
		queuedOn = GameTime
		executedOn = 0
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueItem.Execute
	' Executes the qCallback on this item if it was not yet
	' already executed.
	Public Sub Execute()
		If executed Then Exit Sub ' Do not allow an item's qCallback to ever Execute more than one time
		
		'Mark as execute before actually executing callback; that way, if callback recursively adds the item back into the queue, then we can properly handle it.
		executed = True
		executedOn = GameTime
		
		' Execute qCallback
		If IsObject(qCallback) Then
			Call qCallback(0)
		ElseIf VarType(qCallback) = vbString Then
			If qCallback > "" Then ExecuteGlobal qCallback
		End If
	End Sub
	
	Public Property Get Callback()
		If IsObject(qCallback) Then
			Set Callback = qCallback
		Else
			Callback = qCallback
		End If
	End Property
	
	Public Property Let Callback(cb)
		If IsObject(cb) Then
			Set qCallback = cb
		ElseIf VarType(cb) = vbString Then
			qCallback = cb
		End If
	End Property
End Class


Sub QueueTimer_Timer()
	BallHandlingQueue.Tick
	GeneralPupQueue.Tick
	DMDQueue.Tick
	AudioQueue.Tick
End Sub

Sub WipeAllQueues
	GeneralPupQueue.RemoveAll(True)
	AudioQueue.RemoveAll(True)
	BallHandlingQueue.RemoveAll(True)
	DMDQueue.RemoveAll(True)
End Sub

'***************************************************************
' END VPIN WORKSHOP ADVANCED QUEUING SYSTEM
'***************************************************************


Sub DiscoModeHelper_Hit()
exit Sub
	if EventStarted = 5 Then
		Debug.print "SLowing ball down"
		activeBall.VelY  = 0.1
		activeBall.VelX  = 0.1
	End If
End Sub

Sub FireBackglass
	if DMDType < 1 Then Exit Sub
	GeneralPupQueue.Add "Fire BG","PupEvent 501",1,10,0,0,0,False
	GeneralPupQueue.Add "Default BG","PupEvent 500",1,150,0,0,0,False

	'GeneralPupQueue.Add "Fire BG","FireBG",80,10,0,0,0,False
	'GeneralPupQueue.Add "Default BG","MainBG",80,60,0,0,0,False
End Sub

Sub FireBG
	if DMDType <> 0 Then
		PuPlayer.playlistplayex pBackglass,"pupoverlays","lit.png",0,1
	End If
End Sub

Sub MainBG
	if DMDType <> 0 Then
		PuPlayer.playlistplayex pBackglass,"pupoverlays","main.png",0,1
	End If
End Sub

Sub AttractTimer_Timer()
	Select Case AttractTimerCount
		Case 0
			AttractTimer.Interval = 2000
			pDMDLabelhide "AttractCredits"
			If Credits> 0 Then
				PuPlayer.LabelSet pDMD,"Attract2a"," CREDITS ",1,"{'mt':2,'color': " & cWhite &" }"
				PuPlayer.LabelSet pDMD,"Attract2b"," "&credits&" ",1,"{'mt':2,'color': " & cWhite &" }"  
			Else
				PuPlayer.LabelSet pDMD,"Attract2a"," CREDITS 0 ",1,"{'mt':2,'color': " & cWhite &" }"
				PuPlayer.LabelSet pDMD,"Attract2b"," INSERT COIN ",1,"{'mt':2,'color': " & cWhite &" }"  
			End If
		Case 1
				PuPlayer.LabelSet pDMD,"Attract2a","Max Pelicans Killed",1,"{'mt':2,'color': 12615935 }"
				PuPlayer.LabelSet pDMD,"Attract2b",MaxPelicans,1,"{'mt':2,'color': 12615935 }"   

		Case 2
				PuPlayer.LabelSet pDMD,"Attract2a","Easter Egg High Score",1,"{'mt':2,'color': " & cRed &" }"
				PuPlayer.LabelSet pDMD,"Attract2b",EEHighScore,1,"{'mt':2,'color': " & cRed &" }"  

		Case 3
				PuPlayer.LabelSet pDMD,"Attract2a"," "&HighScoreName(0)&" ",1,"{'mt':2,'color': " & cGold &" }"
				PuPlayer.LabelSet pDMD,"Attract2b",FormatScore(HighScore(0)),1,"{'mt':2,'color': " & cGold &" }" 
		Case 4
				PuPlayer.LabelSet pDMD,"Attract2a"," "&HighScoreName(1)&" ",1,""
				PuPlayer.LabelSet pDMD,"Attract2b",FormatScore(HighScore(1)),1,""   
		Case 5
				PuPlayer.LabelSet pDMD,"Attract2a"," "&HighScoreName(2)&" ",1,""
				PuPlayer.LabelSet pDMD,"Attract2b",FormatScore(HighScore(2)),1,""   
		Case 6
				PuPlayer.LabelSet pDMD,"Attract2a"," "&HighScoreName(3)&" ",1,""
				PuPlayer.LabelSet pDMD,"Attract2b",FormatScore(HighScore(3)),1,""   
		Case 7

				PuPlayer.LabelSet pDMD,"Attract2a"," GAME OVER ",1,""
				PuPlayer.LabelSet pDMD,"Attract2b","",1,""   	
		Case 8
			AttractTimer.Interval = 4000
			ClearPupAttractMessages
			PuPlayer.LabelSet pDMD, "AttractCredits", "Attract\\Hassanchop.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 9
			PuPlayer.LabelSet pDMD, "AttractCredits", "Attract\\Merlin.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 10
			PuPlayer.LabelSet pDMD, "AttractCredits", "Attract\\JoeP.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 11
			AttractTimer.Interval = 3000
			PuPlayer.LabelSet pDMD, "AttractCredits", "Attract\\Zandy.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 12
			PuPlayer.LabelSet pDMD, "AttractCredits", "Attract\\Rock.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 13
			PuPlayer.LabelSet pDMD, "AttractCredits", "Attract\\Dardog.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 14
			PuPlayer.LabelSet pDMD, "AttractCredits", "Attract\\RKP.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 15
			PuPlayer.LabelSet pDMD, "AttractCredits", "Attract\\Final.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 16
			PuPlayer.LabelSet pDMD, "AttractCredits", "Attract\\PinballWizards.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 17
			AttractTimer.Interval = 1200
			Playsound "Chainsaw"
			PuPlayer.LabelSet pDMD, "AttractCredits", "Attract\\Black.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 18
			AttractTimer.Interval = 5000
			PuPlayer.LabelSet pDMD, "AttractCredits", "Attract\\Title3.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
			GeneralPupQueue.Add "AttractCredits","pDMDlabelhide ""Credits1"" ",90,attracttimer.interval,0,0,0,False

	End Select

	AttractTimerCount = AttractTimerCount + 1

	if AttractTimerCount > 18 Then AttractTimerCount = 0
End Sub

'*********************************************MINIGAME*****************************************
'AOR Added
	Dim PuPMiniGameExe    'notice two \\ cuz of json
    Dim PuPMiniGameTitle 
    Dim PuPMiniGameScore 'PuPMiniGameScore="\MiniGame1\score.txt"      'no need to two \\ not json 



	PuPMiniGameExe  ="MiniGame\\AORScarfacePelicans.exe"  'notice two \\ cuz of json
	PuPMiniGameTitle="AORScarfacePelicans"
	PuPMiniGameScore="\MiniGame\Score.txt"      'no need to two \\ not json 




'AORPupBeatGameStdView

Sub AlwaysOnTop(appName, regExpTitle, setOnTop)
			' @description: Makes a window always on top if setOnTop is true, else makes it normal again. Will wait up to 10 seconds for window to load.
			' @author: Jeremy England (SimplyCoded)
			  If (setOnTop) Then setOnTop = "-1" Else setOnTop = "-2"
			  CreateObject("wscript.shell").Run "powershell -Command """ & _    
				"$Code = Add-Type -MemberDefinition '" & vbcrlf & _
				"  [DllImport(\""user32.dll\"")] public static extern bool SetWindowPos(IntPtr hWnd, IntPtr hWndInsertAfter, int X,int Y, int cx, int cy, uint uFlags);" & vbcrlf & _
				"  [DllImport(\""user32.dll\"")] public static extern bool ShowWindow(IntPtr hWnd, int nCmdShow);" & vbcrlf & _
				"  public static void AlwaysOnTop (IntPtr fHandle, int insertAfter) {" & vbcrlf & _
				"    if (insertAfter == -1) { ShowWindow(fHandle, 4); }" & vbcrlf & _
				"    SetWindowPos(fHandle, new IntPtr(insertAfter), 0, 0, 0, 0, 3);" & vbcrlf & _
				"  }' -Name PS -PassThru" & vbcrlf & _
				"for ($s=0;$s -le 9; $s++){$hWnd = (GPS " & appName & " -EA 0 | ? {$_.MainWindowTitle -Match '" & regExpTitle & "'}).MainWindowHandle;Start-Sleep 1;if ($hWnd){break}}" & vbcrlf & _  
				"$Code::AlwaysOnTop($hWnd, " & setOnTop & ")""", 0, True
End Sub


		DIM PuPGameRunning:PuPGameRunning=false
		DIM PuPGameTimeout
		DIM PuPGameInfo
		DIM PuPGameScore
		Dim inminigame
		

sub startminigame
	if bMiniGamePlayed Then Exit Sub
		'PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 13, ""FN"":3, ""OT"": 0 }"      'this will hide overlay if applicable
	pDMDSetHUD 0
	pDMDsetPage 77, "minigameStart"

		if Scorbit.bSessionActive then
			GameModeStrTmp="BL{pink}:Mini Game Started"
			Scorbit.SetGameMode(GameModeStrTmp)
		End If

	FlamingoTimer.enabled = 0
	pDMDLabelHide "Flamingo" 
	PupGameStartMiniGame
end sub
		

Sub PuPGameStartMiniGame
		
	if PuPGameRunning Then Exit Sub

	bMiniGamePlayed = True



			if PupScreenMiniGame = 2 Then
		
				PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":16, ""EX"": """&PuPMiniGameExe  &""", ""WT"": """&PuPMiniGameTitle&""", ""RS"":1 , ""TO"":15 , ""WZ"":0 , ""SH"": 1 , ""FT"":""Visual Pinball Player"" }" 
				AlwaysOnTop PuPMiniGameTitle, ".", True
				'PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":3, ""OT"": 0 }"    '  'AOR, enable this line if using screen 2this will hide overlay if applicable
				PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 12, ""FN"":3, ""OT"": 0 }"   'AOR, this will hide when no B2s and pup on screen 2 (13 custom pos reference)

				PuPGameTimeout=0'-3    'check for timeout  every 500 ms
				PuPGameRunning=true	
				PuPGameTimer.enabled=true
				'PuPlayer.playpause 2
				PuPlayer.playpause 12
			End If

			If PupScreenMiniGame = 5 Then
				PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 5, ""FN"":16, ""EX"": """&PuPMiniGameExe  &""", ""WT"": """&PuPMiniGameTitle&""", ""RS"":1 , ""TO"":15 , ""WZ"":0 , ""SH"": 1 , ""FT"":""Visual Pinball Player"" }" 
				AlwaysOnTop PuPMiniGameTitle, ".", True
				'PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 12, ""FN"":3, ""OT"": 0 }"    '  'AOR, enable this line if using screen 5this will hide overlay if applicable
				PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 5, ""FN"":3, ""OT"": 0 }"    '  'AOR, enable this line if using screen 5this will hide overlay if applicable
				PuPGameTimeout=0'-3    'check for timeout  every 500 ms
				PuPGameRunning=true	
				PuPGameTimer.enabled=true
				'StopTimers	



				Puplayer.playpause 5
				'PuPlayer.playpause 12

				
				BackDoorPost.isDropped = True
		
			End If





			
End Sub


dim miniend:miniend=0
Sub PuPMiniGameEnd(gamescore)
			miniend=1
			
			if PuPGameRunning Then Exit Sub
			inminigame = 0 ': pupevent 992
			'

			pDMDSetHUD 1
			pDMDsetPage pScores, "minigameEnd"
			AddScore(gamescore)
			flamingotimer.enabled = 0
			pDMDLabelHide "Flamingo"
			GeneralPupQueue.Add "Hide Flamingo","pDMDLabelHide ""Flamingo"" ",80,500,0,0,0,False
			bMinigameplayed = True	
			bMiniGameAllowed = False



			pDMDSplashBig "GET READY !!", 3, cPink
			GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",80,3100,0,0,0,False

			EjectBallsInDeskDelay = EjectBallValue 'shoot back the ball

			Dbg "minigame End "&EjectBallsInDeskDelay

End Sub

Sub PuPGameTimer_Timer()  'AOR, need to add a timer to the table, 300 milliseconds, Enabled unchecked  



			PuPGameTimeout=PuPGameTimeout+1


			if PuPGameTimeout = 3 then 
							
			End if
			
			PuPGameInfo= PuPlayer.GameUpdate(PuPMiniGameTitle, 0 , 0 , "") '0=game over, 1=game running
			'CHECK GAME OVER
			if PuPGameInfo=0 AND PuPGameTimeOut>12 Then  'gameover if more than 4 seconds passed

				dbg "minigame timer ended"
			   PuPGameTimer.enabled=false 
			 	PupGameRunning=False
				'dbg2 "MiniGame Stopped"
			   '	PuPGameScore= PuPlayer.GameUpdate(PuPMiniGameTitle, 6 , 0 , "\MiniGame0\Score.txt")   'grab score from minigame   3=gms 6=godot 
				PuPGameScore= PuPlayer.GameUpdate(PuPMiniGameTitle, 6 , 0 , PuPMiniGameScore)   'grab score from minigame
			   'msgbox PuPGameScore  'DO something with the score if its over 0!!!
			    PuPMiniGameEnd(PuPGameScore)  
				CheckMaxPelican
			

'			vpmTimer.addTimer 100, "bsSaucer.AddBall 0 '"

			'PupGameDecideScoreType
		'PupGameScoring
				
	
			
			If PupScreenMiniGame = 2 Then
			'PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":3, ""OT"": 1 }"  'AOR, enable this line if using screen 2
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 12, ""FN"":3, ""OT"": 1 }"   'AOR, this will hide when no B2s and pup on screen 2 (13 custom pos reference)
			PuPlayer.playresume 2
			'PuPlayer.playresume 12

			end If
	
			
			If PupScreenMiniGame = 5 Then
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 11, ""FN"":3, ""OT"": 1 }"  'AOR, enable this line if using screen 5
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 5, ""FN"":3, ""OT"": 1 }"  'AOR, enable this line if using screen 5
			puplayer.playresume 5

			end If
		
			'kickermg.kick 90,4
			iMiniGameCnt(Player) = iMiniGameCnt(Player) + 1	
			End If 
End Sub

Sub RaiseDiverterPin(Enabled)

	If Enabled Then
		DiverterPin.isdropped=0
		Diverter1.isdropped=0
		Diverter2.isdropped=0
	Else
		DiverterPin.isdropped=1
		Diverter1.isdropped=1
		Diverter2.isdropped=1
	End If
End Sub


'*********************************************VR*****************************************
Dim VRRoom
DIM VRThings
Sub LoadVRRoom

	If RenderingMode = 2 or VRTest Then
		VRRoom = VRRoomChoice
		'disable table objects that should not be visible
		Ramp15.visible = 0
		Ramp16.visible = 0
		primitive13.visible = 0
	Else
		VRRoom = 0
		'Ramp15.visible = 1
		'Ramp16.visible = 1
		'primitive13.visible = 1
		for each VRThings in VR_Cab:VRThings.visible = 0:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		for each VRThings in VR_Mega:VRThings.visible = 0:Next
	End If

	If VRRoom = 1 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		for each VRThings in VR_Mega:VRThings.visible = 0:Next
		bMiniGameActive = False  ' Turn off in other VR modes as it is glitchy and breaks game by not releasing ball sometimes
	End If
	If VRRoom = 2 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 1:Next
		for each VRThings in VR_Mega:VRThings.visible = 0:Next
		bMiniGameActive = False  ' Turn off in other VR modes as it is glitchy and breaks game by not releasing ball sometimes
	End If
	If VRRoom = 3 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		for each VRThings in VR_Mega:VRThings.visible = 1:Next
				Ramp15.visible = 0
		Ramp16.visible = 0
		'primitive13.visible = 0
		bMiniGameActive = False  ' Turn off in other VR modes as it is glitchy and breaks game by not releasing ball sometimes
	End If
End Sub

Sub SoftPlungeDetect_Hit()
	WireRampOn True

	pDMDsetPage pScores, "SoftLaunch"
	DMDUpdateAll
	DelayQRClaim.Enabled=False
	Dbg2 " Soft plunge hit"
	if ScorbitActive And bOnTheFirstBallScorbit Then
		bOnTheFirstBallScorbit = False
		HideScorbit
	End If

		if activeball.velY > 0.1 Then ' ball is falling back down plungerlane
	'debug.print "Y:" &activeball.VelY
		If SkillShotReady Then	
			EndSkillshot
			pDMDSplashBig "SKILLSHOT FAILED", 3, cRed
			GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",80,3100,0,0,0,False
		End If
	End If
End Sub

Dim SafetyCounter
Sub SafetyTimer_Timer()
	SafetyCounter = SafetyCounter + 1
	Dbg "SFC: " & SafetyCounter
	if SafetyCounter > 7 Then 
		DropAll "Safety"
		ResetDropDelay = 150
		BallhandlingQueue.Add "ResetDroptargets","ResetDroptargets",90,300,0,0,0,False
		SafetyCounter = 0
		SafetyTimer.Enabled = 0
	End If
	
End Sub

Sub BackupReset_Hit()
	Dbg "BUR: " &SafetyCounter
	if bSupressSafety Then Exit Sub
	SafetyTimer.Enabled = 1
End Sub

Sub FlamingoTimer_Timer()
	if bMiniGameAllowed = 0 or bonusdelay > 0 or bMiniGamePlayed Then 
		pDMDLabelHide "Flamingo" 
		FlamingoTimer.enabled = 0
		Exit Sub
	End If

	FlamingoCounter = FlamingoCounter + 1
	if FlamingoCounter mod 2 = 0 Then
		pDMDLabelHide "Flamingo"
	Else
		PuPlayer.LabelSet pDMD, "Flamingo", "PuPOverlays\\Flamingo.png",1,"{'mt':2,'width':14, 'height':14,'xalign':0,'yalign':0,'ypos':13,'xpos':4}"
	End If
End Sub

Sub ExtraballTimer_Timer()

	EBCounter = EBCounter + 1
	if EBCounter mod 2 = 0 Then
		pDMDLabelHide "ExtraballImage"
	Else
		PuPlayer.LabelSet pDMD, "ExtraballImage", "PuPOverlays\\briefcase.png",1,"{'mt':2,'width':15, 'height':25,'xalign':0,'yalign':0,'ypos':10,'xpos':83}"
	End If
End Sub


Sub BlendLightingTimer_Timer()
	'BlendTheLighting
End Sub

Sub TimerTilt_Timer
	TimerTilt.Enabled = True
	If nTiltLevel > 0 Then nTiltLevel = nTiltLevel - 1
End Sub

Sub Bump2()
Dim i
debug.print "In Bump2: " &Tilt
    If GameStarted AND Tilted = 0 AND Tilt <8 Then
        Tilt = Tilt + 1
        If Tilt = 3 AND TiltWarning = 0 Then
            TiltWarning = 1
            Say "You_fuckin_high_or_what", "131426141"
        End If
        If Tilt = 5 AND TiltWarning = 1 Then
            TiltWarning = 2
            Say "Fuckin_cock_sucker", "8524252524"
        End If
        If Tilt> 7 Then
            Say "heyfuckyouman", "162527171"
            Tilted = True
			pupevent 305
            for i = 210 to 224
                DOF i, DOFOff
            next
            GiOff
            LightSeqTilt.Play SeqAllOff
            TiltObjects 1
        End If

        TiltDelay = 20
    End If
End Sub

Sub Bump
'debug.print "In Bump: " &nTiltLevel
	If BallsOnPlayfield < 1 Then Exit Sub
	nTiltLevel = nTiltLevel + 10
	If nTiltLevel > 15 Then
		TiltWarning = TiltWarning + 1
		Select Case TiltWarning
			Case 1
				pDMDSplashBig "TILT ALERT", 2, cWhite
				GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",60,2100,0,0,0,False
				Say "You_fuckin_high_or_what", "131426141"
			Case 2
				pDMDSplashBig "TILT WARNING", 2, cYellow
				GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",60,2100,0,0,0,False
				Say "Run_while_you_can_stupid_fuck", "13131335142121431"
			Case 3
				pDMDSplashBig "TILT !!", 3, cRed
				if BallinPlunger Then 
					AutoPlungerReady = 1
					AutoPlungerDelay = 10
				End If
				GeneralPupQueue.Add "ClearPupSplashMessages","ClearPupSplashMessages",60,3100,0,0,0,False
				Say "heyfuckyouman", "162527171"
				Tilted = True
				pupevent 305
				GiOff
				LightSeqTilt.Play SeqAllOff
				TiltObjects 1
		End Select
	End If
'if BallinPlunger Then AutoPlungerFire
End Sub

Sub StuckBallHelper_Hit()
	StuckBallHelper.kick 35, 15 + RND(1) * 5
	StuckBallHelper.Enabled = 0
	BallHandlingQueue.Add "Enable Helper","StuckBallHelper.Enabled = 1",60,1500,0,0,0,False
End Sub

