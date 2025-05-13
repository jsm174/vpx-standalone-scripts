Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

' new B2S stuff ************************************************

'Dim Controller	
Set Controller = CreateObject("B2S.Server")
'Controller.B2SName = "MotleyCrue_B2S"
'Controller.Run()

Dim bOnTheFirstBall

Const cGameName="MOTLEYCRUESS"
Const myVersion = "1.0.0"

dim ScorbitActive
ScorbitActive					= 0 	' Is Scorbit Active	
Dim pQRlocation
pQRlocation 					= 15     ' 15 for backglass
Const     ScorbitShowClaimQR	= 1 	' If Scorbit is active this will show a QR Code  on ball 1 that allows player to claim the active player from the app
Const     ScorbitUploadLog		= 0 	' Store local log and upload after the game is over 
Const     ScorbitAlternateUUID  = 0 	' Force Alternate UUID from Windows Machine and saves it in VPX Users directory (C:\Visual Pinball\User\ScorbitUUID.dat)
'/////////////////////////////////////////////////////////////////////
Dim bOnTheFirstBallScorbit

'dmdType
Const pDMDTypeLCD=0
Const pDMDTypeReal=1
Const pDMDTypeFULL=2

'Dim PuPlayer
dim PUPDMDObject  'for realtime mirroring.
Dim pDMDlastchk: pDMDLastchk= -1    'performance of updates
Dim pDMDCurPage: pDMDCurPage= 0     'default page is empty.
Dim pBGCurPage: pBGCurPage=0
Dim pInAttract : pInAttract=false   'pAttract mode


'//////////////////////////////////////////////////////////////////////
'// LUT ---> added by mason
'//////////////////////////////////////////////////////////////////////

'//////////////---- LUT (Colour Look Up Table) ----//////////////
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

Dim LUTset, DisableLUTSelector, LutToggleSound, bLutActive
LutToggleSound = True
LoadLUT
'LUTset = 0			' Override saved LUT for debug
SetLUT
DisableLUTSelector = 0  ' Disables the ability to change LUT option with magna saves in game when set to 1


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
        Case 15: LUTBox.text = "B&W Comic Book"
		Case 16: LUTBox.text = "Skitso New ColorLut"
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

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "MCLUT.txt",True)
	ScoreFile.WriteLine LUTset
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub
Sub LoadLUT
    bLutActive = False
	Dim FileObj, ScoreFile, TextStr
	dim rLine

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		LUTset=0
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "MCLUT.txt") then
		LUTset=0
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "MCLUT.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
		End if
		rLine = TextStr.ReadLine
		If rLine = "" then
			LUTset=0
			Exit Sub
		End if
		LUTset = int (rLine) 
		Set ScoreFile = Nothing
	    Set FileObj = Nothing
End Sub

' **************************************************************

Dim TableName
TableName="MOTLEYCRUE SS" 'limit of 14 characters

Const BallSize = 25  'Ball radius


' **START MOTLEY CRUE SONGS (songs are silent as sound is from PUP videos) **

'MP3 Track Listing - filenames go here, duplicates are allowed
 Dim TrackFilename(18)

' ** 12 MODES **
TrackFilename(1)="MOTLEYCRUE SS/Live Wire.mp3"  'SPINNER
TrackFilename(2)="MOTLEYCRUE SS/Shout At The Devil.mp3"  'ALBUMS
TrackFilename(3)="MOTLEYCRUE SS/Anarchy.mp3"  'POPS
TrackFilename(4)="MOTLEYCRUE SS/Primal Scream.mp3"  'RAMPS
TrackFilename(5)="MOTLEYCRUE SS/Looks That Kill.mp3"  'DROPS
TrackFilename(6)="MOTLEYCRUE SS/Girls Girls Girls.mp3"  'STANDUPS
TrackFilename(7)="MOTLEYCRUE SS/Ten Seconds to Love.mp3"  'ORBITS (outer)
TrackFilename(8)="MOTLEYCRUE SS/Red Hot.mp3"   'SUPER LOOPS (inner)
TrackFilename(9)="MOTLEYCRUE SS/Louder Than Hell.mp3" 'COMBOS
TrackFilename(10)="MOTLEYCRUE SS/On With The Show.mp3"  'MINIS
TrackFilename(11)="MOTLEYCRUE SS/Kickstart My Heart.mp3"  'FOLLOW
TrackFilename(12)="MOTLEYCRUE SS/Wild Side.mp3" 'FRENZY

' ** WIZARD MODES **
TrackFilename(13)="MOTLEYCRUE SS/Home Sweet Home.mp3"   'Mini Wizard Mode
TrackFilename(14)="MOTLEYCRUE SS/Dr. Feelgood.mp3"   'Final Wizard Mode

' ** MULTIBALLS **
TrackFilename(15)="MOTLEYCRUE SS/Too Young To Fall in Love.mp3"  '2 Ball Multiball #1 Bootleg
TrackFilename(16)="MOTLEYCRUE SS/Helter Skelter.mp3"     '2 Ball Multiball #2 Theatrical
TrackFilename(17)="MOTLEYCRUE SS/Sick Love Song.mp3"    '3 Ball Multiball #3 Festival
TrackFilename(18)="MOTLEYCRUE SS/Same Old Situation.mp3"     '3 Ball Multiball #4 World Tour


'-------------------------------
'	Music Pup Tracker
'------------------------------

Sub MusicTrackCheck
	If MusicNumber = 1 Then
		PuPEvent 600
	ElseIf MusicNumber = 2 Then
		PuPEvent 605
	Elseif MusicNumber = 3 Then
		PuPEvent 606
	Elseif MusicNumber = 4 Then
		PuPEvent 607
	Elseif MusicNumber = 5 Then
		PuPEvent 608
	Elseif MusicNumber = 6 Then
		PuPEvent 609
	Elseif MusicNumber = 7 Then
		PuPEvent 610
	Elseif MusicNumber = 8 Then
		PuPEvent 611
	Elseif MusicNumber = 9 Then
		PuPEvent 612
	Elseif MusicNumber = 10 Then
		PuPEvent 613
	Elseif MusicNumber = 11 Then
		PuPEvent 614
	Elseif MusicNumber = 12 Then
		PuPEvent 615
	Elseif MusicNumber = 13 Then
		PuPEvent 616
	Elseif MusicNumber = 14 Then
		PuPEvent 617
	Elseif MusicNumber = 15 Then
		PuPEvent 601
	Elseif MusicNumber = 16 Then
		PuPEvent 602
	Elseif MusicNumber = 17 Then
		PuPEvent 603
	Elseif MusicNumber = 18 Then
		PuPEvent 604
	Else
		
	End If
End Sub

'Playfield GI
'*****GI Lights On
dim xx
For each xx in GI:xx.State = 1: Next


'---UltraDMD--
Dim UltraDMD
Dim folderPath
folderPath = "\MOTLEYCRUE.UltraDMD"

Const UltraDMD_VideoMode_Stretch = 0
Const UltraDMD_VideoMode_Top = 1
Const UltraDMD_VideoMode_Middle = 2
Const UltraDMD_VideoMode_Bottom = 3


'Const UltraDMD_Animation_FadeIn = 0
Const UltraDMD_Animation_FadeOut = 1
'Const UltraDMD_Animation_ZoomIn = 2
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
    If UltraDMD is Nothing Then
        MsgBox "No UltraDMD found.  This table MAY run without it."
        Exit Sub
    End If

    UltraDMD.Init
    If Not UltraDMD.GetMajorVersion = 1 Then
        MsgBox "Incompatible Version of UltraDMD found."
        Exit Sub
    End If

    If UltraDMD.GetMinorVersion < 1 Then
        MsgBox "Incompatible Version of UltraDMD found. Please update to version 1.1 or newer."
        Exit Sub
    End If

    Dim fso
    Set fso = CreateObject("Scripting.FileSystemObject")
    Dim curDir
    curDir = fso.GetAbsolutePathName(".")
    UltraDMD.SetProjectFolder curDir & folderPath
End Sub


Function GetTrackText(astr, limit)
	If Not IsNull(astr) And LEN(astr) > 0 Then
		Dim text
		text = astr
		If InStr(astr,".mp3") Then
			text = mid(text,1,LEN(text) -4) 'remove the termination .m4a
		End If 
		If InStr(astr,"MOTLEYCRUE SS/") Then
			text = replace(text,"MOTLEYCRUE SS/","") 'remove prefix
		End If
		If limit And Len(text) > 18 Then
			text = rtrim(left(text,16)) + "..."
		End If
		GetTrackText = text
	End If
End Function

Sub DMD_SetScoreboardBackground(imageName)
	If Not UltraDMD is Nothing Then
		UltraDMD.SetScoreboardBackgroundImage imageName, 15, 15
	End If
End Sub

Sub DMD_DisplaySongSelect
    If Not UltraDMD is Nothing Then
		DMD_ClearScene
		If UltraDMD.GetMajorVersion = 1 AND UltraDMD.GetMinorVersion < 4 Then
			UltraDMD.DisplayScoreboard 2, 0, TempSongSelect, SongSelectTimerCount, 0, 0, GetTrackText(TrackFilename(TempSongSelect), false), ""
		Else
			UltraDMD.DisplayScoreboard00 2, 0, TempSongSelect, SongSelectTimerCount, 0, 0, GetTrackText(TrackFilename(TempSongSelect), false), ""
		End If
	End If
End Sub


Sub DMD_DisplayRandomAward
	If Not UltraDMD Is Nothing Then
		DMD_ClearScene
		If UltraDMD.GetMajorVersion = 1 And UltraDMD.GetMinorVersion < 4 Then
			UltraDMD.DisplayScoreboard 0, 0, 0, 0, 0, 0, "      "+AwardNames(AwardCycleValue), ""
		Else
			UltraDMD.DisplayScoreboard00 0, 0, 0, 0, 0, 0, "      "+AwardNames(AwardCycleValue), ""
		End If
	End If
End Sub

Sub DMD_DisplayWorldTour
If Not UltraDMD Is Nothing Then
		DMD_ClearScene
		If UltraDMD.GetMajorVersion = 1 And UltraDMD.GetMinorVersion < 4 Then
			UltraDMD.DisplayScoreboard 0, 0, 0, 0, 0, 0, "      "+Cities(CityNameAttempt), ""
		Else
			UltraDMD.DisplayScoreboard00 0, 0, 0, 0, 0, 0, "      "+Cities(CityNameAttempt), ""
		End If
	End If
End Sub


Sub DMD_DisplayHighScore(initials)
	If Not UltraDMD Is Nothing Then
		DMD_ClearScene

		Dim scorepoints
		If Score(CurrentPlayer)>=gsHighScore(3) Then
			scorepoints = Score(CurrentPlayer)
		ElseIf CombosThisGame(CurrentPlayer)>=gvCombosforComboChamp Then
			scorepoints = CombosThisGame(CurrentPlayer)
		ElseIf CheckTrackScoresVal Then
			scorepoints = MusicScore(CurrentPlayer,Z4)
		End If

		If UltraDMD.GetMajorVersion = 1 And UltraDMD.GetMinorVersion < 4 Then
			UltraDMD.DisplayScoreboard 1, 0, scorepoints, 0, 0, 0, "      "+initials, ""
		Else
			UltraDMD.DisplayScoreboard00 1, 0, scorepoints, 0, 0, 0, "      "+initials, ""
		End If
	End If
End Sub

Sub DMD_DisplayMatch(pos, val)
	If Not UltraDMD Is Nothing Then
		If UltraDMD.GetMajorVersion = 1 And UltraDMD.GetMinorVersion < 4 Then
			UltraDMD.DisplayScoreboard PlayersPlayingGame, 0, Score(0) MOD 100, Score(1) MOD 100, Score(2) MOD 100, Score(3) MOD 100, "   "&Space(pos)&val, ""
		Else
			UltraDMD.DisplayScoreboard00 PlayersPlayingGame, 0, Score(0) MOD 100, Score(1) MOD 100, Score(2) MOD 100, Score(3) MOD 100, "   "&Space(pos)&val, ""
		End If
	End If
End Sub

Sub DMD_DisplayScoreboard
    If Not UltraDMD is Nothing Then
		If UltraDMD.IsRendering Then
			Debug.Print "Still rendering..."
		End If
		If Not UltraDMD.IsRendering And BallsRemaining(CurrentPlayer) > 0 Then
			If UltraDMD.GetMajorVersion = 1 AND UltraDMD.GetMinorVersion < 4 Then
				UltraDMD.DisplayScoreboard PlayersPlayingGame, CurrentPlayer+1, Score(0), Score(1), Score(2), Score(3), GetTrackText(TrackFilename(MusicNumber), true), "ball "+CStr(gsBallsPerGame+1-BallsRemaining(CurrentPlayer))
			Else
				UltraDMD.DisplayScoreboard00 PlayersPlayingGame, CurrentPlayer+1, Score(0), Score(1), Score(2), Score(3), GetTrackText(TrackFilename(MusicNumber), true), "ball "+CStr(gsBallsPerGame+1-BallsRemaining(CurrentPlayer))
			End If
		End If
	End If
End Sub

Sub DMD_ClearScene
	If Not UltraDMD is Nothing Then
		UltraDMD.CancelRendering
		Do While UltraDMD.IsRendering
			'wait until rendering is actually cancelled
		Loop
		Debug.Print "DMD Cleared"
	End If
End Sub

Sub DMD_DisplayScores
	If Not UltraDMD is Nothing Then
		DMD_ClearScene
		Dim players
		If IsEmpty(PlayersPlayingGame) Then
			players = 1
		Else
			players = PlayersPlayingGame
		End If
		
		If UltraDMD.GetMajorVersion = 1 AND UltraDMD.GetMinorVersion < 4 Then
			UltraDMD.DisplayScoreboard players, 0, Score(0), Score(1), Score(2), Score(3), "", ""
		Else
			UltraDMD.DisplayScoreboard00 players, 0, Score(0), Score(1), Score(2), Score(3), "", ""
		End If
	End If
End Sub

Sub DMD_DisplayCredits(text)
	If Not UltraDMD is Nothing Then
		UltraDMD.CancelRendering
		UltraDMD.ScrollingCredits "blank.png", text, 15, UltraDMD_Animation_ScrollOnUp, 500, UltraDMD_Animation_None
	End If
End Sub

Sub DMD_DisplaySceneText(toptext, bottomtext)
	DMD_DisplayScene "", toptext, 15, bottomtext, 15, UltraDMD_Animation_None, 10000, UltraDMD_Animation_None
End Sub

Sub DMD_DisplaySceneTextWithPause(toptext, bottomtext, pauseTime)
	DMD_DisplayScene "", toptext, 15, bottomtext, 15, UltraDMD_Animation_None, pauseTime, UltraDMD_Animation_None
End Sub


Sub DMD_ModifyScene(id, toptext, bottomtext)
	If Not UltraDMD is Nothing Then
		UltraDMD.ModifyScene00 id, toptext, bottomtext
	End If
End Sub

Sub DMD_DisplayLogo
	DMD_DisplayScene "logo.png", "", -1, "", -1, UltraDMD_Animation_None, 5000, UltraDMD_Animation_None
End Sub

Sub DMD_DisplayScene(bkgnd, toptext, topBrightness, bottomtext, bottomBrightness, animateIn, pauseTime, animateOut)
    If Not UltraDMD is Nothing Then
		UltraDMD.CancelRendering
        UltraDMD.DisplayScene00 bkgnd, toptext, topBrightness, bottomtext, bottomBrightness, animateIn, pauseTime, animateOut
        If pauseTime > 0 OR animateIn < 14 OR animateOut < 14 Then
            Timer1.Enabled = True
        End If
    End If
End Sub

Sub DMD_DisplaySceneExWithId(id, cancelPrev, bkgnd, toptext, topBrightness, topOutlineBrightness, bottomtext, bottomBrightness, bottomOutlineBrightness, animateIn, pauseTime, animateOut)
	If Not UltraDMD is Nothing Then
		UltraDMD.DisplayScene00ExWithId id, cancelPrev, bkgnd, toptext, topBrightness, topOutlineBrightness, bottomtext, bottomBrightness, bottomOutlineBrightness, animateIn, pauseTime, animateOut
		If pauseTime > 0 OR animateIn < 14 OR animateOut < 14 Then
            Timer1.Enabled = True
		End If
    End If
End Sub





'------------------------------------------CODE-----------------------------------------'
'StartShake
TableName=UCase(TableName)
Dim cGameSaveName
cGameSaveName="musictable"&TableName 'VP Registry settings should be unique this way
Const constEscKey=8 ' escape key for diagnostic menu is 7 on the keyboard
Const constBangBackKey=20'T
Dim CurrentPlayer'current active player
Dim PlayersPlayingGame'number of players playing the current game
' define global variables (for each player)
Dim Score(4)
Dim BonusMultiplier(4)
Dim BallsRemaining(4)'balls remaining to play (inclusive)
Dim ExtraBallsAwards(4)'number of EB's out-standing
Dim BallsInGame'number of balls on playfield (multiball exclusive)
Dim ReplayStart(4,2)
Dim CombosThisGame(4)
' define game flags
Dim bGameInPlay'game started flag
Dim b2BallMultiMode'in 2 ball multiball
Dim b3BallMultiMode'in 3 ball multiball
Dim b4BallMultiMode'in final 4 ball wizard mode
Dim bTilted'game not tilted
Dim bBallInPlungerLane'is there a ball in the plunger lane
Dim bDoorOpen'is the coin door open
Dim bMenuModeActive'are we in the operator menu
Dim bBallSaverActive'the ball saver is activated
Dim bEndOfGame'is the game in 'over state'?
Dim MatchRunning
MatchRunning=FALSE
' define game control variables
Dim LastSwitchHit'id of last switch hit
' and define the game play parameters (would be saved
' in battery backed up ram in a real pinball)
Dim gpBallSaveTime
' local game engine values
Dim gvCombosForComboChamp
Dim gvTiltLevel
Dim gpTiltWarningsGiven
Dim gvStatusModeActive
Dim gvBonusAmount
Dim gvBonusLoops
Dim gvBallsToEject
Dim gvFirstBallPlayed
Dim gvFirstBallEjected
Dim gvCreditsToAward
Dim gvCombosThisBall
Dim gvMatchLoops
Dim gvLastMatchValue
Dim gvLastMatchPosition
Dim opMenuDepth
Dim opMenuIndex
Dim opSubMenuIndex
' The game stats/scores (get written to disk)
Dim gsHighScore(4)' top high score
Dim gsHighScoreName(4)
Dim gsTrackScore(12)
Dim gsTrackScoreName(12)
Dim gsComboChampName
Dim gsJackpot' value of jackpot
Dim gsGamesPlayed
Dim gsCredits
Dim gsGameDifficulty
Dim gsBallsPerGame
Dim gsFreePlay
Dim gsMatchPer
Dim gsTiltWarnings
Dim gsReplayStart
Dim gsReplayLevels
Dim gsNotesForSongSelect 'Default number of notes required to light change song
Dim MusicNumber 'current song playing (internal temporary use only)
MusicNumber=0
Dim MusicPlayed(12,3,1) ' tracks 12 songs for 4 players having been started, followed by number of times selected this game
Dim MusicCompleted(12,3,1) 'tracks 12 songs for 4 players having been completed, followed by the number of times completed
Dim MusicCompleted2(12,3,1) 'tracks 12 songs for 4 players having been completed total for game, followed by the number of times completed
Dim OldMusicCompleted(12,3,1) 'temp holder to check completions easier
Dim NoteCount(3,7) 'Tracks notes that have been 'hit'
Dim LastSongPlayed(3) 'Tracks last mode when ball drained - not including multiballs
Dim SkillShotActive
Dim SongSelectInProgress
Dim SkillValue1
Dim SkillValue2
Dim SkillValue3
Dim AwardedSkills(3,3) 'awarded skill shots for this game
Dim MusicScore(3,12) 'running total for this game for songs for each player
Dim MusicScoreTemp(3,12) 'temporary count to check if score is above completion level this time
Dim RandomAwardAvailable(3)
Dim RandomAwardShotsRequired(3)
Dim gsRandomAwardShotsRequiredDefault
Dim RandomAwardsAwarded(3,2)
Dim PriorSong,TempSongSelect
Dim FollowMeDifficulty(3)
Dim FollowMeCounter(3)
Dim PopHits(3)
Dim PopHitsTemp(3)
Dim BallsInSubway
Dim ComboValue
Dim CheckTrackScoresVal
Dim Z4 'needs to be global used for tracking the lowest numbered track record that is broken by a current player
Dim WizardModeAvailable(3)
Dim MiniWizardModeAvailable(3)
Dim WizardModeDisplaysActive
WizardModeDisplaysActive=FALSE
Dim OldHurryUp
OldHurryUp=0
Dim FollowMeVal,FollowMeLights
FollowMeLights=Array(LightProgress1,LightProgress2,LightProgress3,LightProgress4,LightProgress5,LightProgress6,LightProgress7,LightProgress6,LightProgress5,LightProgress4,LightProgress3,LightProgress2)
Dim bPlungerFlag
bPlungerFlag=TRUE
Dim SongSelectTimerCount
Dim BallSaved
BallSaved=FALSE
Dim PXO2,PXO3,PXO4,PXO5,ComH
PXO3=0
Dim MiniHPNum,MiniHPTotal,MiniHPVal,MiniHPValReal,MiniHPCurrent
Dim AQ,AQW,AQWE
Dim BonusTime,CombosStatusText,SongsStatusText
Dim EOBA
EOBA=0
Dim MTM
Dim Clock
Clock=0
Dim S1A,S1B,S1C,S1D,S1E,S1F,S1G,S1H,S1I,S1J,S1K,S1L,S1M,S1N,S1Z,S3Z
Dim S2A,S2B,S2C,S2D,S2E,S2F,S2G,S2H,S2I,S2J,S2K,S2L,S2M,S2N,S2Z,S4Z
Dim S5A,S5B,S5C,S5D,S5E,S5F,S5G,S5H,S5I,S5J,S5K,S5L,S5M,S5N,S5Z,S7Z
Dim S6A,S6B,S6C,S6D,S6E,S6F,S6G,S6H,S6I,S6J,S6K,S6L,S6M,S6N,S6Z,S8Z
Dim T2A,T2B,T2C,T2D,T2E,T2F,T2G,T2H,T2I,T2J,T2K,T2L,T2M,T2N,T2Z,T4Z
S1A=0:S1B=0:S1C=0:S1D=0:S1E=0:S1F=0:S1G=0:S1H=0:S1I=0:S1J=0:S1K=0:S1L=0:S1M=0:S1N=0:S1Z=0:S3Z=0
S2A=0:S2B=0:S2C=0:S2D=0:S2E=0:S2F=0:S2G=0:S2H=0:S2I=0:S2J=0:S2K=0:S2L=0:S2M=0:S2N=0:S2Z=0:S4Z=0
S5A=0:S5B=0:S5C=0:S5D=0:S5E=0:S5F=0:S5G=0:S5H=0:S5I=0:S5J=0:S5K=0:S5L=0:S5M=0:S5N=0:S5Z=0:S7Z=0
S6A=0:S6B=0:S6C=0:S6D=0:S6E=0:S6F=0:S6G=0:S6H=0:S6I=0:S6J=0:S6K=0:S6L=0:S6M=0:S6N=0:S6Z=0:S8Z=0
T2A=0:T2B=0:T2C=0:T2D=0:T2E=0:T2F=0:T2G=0:T2H=0:T2I=0:T2J=0:T2K=0:T2L=0:T2M=0:T2N=0:T2Z=0:T4Z=0
Dim HS1A,HS1B,HS1C,HS1D,HS1E,HS1F,HS1G,HS1H,HS1I,HS1J,HS1K,HS1L,HS1M,HS1N,HS1O,HS1Z,HS3Z
HS1A=0:HS1B=0:HS1C=0:HS1D=0:HS1E=0:HS1F=0:HS1G=0:HS1H=0:HS1I=0:HS1J=0:HS1K=0:HS1L=0:HS1M=0:HS1N=0:HS1O=0:HS1Z=0:HS3Z=0
Dim LPA(28)
Dim PH1 'Set up placeholder counters for text position queue
Dim phCredits1,phCredits2,Q
Dim PT,LeftUp,RightUp
PT=0:LeftUp=FALSE:RightUp=FALSE
Dim HurryUpActive
HurryUpActive=FALSE
Dim SP1,SP2,SP3,SP4,SP5,SP6,SP7
SP1=0:SP2=0:SP3=0:SP4=0:SP5=0:SP6=0:SP7=0
Dim PlaceLamp,TurnTable,DisplayName,TempLength,FirstRun
FirstRun=TRUE
Dim RandomCalloutCount
RandomCalloutCount=0
Dim PXZ1,PXZ2,PXZ3,PXZ4 'one variable for each of the three EQ Levels plus one randomizer for mid-range
Dim PlayedMiniWizard(3)
Dim Loops(3)
Dim LRamps(3)
Dim RRamps(3)
Dim PlayerBallsInLeftLock(3)
Dim PlayerBallsInTopLock(3)
Dim LeftLockOpened(3)
Dim LastLockHit(3)
Dim OldJP1,OldJP2,NewJP1,NewJP2
Dim TWA(14)
Dim MiniSingleVal,MiniSingleValReal,HurryUpSingleActive,MiniSingleCurrent,OldSingleHurryUp
MiniSingleCurrent=0:OldSingleHurryUp=0
MiniSingleVal=500000:MiniSingleValReal=500000:HurryUpSingleActive=FALSE
Dim ValidAwards(15),AwardSelectionTotal,AwardChosen,AwardNames(15),AwardCycleValue,FinalAwardSelectionValue,TotalCycles,CycleDirection
Dim Bumper1Frame,Bumper2Frame,Bumper3Frame
Dim TrapDoorAnimationEnabled
Dim ExtendTime
ExtendTime=FALSE
Dim AwardedEB
AwardedEB=FALSE
Dim TrackCallouts(9),rcallout,TotalCallOuts,NCSound,CurrentSound,CurrentMotorSound
TotalCallouts=10:CurrentSound="":CurrentMotorSound=""

AwardNames(1)="  MULTI-BALL  " 
AwardNames(2)=" SMALL POINTS " 
AwardNames(3)="   JACKPOT    " 
AwardNames(4)="COMPLETE MODE " 
AwardNames(5)=" +PLAYFIELD X "
AwardNames(6)="   +BONUS X   " 
AwardNames(7)="LITE XTRA BALL" 
AwardNames(8)=" LITE SPECIAL " 
AwardNames(9)=" LITE OUTLANE " 
AwardNames(10)="LITE LEFT LOCK" 
AwardNames(11)=" LITE RT LOCK "
AwardNames(12)=" SELECT SONG  " 
AwardNames(13)="LITE KICKBACK " 
AwardNames(14)="   HURRYUP    " 
AwardNames(15)=" 20 SEC SAVE  " 

'Animated Lamp effects
Dim TrackLights(12)
Set TrackLights(1)=LightTrack1 
Set TrackLights(2)=LightTrack2 
Set TrackLights(3)=LightTrack3 
Set TrackLights(4)=LightTrack4 
Set TrackLights(5)=LightTrack5 
Set TrackLights(6)=LightTrack6 
Set TrackLights(7)=LightTrack7 
Set TrackLights(8)=LightTrack8 
Set TrackLights(9)=LightTrack9 
Set TrackLights(10)=LightTrack10 
Set TrackLights(11)=LightTrack11 
Set TrackLights(12)=LightTrack12 

Dim LampList(56),LampState(56,4),ProgressLamps(7),JackpotLamps(7),ProgressState(7,4),JackpotState(7,4),X
Set LampList(1)=LightRollover1
Set LampList(2)=LightRollover2
Set LampList(3)=LightRollover3
Set LampList(4)=LightLock1
Set LampList(5)=LightLock2
Set LampList(6)=LightLock3
Set LampList(7)=LightExtraBall
Set LampList(8)=LightSpecial
Set LampList(9)=LightTrapDoor1
Set LampList(10)=LightTrapDoor2
Set LampList(11)=LightTrapDoor3
Set LampList(12)=LightNote1
Set LampList(13)=LightNote2
Set LampList(14)=LightNote3
Set LampList(15)=LightNote4
Set LampList(16)=LightNote5
Set LampList(17)=LightNote6
Set LampList(18)=LightNote7
Set LampList(19)=LightYStand1
Set LampList(20)=LightYStand2
Set LampList(21)=LightYStand3
Set LampList(22)=LightRStand1
Set LampList(23)=LightRStand2
Set LampList(24)=LightRStand3
Set LampList(25)=LightBand1
Set LampList(26)=LightBand2
Set LampList(27)=LightBand3
Set LampList(28)=LightBand4
Set LampList(29)=LightMB1
Set LampList(30)=LightMB2
Set LampList(31)=LightMB3
Set LampList(32)=LightMB4
Set LampList(33)=LightLeftInlane
Set LampList(34)=LightRightInlane1
Set LampList(35)=LightRightInlane2
Set LampList(36)=LightKickback
Set LampList(37)=LightLeftSpecial
Set LampList(38)=LightRightSpecial
Set LampList(39)=LightTrack1
Set LampList(40)=LightTrack2
Set LampList(41)=LightTrack3
Set LampList(42)=LightTrack4
Set LampList(43)=LightTrack5
Set LampList(44)=LightTrack6
Set LampList(45)=LightTrack7
Set LampList(46)=LightTrack8
Set LampList(47)=LightTrack9
Set LampList(48)=LightTrack10
Set LampList(49)=LightTrack11
Set LampList(50)=LightTrack12
Set LampList(51)=LightMiniWM
Set LampList(52)=LightWizardMode
Set LampList(53)=Light2X
Set LampList(54)=Light3X
Set LampList(55)=Light4X
Set LampList(56)=LightShootAgain

'special case -
Set ProgressLamps(1)=LightProgress1
Set ProgressLamps(2)=LightProgress2
Set ProgressLamps(3)=LightProgress3
Set ProgressLamps(4)=LightProgress4
Set ProgressLamps(5)=LightProgress5
Set ProgressLamps(6)=LightProgress6
Set ProgressLamps(7)=LightProgress7
Set JackpotLamps(1)=LightJackpot1
Set JackpotLamps(2)=LightJackpot2
Set JackpotLamps(3)=LightJackpot3
Set JackpotLamps(4)=LightJackpot4
Set JackpotLamps(5)=LightJackpot5
Set JackpotLamps(6)=LightJackpot6
Set JackpotLamps(7)=LightJackpot7

'Wizard Mode scoring has constant 4x bonus multiplier and 4x playfield multiplier, mini wizard mode is constant hurryup scores, no need to add extra scoring for those

'MODE 1 - SPINNER
Sub Spinner1_Spin
	PlaySound"fx_spinner"
	DOF 222, DOFPulse
	If MusicNumber>0 Then 'If ball in play, scoring is ok
		If MusicNumber=1 Then'If mode 1 (Super Spinners)
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+3000 ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+3000 ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(3000) 
			AddScore 3000 'add it
			'check it for completion
			'this mode completes by scoring 300 spins at approx 10 spins per shot, for approximately 10 shots
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=700000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack1.State=1
				CheckModeComplete
			End If
		Else
			If MusicNumber=12 Then
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+1250 ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+1250 ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(1250)
				AddScore 1250
				'This mode completes by achieving 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack12.State=1
					CheckModeComplete
				End If
			Else
				'standard scoring/multiball scoring/wizard mode scoring - spinner always scores 1000/spin during play
				AddScore 1000
			End If
		End If
	End If
End Sub
'END OF MODE 1

'MODE 2 - ALBUMS
Sub LeftInlane_Hit
	PlaySound"fx_sensor"
	DOF 215, DOFPulse
'	RW2.IsDropped=0
	If MusicNumber>0 Then 'If ball in play, scoring is ok
		Dim PAD 'points award
		If MusicNumber=2 Then'If mode 2 (ALBUMS)
			If LightLeftInlane.State=2 Then
				PAD=40000
				If LightRightInlane1.State=1 And LightRightInlane2.State=1 And LightRollover1.State=1 And LightRollover2.State=1 And LightRollover3.State=1 Then
					PAD=PAD+50000 '50000 additional points and reset all album lamps to blinking
					LightRightInlane1.State=2:LightRightInlane2.State=2:LightRollover1.State=2:LightRollover2.State=2:LightRollover3.State=2
				Else
					LightLeftInlane.State=1 'otherwise, simply change to solid color
				End If
			Else
				PAD=20000 'solid color is worth half as much as blinking
			End If
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PAD ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PAD ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PAD)
			AddScore PAD 'add it
			'check it for completion
			'This mode completes by either hitting each blinking target twice and then each single lit target once, or a combination of hits to achieve >680,000 points
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=680000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack2.State=1
				CheckModeComplete
			End If
		Else
			If MusicNumber=12 Then
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+12500 ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+12500 ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(12500)
				AddScore 12500
				'This mode completes by achieving 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack12.State=1
					CheckModeComplete
				End If
			Else
				'standard scoring or wizard mode scoring
				AddScore 10000
			End If
			LightLeftInlane.State=1
			CheckRolloverLanesBonus
		End If
	End If
End Sub
'Sub LeftInlane_unHit:RW2.IsDropped=1:End Sub

Sub RightInlane1_Hit
	PlaySound"fx_sensor"
	DOF 216, DOFPulse
'	RW3.IsDropped=0
	If MusicNumber>0 Then 'If ball in play, scoring is ok
		Dim PAD 'points award
		If MusicNumber=2 Then'If mode 2 (ALBUMS)
			If LightRightInlane1.State=2 Then
				PAD=40000
				If LightLeftInlane.State=1 And LightRightInlane2.State=1 And LightRollover1.State=1 And LightRollover2.State=1 And LightRollover3.State=1 Then
					PAD=PAD+50000 '50000 additional points and reset all album lamps to blinking
					LightLeftInlane.State=2:LightRightInlane2.State=2:LightRollover1.State=2:LightRollover2.State=2:LightRollover3.State=2
				Else
					LightRightInlane1.State=1 'otherwise, simply change to solid color
				End If
			Else
				PAD=20000 'solid color is worth half as much as blinking
			End If
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PAD ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PAD ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PAD)
			AddScore PAD 'add it
			'check it for completion
			'This mode completes by either hitting each blinking target twice and then each single lit target once, or a combination of hits to achieve >680,000 points
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=680000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack2.State=1
				CheckModeComplete
			End If
		Else
			If MusicNumber=12 Then
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+12500 ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+12500 ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(12500)
				AddScore 12500
				'This mode completes by achieving 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack12.State=1
					CheckModeComplete
				End If
			Else
				'standard scoring or wizard mode scoring
				AddScore 10000
			End If
			LightRightInlane1.State=1
			CheckRolloverLanesBonus
		End If
	End If
End Sub
'Sub RightInlane1_unHit:RW3.IsDropped=1:End Sub

Sub RightInlane2_Hit
	PlaySound"fx_sensor"
	DOF 217, DOFPulse
'	RW4.IsDropped=0
	If MusicNumber>0 Then 'If ball in play, scoring is ok
		Dim PAD 'points award
		If MusicNumber=2 Then'If mode 2 (ALBUMS)
			If LightRightInlane2.State=2 Then
				PAD=40000
				If LightLeftInlane.State=1 And LightRightInlane1.State=1 And LightRollover1.State=1 And LightRollover2.State=1 And LightRollover3.State=1 Then
					PAD=PAD+50000 '50000 additional points and reset all album lamps to blinking
					LightLeftInlane.State=2:LightRightInlane1.State=2:LightRollover1.State=2:LightRollover2.State=2:LightRollover3.State=2
				Else
					LightRightInlane2.State=1 'otherwise, simply change to solid color
				End If
			Else
				PAD=20000 'solid color is worth half as much as blinking
			End If
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PAD ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PAD ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PAD)
			AddScore PAD 'add it
			'check it for completion
			'This mode completes by either hitting each blinking target twice and then each single lit target once, or a combination of hits to achieve >680,000 points
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=680000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack2.State=1
				CheckModeComplete
			End If
		Else
			If MusicNumber=12 Then
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+12500 ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+12500 ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(12500)
				AddScore 12500
				'This mode completes by achieving 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack12.State=1
					CheckModeComplete
				End If
			Else
				'standard scoring or wizard mode scoring
				AddScore 10000
			End If
			LightRightInlane2.State=1
			CheckRolloverLanesBonus
		End If
	End If
End Sub
'Sub RightInlane2_unHit:RW4.IsDropped=1:End Sub

Sub Rollover1_Hit
	PlaySound"fx_sensor"
	DOF 210, DOFPulse
'	RW6.IsDropped=0
	If HurryUpSingleActive=TRUE And HurrySingle.Enabled Then HurrySingle.Enabled=0 'disable single hurryup as the ball is in the bumpers
	If HurryUpActive=TRUE And HurrySmall.Enabled Then HurrySmall.Enabled=0 'disable hurryup as the ball is in the bumpers
	If MusicNumber>0 Then 'If ball in play, scoring is ok
		Dim PAD 'points award
		If MusicNumber=2 Then'If mode 2 (ALBUMS)
			If LightRollover1.State=2 Then
				PAD=40000
				If LightLeftInlane.State=1 And LightRightInlane1.State=1 And LightRightInlane2.State=1 And LightRollover2.State=1 And LightRollover3.State=1 Then
					PAD=PAD+50000 '50000 additional points and reset all album lamps to blinking
					LightLeftInlane.State=2:LightRightInlane1.State=2:LightRightInlane2.State=2:LightRollover2.State=2:LightRollover3.State=2
				Else
					LightRollover1.State=1 'otherwise, simply change to solid color
				End If
			Else
				PAD=20000 'solid color is worth half as much as blinking
			End If
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PAD ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PAD ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PAD)
			AddScore PAD 'add it
			'check it for completion
			'This mode completes by either hitting each blinking target twice and then each single lit target once, or a combination of hits to achieve >680,000 points
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=680000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack2.State=1
				CheckModeComplete
			End If
		Else
			If MusicNumber=12 Then
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+12500 ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+12500 ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(12500)
				AddScore 12500
				'This mode completes by achieving 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack12.State=1
					CheckModeComplete
				End If
			Else
				'standard scoring or wizard mode scoring
				AddScore 10000
			End If
			LightRollover1.State=1
			CheckRolloverLanesBonus
		End If
	End If
End Sub
'Sub Rollover1_unHit:RW6.IsDropped=1:End Sub

Sub Rollover2_Hit
	PlaySound"fx_sensor"
	DOF 211, DOFPulse
'	RW7.IsDropped=0
	If HurryUpSingleActive=TRUE And HurrySingle.Enabled Then HurrySingle.Enabled=0 'disable single hurryup as the ball is in the bumpers
	If HurryUpActive=TRUE And HurrySmall.Enabled Then HurrySmall.Enabled=0 'disable hurryup as the ball is in the bumpers
	If MusicNumber>0 Then 'If ball in play, scoring is ok
		Dim PAD 'points award
		If MusicNumber=2 Then'If mode 2 (ALBUMS) - Mode 2 Albums does NOT increase bonus multiplier
			If LightRollover2.State=2 Then
				PAD=40000
				If LightLeftInlane.State=1 And LightRightInlane1.State=1 And LightRightInlane2.State=1 And LightRollover1.State=1 And LightRollover3.State=1 Then
					PAD=PAD+50000 '50000 additional points and reset all album lamps to blinking
					LightLeftInlane.State=2:LightRightInlane1.State=2:LightRightInlane2.State=2:LightRollover1.State=2:LightRollover3.State=2
				Else
					LightRollover2.State=1 'otherwise, simply change to solid color
				End If
			Else
				PAD=20000 'solid color is worth half as much as blinking
			End If
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PAD ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PAD ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PAD)
			AddScore PAD 'add it
			'check it for completion
			'This mode completes by either hitting each blinking target twice and then each single lit target once, or a combination of hits to achieve >680,000 points
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=680000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack2.State=1
				CheckModeComplete
			End If
		Else
			If MusicNumber=12 Then
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+12500 ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+12500 ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(12500)
				AddScore 12500
				'This mode completes by achieving 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack12.State=1
					CheckModeComplete
				End If
			Else
				'standard scoring or wizard mode scoring
				AddScore 10000
			End If
			LightRollover2.State=1
			CheckRolloverLanesBonus
		End If
	End If
End Sub
'Sub Rollover2_unHit:RW7.IsDropped=1:End Sub

Sub Rollover3_Hit
	PlaySound"fx_sensor"
	DOF 212, DOFPulse
'	RW8.IsDropped=0
	If HurryUpSingleActive=TRUE And HurrySingle.Enabled Then HurrySingle.Enabled=0 'disable single hurryup as the ball is in the bumpers
	If HurryUpActive=TRUE And HurrySmall.Enabled Then HurrySmall.Enabled=0 'disable hurryup as the ball is in the bumpers
	If MusicNumber>0 Then 'If ball in play, scoring is ok
		Dim PAD 'points award
		If MusicNumber=2 Then'If mode 2 (ALBUMS)
			If LightRollover3.State=2 Then
				PAD=40000
				If LightLeftInlane.State=1 And LightRightInlane1.State=1 And LightRightInlane2.State=1 And LightRollover1.State=1 And LightRollover2.State=1 Then
					PAD=PAD+50000 '50000 additional points and reset all album lamps to blinking
					LightLeftInlane.State=2:LightRightInlane1.State=2:LightRightInlane2.State=2:LightRollover1.State=2:LightRollover2.State=2
				Else
					LightRollover3.State=1 'otherwise, simply change to solid color
				End If
			Else
				PAD=20000 'solid color is worth half as much as blinking
			End If
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PAD ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PAD ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PAD)
			AddScore PAD 'add it
			'check it for completion
			'This mode completes by either hitting each blinking target twice and then each single lit target once, or a combination of hits to achieve >680,000 points
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=680000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack2.State=1
				CheckModeComplete
			End If
		Else
			If MusicNumber=12 Then
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+12500 ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+12500 ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(12500)
				AddScore 12500
				'This mode completes by achieving 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack12.State=1
					CheckModeComplete
				End If
			Else
				'standard scoring or wizard mode scoring
				AddScore 10000
			End If
			LightRollover3.State=1
			CheckRolloverLanesBonus
		End If
	End If
End Sub
'Sub Rollover3_unHit:RW8.IsDropped=1:End Sub

Sub CheckRolloverLanesBonus
	If LightLeftInlane.State=1 And LightRightInlane1.State=1 And LightRightInlane2.State=1 And LightRollover1.State=1 And LightRollover2.State=1 And LightRollover3.State=1 Then 'increase multiplier
		If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then 'If PLAYFIELD BONUS MULTIPLIER ACTIVE then
			Select Case BonusMultiplier(CurrentPlayer)
				'there is no Light1X lamp
				Case 2:Light2X.State=0 'turn off 2x lamp
				Case 3:Light3X.State=0 'turn off 3x lamp
			End Select
			BonusMultiplier(CurrentPlayer)=BonusMultiplier(CurrentPlayer)+1 'increase bonus multiplier and playfield bonus multiplier
			If BonusMultiplier(CurrentPlayer)>4 Then
				BonusMultiplier(CurrentPlayer)=4
				AddScore 25000
			End If
			Select Case BonusMultiplier(CurrentPlayer)
				Case 2:Light2X.State=2 'turn on 2X lamp
				Case 3:Light3X.State=2 'turn on 3X lamp
				Case 4:Light4X.State=2 'turn on 4X lamp
			End Select
			LightLeftInlane.State=0:LightRightInlane1.State=0:LightRightInlane2.State=0:LightRollover1.State=0:LightRollover2.State=0:LightRollover3.State=0 'reset rollover lamps
		Else
			Select Case BonusMultiplier(CurrentPlayer)
				'there is no Light1X lamp
				Case 2:Light2X.State=0 'turn off 2x lamp
				Case 3:Light3X.State=0 'turn off 3x lamp
			End Select
			BonusMultiplier(CurrentPlayer)=BonusMultiplier(CurrentPlayer)+1 'increase bonus multiplier
			If BonusMultiplier(CurrentPlayer)>4 Then
				BonusMultiplier(CurrentPlayer)=4
				AddScore 25000
			End If
			Select Case BonusMultiplier(CurrentPlayer)
				Case 2:Light2X.State=1 'turn on 2X lamp
				Case 3:Light3X.State=1 'turn on 3X lamp
				Case 4:Light4X.State=1 'turn on 4X lamp
			End Select
			LightLeftInlane.State=0:LightRightInlane1.State=0:LightRightInlane2.State=0:LightRollover1.State=0:LightRollover2.State=0:LightRollover3.State=0 'reset rollover lamps
		End If
	End If
End Sub
'END OF MODE 2

'MODE 3 - POP BUMPERS
Sub Bumper1_Hit
	If HurryUpSingleActive=TRUE And HurrySingle.Enabled Then HurrySingle.Enabled=0 'disable single hurryup as the ball is in the bumpers
	If HurryUpActive=TRUE And HurrySmall.Enabled Then HurrySmall.Enabled=0 'disable hurryup as the ball is in the bumpers
	If MusicNumber>0 Then 'If ball in play, scoring is ok
		If LightRightSpecial.State=1 Then
			LightLeftSpecial.State=1
			LightRightSpecial.State=0
            Bumper1L.State = 1
	        Me.TimerEnabled = 1
		Else
			If LightLeftSpecial.State=1 Then
				LightLeftSpecial.State=0
				LightRightSpecial.State=1
                Bumper1L.State = 1
 	            Me.TimerEnabled = 0  
			End If
		End If
		Dim PAD
		PopHits(CurrentPlayer)=PopHits(CurrentPlayer)+1 'increase pop bumpr hit tracking for this game
		PlaySound SoundFXDOF("fx_bumper1",205,DOFPulse,DOFContactors)
		If MusicNumber=3 Then'If mode 3 (BUMPERS)
			PT=PT+1
			If PT MOD 10=0 Then:BGate.Open=TRUE:CGate.Open=TRUE:End If 'after 10 pop hits minimum, open the gates
			PopHitsTemp(CurrentPlayer)=PopHitsTemp(CurrentPlayer)+1
			PAD=210+(INT(PopHits(CurrentPlayer)/20)*500)
			If PAD>3210 Then PAD=3210
			PAD=PAD+5000
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PAD ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PAD ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PAD)
			AddScore PAD 'add it
			'check it for completion
			'This mode completes by scoring 100 bumper hits
			If PopHitsTemp(CurrentPlayer)=100 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack3.State=1
				CheckModeComplete
			End If
		Else
			'standard scoring or wizard mode scoring
			PAD=210+(INT(PopHits(CurrentPlayer)/20)*500)
			If PAD>3210 Then PAD=3210
			If MusicNumber=12 Then
				Dim PADTemp
				PADTemp=Round(PAD*1.25)
				PADTemp=INT(PADTemp/10)*10 'ensure value will end with 10 points
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PADTemp ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PADTemp ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PADTemp)
				AddScore PADTemp
				'This mode completes by achieving 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack12.State=1
					CheckModeComplete
				End If
			Else
				AddScore PAD
			End If
		End If
	End If
	If bTilted=FALSE Then
		FlasherPops.TimerEnabled=0
		FlasherPops.TimerEnabled=1
		FlasherPops.State=1
		Bumper1Frame=0
'		Ring1a.IsDropped=1:Ring1b.IsDropped=1:Ring1c.IsDropped=1
		If MusicNumber<>3 Then Bumper1L.State=1 'flash the bumper in standard mode when hit
		Bumper1.TimerEnabled=0
		Bumper1.TimerEnabled=1
		If bBallSaverActive=TRUE Then 'extend time for ball saver if ball is stuck in the bumpers
			Timer_BallSave.Enabled=0
			Timer_BallSave.Enabled=1
		End If
	End If
End Sub

Sub FlasherPops_Timer
	FlasherPops.TimerEnabled=0
	FlasherPops.State=0
End Sub

Sub Bumper1_Timer
	Bumper1Frame=Bumper1Frame+1
	Select Case Bumper1Frame
'		Case 1:Ring1a.IsDropped=1:Ring1b.IsDropped=1:Ring1c.IsDropped=0
'		Case 2:Ring1a.IsDropped=1:Ring1b.IsDropped=0:Ring1c.IsDropped=1
'		Case 3:Ring1a.IsDropped=0:Ring1b.IsDropped=1:Ring1c.IsDropped=1
'		Case 4:Ring1a.IsDropped=1:Ring1b.IsDropped=0:Ring1c.IsDropped=1
'		Case 5:Ring1a.IsDropped=1:Ring1b.IsDropped=1:Ring1c.IsDropped=0
'		Case 6:Ring1a.IsDropped=1:Ring1b.IsDropped=1:Ring1c.IsDropped=1
		Case 6:Bumper1.TimerEnabled=0:If MusicNumber<>3 Then:Bumper1L.State=0:End If 'turn the bumper bulb off
	End Select
End Sub

Sub Bumper2_Hit
	If HurryUpSingleActive=TRUE And HurrySingle.Enabled Then HurrySingle.Enabled=0 'disable single hurryup as the ball is in the bumpers
	If HurryUpActive=TRUE And HurrySmall.Enabled Then HurrySmall.Enabled=0 'disable hurryup as the ball is in the bumpers
	If MusicNumber>0 Then 'If ball in play, scoring is ok
		If LightRightSpecial.State=1 Then
			LightLeftSpecial.State=1
			LightRightSpecial.State=0
            Bumper2L.State = 1
	        Me.TimerEnabled = 1
		Else
			If LightLeftSpecial.State=1 Then
				LightLeftSpecial.State=0
				LightRightSpecial.State=1
                Bumper2L.State = 0
	            Me.TimerEnabled = 1
			End If
		End If
		Dim PAD
		PopHits(CurrentPlayer)=PopHits(CurrentPlayer)+1 'increase pop bumpr hit tracking for this game
		PlaySound SoundFXDOF("fx_bumper2",207,DOFPulse,DOFContactors) 
		If MusicNumber=3 Then'If mode 3 (BUMPERS)
			PT=PT+1
			If PT MOD 10=0 Then:BGate.Open=TRUE:CGate.Open=TRUE:End If 'after 10 pop hits minimum, open the gates
			PopHitsTemp(CurrentPlayer)=PopHitsTemp(CurrentPlayer)+1
			PAD=210+(INT(PopHits(CurrentPlayer)/20)*500)
			If PAD>3210 Then PAD=3210
			PAD=PAD+5000
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PAD ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PAD ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PAD)
			AddScore PAD 'add it
			'check it for completion
			'This mode completes by scoring 100 bumper hits
			If PopHitsTemp(CurrentPlayer)=100 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack3.State=1
				CheckModeComplete
			End If
		Else
			'standard scoring or wizard mode scoring
			PAD=210+(INT(PopHits(CurrentPlayer)/20)*500)
			If PAD>3210 Then PAD=3210
			If MusicNumber=12 Then
				Dim PADTemp
				PADTemp=Round(PAD*1.25)
				PADTemp=INT(PADTemp/10)*10 'ensure value will end with 10 points
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PADTemp ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PADTemp ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PADTemp)
				AddScore PADTemp
				'This mode completes by achieving 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack12.State=1
					CheckModeComplete
				End If
			Else
				AddScore PAD
			End If
		End If
	End If
	If bTilted=FALSE Then
		FlasherPops.TimerEnabled=0
		FlasherPops.TimerEnabled=1
		FlasherPops.State=1
		Bumper2Frame=0
'		Ring2a.IsDropped=1:Ring2b.IsDropped=1:Ring2c.IsDropped=1
		If MusicNumber<>3 Then Bumper2L.State=1 'flash the bumper in standard mode when hit
		Bumper2.TimerEnabled=0
		Bumper2.TimerEnabled=1
		If bBallSaverActive=TRUE Then 'extend time for ball saver if ball is stuck in the bumpers
			Timer_BallSave.Enabled=0
			Timer_BallSave.Enabled=1
		End If
	End If
End Sub

Sub Bumper2_Timer
	Bumper2Frame=Bumper2Frame+1
	Select Case Bumper2Frame
'		Case 1:Ring2a.IsDropped=1:Ring2b.IsDropped=1:Ring2c.IsDropped=0
'		Case 2:Ring2a.IsDropped=1:Ring2b.IsDropped=0:Ring2c.IsDropped=1
'		Case 3:Ring2a.IsDropped=0:Ring2b.IsDropped=1:Ring2c.IsDropped=1
'		Case 4:Ring2a.IsDropped=1:Ring2b.IsDropped=0:Ring2c.IsDropped=1
'		Case 5:Ring2a.IsDropped=1:Ring2b.IsDropped=1:Ring2c.IsDropped=0
'		Case 6:Ring2a.IsDropped=1:Ring2b.IsDropped=1:Ring2c.IsDropped=1
		Case 6:Bumper2.TimerEnabled=0:If MusicNumber<>3 Then:Bumper2L.State=0:End If 'turn the bumper bulb off
	End Select
End Sub


Sub Bumper3_Hit
	If HurryUpSingleActive=TRUE And HurrySingle.Enabled Then HurrySingle.Enabled=0 'disable single hurryup as the ball is in the bumpers
	If HurryUpActive=TRUE And HurrySmall.Enabled Then HurrySmall.Enabled=0 'disable hurryup as the ball is in the bumpers
	If MusicNumber>0 Then 'If ball in play, scoring is ok
		If LightRightSpecial.State=1 Then
			LightLeftSpecial.State=1
			LightRightSpecial.State=0
            Bumper3L.State = 1
	        Me.TimerEnabled = 1
		Else
			If LightLeftSpecial.State=1 Then
				LightLeftSpecial.State=0
				LightRightSpecial.State=1
                Bumper3L.State = 0
	            Me.TimerEnabled = 1
			End If
		End If
		Dim PAD
		PopHits(CurrentPlayer)=PopHits(CurrentPlayer)+1 'increase pop bumpr hit tracking for this game
		PlaySound SoundFXDOF("fx_bumper3",206,DOFPulse,DOFContactors)
		If MusicNumber=3 Then'If mode 3 (BUMPERS)
			PT=PT+1
			If PT MOD 10=0 Then:BGate.Open=TRUE:CGate.Open=TRUE:End If 'after 10 pop hits minimum, open the gates
			PopHitsTemp(CurrentPlayer)=PopHitsTemp(CurrentPlayer)+1
			PAD=210+(INT(PopHits(CurrentPlayer)/20)*500)
			If PAD>3210 Then PAD=3210
			PAD=PAD+5000
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PAD ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PAD ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PAD)
			AddScore PAD 'add it
			'check it for completion
			'This mode completes by scoring 100 bumper hits
			If PopHitsTemp(CurrentPlayer)=100 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack3.State=1
				CheckModeComplete
			End If
		Else
			'standard scoring or wizard mode scoring
			PAD=210+(INT(PopHits(CurrentPlayer)/20)*500)
			If PAD>3210 Then PAD=3210
			If MusicNumber=12 Then
				Dim PADTemp
				PADTemp=Round(PAD*1.25)
				PADTemp=INT(PADTemp/10)*10 'ensure value will end with 10 points
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PADTemp ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PADTemp ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PADTemp)
				AddScore PADTemp
				'This mode completes by achieving 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack12.State=1
					CheckModeComplete
				End If
			Else
				AddScore PAD
			End If
		End If
	End If
	If bTilted=FALSE Then
		FlasherPops.TimerEnabled=0
		FlasherPops.TimerEnabled=1
		FlasherPops.State=1
		Bumper3Frame=0
'		Ring3a.IsDropped=1:Ring3b.IsDropped=1:Ring3c.IsDropped=1
		If MusicNumber<>3 Then Bumper3L.State=1 'flash the bumper in standard mode when hit
		Bumper3.TimerEnabled=0
		Bumper3.TimerEnabled=1
		If bBallSaverActive=TRUE Then 'extend time for ball saver if ball is stuck in the bumpers
			Timer_BallSave.Enabled=0
			Timer_BallSave.Enabled=1
		End If
	End If
End Sub

Sub Bumper3_Timer
	Bumper3Frame=Bumper3Frame+1
	Select Case Bumper3Frame
'		Case 1:Ring3a.IsDropped=1:Ring3b.IsDropped=1:Ring3c.IsDropped=0
'		Case 2:Ring3a.IsDropped=1:Ring3b.IsDropped=0:Ring3c.IsDropped=1
'		Case 3:Ring3a.IsDropped=0:Ring3b.IsDropped=1:Ring3c.IsDropped=1
'		Case 4:Ring3a.IsDropped=1:Ring3b.IsDropped=0:Ring3c.IsDropped=1
'		Case 5:Ring3a.IsDropped=1:Ring3b.IsDropped=1:Ring3c.IsDropped=0
'		Case 6:Ring3a.IsDropped=1:Ring3b.IsDropped=1:Ring3c.IsDropped=1
		Case 6:Bumper3.TimerEnabled=0:If MusicNumber<>3 Then:Bumper3L.State=0:End If 'turn the bumper bulb off
	End Select
End Sub

Sub Trigger1_unHit
	HandlePT
	If bTilted=FALSE Then
		If ActiveBall.VelY<0 Then
			If HurryUpSingleActive=TRUE And Hurry5.State=2 Then AwardHurryUpSingle'Small HurryUp is active
			If LightMiniWM.State=1 Then 'Mini Wizard Mode is active!  All Hurryups all the time
				If Hurry5.State=2 Then 'if hurryup is lit,
					Hurry5.State=0 'turn off the hurryup light
					LightProgress5.State=0
					AwardMiniHurryUp
					If LightMiniWM.State=1 Then 'Mini Wizard Mode Running
						HurryUpActive=TRUE
						HurrySmall.Enabled=1
						DisplayScore
					End If
				End If
			End If
		Else
			If HurryUpActive=TRUE And HurrySmall.Enabled=0 Then HurrySmall.Enabled=1:DOF 236, DOFPulse 're-enable hurryup as the ball is out of the bumpers
			If HurryUpSingleActive=TRUE And HurrySingle.Enabled=0 Then HurrySingle.Enabled=1:DOF 237, DOFPulse 're-enable single hurryup as the ball is out of the bumpers
		End If
	End If
End Sub

Sub Trigger2_unHit
	HandlePT
	If bTilted=FALSE Then
		If ActiveBall.VelY<0 Then
			If HurryUpSingleActive=TRUE And Hurry3.State=2 Then AwardHurryUpSingle'Small HurryUp is active
			If LightMiniWM.State=1 Then 'Mini Wizard Mode is active!  All Hurryups all the time
				If Hurry3.State=2 Then 'if hurryup is lit,
					Hurry3.State=0 'turn off the hurryup light
					LightProgress3.State=0
					AwardMiniHurryUp
					If LightMiniWM.State=1 Then 'Mini Wizard Mode Running
						HurryUpActive=TRUE
						HurrySmall.Enabled=1
						DisplayScore
					End If
				End If
			End If
		Else
			If HurryUpActive=TRUE And HurrySmall.Enabled=0 Then HurrySmall.Enabled=1:DOF 236, DOFPulse 're-enable hurryup as the ball is out of the bumpers
			If HurryUpSingleActive=TRUE And HurrySingle.Enabled=0 Then HurrySingle.Enabled=1:DOF 237, DOFPulse 're-enable single hurryup as the ball is out of the bumpers
		End If
	End If
End Sub

Sub HandlePT
	If MusicNumber=3 Then  'close gates until minimum pop bumper hits per visit is achieved
		If bTilted=FALSE Then
			PT=0
			BGate.Open=FALSE
			CGate.Open=FALSE
		End If
	Else
		BGate.Open=TRUE
		CGate.Open=TRUE
	End If
End Sub
'END OF MODE 3

'MODE 4 - RAMPS
Sub RightRamp_Hit
	LastSwitchHit=5 'RightRamp
	If bTilted=FALSE Then
		If SkillShotTimer.Enabled Then 'skill shot check
			If Drop4.IsDropped Then:Drop4.IsDropped=0:PlaySound SoundFXDOF("fx_solenoid",227,DOFPulse,DOFContactors) End If 'raise the drop target and make a sound only if it is down
			Skill2.State=0 'Turn off skill shot 2 lamp
			If Skill3SignOff.TimerEnabled Then Skill3SignOff.TimerEnabled=0 'disable blinking skill shot sign
			If Not Skill3SignOn.IsDropped Then Skill3SignOn.IsDropped=1
			If Skill3SignOff.IsDropped Then Skill3SignOff.IsDropped=0 'turn off the skill shot sign
			LightProgress6.State=1 'shot was made in time, so make light solid!!!
			SkillValue1=SkillValue1*2' (double value for skill shot)
			SkillShotTimer.Enabled=0 'shot was made, so disable all other skill shots
		End If
		'still accept progression if the ramps mode is active during the skill shot time period -
		If MusicNumber>0 Then 'If ball in play, scoring is ok
			If MusicNumber=4 Then'If mode 4 (RAMPS)
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+125000 ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+125000 ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(125000)
				AddScore 125000 'add it
				'check it for completion
				'This mode completes by scoring 8 ramp shots during the mode for a total of 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack4.State=1
					CheckModeComplete
				End If
			End If
			If MusicNumber=11 Then 'Follow Me Mode
				If LightProgress6.State=2 Then
					If ActiveBall.VelY>0 Then
						FollowMeCounter(CurrentPlayer)=FollowMeCounter(CurrentPlayer)+1 'increment counter by 1
						MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+100000 ' track it for total
						MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+100000 ' track it for completion
						If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(100000)
						AddScore 100000 'add it
						'check it for completion
						'This mode completes by scoring 7 lit shots
						If FollowMeCounter(CurrentPlayer)=7 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
							PlaySound"AModeComplete"
							DOF 240, DOFPulse
							MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
							MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
							LightTrack11.State=1
							CheckModeComplete
						End If
					Else 'light is not lit, so handle standard scoring routines
						AddScore 75000
					End If
				End If
			Else
				If MusicNumber<>4 Then
					If MusicNumber=12 Then
						MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+93750 ' track it for total
						MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+93750 ' track it for completion
						If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(93750)
						AddScore 93750
						'This mode completes by achieving 1000000 points
						If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
							PlaySound"AModeComplete"
							DOF 240, DOFPulse
							MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
							MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
							LightTrack12.State=1
							CheckModeComplete
						End If
					Else
						'standard scoring or wizard mode scoring
						AddScore 75000
					End If
				End If
			End If
		End If
		If b2BallMultiMode Or b3BallMultiMode Then
			If LightJackpot6.State=2 Then
				'A MULTIBALL IS IN PROGRESS, DETERMINE WHICH ONE AND AWARD PROPER SCORING
				If b2BallMultiMode Then UpdateJackpot1
				If b3BallMultiMode Then UpdateJackpot2
			End If
		End If
		'handle note lamps for song selection
		If MusicNumber<13 And MusicNumber>0 Then
			If Not b2BallMultiMode And Not b3BallMultiMode Then
				If ActiveBall.VelY>0 Then
					If HurryUpSingleActive=FALSE Then
						CheckCombo(6)
						CheckRightRampLock
						If LightNote6.State=2 Then
							NoteCount(CurrentPlayer,6)=NoteCount(CurrentPlayer,6)+1
							LightNote6.State=0 'turn it off
						End If
						CheckNotes
					End If
				End If
			End If
		End If
		If bTilted=FALSE Then
			If HurryUpSingleActive=TRUE And Hurry6.State=2 Then AwardHurryUpSingle'Small HurryUp is active
			If LightMiniWM.State=1 Then 'Mini Wizard Mode is active!  All Hurryups all the time
				If Hurry6.State=2 Then 'if hurryup is lit,
					Hurry6.State=0 'turn off the hurryup light
					LightProgress6.State=0
					AwardMiniHurryUp
					If LightMiniWM.State=1 Then 'Mini Wizard Mode Running
						HurryUpActive=TRUE
						HurrySmall.Enabled=1
						DisplayScore
					End If
				End If
			End If
		End If
	End If
End Sub

Sub CheckRightRampLock
	RRamps(CurrentPlayer)=RRamps(CurrentPlayer)+1
	If RRamps(CurrentPlayer)>18 Then RRamps(CurrentPlayer)=18
	Select Case RRamps(CurrentPlayer)
		Case 1:LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=0
		Case 2:LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=0
		Case 3:LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=0
		Case 4:LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=0
		Case 5:LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=2 'blink right ramp lock 1 light
		Case 6:LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=2:TrapDoorRamp.Collidable=0:TrapDoorWall.IsDropped=1:TrapDoorStop.IsDropped=0 'open trapdoor
		Case 7:LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=1
		Case 8:LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=1
		Case 9:LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=1
		Case 10:LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=1
		Case 11:LightTrapDoor1.State=0:LightTrapDoor2.State=2:LightTrapDoor3.State=1 'blink right ramp lock 2 light
		Case 12:LightTrapDoor1.State=0:LightTrapDoor2.State=2:LightTrapDoor3.State=1:TrapDoorRamp.Collidable=0:TrapDoorWall.IsDropped=1:TrapDoorStop.IsDropped=0 'open trapdoor
		Case 13:LightTrapDoor1.State=0:LightTrapDoor2.State=1:LightTrapDoor3.State=1
		Case 14:LightTrapDoor1.State=0:LightTrapDoor2.State=1:LightTrapDoor3.State=1
		Case 15:LightTrapDoor1.State=0:LightTrapDoor2.State=1:LightTrapDoor3.State=1
		Case 16:LightTrapDoor1.State=0:LightTrapDoor2.State=1:LightTrapDoor3.State=1
		Case 17:LightTrapDoor1.State=2:LightTrapDoor2.State=1:LightTrapDoor3.State=1 'blink right ramp lock 3 light
		Case 18:LightTrapDoor1.State=2:LightTrapDoor2.State=1:LightTrapDoor3.State=1:TrapDoorRamp.Collidable=0:TrapDoorWall.IsDropped=1:TrapDoorStop.IsDropped=0 'open trapdoor
	End Select
End Sub

Sub LeftRamp_Hit
	LastSwitchHit=7 'LeftRamp
	If bTilted=FALSE Then
		If LightExtraBall.State=2 Then
			LightExtraBall.State=0
			AwardExtraBall CurrentPlayer,1,FALSE
		End If
		If LightSpecial.State=2 Then
			If gsFreePlay Then
				' award and extra ball to the player
				AwardExtraBall CurrentPlayer,1,TRUE
			Else
				' award a credit
				AwardFreeGame 1
			End If
			LightSpecial.State=0
		End If
		If MusicNumber>0 Then 'If ball in play, scoring is ok
			If MusicNumber=4 Then'If mode 4 (RAMPS)
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+125000 ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+125000 ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(125000)
				AddScore 125000 'add it
				'check it for completion
				'This mode completes by scoring 8 ramp shots during the mode for a total of 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack4.State=1
					CheckModeComplete
				End If
			End If
			If MusicNumber=11 Then 'Follow Me Mode
				If LightProgress4.State=2 Then
					If ActiveBall.VelX<0 Then
						FollowMeCounter(CurrentPlayer)=FollowMeCounter(CurrentPlayer)+1 'increment counter by 1
						MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+100000 ' track it for total
						MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+100000 ' track it for completion
						If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(100000)
						AddScore 100000 'add it
						'check it for completion
						'This mode completes by scoring 7 lit shots
						If FollowMeCounter(CurrentPlayer)=7 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
							PlaySound"AModeComplete"
							DOF 240, DOFPulse
							MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
							MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
							LightTrack11.State=1
							CheckModeComplete
						End If
					End If
				Else 'light is not lit, so handle standard scoring routines
					AddScore 75000
				End If
			Else
				If MusicNumber<>4 Then
					If MusicNumber=12 Then
						MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+93750 ' track it for total
						MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+93750 ' track it for completion
						If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(93750)
						AddScore 93750
						'This mode completes by achieving 1000000 points
						If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
							PlaySound"AModeComplete"
							DOF 240, DOFPulse
							MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
							MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
							LightTrack12.State=1
							CheckModeComplete
						End If
					Else
						'standard scoring or wizard mode scoring
						AddScore 75000
					End If
				End If
			End If
		End If
		If b2BallMultiMode Or b3BallMultiMode Then
			If LightJackpot4.State=2 Then
				'A MULTIBALL IS IN PROGRESS, DETERMINE WHICH ONE AND AWARD PROPER SCORING
				If b2BallMultiMode Then UpdateJackpot1
				If b3BallMultiMode Then UpdateJackpot2
			End If
		End If
		'handle note lamps for song selection
		If MusicNumber<13 And MusicNumber>0 Then
			If Not b2BallMultiMode And Not b3BallMultiMode Then
				If ActiveBall.VelX<0 Then
					If HurryUpSingleActive=FALSE Then
						CheckCombo(4)
						CheckLeftRampLock
						If LightNote4.State=2 Then
							NoteCount(CurrentPlayer,4)=NoteCount(CurrentPlayer,4)+1
							LightNote4.State=0 'turn it off
							CheckNotes
						End If
					End If
				End If
			End If
		End If
		If ActiveBall.VelX<0 Then
			If HurryUpSingleActive=TRUE And Hurry4.State=2 Then AwardHurryUpSingle'Small HurryUp is active
			If LightMiniWM.State=1 Then 'Mini Wizard Mode is active!  All Hurryups all the time
				If Hurry4.State=2 Then 'if hurryup is lit,
					Hurry4.State=0 'turn off the hurryup light
					LightProgress4.State=0
					AwardMiniHurryUp
					If LightMiniWM.State=1 Then 'Mini Wizard Mode Running
						HurryUpActive=TRUE
						HurrySmall.Enabled=1
						DisplayScore
					End If
				End If
			End If
		End If
	End If
End Sub

Sub CheckLeftRampLock
	LRamps(CurrentPlayer)=LRamps(CurrentPlayer)+1
	If LRamps(CurrentPlayer)>18 Then LRamps(CurrentPlayer)=18
	Select Case LRamps(CurrentPlayer)
		Case 1:LightLock1.State=0:LightLock2.State=0:LightLock3.State=0 'blink left ramp lock 1 light
		Case 2:LightLock1.State=0:LightLock2.State=0:LightLock3.State=0 'blink left ramp lock 1 light
		Case 3:LightLock1.State=0:LightLock2.State=0:LightLock3.State=0 'blink left ramp lock 1 light
		Case 4:LightLock1.State=0:LightLock2.State=0:LightLock3.State=0 'blink left ramp lock 1 light
		Case 5:LightLock1.State=2:LightLock2.State=0:LightLock3.State=0 'blink left ramp lock 1 light
		Case 6:LockDiverter.IsDropped=1:LockDiverter2.IsDropped=0:LeftLockOpened(CurrentPlayer)=1 'open lock access
		Case 7:LightLock1.State=1:LightLock2.State=0:LightLock3.State=0 'blink left ramp lock 1 light
		Case 8:LightLock1.State=1:LightLock2.State=0:LightLock3.State=0 'blink left ramp lock 1 light
		Case 9:LightLock1.State=1:LightLock2.State=0:LightLock3.State=0 'blink left ramp lock 1 light
		Case 10:LightLock1.State=1:LightLock2.State=0:LightLock3.State=0 'blink left ramp lock 1 light
		Case 11:LightLock1.State=1:LightLock2.State=2:LightLock3.State=0 'blink left ramp lock 2 light
		Case 12:LockDiverter.IsDropped=1:LockDiverter2.IsDropped=0:LeftLockOpened(CurrentPlayer)=1 'open lock access
		Case 13:LightLock1.State=1:LightLock2.State=1:LightLock3.State=0 'blink left ramp lock 1 light
		Case 14:LightLock1.State=1:LightLock2.State=1:LightLock3.State=0 'blink left ramp lock 1 light
		Case 15:LightLock1.State=1:LightLock2.State=1:LightLock3.State=0 'blink left ramp lock 1 light
		Case 16:LightLock1.State=1:LightLock2.State=1:LightLock3.State=0 'blink left ramp lock 1 light
		Case 17:LightLock1.State=1:LightLock2.State=1:LightLock3.State=2 'blink left ramp lock 3 light
		Case 18:LockDiverter.IsDropped=1:LockDiverter2.IsDropped=0:LeftLockOpened(CurrentPlayer)=1 'open lock access
	End Select
End Sub
'END OF MODE 4 - RAMPS

'MODE 5 - BAND Drop Targets
Sub Drop1_Hit
	Drop1.IsDropped=1
	PlaySound SoundFXDOF("fx_droptarget",227,DOFPulse,DOFDropTargets)
	If MusicNumber>0 Then 'If ball in play, scoring is ok
		LightBand1.State=1
		If MusicNumber=5 Then'If mode 5 (BAND)
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+75000 ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+75000 ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(75000)
			AddScore 75000 'add it
			'check it for completion
			'This mode completes by scoring 10 drop targets during the mode for a total of 750000 points
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=750000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack5.State=1
				CheckModeComplete
			End If
		Else
			If MusicNumber=12 Then
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+25000 ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+25000 ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(25000)
				AddScore 25000
				'This mode completes by achieving 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack12.State=1
					CheckModeComplete
				End If
			Else
				'standard scoring or wizard mode scoring
				AddScore 20000
			End If
		End If
	End If
	CheckDrops
End Sub

Sub Drop2_Hit
	Drop2.IsDropped=1
	PlaySound SoundFXDOF("fx_droptarget",227,DOFPulse,DOFDropTargets)
	If MusicNumber>0 Then 'If ball in play, scoring is ok
		LightBand2.State=1
		If MusicNumber=5 Then'If mode 5 (BAND)
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+75000 ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+75000 ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(75000)
			AddScore 75000 'add it
			'check it for completion
			'This mode completes by scoring 10 drop targets during the mode for a total of 750000 points
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=750000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack5.State=1
				CheckModeComplete
			End If
		Else
			If MusicNumber=12 Then
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+25000 ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+25000 ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(25000)
				AddScore 25000
				'This mode completes by achieving 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack12.State=1
					CheckModeComplete
				End If
			Else
				'standard scoring or wizard mode scoring
				AddScore 20000
			End If
		End If
	End If
	CheckDrops
End Sub

Sub Drop3_Hit
	Drop3.IsDropped=1
	PlaySound SoundFXDOF("fx_droptarget",227,DOFPulse,DOFDropTargets)
	If MusicNumber>0 Then 'If ball in play, scoring is ok
		LightBand3.State=1
		If MusicNumber=5 Then'If mode 5 (BAND)
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+75000 ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+75000 ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(75000)
			AddScore 75000 'add it
			'check it for completion
			'This mode completes by scoring 10 drop targets during the mode for a total of 750000 points
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=750000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack5.State=1
				CheckModeComplete
			End If
		Else
			If MusicNumber=12 Then
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+25000 ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+25000 ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(25000)
				AddScore 25000
				'This mode completes by achieving 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack12.State=1
					CheckModeComplete
				End If
			Else
				'standard scoring or wizard mode scoring
				AddScore 20000
			End If
		End If
	End If
	CheckDrops
End Sub

Sub Drop4_Hit
	Drop4.IsDropped=1
	PlaySound SoundFXDOF("fx_droptarget",227,DOFPulse,DOFDropTargets)
	If MusicNumber>0 Then 'If ball in play, scoring is ok
		LightBand4.State=1
		If MusicNumber=5 Then'If mode 5 (BAND)
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+75000 ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+75000 ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(75000)
			AddScore 75000 'add it
			'check it for completion
			'This mode completes by scoring 10 drop targets during the mode for a total of 750000 points
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=750000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack5.State=1
				CheckModeComplete
			End If
		Else
			If MusicNumber=12 Then
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+25000 ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+25000 ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(25000)
				AddScore 25000
				'This mode completes by achieving 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack12.State=1
					CheckModeComplete
				End If
			Else
				'standard scoring or wizard mode scoring
				AddScore 20000
			End If
		End If
	End If
	CheckDrops
End Sub

Sub CheckDrops
	RandomAwardShotsRequired(CurrentPlayer)=RandomAwardShotsRequired(CurrentPlayer)-1
	If RandomAwardShotsRequired(CurrentPlayer)<0 Then RandomAwardShotsRequired(CurrentPlayer)=0
	If RandomAwardShotsRequired(CurrentPlayer)=0 Then
		If Not b2BallMultiMode And Not b3BallMultiMode And Not b4BallMultiMode Then
			RandomAwardAvailable(CurrentPlayer)=1
			If MusicNumber<>11 Then RandomAward.State=2 'only start blinking the random award lamp if we're not in follow me mode
		End If
	End If
	If Drop1.IsDropped And Drop2.IsDropped And Drop3.IsDropped And Drop4.IsDropped Then
		PlaySound SoundFXDOF("fx_solenoid",228,DOFPulse,DOFContactors):Drop1.IsDropped=0:Drop2.IsDropped=0:Drop3.IsDropped=0:Drop4.Isdropped=0
		If MusicNumber=5 Then
			LightBand1.State=2:LightBand2.State=2:LightBand3.State=2:LightBand4.State=2
		Else
			LightBand1.State=0:LightBand2.State=0:LightBand3.State=0:LightBand4.State=0
		End If
	End If
End Sub
'END OF MODE 5 - BAND Drop Targets

'MODE 6 - STAND Standup Targets
Sub RStand1_Hit
PlaySound SoundFXDOF("fx_target",223,DOFPulse,DOFTargets)
'animate
RStand1.IsDropped = 1: RStand1.TimerEnabled = 1
	If MusicNumber>0 Then 'If ball in play, scoring is ok
		Dim PAD,NHit
		NHit=0
		If LightRStand1.State<>1 Then
			LightRStand1.State=1:NHit=1
		End If
		If MusicNumber=6 Then'If mode 6 (STAND)
			If gsGameDifficulty<2 Then
				PAD=(INT(RND*10001)+5000)*10 'generates number from 1 to 10000 + 5000 * 10 for a range of 50000-150000
			Else
				PAD=50000 'tournament score during mode
			End If
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PAD ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PAD ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PAD)
			AddScore PAD 'add it
			'check it for completion
			'This mode completes by scoring 1000000 points
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack6.State=1
				CheckModeComplete
			End If
		Else
			'standard scoring or wizard mode scoring
			If gsGameDifficulty<2 Then
				PAD=(INT(RND*1501)+2500)*10 'generates number from 1 to 1500 + 2500 * 10 for a range of 25000-45000
			Else
				PAD=25000 'tournament score during normal play
			End If
			If MusicNumber=12 Then
				Dim PADTemp
				PADTemp=Round(PAD*1.25)'kill any decimals
				PADTemp=INT(PADTEMP/10)*10 'ensure score ends in a value of 10
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PADTemp ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PADTemp ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PADTemp)
				AddScore PADTemp
				'This mode completes by achieving 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack12.State=1
					CheckModeComplete
				End If
			Else
				'standard scoring or wizard mode scoring
				AddScore PAD
			End If
		End If
		CheckStands(NHit)
	End If
End Sub

Sub RStand1_Timer:RStand1.IsDropped = 0: RStand1.TimerEnabled = 0:End Sub

Sub RStand2_Hit
PlaySound SoundFXDOF("fx_target",223,DOFPulse,DOFTargets)
'animate
RStand2.IsDropped = 1: RStand2.TimerEnabled = 1
	If MusicNumber>0 Then 'If ball in play, scoring is ok
		Dim PAD,NHit
		NHit=0
		If LightRStand2.State<>1 Then
			LightRStand2.State=1:NHit=1
		End If
		If MusicNumber=6 Then'If mode 6 (STAND)
			If gsGameDifficulty<2 Then
				PAD=(INT(RND*10001)+5000)*10 'generates number from 1 to 10000 + 5000 * 10 for a range of 50000-150000
			Else
				PAD=50000 'tournament score during mode
			End If
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PAD ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PAD ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PAD)
			AddScore PAD 'add it
			'check it for completion
			'This mode completes by scoring 1000000 points
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack6.State=1
				CheckModeComplete
			End If
		Else
			'standard scoring or wizard mode scoring
			If gsGameDifficulty<2 Then
				PAD=(INT(RND*1501)+2500)*10 'generates number from 1 to 1500 + 2500 * 10 for a range of 25000-45000
			Else
				PAD=25000 'tournament score during normal play
			End If
			If MusicNumber=12 Then
				Dim PADTemp
				PADTemp=Round(PAD*1.25)'kill any decimals
				PADTemp=INT(PADTEMP/10)*10 'ensure score ends in a value of 10
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PADTemp ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PADTemp ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PADTemp)
				AddScore PADTemp
				'This mode completes by achieving 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack12.State=1
					CheckModeComplete
				End If
			Else
				'standard scoring or wizard mode scoring
				AddScore PAD
			End If
		End If
		CheckStands(NHit)
	End If
End Sub

Sub RStand2_Timer:RStand2.IsDropped = 0: RStand2.TimerEnabled = 0:End Sub

Sub RStand3_Hit
PlaySound SoundFXDOF("fx_target",223,DOFPulse,DOFTargets)
'animate
RStand3.IsDropped = 1: RStand3.TimerEnabled = 1
	If MusicNumber>0 Then 'If ball in play, scoring is ok
		Dim PAD,NHit
		NHit=0
		If LightRStand3.State<>1 Then
			LightRStand3.State=1:NHit=1
		End If
		If MusicNumber=6 Then'If mode 6 (STAND)
			If gsGameDifficulty<2 Then
				PAD=(INT(RND*10001)+5000)*10 'generates number from 1 to 10000 + 5000 * 10 for a range of 50000-150000
			Else
				PAD=50000 'tournament score during mode
			End If
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PAD ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PAD ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PAD)
			AddScore PAD 'add it
			'check it for completion
			'This mode completes by scoring 1000000 points
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack6.State=1
				CheckModeComplete
			End If
		Else
			'standard scoring or wizard mode scoring
			If gsGameDifficulty<2 Then
				PAD=(INT(RND*1501)+2500)*10 'generates number from 1 to 1500 + 2500 * 10 for a range of 25000-45000
			Else
				PAD=25000 'tournament score during normal play
			End If
			If MusicNumber=12 Then
				Dim PADTemp
				PADTemp=Round(PAD*1.25)'kill any decimals
				PADTemp=INT(PADTEMP/10)*10 'ensure score ends in a value of 10
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PADTemp ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PADTemp ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PADTemp)
				AddScore PADTemp
				'This mode completes by achieving 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack12.State=1
					CheckModeComplete
				End If
			Else
				'standard scoring or wizard mode scoring
				AddScore PAD
			End If
		End If
		CheckStands(NHit)
	End If
End Sub

Sub RStand3_Timer:RStand3.IsDropped = 0: RStand3.TimerEnabled = 0:End Sub

Sub CheckStands(NHit)
	If NHit=1 Then
		RandomAwardShotsRequired(CurrentPlayer)=RandomAwardShotsRequired(CurrentPlayer)-1
		If RandomAwardShotsRequired(CurrentPlayer)<0 Then RandomAwardShotsRequired(CurrentPlayer)=0
		If RandomAwardShotsRequired(CurrentPlayer)=0 Then
			If Not b2BallMultiMode And Not b3BallMultiMode And Not b4BallMultiMode Then
				RandomAwardAvailable(CurrentPlayer)=1
				If MusicNumber<>11 Then RandomAward.State=2 'only start blinking the random award lamp if we're not in follow me mode
			End If
		End If
	End If
	If LightRStand1.State=1 And LightRStand2.State=1 And LightRStand3.State=1 Then
		If MusicNumber=6 Then
			LightRStand1.State=2:LightRStand2.State=2:LightRStand3.State=2
		Else
			LightRStand1.State=0:LightRStand2.State=0:LightRStand3.State=0
		End If

	End If
End Sub
'END OF MODE 6 - STAND Standup Targets

'MODE 7 - ORBITS
Sub LeftOrbit_Hit
	DOF 233, DOFPulse
	If bTilted=FALSE Then
		If SkillShotActive=TRUE Then
			If Skill3SignOff.TimerEnabled Then Skill3SignOff.TimerEnabled=0 'disable blinking skill shot sign
			If Not Skill3SignOn.IsDropped Then Skill3SignOn.IsDropped=1
			If Skill3SignOff.IsDropped Then Skill3SignOff.IsDropped=0 'turn off the skill shot sign
		End If
		Dim PAD
		PAD=2010'ensure some points are awarded regardless, if the machine isn't tilted
		If MusicNumber=7 Then'If mode 7 (Orbits)
			If LastSwitchHit=3 Then
				PAD=70000
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PAD ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PAD ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PAD)
				AddScore PAD 'add it
				'check it for completion
				'This mode completes by scoring 10 orbits
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=700000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack7.State=1
					CheckModeComplete
				End If
			End If
		End If
		If MusicNumber=11 Then 'Follow Me Mode
			If LightProgress2.State=2 Then
				If ActiveBall.VelY<0 Then
					FollowMeCounter(CurrentPlayer)=FollowMeCounter(CurrentPlayer)+1 'increment counter by 1
					MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+100000 ' track it for total
					MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+100000 ' track it for completion
					If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(100000)
					AddScore 100000 'add it
					'check it for completion
					'This mode completes by scoring 7 lit shots
					If FollowMeCounter(CurrentPlayer)=7 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
						PlaySound"AModeComplete"
						DOF 240, DOFPulse
						MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
						MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
						LightTrack11.State=1
						CheckModeComplete
					End If
				End If
			Else 'light is not lit, so handle standard scoring routines
				AddScore PAD
			End If
		Else
			If MusicNumber<>7 Then
				'standard scoring or wizard mode scoring
				If LastSwitchHit=3 Then PAD=20000 'standard score for a completed right to left orbit shot
				If MusicNumber=12 Then
					Dim PADTemp
					If LastSwitchHit=3 Then
						PADTemp=25000
					Else
						PADTemp=2510
					End If
					MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PADTemp ' track it for total
					MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PADTemp ' track it for completion
					If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PADTemp)
					AddScore PADTemp
					'This mode completes by achieving 1000000 points
					If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
						PlaySound"AModeComplete"
						DOF 240, DOFPulse
						MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
						MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
						LightTrack12.State=1
						CheckModeComplete
					End If
				Else
					AddScore PAD
				End If
			End If
		End If
		'handle note lamps for song selection
		If MusicNumber<13 And MusicNumber>0 Then
			If Not b2BallMultiMode And Not b3BallMultiMode Then
				If ActiveBall.VelY<0 Then
					If HurryUpSingleActive=FALSE Then
						Loops(CurrentPlayer)=Loops(CurrentPlayer)+1
						If Loops(CurrentPlayer)=12 Then LightMB1.State=2
						If Loops(CurrentPlayer)=24 Then LightMB2.State=2
						If LightMB1.State=2 Or LightMB2.State=2 Then
							CreateNewAutoBall(100):TimerPlungerFire.Enabled=1
							EndMusic
							b2BallMultiMode=TRUE 'enable two ball multiball
							DOF 235, DOFPulse
							Skill1.State=0:TrapDoorWall.IsDropped=0:TrapDoorRamp.Collidable=1:TrapDoorStop.IsDropped=1
							If LightMB1.State=2 Then
								DisplayText"BEGIN BOOTLEG ",1,1
								DisplayText"  MULTI-BALL  ",2,1
								DMD_DisplaySceneTextWithPause "BEGIN BOOTLEG", "MULTI-BALL", 4000
							Else
								DisplayText"  THEATRICAL  ",1,1
								DisplayText" PERFORMANCE  ",2,1
								DMD_DisplaySceneTextWithPause "THEATRICAL", "PERFORMANCE", 4000
							End If
							SaveLampStatesSmall
							BallSaved=TRUE:bBallSaverActive=FALSE:Timer_BallSave.Enabled=0 'disable standard ballsaver
							RandomAward.State=0
							MultiballSave.Enabled=1
							LightShootAgain.State=2
							Table1_MusicDone
							If LightMB1.State=2 Then:OldJP1=0:NewJP1=0:End If
							If LightMB2.State=2 Then:OldJP1=0:OldJP2=0:NewJP1=0:NewJP2=0:End If
							'combos aren't valid during multiball for 'combos mode' so turn off the lamps, however, combos do count (once, for this current shot) if a combo started the multiball mode
							LightProgress1.State=0:LightProgress2.State=0:LightProgress3.State=0:LightProgress4.State=0:LightProgress5.State=0:LightProgress6.State=0:LightProgress7.State=0
							LightProgress1.TimerEnabled=0:LightProgress2.TimerEnabled=0:LightProgress3.TimerEnabled=0:LightProgress4.TimerEnabled=0:LightProgress5.TimerEnabled=0:LightProgress6.TimerEnabled=0:LightProgress7.TimerEnabled=0
							LightJackpot1.State=0:LightJackpot2.State=0:LightJackpot3.State=0:LightJackpot4.State=0:LightJackpot5.State=0:LightJackpot6.State=0:LightJackpot7.State=0
							LightJackpot1.State=1:LightJackpot2.State=1:LightJackpot3.State=1:LightJackpot4.State=1:LightJackpot5.State=1:LightJackpot6.State=1:LightJackpot7.State=1
							LightJackpot1.State=0:LightJackpot2.State=0:LightJackpot3.State=0:LightJackpot4.State=0:LightJackpot5.State=0:LightJackpot6.State=0:LightJackpot7.State=0
							If MusicNumber<>9 Then
								FixLamps
							Else
								For X=1 To 7:TWA(X)=0:Next
							End If
							GrabNewJackpots
						End If
						CheckCombo(2)
						If b2BallMultiMode=FALSE Then
							If LightNote2.State=2 Then
								NoteCount(CurrentPlayer,2)=NoteCount(CurrentPlayer,2)+1
								LightNote2.State=0 'turn it off
								CheckNotes
							End If
						End If
					End If
				End If
			Else
				If ActiveBall.VelY<0 Then
					If LightJackpot2.State=2 Then
						'A MULTIBALL IS IN PROGRESS, DETERMINE WHICH ONE AND AWARD PROPER SCORING
						If b2BallMultiMode Then UpdateJackpot1
						If b3BallMultiMode Then UpdateJackpot2
					End If
				End If
			End If
		End If
		LastSwitchHit=4 'LeftOrbit
	End If
End Sub

Sub FixLamps
	For X=1 To 7:TWA(X)=0:Next
	Select Case MusicNumber
		Case 1:TWA(2)=2 'spinner
		Case 2:
		Case 3:
		Case 4:TWA(4)=2:TWA(6)=2 'ramps
		Case 5:
		Case 6:
		Case 7:TWA(2)=2:TWA(7)=2 'orbits
		Case 8:TWA(3)=2:TWA(5)=2 'loops
		Case 9:'For X=1 To 7:TWA(X)=2:Next 'combos are ignored for fixlamps routine
		Case 10:
		Case 11:
		Case 12:
	End Select
	For X=1 To 7
		If TWA(X)=2 Then ProgressLamps(X).State=2
	Next
End Sub

Sub UpdateJackpot1
	AddScore gsJackpot 'Award Jackpot Value
	If LightMB1.State=2 Then AddJackpot(20000) 'Increase Jackpot Value by 20000
	If LightMB2.State=2 Then AddJackpot(30000) 'Increase Jackpot Value by 30000
	GrabNewJackpots
	PlaySound"YCheer"
End Sub

Sub UpdateJackpot2
	AddScore gsJackpot 'Award Jackpot Value
	If LastLockHit(CurrentPlayer)="TOP" Then
		'half of jackpots are lit for this multiball and alternate when one is hit 4/7 split
		If LightJackpot1.State=2 Then
			LightJackpot1.State=0:LightJackpot2.State=0:LightJackpot3.State=0:LightJackpot4.State=0:LightJackpot5.State=2:LightJackpot6.State=2:LightJackpot7.State=2
		Else
			LightJackpot1.State=2:LightJackpot2.State=2:LightJackpot3.State=2:LightJackpot4.State=2:LightJackpot5.State=0:LightJackpot6.State=0:LightJackpot7.State=0
		End If
	Else
		'half of jackpots are lit for this multiball and alternate when one is hit even/odd split
		If LightJackpot1.State=2 Then
			LightJackpot1.State=0:LightJackpot2.State=2:LightJackpot3.State=0:LightJackpot4.State=2:LightJackpot5.State=0:LightJackpot6.State=2:LightJackpot7.State=0
		Else
			LightJackpot1.State=2:LightJackpot2.State=0:LightJackpot3.State=2:LightJackpot4.State=0:LightJackpot5.State=2:LightJackpot6.State=0:LightJackpot7.State=2
		End If
	End If
	If LightMB3.State=2 Then AddJackpot(40000) 'Increase Jackpot Value by 40000
	If LightMB4.State=2 Then AddJackpot(50000) 'Increase Jackpot Value by 50000
	PlaySound"YCheer"
End Sub

Sub SaveLampStatesSmall
	LightProgress1.TimerEnabled=0:LightProgress2.TimerEnabled=0:LightProgress3.TimerEnabled=0:LightProgress4.TimerEnabled=0:LightProgress5.TimerEnabled=0:LightProgress6.TimerEnabled=0:LightProgress7.TimerEnabled=0
	For X=1 To 14:TWA(X)=0:Next
	Select Case MusicNumber
		Case 1:TWA(2)=2 'spinner
		Case 2:
		Case 3:
		Case 4:TWA(4)=2:TWA(6)=2 'ramps
		Case 5:
		Case 6:
		Case 7:TWA(2)=2:TWA(7)=2 'orbits
		Case 8:TWA(3)=2:TWA(5)=2 'loops
		Case 9:For X=1 To 7:TWA(X)=2:Next 'combos
		Case 10:
		Case 11:
		Case 12:
	End Select
	'If MusicNumber=11 'saved within followme_mode
	'Save Note States
	TWA(8)=LightNote1.State
	TWA(9)=LightNote2.State
	TWA(10)=LightNote3.State
	TWA(11)=LightNote4.State
	TWA(12)=LightNote5.State
	TWA(13)=LightNote6.State
	TWA(14)=LightNote7.State
	LightNote1.State=0:LightNote2.State=0:LightNote3.State=0:LightNote4.State=0:LightNote5.State=0:LightNote6.State=0:LightNote7.State=0
End Sub

Sub RestoreLampStatesSmall
	For X=1 To 7:JackpotLamps(X).State=0:ProgressLamps(X).State=0:ProgressLamps(X).State=TWA(X):Next
	LightProgress1.TimerEnabled=0:LightProgress2.TimerEnabled=0:LightProgress3.TimerEnabled=0:LightProgress4.TimerEnabled=0:LightProgress5.TimerEnabled=0:LightProgress6.TimerEnabled=0:LightProgress7.TimerEnabled=0
	LightNote1.State=TWA(8)
	LightNote2.State=TWA(9)
	LightNote3.State=TWA(10)
	LightNote4.State=TWA(11)
	LightNote5.State=TWA(12)
	LightNote6.State=TWA(13)
	LightNote7.State=TWA(14)
End Sub

Sub GrabNewJackpots
	If LightMB1.State=2 Then
		'light 1 shot for jackpot at a time
		Do Until NewJP1<>OldJP1
			NewJP1=INT(RND*7)+1 'generate random number from 1-7
		Loop
		If OldJP1>0 Then
			JackpotLamps(OldJP1).State=0':ProgressLamps(OldJP1).State=0:ProgressLamps(OldJP1).State=TWA(OldJP1)
			'ProgressLamps(NewJP1).State=0:
			JackpotLamps(NewJP1).State=0:JackpotLamps(NewJP1).State=2
		Else
			'ProgressLamps(NewJP1).State=0
			JackpotLamps(NewJP1).State=0:JackpotLamps(NewJP1).State=2
		End If
		OldJP1=NewJP1
		JackpotLamps(NewJP1).State=2
	End If
	If LightMB2.State=2 Then
		'light 2 shots for jackpot at a time
		Do Until NewJP1<>OldJP1 And NewJP1<>OldJP2 And NewJP2<>OldJP1 And NewJP2<>OldJP2 And NewJP2<>NewJP1 'do until nothing is the same as what was previously
			NewJP1=INT(RND*7)+1 'generate random number from 1-7
			NewJP2=INT(RND*7)+1 'generate random number from 1-7
		Loop
		If OldJP1>0 Then
			JackpotLamps(OldJP1).State=0':ProgressLamps(OldJP1).State=0:ProgressLamps(OldJP1).State=TWA(OldJP1)
			JackpotLamps(OldJP2).State=0':ProgressLamps(OldJP2).State=0:ProgressLamps(OldJP2).State=TWA(OldJP2)
			'ProgressLamps(NewJP1).State=0:
			JackpotLamps(NewJP1).State=0:JackpotLamps(NewJP1).State=2
			'ProgressLamps(NewJP2).State=0:
			JackpotLamps(NewJP2).State=0:JackpotLamps(NewJP2).State=2
		Else
			'ProgressLamps(NewJP1).State=0:
			JackpotLamps(NewJP1).State=0:JackpotLamps(NewJP1).State=2
			'ProgressLamps(NewJP2).State=0:
			JackpotLamps(NewJP2).State=0:JackpotLamps(NewJP2).State=2
		End If
		OldJP1=NewJP1
		OldJP2=NewJP2
		JackpotLamps(NewJP1).State=2
		JackpotLamps(NewJP2).State=2
	End If
	If MusicNumber=11 Then
		For X=1 To 7
			If JackpotLamps(X).State<>2 Then
				JackpotLamps(X).State=0
				'ProgressLamps(X).State=0
				'ProgressLamps(X).State=TWA(X)
			End If
		Next
	End If
End Sub

Sub RightOrbit_Hit
	DOF 234, DOFPulse
	If bTilted=FALSE Then
		If SkillShotActive=TRUE Then
			If Drop4.IsDropped Then:Drop4.IsDropped=0:PlaySound"fx_solenoid":End If 'raise the drop target and make a sound only if it is down
			Skill2.State=0 'Turn off skill shot 2 lamp
			Skill3SignOff.TimerEnabled=0 'disable blinking skill shot sign
			Skill3SignOn.IsDropped=1
			Skill3SignOff.IsDropped=0 'turn off the skill shot sign
		End If
		Dim PAD
		PAD=2010'ensure some points are awarded regardless, if the machine isn't tilted
		If MusicNumber=7 Then'If mode 7 (Orbits)
			If LastSwitchHit=4 Then
				PAD=70000
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PAD ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PAD ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PAD)
				AddScore PAD 'add it
				'check it for completion
				'This mode completes by scoring 10 orbits
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=700000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack7.State=1
					CheckModeComplete
				End If
			End If
		End If
		If MusicNumber=11 Then 'Follow Me Mode
			If LightProgress7.State=2 Then
				If ActiveBall.VelY<0 Then
					FollowMeCounter(CurrentPlayer)=FollowMeCounter(CurrentPlayer)+1 'increment counter by 1
					MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+100000 ' track it for total
					MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+100000 ' track it for completion
					If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(100000)
					AddScore 100000 'add it
					'check it for completion
					'This mode completes by scoring 7 lit shots
					If FollowMeCounter(CurrentPlayer)=7 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
						PlaySound"AModeComplete"
						DOF 240, DOFPulse
						MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
						MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
						LightTrack11.State=1
						CheckModeComplete
					End If
				End If
			Else 'light is not lit, so handle standard scoring routines
				AddScore PAD
			End If
		Else
			If MusicNumber<>7 Then
				'standard scoring or wizard mode scoring
				If LastSwitchHit=4 Then PAD=10000 'standard score for a completed left orbit shot
				If MusicNumber=12 Then
					Dim PADTemp
					If LastSwitchHit=4 Then
						PADTemp=12500
					Else
						PADTemp=2510
					End If
					MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PADTemp ' track it for total
					MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PADTemp ' track it for completion
					If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PADTemp)
					AddScore PADTemp
					'This mode completes by achieving 1000000 points
					If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
						PlaySound"AModeComplete"
						DOF 240, DOFPulse
						MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
						MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
						LightTrack12.State=1
						CheckModeComplete
					End If
				Else
					AddScore PAD
				End If
			End If
		End If
		'handle note lamps for song selection
		If MusicNumber<13 And MusicNumber>0 Then
			If Not b2BallMultiMode And Not b3BallMultiMode And Not b4BallMultiMode Then
				If ActiveBall.VelY<0 Then
					If HurryUpSingleActive=FALSE Then
						Loops(CurrentPlayer)=Loops(CurrentPlayer)+1
						If Loops(CurrentPlayer)=12 Then LightMB1.State=2
						If Loops(CurrentPlayer)=24 Then LightMB2.State=2
						If LightMB1.State=2 Or LightMB2.State=2 Then
							CreateNewAutoBall(100):TimerPlungerFire.Enabled=1
							EndMusic
							b2BallMultiMode=TRUE 'enable two ball multiball
							DOF 235, DOFPulse
							Skill1.State=0:TrapDoorWall.IsDropped=0:TrapDoorRamp.Collidable=1:TrapDoorStop.IsDropped=1
							If LightMB1.State=2 Then
								DisplayText"BEGIN BOOTLEG ",1,1
								DisplayText"  MULTI-BALL  ",2,1
								DMD_DisplaySceneTextWithPause "BEGIN BOOTLEG", "MULTI-BALL", 4000
							Else
								DisplayText"  THEATRICAL  ",1,1
								DisplayText" PERFORMANCE  ",2,1
								DMD_DisplaySceneTextWithPause "THEATRICAL", "PERFORMANCE", 4000
							End If
							SaveLampStatesSmall
							BallSaved=TRUE:bBallSaverActive=FALSE:Timer_BallSave.Enabled=0 'disable standard ballsaver
							RandomAward.State=0
							MultiballSave.Enabled=1
							LightShootAgain.State=2
							Table1_MusicDone
							If LightMB1.State=2 Then:OldJP1=0:NewJP1=0:End If
							If LightMB2.State=2 Then:OldJP1=0:OldJP2=0:NewJP1=0:NewJP2=0:End If
							'combos aren't valid during multiball for 'combos mode' so turn off the lamps, however, combos do count (once, for this current shot) if a combo started the multiball mode
							LightProgress1.State=0:LightProgress2.State=0:LightProgress3.State=0:LightProgress4.State=0:LightProgress5.State=0:LightProgress6.State=0:LightProgress7.State=0
							LightProgress1.TimerEnabled=0:LightProgress2.TimerEnabled=0:LightProgress3.TimerEnabled=0:LightProgress4.TimerEnabled=0:LightProgress5.TimerEnabled=0:LightProgress6.TimerEnabled=0:LightProgress7.TimerEnabled=0
							LightJackpot1.State=1:LightJackpot2.State=1:LightJackpot3.State=1:LightJackpot4.State=1:LightJackpot5.State=1:LightJackpot6.State=1:LightJackpot7.State=1
							LightJackpot1.State=0:LightJackpot2.State=0:LightJackpot3.State=0:LightJackpot4.State=0:LightJackpot5.State=0:LightJackpot6.State=0:LightJackpot7.State=0
							If MusicNumber<>9 Then
								FixLamps
							Else
								For X=1 To 7:TWA(X)=0:Next
							End If
							GrabNewJackpots
						End If
						CheckCombo(7)
						If LightNote7.State=2 Then
							NoteCount(CurrentPlayer,7)=NoteCount(CurrentPlayer,7)+1
							LightNote7.State=0 'turn it off
							CheckNotes
						End If
					End If
				End If
			Else
				If ActiveBall.VelY<0 Then
					If LightJackpot7.State=2 Then
						'A MULTIBALL IS IN PROGRESS, DETERMINE WHICH ONE AND AWARD PROPER SCORING
						If b2BallMultiMode Then UpdateJackpot1
						If b3BallMultiMode Then UpdateJackpot2
					End If
				End If
			End If
		End If
		LastSwitchHit=3 'RightOrbit
		If ActiveBall.VelY<0 Then
			If HurryUpSingleActive=TRUE And Hurry7.State=2 Then AwardHurryUpSingle'Small HurryUp is active
			If LightMiniWM.State=1 Then 'Mini Wizard Mode is active!  All Hurryups all the time
				If Hurry7.State=2 Then 'if hurryup is lit,
					Hurry7.State=0 'turn off the hurryup light
					LightProgress7.State=0
					AwardMiniHurryUp
					If LightMiniWM.State=1 Then 'Mini Wizard Mode Running
						HurryUpActive=TRUE
						HurrySmall.Enabled=1
						DisplayScore
					End If
				End If
			End If
		End If
	End If
End Sub
'END OF MODE 7 - ORBITS

'MODE 8 - LOOPS
Sub LeftMiniLoop_Hit
	If bTilted=FALSE Then
		Dim PAD
		PAD=1010'ensure some points are awarded regardless, if the machine isn't tilted
		If MusicNumber=8 Then'If mode 8 (Loops)
			If LastSwitchHit=8 Then
				PAD=140000
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PAD ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PAD ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PAD)
				AddScore PAD 'add it
				'check it for completion
				'This mode completes by scoring 5 mini loops
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=700000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack8.State=1
					CheckModeComplete
				End If
			End If
		End If
		If MusicNumber=11 Then 'Follow Me Mode
			If LightProgress3.State=2 Then
				If ActiveBall.VelY<0 Then
					FollowMeCounter(CurrentPlayer)=FollowMeCounter(CurrentPlayer)+1 'increment counter by 1
					MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+100000 ' track it for total
					MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+100000 ' track it for completion
					If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(100000)
					AddScore 100000 'add it
					'check it for completion
					'This mode completes by scoring 7 lit shots
					If FollowMeCounter(CurrentPlayer)=7 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
						PlaySound"AModeComplete"
						DOF 240, DOFPulse
						MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
						MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
						LightTrack11.State=1
						CheckModeComplete
					End If
				End If
			Else 'light is not lit, so handle standard scoring routines
				AddScore PAD
			End If
		Else
			If MusicNumber<>8 Then
				'standard scoring or wizard mode scoring
				If LastSwitchHit=8 Then PAD=30000 'standard score for a completed left orbit shot
				If MusicNumber=12 Then
					Dim PADTemp
					If LastSwitchHit=8 Then
						PADTemp=37500
					Else
						PADTemp=1260
					End If
					MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PADTemp ' track it for total
					MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PADTemp ' track it for completion
					If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PADTemp)
					AddScore PADTemp
					'This mode completes by achieving 1000000 points
					If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
						PlaySound"AModeComplete"
						DOF 240, DOFPulse
						MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
						MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
						LightTrack12.State=1
						CheckModeComplete
					End If
				Else
					AddScore PAD
				End If
			End If
		End If
		'handle note lamps for song selection
		If MusicNumber<13 And MusicNumber>0 Then
			If Not b2BallMultiMode And Not b3BallMultiMode And Not b4BallMultiMode Then
				If ActiveBall.VelY<0 Then
					If HurryUpSingleActive=FALSE Then
						Loops(CurrentPlayer)=Loops(CurrentPlayer)+1
						If Loops(CurrentPlayer)=12 Then LightMB1.State=2
						If Loops(CurrentPlayer)=24 Then LightMB2.State=2
						If LightMB1.State=2 Or LightMB2.State=2 Then
							CreateNewAutoBall(100):TimerPlungerFire.Enabled=1
							EndMusic
							b2BallMultiMode=TRUE 'enable two ball multiball
							DOF 235, DOFPulse
							Skill1.State=0:TrapDoorWall.IsDropped=0:TrapDoorRamp.Collidable=1:TrapDoorStop.IsDropped=1
							If LightMB1.State=2 Then
								DisplayText"BEGIN BOOTLEG ",1,1
								DisplayText"  MULTI-BALL  ",2,1
								DMD_DisplaySceneTextWithPause "BEGIN BOOTLEG", "MULTI-BALL", 4000
							Else
								DisplayText"  THEATRICAL  ",1,1
								DisplayText" PERFORMANCE  ",2,1
								DMD_DisplaySceneTextWithPause "THEATRICAL", "PERFORMANCE", 4000
							End If
							SaveLampStatesSmall
							BallSaved=TRUE:bBallSaverActive=FALSE:Timer_BallSave.Enabled=0 'disable standard ballsaver
							RandomAward.State=0
							MultiballSave.Enabled=1
							LightShootAgain.State=2
							Table1_MusicDone
							If LightMB1.State=2 Then:OldJP1=0:NewJP1=0:End If
							If LightMB2.State=2 Then:OldJP1=0:OldJP2=0:NewJP1=0:NewJP2=0:End If
							'combos aren't valid during multiball for 'combos mode' so turn off the lamps, however, combos do count (once, for this current shot) if a combo started the multiball mode
							LightProgress1.State=0:LightProgress2.State=0:LightProgress3.State=0:LightProgress4.State=0:LightProgress5.State=0:LightProgress6.State=0:LightProgress7.State=0
							LightProgress1.TimerEnabled=0:LightProgress2.TimerEnabled=0:LightProgress3.TimerEnabled=0:LightProgress4.TimerEnabled=0:LightProgress5.TimerEnabled=0:LightProgress6.TimerEnabled=0:LightProgress7.TimerEnabled=0
							LightJackpot1.State=1:LightJackpot2.State=1:LightJackpot3.State=1:LightJackpot4.State=1:LightJackpot5.State=1:LightJackpot6.State=1:LightJackpot7.State=1
							LightJackpot1.State=0:LightJackpot2.State=0:LightJackpot3.State=0:LightJackpot4.State=0:LightJackpot5.State=0:LightJackpot6.State=0:LightJackpot7.State=0
							If MusicNumber<>9 Then
								FixLamps
							Else
								For X=1 To 7:TWA(X)=0:Next
							End If
							GrabNewJackpots
						End If
						CheckCombo(3)
						If LightNote3.State=2 Then
							NoteCount(CurrentPlayer,3)=NoteCount(CurrentPlayer,3)+1
							LightNote3.State=0 'turn it off
							CheckNotes
						End If
					End If
				End If
			Else
				If ActiveBall.VelY<0 Then
					If LightJackpot3.State=2 Then
						'A MULTIBALL IS IN PROGRESS, DETERMINE WHICH ONE AND AWARD PROPER SCORING
						If b2BallMultiMode Then UpdateJackpot1
						If b3BallMultiMode Then UpdateJackpot2
					End If
				End If
			End If
		End If
		LastSwitchHit=9 'LeftMiniLoop
	End If
End Sub

Sub RightMiniLoop_Hit
	If bTilted=FALSE Then
		Dim PAD
		PAD=1010'ensure some points are awarded regardless, if the machine isn't tilted
		If MusicNumber=8 Then'If mode 8 (Loops)
			If LastSwitchHit=9 Then
				PAD=140000
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PAD ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PAD ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PAD)
				AddScore PAD 'add it
				'check it for completion
				'This mode completes by scoring 5 mini loops
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=700000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack8.State=1
					CheckModeComplete
				End If
			End If
		End If
		If MusicNumber=11 Then 'Follow Me Mode
			If LightProgress5.State=2 Then
				If ActiveBall.VelY<0 Then
					FollowMeCounter(CurrentPlayer)=FollowMeCounter(CurrentPlayer)+1 'increment counter by 1
					MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+100000 ' track it for total
					MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+100000 ' track it for completion
					If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(100000)
					AddScore 100000 'add it
					'check it for completion
					'This mode completes by scoring 7 lit shots
					If FollowMeCounter(CurrentPlayer)=7 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
						PlaySound"AModeComplete"
						DOF 240, DOFPulse
						MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
						MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
						LightTrack11.State=1
						CheckModeComplete
					End If
				End If
			Else 'light is not lit, so handle standard scoring routines
				AddScore PAD
			End If
		Else
			If MusicNumber<>8 Then
				'standard scoring or wizard mode scoring
				If LastSwitchHit=9 Then PAD=30000 'standard score for a completed left orbit shot
				If MusicNumber=12 Then
					Dim PADTemp
					If LastSwitchHit=9 Then
						PADTemp=37500
					Else
						PADTemp=1260
					End If
					MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PADTemp ' track it for total
					MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PADTemp ' track it for completion
					If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PADTemp)
					AddScore PADTemp
					'This mode completes by achieving 1000000 points
					If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
						PlaySound"AModeComplete"
						DOF 240, DOFPulse
						MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
						MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
						LightTrack12.State=1
						CheckModeComplete
					End If
				Else
					AddScore PAD
				End If
			End If
		End If
		'handle note lamps for song selection
		If MusicNumber<13 And MusicNumber>0 Then
			If Not b2BallMultiMode And Not b3BallMultiMode And Not b4BallMultiMode Then
				If ActiveBall.VelY<0 Then
					If HurryUpSingleActive=FALSE Then
						Loops(CurrentPlayer)=Loops(CurrentPlayer)+1
						If Loops(CurrentPlayer)=12 Then LightMB1.State=2
						If Loops(CurrentPlayer)=24 Then LightMB2.State=2
						If LightMB1.State=2 Or LightMB2.State=2 Then
							CreateNewAutoBall(100):TimerPlungerFire.Enabled=1
							EndMusic
							b2BallMultiMode=TRUE 'enable two ball multiball
							DOF 235, DOFPulse
							Skill1.State=0:TrapDoorWall.IsDropped=0:TrapDoorRamp.Collidable=1:TrapDoorStop.IsDropped=1
							If LightMB1.State=2 Then
								DisplayText"BEGIN BOOTLEG ",1,1
								DisplayText"  MULTI-BALL  ",2,1
								DMD_DisplaySceneTextWithPause "BEGIN BOOTLEG", "MULTI-BALL", 4000
							Else
								DisplayText"  THEATRICAL  ",1,1
								DisplayText" PERFORMANCE  ",2,1
								DMD_DisplaySceneTextWithPause "THEATRICAL", "PERFORMANCE", 4000
							End If
							SaveLampStatesSmall
							BallSaved=TRUE:bBallSaverActive=FALSE:Timer_BallSave.Enabled=0 'disable standard ballsaver
							RandomAward.State=0
							MultiballSave.Enabled=1
							LightShootAgain.State=2
							Table1_MusicDone
							If LightMB1.State=2 Then:OldJP1=0:NewJP1=0:End If
							If LightMB2.State=2 Then:OldJP1=0:OldJP2=0:NewJP1=0:NewJP2=0:End If
							'combos aren't valid during multiball for 'combos mode' so turn off the lamps, however, combos do count (once, for this current shot) if a combo started the multiball mode
							LightProgress1.State=0:LightProgress2.State=0:LightProgress3.State=0:LightProgress4.State=0:LightProgress5.State=0:LightProgress6.State=0:LightProgress7.State=0
							LightProgress1.TimerEnabled=0:LightProgress2.TimerEnabled=0:LightProgress3.TimerEnabled=0:LightProgress4.TimerEnabled=0:LightProgress5.TimerEnabled=0:LightProgress6.TimerEnabled=0:LightProgress7.TimerEnabled=0
							LightJackpot1.State=1:LightJackpot2.State=1:LightJackpot3.State=1:LightJackpot4.State=1:LightJackpot5.State=1:LightJackpot6.State=1:LightJackpot7.State=1
							LightJackpot1.State=0:LightJackpot2.State=0:LightJackpot3.State=0:LightJackpot4.State=0:LightJackpot5.State=0:LightJackpot6.State=0:LightJackpot7.State=0
							If MusicNumber<>9 Then
								FixLamps
							Else
								For X=1 To 7:TWA(X)=0:Next
							End If
							GrabNewJackpots
						End If
						CheckCombo(5)
						If LightNote5.State=2 Then
							NoteCount(CurrentPlayer,5)=NoteCount(CurrentPlayer,5)+1
							LightNote5.State=0 'turn it off
							CheckNotes
						End If
					End If
				End If
			Else
				If ActiveBall.VelY<0 Then
					If LightJackpot5.State=2 Then
						'A MULTIBALL IS IN PROGRESS, DETERMINE WHICH ONE AND AWARD PROPER SCORING
						If b2BallMultiMode Then UpdateJackpot1
						If b3BallMultiMode Then UpdateJackpot2
					End If
				End If
			End If
		End If
		LastSwitchHit=8 'RightMiniLoop
	End If
End Sub
'END OF MODE 8 - LOOPS

'MODE 9 - COMBOS
Sub CheckCombo(LightNum)
	If MusicNumber>0 And MusicNumber<13 Then 'combos don't apply during multiball, tilt, or mini/full wizard modes
		If Not b2BallMultiMode And Not b3BallMultiMode And Not b4BallMultiMode Then
			If ComboTimer.Enabled Then
				If MusicNumber<>11 Then 'Don't touch the lights if 'Follow Me' mode is running
					Select Case LightNum
						Case 1:If LightProgress1.State<>1 Then SP1=LightProgress1.State
								LightProgress1.State=1:LightProgress1.TimerEnabled=0:LightProgress1.TimerEnabled=1
						Case 2:SP2=LightProgress2.State:LightProgress2.State=1:LightProgress2.TimerEnabled=0:LightProgress2.TimerEnabled=1
						Case 3:SP3=LightProgress3.State:LightProgress3.State=1:LightProgress3.TimerEnabled=0:LightProgress3.TimerEnabled=1
						Case 4:SP4=LightProgress4.State:LightProgress4.State=1:LightProgress4.TimerEnabled=0:LightProgress4.TimerEnabled=1
						Case 5:SP5=LightProgress5.State:LightProgress5.State=1:LightProgress5.TimerEnabled=0:LightProgress5.TimerEnabled=1
						Case 6:SP6=LightProgress6.State:LightProgress6.State=1:LightProgress6.TimerEnabled=0:LightProgress6.TimerEnabled=1
						Case 7:SP7=LightProgress7.State:LightProgress7.State=1:LightProgress7.TimerEnabled=0:LightProgress7.TimerEnabled=1
					End Select
				End If
				ComboTimer.Enabled=0
				ComboTimer.Enabled=1 'reset timer for another 5 seconds
				gvCombosThisBall=gvCombosThisBall+1 'increment combo count
				CombosThisGame(CurrentPlayer)=CombosThisGame(CurrentPlayer)+1 'increment total for this player
				If CombosThisGame(CurrentPlayer)>999 Then CombosThisGame(CurrentPlayer)=999
				If MusicNumber=9 Then 'combo mode is active, so all combos count double and add to this mode
					MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+ComboValue*2 ' track it for total
					MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+ComboValue*2 ' track it for completion
					If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(ComboValue*2)
					AddScore ComboValue*2
					'This mode completes by achieving 1000000 points
					If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
						PlaySound"AModeComplete"
						DOF 240, DOFPulse
						MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
						MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
						LightTrack9.State=1
						CheckModeComplete
					End If
				End If
				If MusicNumber=12 Then 'combos are worth 125% during frenzy song mode 12
					MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+ComboValue*1.25 ' track it for total
					MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+ComboValue*1.25 ' track it for completion
					If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(ComboValue*1.25)
					AddScore ComboValue*1.25
					'This mode completes by achieving 1000000 points
					If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
						PlaySound"AModeComplete"
						DOF 240, DOFPulse
						MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
						MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
						LightTrack12.State=1
						CheckModeComplete
					End If
				End If
				Select Case ComboValue 'increment combo value for next possible shot
					Case 25000:ComboValue=50000
					Case 50000:ComboValue=100000
					Case 100000:ComboValue=200000
				End Select
			Else
				ComboTimer.Enabled=1 'Combo was not active, so start the combo timer
			End If
		End If
	End If
End Sub

Sub ComboTimer_Timer 'Combo timer has expired from 'natural causes' or 'old age'
	ComboValue=25000 'reset combo value to lowest level
	ComboTimer.Enabled=0
	If MusicNumber<>11 Then 'Don't touch the lights if 'Follow Me' mode is running
		If Not b2BallMultiMode And Not b3BallMultiMode And Not b4BallMultiMode Then 'don't touch the lights if Multiball is in progress
			If SP1>0 Then LightProgress1_Timer
			If SP2>0 Then LightProgress2_Timer
			If SP3>0 Then LightProgress3_Timer
			If SP4>0 Then LightProgress4_Timer
			If SP5>0 Then LightProgress5_Timer
			If SP6>0 Then LightProgress6_Timer
			If SP7>0 Then LightProgress7_Timer
		End If
	End If
End Sub

Sub LightProgress1_Timer:LightProgress1.State=SP1:LightProgress1.TimerEnabled=0:End Sub
Sub LightProgress2_Timer:LightProgress2.State=SP2:LightProgress2.TimerEnabled=0:End Sub
Sub LightProgress3_Timer:LightProgress3.State=SP3:LightProgress3.TimerEnabled=0:End Sub
Sub LightProgress4_Timer:LightProgress4.State=SP4:LightProgress4.TimerEnabled=0:End Sub
Sub LightProgress5_Timer:LightProgress5.State=SP5:LightProgress5.TimerEnabled=0:End Sub
Sub LightProgress6_Timer:LightProgress6.State=SP6:LightProgress6.TimerEnabled=0:End Sub
Sub LightProgress7_Timer:LightProgress7.State=SP7:LightProgress7.TimerEnabled=0:End Sub
'END OF MODE 9 - COMBOS

'MODE 10 - MINI Yellow Standup Targets
Sub Mini1_Hit
	PlaySound SoundFXDOF("fx_target",224,DOFPulse,DOFTargets)
	If bTilted=FALSE Then
		Dim PAD
		PAD=25000'ensure some points are awarded regardless, if the machine isn't tilted
		LightYStand1.State=1
		If MusicNumber=10 Then'If mode 10 (MINIS)
			PAD=50000
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PAD ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PAD ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PAD)
			AddScore PAD 'add it
			'check it for completion
			'This mode completes by scoring 6 hits during the mode
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=300000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack10.State=1
				CheckModeComplete
			End If
		Else
			If MusicNumber=12 Then
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+31250 ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+31250 ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(31250)
				AddScore 31250
				'This mode completes by achieving 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack12.State=1
					CheckModeComplete
				End If
			Else
				AddScore PAD
			End If
		End If
		CheckMinis
	End If
End Sub

Sub Mini2_Hit
	PlaySound SoundFXDOF("fx_target",225,DOFPulse,DOFTargets)
	If bTilted=FALSE Then
		Dim PAD
		PAD=25000'ensure some points are awarded regardless, if the machine isn't tilted
		LightYStand2.State=1
		If MusicNumber=10 Then'If mode 10 (MINIS)
			PAD=50000
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PAD ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PAD ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PAD)
			AddScore PAD 'add it
			'check it for completion
			'This mode completes by scoring 6 hits during the mode
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=300000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack10.State=1
				CheckModeComplete
			End If
		Else
			If MusicNumber=12 Then
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+31250 ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+31250 ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(31250)
				AddScore 31250
				'This mode completes by achieving 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack12.State=1
					CheckModeComplete
				End If
			Else
				AddScore PAD
			End If
		End If
		CheckMinis
	End If
End Sub

Sub Mini3_Hit
	PlaySound SoundFXDOF("fx_target",225,DOFPulse,DOFTargets)
	If bTilted=FALSE Then
		Dim PAD
		PAD=25000'ensure some points are awarded regardless, if the machine isn't tilted
		LightYStand3.State=1
		If MusicNumber=10 Then'If mode 10 (MINIS)
			PAD=50000
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PAD ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PAD ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PAD)
			AddScore PAD 'add it
			'check it for completion
			'This mode completes by scoring 6 hits during the mode
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=300000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack10.State=1
				CheckModeComplete
			End If
		Else
			If MusicNumber=12 Then
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+31250 ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+31250 ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(31250)
				AddScore 31250
				'This mode completes by achieving 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack12.State=1
					CheckModeComplete
				End If
			Else
				AddScore PAD
			End If
		End If
		CheckMinis
	End If
End Sub

Sub CheckMinis
	If LightYStand1.State=1 And LightYStand2.State=1 And LightYStand3.State=1 Then
		If MusicNumber=10 Then
			LightYStand1.State=2:LightYStand2.State=2:LightYStand3.State=2
		Else
			LightYStand1.State=0:LightYStand2.State=0:LightYStand3.State=0
		End If
		If LightKickback.State=2 Then KickbackSaver.Enabled=0 'stop master timer
		LightKickback.State=1 'ensure the kickback light is always lit when minis are completed
	End If
End Sub
'END OF MODE 10

'MODE 11 = FOLLOW ME - handled with above shots plus saucer shot
Sub Trigger3_Hit
	If bTilted=FALSE Then
		If ActiveBall.VelY<0 Then
			If HurryUpSingleActive=TRUE And Hurry2.State=2 Then AwardHurryUpSingle'Small HurryUp is active
			If LightMiniWM.State=1 Then 'Mini Wizard Mode is active!  All Hurryups all the time
				If Hurry2.State=2 Then 'if hurryup is lit,
					Hurry2.State=0 'turn off the hurryup light
					LightProgress2.State=0
					AwardMiniHurryUp
					If LightMiniWM.State=1 Then 'Mini Wizard Mode Running
						HurryUpActive=TRUE
						HurrySmall.Enabled=1
						DisplayScore
					End If
				End If
			End If
		End If
	End If
End Sub

Sub FollowMe_Timer
	If FollowMeDifficulty(CurrentPlayer)+1=0 Then
		'strafe in sequence
		If FollowMeVal>-1 Then
			FollowMeLights(FollowMeVal).State=0
			If b2BallMultiMode=TRUE Or HurryUpSingleActive=TRUE Then UpdateTinyLights(0)
		End If
		FollowMeVal=FollowMeVal+1
		If FollowMeVal>11 Then FollowMeVal=0
	End If
	If FollowMeDifficulty(CurrentPlayer)+1=1 Then
		'randomize shots
		If FollowMeVal>-1 Then FollowMeLights(FollowMeVal).State=0
		If b2BallMultiMode=TRUE Or HurryUpSingleActive=TRUE Then UpdateTinyLights(0)
		FollowMeVal=INT(RND*8) 'produces random value from 1-7
	End If
	If FollowMeDifficulty(CurrentPlayer)+1>1 Then
		If FollowMeVal>-1 Then FollowMeLights(FollowMeVal).State=0
		If b2BallMultiMode=TRUE Or HurryUpSingleActive=TRUE Then UpdateTinyLights(0)
		FollowMeVal=INT(RND*8) 'produces random value from 1-7
	End If
	FollowMeLights(FollowMeVal).State=2
	If b2BallMultiMode=TRUE Or HurryUpSingleActive=TRUE Then UpdateTinyLights(2)
End Sub

Sub UpdateTinyLights(STE)
	Select Case FollowMeVal
		Case 0:TWA(1)=STE
		Case 1:TWA(2)=STE
		Case 2:TWA(3)=STE
		Case 3:TWA(4)=STE
		Case 4:TWA(5)=STE
		Case 5:TWA(6)=STE
		Case 6:TWA(7)=STE
		Case 7:TWA(6)=STE
		Case 8:TWA(5)=STE
		Case 9:TWA(4)=STE
		Case 10:TWA(3)=STE
		Case 11:TWA(2)=STE
	End Select
	For X=1 To 7
		If JackpotLamps(X).State<>2 Then
			JackpotLamps(X).State=0
			JackpotLamps(X).State=1
			JackpotLamps(X).State=0
			If HurryUpSingleActive=TRUE Then
				ProgressLamps(X).State=TWA(X)
				ProgressLamps(X).State=0
			End If
		End If
	Next
	If HurryUpSingleActive=TRUE Then
		Select Case MiniSingleCurrent
			Case 1:If LightProgress1.State<>2 Then:LightProgress1.State=2:End If
			Case 2:If LightProgress2.State<>2 Then:LightProgress2.State=2:End If
			Case 3:If LightProgress3.State<>2 Then:LightProgress3.State=2:End If
			Case 4:If LightProgress4.State<>2 Then:LightProgress4.State=2:End If
			Case 5:If LightProgress5.State<>2 Then:LightProgress5.State=2:End If
			Case 6:If LightProgress6.State<>2 Then:LightProgress6.State=2:End If
			Case 7:If LightProgress7.State<>2 Then:LightProgress7.State=2:End If
		End Select
	End If
End Sub

Sub Kicker5_Hit
	PlaySound"fx_hole_enter"
	ClearBallID
	Kicker5.DestroyBall
	BallsInSubway=BallsInSubway+1
	If bBallSaverActive=TRUE Then Timer_BallSave.Enabled=0
	If bTilted=FALSE Then
		If MusicNumber=11 Then 'Follow Me Mode
			If LightProgress1.State=2 Then
				FollowMeCounter(CurrentPlayer)=FollowMeCounter(CurrentPlayer)+1 'increment counter by 1
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+100000 ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+100000 ' track it for completion
				If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(100000)
				AddScore 100000 'add it
				'check it for completion
				'This mode completes by scoring 7 lit shots
				If FollowMeCounter(CurrentPlayer)=7 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack11.State=1
					CheckModeComplete
				End If
			End If
			If HurryUpSingleActive=TRUE Then 'Small HurryUp is active
				If Hurry1.State=2 Then
					AwardHurryUpSingle
					CreateBallID Kicker5
					Kicker5.Kick 177,8
					PlaySound SoundFXDOF("fx_Popper",221,DOFPulse,DOFContactors)
					BallsInSubway=BallsInSubway-1
					Exit Sub
				End If
			End If
			If LightJackpot1.State=2 Then
				'A MULTIBALL IS IN PROGRESS, DETERMINE WHICH ONE AND AWARD PROPER SCORING
				If b2BallMultiMode Then UpdateJackpot1
				If b3BallMultiMode Then UpdateJackpot2
			End If
		Else
			'check random awards here during normal play if not in multiball, and not in mini or wizard mode
			If MusicNumber<13 And MusicNumber>0 Then
				If Not b2BallMultiMode And Not b3BallMultiMode Then
					'check random award lamp
					CheckCombo(1)
					ComboTimer.Enabled=0 'disable the combo timer as the ball isn't in play
					ExtendTime=TRUE 'signals to delay starting the combo timer until the ball is back in play
					If RandomAwardAvailable(CurrentPlayer)>0 Then
						RandomAward.State=0 'turn off the light
						RandomAwardAvailable(CurrentPlayer)=0 'no longer available
						RandomAwardShotsRequired(CurrentPlayer)=gsRandomAwardShotsRequiredDefault 'reset the number of shots required
						'run random award routine and set up timer to kick out the ball when random award is done
						If gsGameDifficulty<2 Then
							RandomAwardRoutine
							Exit Sub
						Else
							If MusicNumber=12 Then 'Frenzy Scoring
								MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+50000 ' track it for total
								MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+50000 ' track it for completion
								If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(50000)
								AddScore 50000
								'This mode completes by achieving 1000000 points
								If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
									PlaySound"AModeComplete"
									DOF 240, DOFPulse
									MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
									MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
									LightTrack12.State=1
									CheckModeComplete
								End If
								If HurryUpSingleActive=TRUE Then 'Small HurryUp is active
									If Hurry1.State=2 Then
										AwardHurryUpSingle
										CreateBallID Kicker5
										Kicker5.Kick 177,8
										PlaySound SoundFXDOF("fx_Popper",221,DOFPulse,DOFContactors)
										BallsInSubway=BallsInSubway-1
										ExtendTime=FALSE
										ComboTimer.Enabled=1
										Exit Sub
									End If
								End If
							Else
								AddScore 50000 'tournament difficulty always awards 50,000 points for random award kicker
							End If
						End If
					End If
				Else
					If LightJackpot1.State=2 Then
						'A MULTIBALL IS IN PROGRESS, DETERMINE WHICH ONE AND AWARD PROPER SCORING
						If b2BallMultiMode Then UpdateJackpot1
						If b3BallMultiMode Then UpdateJackpot2
					Else
						AddScore 50000
					End If
				End If
			End If
		End If
		'handle note lamps for song selection
		If MusicNumber<13 And MusicNumber>0 Then
			If Not b2BallMultiMode And Not b3BallMultiMode Then
				If LightNote1.State=2 Then
					NoteCount(CurrentPlayer,1)=NoteCount(CurrentPlayer,1)+1
					LightNote1.State=0 'turn it off
					CheckNotes
				End If
			End If
		End If
		If LightMiniWM.State=1 Then 'Mini Wizard Mode is active!  All Hurryups all the time
			If Hurry1.State=2 Then 'if hurryup is lit,
				Hurry1.State=0 'turn off the hurryup light
				LightProgress1.State=0
				AwardMiniHurryUp
				If LightMiniWM.State=1 Then 'if mini wizard mode was completed, then this light would have been turned off in the checkmodecomplete routine so hang onto the ball and start wizard mode
					SubwayExit.Enabled=1
					RandomAward.State=2 'not tilted, so blink the kickout light
				Else
					'Start Full Wizard Mode Instantly right now
					PXO3=0
					WMStartTimer.Enabled=1
				End If
			Else
				SubwayExit.Enabled=1
				RandomAward.State=2 'not tilted, so blink the kickout light
			End If
		Else
			SubwayExit.Enabled=1
			RandomAward.State=2 'not tilted, so blink the kickout light
		End If
	Else
		SubwayExit.Enabled=1
		RandomAward.state = 2
	End If
End Sub
'END OF MODE 11 = FOLLOW ME - handled with above shots plus saucer shot

'MODE 12 = FRENZY - Scoring=125% during this mode, mode is completed at 1 million points
'Scoring and mode checks are handled in the addscore routine

'EXTRA SCORING SWITCHES
Sub LeftSlingshot_Slingshot
	If bTilted=FALSE Then
		PlaySound SoundFXDOF("fx_slingshot",203,DOFPulse,DOFContactors)
		If LightRightSpecial.State=1 Then
			LightLeftSpecial.State=1
			LightRightSpecial.State=0
		Else
			If LightLeftSpecial.State=1 Then
				LightLeftSpecial.State=0
				LightRightSpecial.State=1
			End If
		End If
		If MusicNumber=12 Then
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+260 ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+260 ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(260)
			AddScore 260
			'This mode completes by achieving 1000000 points
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack12.State=1
				CheckModeComplete
			End If
		Else
			AddScore 210
		End If
	End If
End Sub

Sub RightSlingshot_Slingshot
	If bTilted=FALSE Then
		PlaySound SoundFXDOF("fx_slingshot",204,DOFPulse,DOFContactors)
		If LightRightSpecial.State=1 Then
			LightLeftSpecial.State=1
			LightRightSpecial.State=0
		Else
			If LightLeftSpecial.State=1 Then
				LightLeftSpecial.State=0
				LightRightSpecial.State=1
			End If
		End If
		If MusicNumber=12 Then
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+260 ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+260 ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(260)
			AddScore 260
			'This mode completes by achieving 1000000 points
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack12.State=1
				CheckModeComplete
			End If
		Else
			AddScore 210
		End If
	End If
End Sub

Sub RightOutlane_Hit
	PlaySound"fx_sensor"
	DOF 218, DOFPulse
'	RW5.IsDropped=0
	If bTilted=FALSE Then
		If LightRightSpecial.State=1 Then
			' If free play award a extra ball, Else a free credit
			If gsFreePlay Then
				' award and extra ball to the player
				AwardExtraBall CurrentPlayer,1,TRUE
			Else
				' award a credit
				AwardFreeGame 1
			End If
			LightRightSpecial.State=0
		End If
		If MusicNumber=12 Then
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+62500 ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+62500 ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(62500)
			AddScore 62500
			'This mode completes by achieving 1000000 points
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack12.State=1
				CheckModeComplete
			End If
		Else
			AddScore 50000
		End If
	End If
End Sub
'Sub RightOutlane_unHit:RW5.IsDropped=1:End Sub

Sub LeftOutlane_Hit
	PlaySound"fx_sensor"
	DOF 214, DOFPulse
'	RW1.IsDropped=0
	If bTilted=FALSE Then
		If LightLeftSpecial.State=1 Then
			' If free play award a extra ball, Else a free credit
			If gsFreePlay Then
				' award and extra ball to the player
				AwardExtraBall CurrentPlayer,1,TRUE
			Else
				' award a credit
				AwardFreeGame 1
			End If
			LightLeftSpecial.State=0
		End If
		If MusicNumber=12 Then
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+62500 ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+62500 ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(62500)
			AddScore 62500
			'This mode completes by achieving 1000000 points
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack12.State=1
				CheckModeComplete
			End If
		Else
			AddScore 50000
		End If
		If LightKickback.State=1 Or LightKickback.State=2 Then
			If LightKickback.State=1 Then LightKickback.State=2 'blink the kickback lamp - allow for 3 second grace period between hits where lamp is blinking and still works
			Kickback.Fire
			PlaySound SoundFXDOF("fx_solenoid2",230,DOFPulse,DOFContactors)
			Kickback.TimerEnabled=0 'stop the kickback timer
			Kickback.TimerEnabled=1 'start the kickback timer
			KickbackSaver.Enabled=0 'stop master timer
			KickbackSaver.Enabled=1 'start master timer
		End If
	End If
End Sub
'Sub LeftOutlane_unHit:RW1.IsDropped=1:End Sub

Sub Kickback_Timer
	Kickback.TimerEnabled=0 'disable the kickback timer
	Kickback.Pullback 'reset the kickback plunger
End Sub

Sub KickbackSaver_Timer
	KickbackSaver.Enabled=0 'disable master timer
	LightKickback.State=0 'turn off kickback lamp
End Sub

Sub PointRubber_Hit
	PlaySound"fx_rubber"
	If HurryUpSingleActive=TRUE And HurrySingle.Enabled Then HurrySingle.Enabled=0 'disable single hurryup as the ball is in the bumpers
	If HurryUpActive=TRUE And HurrySmall.Enabled Then HurrySmall.Enabled=0 'disable hurryup as the ball is in the bumpers
	If bTilted=FALSE Then
		If MusicNumber=12 Then
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+140 ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+140 ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(140)
			AddScore 140 '125% frenzy scoring
			'This mode completes by achieving 1000000 points
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack12.State=1
				CheckModeComplete
			End If
		Else
			AddScore 110
		End If
	End If
End Sub

Sub WhiteStandup_Hit
	PlaySound SoundFXDOF("fx_target",228,DOFPulse,DOFContactors)
	If bTilted=FALSE Then
		Dim PAD,SActivated
		PAD=5000:SActivated=0
		If INT(RND*9)<2 Then
			ShortJamAnim'ShortJam Animation - less than 25% chance of activating to award double points
			If gsGameDifficulty<2 Then PAD=10000
		End If
		If MusicNumber=12 Then
			MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+PAD*1.25 ' track it for total
			MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+PAD*1.25 ' track it for completion
			If Light2X.State=2 Or Light3X.State=2 Or Light4X.State=2 Then HandlePFMultiplierModeScoring(PAD*1.25)
			AddScore PAD*1.25 '125% frenzy scoring
			'This mode completes by achieving 1000000 points
			If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
				PlaySound"AModeComplete"
				DOF 240, DOFPulse
				MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
				LightTrack12.State=1
				CheckModeComplete
			End If
		Else
			AddScore PAD
		End If
	End If
End Sub

Sub ShortJamAnim
End Sub
'END OF EXTRA SCORING SWITCHES

Sub CheckNotes
	Dim Z1,TempNoteCount
	TempNoteCount=0
	For Z1=1 To 7
		TempNoteCount=TempNoteCount+NoteCount(CurrentPlayer,Z1)
	Next
	LightNote1.State=0 And LightNote2.State=0 And LightNote3.State=3 And LightNote4.State=0 And LightNote5.State=0 And LightNote6.State=0 And LightNote7.State=0
	If TempNoteCount<gsNotesForSongSelect Then
		If NoteCount(CurrentPlayer,1)<1 Then LightNote1.State=2
		If NoteCount(CurrentPlayer,2)<1 Then LightNote2.State=2
		If NoteCount(CurrentPlayer,3)<1 Then LightNote3.State=2
		If NoteCount(CurrentPlayer,4)<1 Then LightNote4.State=2
		If NoteCount(CurrentPlayer,5)<1 Then LightNote5.State=2
		If NoteCount(CurrentPlayer,6)<1 Then LightNote6.State=2
		If NoteCount(CurrentPlayer,7)<1 Then LightNote7.State=2
	End If
	If TempNoteCount>=gsNotesForSongSelect Then 'enough notes have been hit to activate the change song kicker, open the trapdoor and blink the lamp for it
		LightNote1.State=0 And LightNote2.State=0 And LightNote3.State=3 And LightNote4.State=0 And LightNote5.State=0 And LightNote6.State=0 And LightNote7.State=0
		Skill1.State=2
		PrepareWMStuff
		CheckModeComplete
	End If
End Sub

'place a textbox on the table for this to work - enable the timer for debug count
sub textbox2_timer
	Dim Z1,TempNoteCount
	TempNoteCount=0
	For Z1=1 To 7
		TempNoteCount=TempNoteCount+NoteCount(CurrentPlayer,Z1)
	Next
	textbox2.text=tempnotecount
end sub

Sub CheckModeComplete
	If MusicPlayed(1,CurrentPlayer,0)>0 And MusicPlayed(2,CurrentPlayer,0)>0 And MusicPlayed(3,CurrentPlayer,0)>0 And MusicPlayed(4,CurrentPlayer,0)>0 Then
		If MusicPlayed(5,CurrentPlayer,0)>0 And MusicPlayed(6,CurrentPlayer,0)>0 And MusicPlayed(7,CurrentPlayer,0)>0 And MusicPlayed(8,CurrentPlayer,0)>0 Then
			If MusicPlayed(9,CurrentPlayer,0)>0 And MusicPlayed(10,CurrentPlayer,0)>0 And MusicPlayed(11,CurrentPlayer,0)>0 And MusicPlayed(12,CurrentPlayer,0)>0 Then
				'all songs have been played - check for all completions
				If LightTrack1.State=1 And LightTrack2.State=1 And LightTrack3.State=1 And LightTrack4.State=1 And LightTrack5.State=1 And LightTrack6.State=1 Then
					If LightTrack7.State=1 And LightTrack8.State=1 And LightTrack9.State=1 And LightTrack10.State=1 And LightTrack11.State=1 And LightTrack12.State=1 Then
						'activate wizard mode lamp
						LightWizardMode.State=2
						LightMiniWM.State=0 'turn off mini wizard mode
						If Not b2BallMultiMode And Not b3BallMultiMode Then
							PrepareWMStuff
							If BallsInSubway=0 Then
								PlaceLamp=0
								BeginWizardShow.Enabled=1
							End If
						End If
						NoteCount(CurrentPlayer,1)=20 'ensure note count is enough to trigger opening trapdoor
						WizardModeAvailable(CurrentPlayer)=1
						MiniWizardModeAvailable(CurrentPlayer)=0
					Else
						LightWizardMode.State=0
						WizardModeAvailable(CurrentPlayer)=0
						LightMiniWM.State=2 'turn on mini wizard mode
						MiniWizardModeAvailable(CurrentPlayer)=1
					End If
				Else
					LightWizardMode.State=0
					WizardModeAvailable(CurrentPlayer)=0
					LightMiniWM.State=2 'turn on mini wizard mode
					MiniWizardModeAvailable(CurrentPlayer)=1
				End If
			End If
		End If
	End If
End Sub

Sub PrepareWMStuff
	Skill1.State=2:TrapDoorRamp.Collidable=0:TrapDoorWall.IsDropped=1:TrapDoorStop.IsDropped=0
	LightNote1.State=0:LightNote2.State=0:LightNote3.State=0:LightNote4.State=0:LightNote5.State=0:LightNote6.State=0:LightNote7.State=0
End Sub


Sub AwardMiniHurryUp
	AddScore MiniHPVal 'award the hurryup points!
	HurrySmall.Enabled=0 'stop the small countdown timer as the award has been collected
	HurryUpActive=FALSE
	MiniHPNum=MiniHPNum+1 'increase hurryup number collected by 1
	For X=1 To 12
		If TrackLights(X).State<>1 Then
			TrackLights(X).State=1
			Exit For 'Find first incomplete mode and mark it as being completed through mini wizard mode - not added to completed songs bonus count
		End If
	Next
	If MiniHPNum>=MiniHPTotal Then'if collected hurryups for this mode are equal to the ones required, then begin the full fledged wizard mode!
		CheckModeComplete
		PlaceLamp=0
		BeginWizardShow.Enabled=1
'		If BallsInSubway=0 Then 'if ball is in play on the playfield, then light kicker1 to begin wizard mode
'		End If
	Else 'not quite done yet - start another hurryup
		InitHurryUp
	End If
End Sub

Sub BeginWizardShow_Timer
	PlaceLamp=PlaceLamp+1
	If PlaceLamp>23 Then PlaceLamp=1
	Select Case PlaceLamp
		Case 1:LightNote6.State=0:LightProgress6.State=0:LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=0
		Case 2:LightNote6.State=1:LightProgress6.State=0:LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=0
		Case 3:LightNote6.State=1:LightProgress6.State=1:LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=0
		Case 4:LightNote6.State=1:LightProgress6.State=1:LightTrapDoor1.State=1:LightTrapDoor2.State=0:LightTrapDoor3.State=0
		Case 5:LightNote6.State=1:LightProgress6.State=1:LightTrapDoor1.State=1:LightTrapDoor2.State=1:LightTrapDoor3.State=0
		Case 6:LightNote6.State=1:LightProgress6.State=1:LightTrapDoor1.State=1:LightTrapDoor2.State=1:LightTrapDoor3.State=1
		Case 7:LightNote6.State=0:LightProgress6.State=1:LightTrapDoor1.State=1:LightTrapDoor2.State=1:LightTrapDoor3.State=1
		Case 8:LightNote6.State=0:LightProgress6.State=0:LightTrapDoor1.State=1:LightTrapDoor2.State=1:LightTrapDoor3.State=1
		Case 9:LightNote6.State=0:LightProgress6.State=0:LightTrapDoor1.State=0:LightTrapDoor2.State=1:LightTrapDoor3.State=1
		Case 10:LightNote6.State=0:LightProgress6.State=0:LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=1
		Case 11:LightNote6.State=0:LightProgress6.State=0:LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=0
		Case 12:LightNote6.State=1:LightProgress6.State=1:LightTrapDoor1.State=1:LightTrapDoor2.State=1:LightTrapDoor3.State=1
		Case 13:LightNote6.State=0:LightProgress6.State=0:LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=0
		Case 14:LightNote6.State=0:LightProgress6.State=0:LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=1
		Case 15:LightNote6.State=0:LightProgress6.State=0:LightTrapDoor1.State=0:LightTrapDoor2.State=1:LightTrapDoor3.State=1
		Case 16:LightNote6.State=0:LightProgress6.State=0:LightTrapDoor1.State=1:LightTrapDoor2.State=1:LightTrapDoor3.State=1
		Case 17:LightNote6.State=0:LightProgress6.State=1:LightTrapDoor1.State=1:LightTrapDoor2.State=1:LightTrapDoor3.State=1
		Case 18:LightNote6.State=0:LightProgress6.State=0:LightTrapDoor1.State=1:LightTrapDoor2.State=0:LightTrapDoor3.State=0
		Case 19:LightNote6.State=0:LightProgress6.State=1:LightTrapDoor1.State=1:LightTrapDoor2.State=1:LightTrapDoor3.State=0
		Case 20:LightNote6.State=1:LightProgress6.State=1:LightTrapDoor1.State=1:LightTrapDoor2.State=1:LightTrapDoor3.State=1
		Case 21:LightNote6.State=1:LightProgress6.State=1:LightTrapDoor1.State=1:LightTrapDoor2.State=1:LightTrapDoor3.State=1
		Case 22:LightNote6.State=0:LightProgress6.State=1:LightTrapDoor1.State=1:LightTrapDoor2.State=1:LightTrapDoor3.State=0
		Case 23:LightNote6.State=0:LightProgress6.State=0:LightTrapDoor1.State=1:LightTrapDoor2.State=0:LightTrapDoor3.State=0
	End Select
End Sub

Sub HandlePFMultiplierModeScoring(points)
	If bTilted=FALSE Then
	Dim tCount
	tCount=1
		If Light3X.State=2 Then tCount=2 'account for playfield score multiplier - already added scoring once, so remove 1 multiplier making this 2
		If Light4X.State=2 Then tCount=3 'account for playfield score multiplier - already added scoring once, so remove 1 multiplier making this 3
		If MusicScore(CurrentPlayer,MusicNumber)+Points*tCount<100000000000000 Then MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+Points*tCount
		If MusicScoreTemp(CurrentPlayer,MusicNumber)+Points*tCount<100000000000000 Then MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+Points*tCount
	End If
End Sub

Sub RandomAwardRoutine
	LeftUp=FALSE
	RightUp=FALSE
	LeftFlipper.RotateToStart 'reset flippers
	TopLeftFlipper.RotateToStart
	RightFlipper.RotateToStart
	STimer.Enabled=0 'disable status timer
	gvStatusModeActive=FALSE 'disable status mode
	DisplayFlushQueue 'clear any display effects
	QTimer.Enabled=0
	Timer1.Enabled=0
	WizardModeDisplaysActive=TRUE
	PXO5=0
	RandomAwardTimer.Enabled=1
	RandomAwardTimer_Timer
End Sub

Sub RandomAwardTimer_Timer
	PXO5=PXO5+20
	If PXO5<900 Then
		Select Case PXO5
			Case 20:DisplayText3"M             ",1,1:DisplayText3"              ",2,1:DMD_SetScoreboardBackground "randomaward.png"
			Case 80:DisplayText3"OM           A",1,1
			Case 140:DisplayText3"DOM         AW",1,1
			Case 200:DisplayText3"NDOM       AWA",1,1
			Case 260:DisplayText3"ANDOM     AWAR",1,1
			Case 320:DisplayText3"RANDOM   AWARD",1,1
			Case 380:DisplayText3" RANDOM AWARD ",1,1
			Case 880:'Wait half a second before running checks
						AwardChosen=0
						CycleDirection=INT(RND*2) 'direction is either 0 or 1
						TotalCycles=INT(RND*2)+2 'set total cycles between 2 and 3
						For X=1 To 15:ValidAwards(X)=0:Next
						If MultiballSave.Enabled=0 And RandomAwardsAwarded(CurrentPlayer,0)<>15 And RandomAwardsAwarded(CurrentPlayer,1)<>15 And RandomAwardsAwarded(CurrentPlayer,2)<>15 Then ValidAwards(15)=1
						If HurryUpSingleActive=FALSE And RandomAwardsAwarded(CurrentPlayer,0)<>14 And RandomAwardsAwarded(CurrentPlayer,1)<>14 And RandomAwardsAwarded(CurrentPlayer,2)<>14 Then ValidAwards(14)=1
						If LightKickback.State<>1 And RandomAwardsAwarded(CurrentPlayer,0)<>13 And RandomAwardsAwarded(CurrentPlayer,1)<>13 And RandomAwardsAwarded(CurrentPlayer,2)<>13 Then ValidAwards(13)=1
						Dim Z1,TempNoteCount
						TempNoteCount=0
						For Z1=1 To 7:TempNoteCount=TempNoteCount+NoteCount(CurrentPlayer,Z1):Next
						If TempNoteCount<gsNotesForSongSelect And RandomAwardsAwarded(CurrentPlayer,0)<>12 And RandomAwardsAwarded(CurrentPlayer,1)<>12 And RandomAwardsAwarded(CurrentPlayer,2)<>12 Then ValidAwards(12)=1
						If (RRamps(CurrentPlayer)<5) Or (RRamps(CurrentPlayer)>6 And RRamps(CurrentPlayer)<11) Or (RRamps(CurrentPlayer)>12 And RRamps(CurrentPlayer)<17) Then
							If RandomAwardsAwarded(CurrentPlayer,0)<>11 And RandomAwardsAwarded(CurrentPlayer,1)<>11 And RandomAwardsAwarded(CurrentPlayer,2)<>11 Then ValidAwards(11)=1
						End If
						If (LRamps(CurrentPlayer)<5) Or (LRamps(CurrentPlayer)>6 And LRamps(CurrentPlayer)<11) Or (LRamps(CurrentPlayer)>12 And LRamps(CurrentPlayer)<17) Then
							If RandomAwardsAwarded(CurrentPlayer,0)<>10 And RandomAwardsAwarded(CurrentPlayer,1)<>10 And RandomAwardsAwarded(CurrentPlayer,2)<>10 Then ValidAwards(10)=1
						End If
						If LightRightSpecial.State=0 Or LightLeftSpecial.State=0 Then
							If RandomAwardsAwarded(CurrentPlayer,0)<>9 And RandomAwardsAwarded(CurrentPlayer,1)<>9 And RandomAwardsAwarded(CurrentPlayer,2)<>9 Then ValidAwards(9)=1
						End If
						If LightSpecial.State=0 And RandomAwardsAwarded(CurrentPlayer,0)<>8 And RandomAwardsAwarded(CurrentPlayer,1)<>8 And RandomAwardsAwarded(CurrentPlayer,2)<>8 Then ValidAwards(8)=1
						If LightExtraBall.State=0 And ExtraBallsAwards(CurrentPlayer)=0 And RandomAwardsAwarded(CurrentPlayer,0)<>7 And RandomAwardsAwarded(CurrentPlayer,1)<>7 And RandomAwardsAwarded(CurrentPlayer,2)<>7 Then ValidAwards(7)=1
						If BonusMultiplier(CurrentPlayer)<4 And RandomAwardsAwarded(CurrentPlayer,0)<>6 And RandomAwardsAwarded(CurrentPlayer,1)<>6 And RandomAwardsAwarded(CurrentPlayer,2)<>6 Then ValidAwards(6)=1
						If Light4X.State<2 And RandomAwardsAwarded(CurrentPlayer,0)<>5 And RandomAwardsAwarded(CurrentPlayer,1)<>5 And RandomAwardsAwarded(CurrentPlayer,2)<>5 Then ValidAwards(5)=1
						Dim IncompleteModes
						IncompleteModes=0
						For X=1 To 12
							If TrackLights(X).State>1 Then IncompleteModes=IncompleteModes+1
						Next
						If IncompleteModes>1 And RandomAwardsAwarded(CurrentPlayer,0)<>4 And RandomAwardsAwarded(CurrentPlayer,1)<>4 And RandomAwardsAwarded(CurrentPlayer,2)<>4 Then ValidAwards(4)=1
						If RandomAwardsAwarded(CurrentPlayer,0)<>3 And RandomAwardsAwarded(CurrentPlayer,1)<>3 And RandomAwardsAwarded(CurrentPlayer,2)<>3 Then ValidAwards(3)=1
						If RandomAwardsAwarded(CurrentPlayer,0)<>2 And RandomAwardsAwarded(CurrentPlayer,1)<>2 And RandomAwardsAwarded(CurrentPlayer,2)<>2 Then ValidAwards(2)=1
						If BeginWizardShow.Enabled=0 And RandomAwardsAwarded(CurrentPlayer,0)<>1 And RandomAwardsAwarded(CurrentPlayer,1)<>1 And RandomAwardsAwarded(CurrentPlayer,2)<>1 Then ValidAwards(1)=1
						AwardSelectionTotal=0
						For X=1 To 15
							If ValidAwards(X)=1 Then AwardSelectionTotal=AwardSelectionTotal+1
						Next
						AwardChosen=INT(RND*AwardSelectionTotal)+1 'from 1 to total valid awards
						For X=1 To 15
							If ValidAwards(X)=1 Then
								AwardChosen=AwardChosen-1
								If AwardChosen=0 Then
									FinalAwardSelectionValue=X
									Exit For
								End If
							End If
						Next
						AwardCycleValue=INT(RND*15)+1 'randomize the initial start of award cycling
		End Select
	Else
		If (PXO5-900)/80=INT((PXO5-900)/80) Then 'every 80ms change the value until finished
			If CycleDirection=1 Then
				AwardCycleValue=AwardCycleValue+1
				If AwardCycleValue>15 Then
					AwardCycleValue=1
					TotalCycles=TotalCycles-1
				End If
			Else
				AwardCycleValue=AwardCycleValue-1
				If AwardCycleValue<1 Then
					AwardCycleValue=15
					TotalCycles=TotalCycles-1
				End If
			End If
			PlaySound"ZChange2"
			DisplayText3 AwardNames(AwardCycleValue),2,1:DMD_DisplayRandomAward
			If TotalCycles=0 And AwardCycleValue=FinalAwardSelectionValue Then
				DisplayText3 AwardNames(AwardCycleValue),2,2 'blink award name that was selected
				DMD_DisplaySceneTextWithPause AwardNames(AwardCycleValue), "", 4000
				DMD_SetScoreboardBackground ""
				QTimer.Enabled=0:QTimer.Enabled=1 'enables clearing the display
				RandomAwardTimer.Enabled=0
				WizardModeDisplaysActive=FALSE
				GiveAward 'exit timer routine and give out award
			End If
		End If
	End If
End Sub

Sub GiveAward
	'kick out the ball, then give the award
	SubwayExit.Enabled=1
	RandomAward.State=2 'not tilted, so blink the kickout light
	RandomAwardsAwarded(CurrentPlayer,2)=RandomAwardsAwarded(CurrentPlayer,1)
	RandomAwardsAwarded(CurrentPlayer,1)=RandomAwardsAwarded(CurrentPlayer,0)
	RandomAwardsAwarded(CurrentPlayer,0)=FinalAwardSelectionValue
	Select Case FinalAwardSelectionValue
		Case 1:	If Loops(CurrentPlayer)<12 Then
					Loops(CurrentPlayer)=12
				Else
					Loops(CurrentPlayer)=24
				End If
				If Loops(CurrentPlayer)=12 Then LightMB1.State=2
				If Loops(CurrentPlayer)=24 Then LightMB2.State=2
				If LightMB1.State=2 Or LightMB2.State=2 Then
					CreateNewAutoBall(100):TimerPlungerFire.Enabled=1
					EndMusic
					b2BallMultiMode=TRUE 'enable two ball multiball
					DOF 235, DOFPulse
					Skill1.State=0:TrapDoorWall.IsDropped=0:TrapDoorRamp.Collidable=1:TrapDoorStop.IsDropped=1
					If LightMB1.State=2 Then
						DisplayText"BEGIN BOOTLEG ",1,1
						DisplayText"  MULTI-BALL  ",2,1
						DMD_DisplaySceneTextWithPause "BEGIN BOOTLEG", "MULTI-BALL", 4000
					Else
						DisplayText"  THEATRICAL  ",1,1
						DisplayText" PERFORMANCE  ",2,1
						DMD_DisplaySceneTextWithPause "THEATRICAL", "PERFORMANCE", 4000
					End If
					SaveLampStatesSmall
					BallSaved=TRUE:bBallSaverActive=FALSE:Timer_BallSave.Enabled=0 'disable standard ballsaver
					RandomAward.State=0
					MultiballSave.Enabled=1
					LightShootAgain.State=2
					Table1_MusicDone
					If LightMB1.State=2 Then:OldJP1=0:NewJP1=0:End If
					If LightMB2.State=2 Then:OldJP1=0:OldJP2=0:NewJP1=0:NewJP2=0:End If
					'combos aren't valid during multiball for 'combos mode' so turn off the lamps, however, combos do count (once, for this current shot) if a combo started the multiball mode
					LightProgress1.State=0:LightProgress2.State=0:LightProgress3.State=0:LightProgress4.State=0:LightProgress5.State=0:LightProgress6.State=0:LightProgress7.State=0
					LightProgress1.TimerEnabled=0:LightProgress2.TimerEnabled=0:LightProgress3.TimerEnabled=0:LightProgress4.TimerEnabled=0:LightProgress5.TimerEnabled=0:LightProgress6.TimerEnabled=0:LightProgress7.TimerEnabled=0
					LightJackpot1.State=1:LightJackpot2.State=1:LightJackpot3.State=1:LightJackpot4.State=1:LightJackpot5.State=1:LightJackpot6.State=1:LightJackpot7.State=1
					LightJackpot1.State=0:LightJackpot2.State=0:LightJackpot3.State=0:LightJackpot4.State=0:LightJackpot5.State=0:LightJackpot6.State=0:LightJackpot7.State=0
					If MusicNumber<>9 Then
						FixLamps
					Else
						For X=1 To 7:TWA(X)=0:Next
					End If
					GrabNewJackpots
				End If
		Case 2:AddScore INT((RND*10000)+1)*10
		Case 3:AddScore gsJackpot
		Case 4:	'Must have two played but incomplete modes to finish one, and can't finish the last incomplete mode currently being played
				For X=1 To 12
					If TrackLights(X).State>1 And MusicNumber<>X Then
						TrackLights(X).State=1
						MusicCompleted(X,CurrentPlayer,0)=MusicCompleted(X,CurrentPlayer,0)+1
						MusicCompleted2(X,CurrentPlayer,0)=MusicCompleted2(X,CurrentPlayer,0)+1
						CheckModeComplete
					End If
				Next
		Case 5:IncreasePlayfieldScoring
		Case 6:LightLeftInlane.State=1:LightRightInlane1.State=1:LightRightInlane2.State=1:LightRollover1.State=1:LightRollover2.State=1:LightRollover3.State=1
				CheckRolloverLanesBonus
				If MusicNumber=2 Then 'usually you can't increase multipliers during mode 2 - we make an exception for random awards
					LightLeftInlane.State=2:LightRightInlane1.State=2:LightRightInlane2.State=2:LightRollover1.State=2:LightRollover2.State=2:LightRollover3.State=2
				End If
		Case 7:LightExtraBall.State=2
		Case 8:LightSpecial.State=2
		Case 9:If LightRightSpecial.State=0 Then
					LightRightSpecial.State=1
				Else
					LightLeftSpecial.State=1
				End If
		Case 10:If LRamps(CurrentPlayer)<5 Then LRamps(CurrentPlayer)=4
				If LRamps(CurrentPlayer)>6 And LRamps(CurrentPlayer)<11 Then LRamps(CurrentPlayer)=10
				If LRamps(CurrentPlayer)>12 And LRamps(CurrentPlayer)<17 Then LRamps(CurrentPlayer)=16
				CheckLeftRampLock
		Case 11:If RRamps(CurrentPlayer)<5 Then RRamps(CurrentPlayer)=4
				If RRamps(CurrentPlayer)>6 And RRamps(CurrentPlayer)<11 Then RRamps(CurrentPlayer)=10
				If RRamps(CurrentPlayer)>12 And RRamps(CurrentPlayer)<17 Then RRamps(CurrentPlayer)=16
				CheckRightRampLock
		Case 12:NoteCount(CurrentPlayer,1)=10
				CheckNotes
		Case 13:If LightKickback.State=2 Then KickbackSaver.Enabled=0 'stop master timer
				LightKickback.State=1 'ensure the kickback light is always lit when minis are completed
		Case 14:SubwayExit.Enabled=0:SubwayExit_Timer:InitHurryUpSingle
		Case 15:BallSaved=TRUE:bBallSaverActive=FALSE:Timer_BallSave.Enabled=0 'disable standard ballsaver
				MultiballSave.Enabled=1:LightShootAgain.State=2
	End Select
End Sub

Sub TrapAnimTrigger_Hit
	If TrapDoorStop.IsDropped=0 Then 'do ANIMATION
		TrapDoorAnimationEnabled=TRUE
	End If
End Sub

Sub Trigger4_Hit
	If TrapDoorAnimationEnabled=TRUE Then
		AnimTrapDoor1.IsDropped=1
		AnimTrapDoor2.IsDropped=0
	End If
	'hideScorbit
End Sub

Sub RandomCallOut_Timer
	RandomCalloutCount=RandomCalloutCount+1
End Sub

'autoplunger routine
'CreateNewAutoBall(400)
'TimerPlungerFire.Enabled=1

Sub RenderDelay_Timer
	RenderDelay.Enabled=0
	PlaySound"ZWilliamsBoot"
End Sub

Sub Table1_Init
	LoadUltraDMD
	LoadEM

	Dbg "Starting Table"

'	RW1.IsDropped=1:RW2.IsDropped=1:RW3.IsDropped=1:RW4.IsDropped=1:RW5.IsDropped=1:RW6.IsDropped=1:RW7.IsDropped=1:RW8.IsDropped=1
	TrapDoorAnimationEnabled=FALSE
	LockDiverter2.IsDropped=1
	AnimTrapDoor2.IsDropped=1
'	RStand1a.IsDropped=1:RStand2a.IsDropped=1:RStand3a.IsDropped=1
	Set Turntable=New cTurntable
	TurnTable.InitTurntable TurnTable1,20,TRUE'trigger TurnTable1, speed 300, spinning clockwise
	TurnTable.SpinUp=60'fast accleration to top speed/33% or 3 seconds to achieve speed
	TurnTable.SpinDown=90'1.5 times slowdown, 4.5 seconds to stop
	TurnTable.MotorOff:lockl1.State=0: lockl2.State=0'turns motor on - motor is off until multiball starts
	
	TurnTableUpdateNow.Enabled=0
	If CurrentMotorSound="fx_MotorOn" Then
		StopSound"fx_MotorOn"
		PlaySound"fx_MotorOff"
		CurrentMotorSound=""
	End If
	If CurrentMotorSound="fx_MotorRun" Then
		StopSound"fx_MotorRun"

		PlaySound"fx_MotorOff"
		CurrentMotorSound=""
	End If
	If MotorStart.Enabled Then MotorStart.Enabled=0
	
	BGate.Open=TRUE:CGate.Open=TRUE
	Skill3SignON.IsDropped=1:Skill3SignOFF.IsDropped=0 'to be sure
	bEndOfGame=TRUE
	If Len(TableName)>14 Then DisplayName=Left(TableName,14)
	If Len(TableName)<14 Then
		TempLength=14-Len(TableName)
		DisplayName=TableName
		For X=1 To TempLength
			DisplayName=DisplayName&" "
		Next
	End If
	DisplayText"   TESTING    ",2,1:DisplayText"   TESTING    ",4,1
	If RenderDelay.Enabled=0 Then PlaySound"ZWilliamsBoot"
	'initial light sequence to power on machine
	'LightSeq1_PlayDone'start attract mode light sequencer on a loop


	bOnTheFirstBallScorbit = False
	
	if HasPuP Then

		PuPStart(cPuPPack)  ' Start Pup Pack
		PuPinit
		if ScorbitActive = 0 Then Pupevent 999
		'PuPlayer.playlistplayex pQRlocation,"PuPOverlays","Blank.png",0,1
	End If




	
	AutoPlunger.PullBack:Kickback.PullBack
	DelayTimer.Enabled=1
	Bumper1.Threshold=1:Bumper2.Threshold=1:Bumper3.Threshold=1
	RightSlingshot.SlingshotStrength=6:LeftSlingshot.SlingshotStrength=6
	'kill the last switch
	LastSwitchHit=0
	'Set up the game parameters which are stored in battery backed up ram
	'These are the 'Factory Default Values'
	ResetHSTD
	gsJackpot=100000'jackpot starts at 100000
	gsGamesPlayed=0
	gsCredits=0
	gsNotesForSongSelect=5
	gsGameDifficulty=1'factory default (normal)
	gsBallsPerGame=3
	gsFreePlay=False
	gsMatchPer=10'10%
	gsTiltWarnings=3'warning,warning,tilt
	gsReplayStart=50000000
	gsReplayLevels=1
	'load the game stats from disk (will overwrite the above game stats if the file exists
	Dim i,Value
	For i=0 To 3'top four high scores
		Value=LoadValue(cGameSaveName,"HighScore"&i)
		If Value<>"" Then gsHighScore(i)=CDbl(Value)
		Value=LoadValue(cGameSaveName,"HighScoreName"&i)
		If Value<>"" Then gsHighScoreName(i)=Value
	Next
	For i=1 To 12'individual track records
		Value=LoadValue(cGameSaveName,"Track"&i&"Score")
		If Value<>"" Then gsTrackScore(i)=CDbl(Value)
		Value=LoadValue(cGameSaveName,"Track"&i&"ScoreName")
		If Value<>"" Then gsTrackScoreName(i)=Value
	Next
	For i=0 To 3'last game scores
		Value=LoadValue(cGameSaveName,"OldScore"&i)
		If Value<>"" Then Score(i)=CDbl(Value)
	Next
	Value=LoadValue(cGameSaveName,"NotesForSongSelect")
	If Value<>"" Then gsNotesForSongSelect=CDbl(Value)
	Value=LoadValue(cGameSaveName,"ComboChamp")
	If Value<>"" Then gvCombosForComboChamp=CDbl(Value)
	If gvCombosforComboChamp>999 Then gvCombosforComboChamp=999
	Value=LoadValue(cGameSaveName,"ComboChampName")
	If Value<>"" Then gsComboChampName=Value
	Value=LoadValue(cGameSaveName,"GamesPlayed")
	If Value<>"" Then gsGamesPlayed=CDbl(Value)
	Value=LoadValue(cGameSaveName,"Credits")
	If Value<>"" Then gsCredits=CDbl(Value)
	Value=LoadValue(cGameSaveName,"GameDifficulty")
	If Value<>"" Then gsGameDifficulty=CDbl(Value)
	Value=LoadValue(cGameSaveName,"BallsPerGame")
	If Value<>"" Then gsBallsPerGame=CDbl(Value)
	Value=LoadValue(cGameSaveName,"FreePlay")
	If Value<>"" Then gsFreePlay=CBool(Value)
	Value=LoadValue(cGameSaveName,"MatchPer")
	If Value<>"" Then gsMatchPer=CDbl(Value)
	Value=LoadValue(cGameSaveName,"TiltWarnings")
	If Value<>"" Then gsTiltWarnings=CDbl(Value)
	Value=LoadValue(cGameSaveName,"ReplayStart")
	If Value<>"" Then gsReplayStart=CDbl(Value)
	Value=LoadValue(cGameSaveName,"ReplayLevels")
	If Value<>"" Then gsReplayLevels=CDbl(Value)
	'Set any game variables
	bGameInPlay=FALSE'game started flag
	hsbModeActive=FALSE'high score entry
	bMenuModeActive=FALSE
	bDoorOpen=FALSE'coin door is closed
	Score(0)=0:Score(1)=0:Score(2)=0:Score(3)=0
	gvCreditsToAward=0
	If gsCredits > 0 Then DOF 209, DOFOn End If
	TimerBonusDisplay.Interval=4000'delay before status display
	'ensure anything else is reset
	EndOfGame
End Sub

Sub DelayTimer_Timer
	DelayTimer.Enabled=0
	StartAttractModeDisplays
End Sub

'turn on turntable
'TTPOST.IsDropped=0:TurnTable.MotorOn:Kicker6.CreateBall:Kicker6.Kick 120,4

'turn off turntable
'TTPOST.IsDropped=1:TurnTable.MotorOff

Sub TurnTable1_Hit:Turntable.AddBall  ActiveBall:End Sub
Sub TurnTable1_UnHit:Turntable.RemoveBall ActiveBall:End Sub
Sub TurnTableUpdateNow_Timer:Turntable.ProcessBalls:Turntable.ComputeSpin:End Sub

Sub Gate7_Hit
	bBallInPlungerLane=FALSE
	gvFirstBallEjected=TRUE
	LastSwitchHit=1 'Gate 7
	If bTilted=FALSE Then
		If PlayersPlayingGame=1 Then
			If gsBallsPerGame+1-BallsRemaining(0)=1 Then
				FlashInsertCoin
			End If
		End If
	End If
End Sub

Sub SkillStart_Hit
	If bTilted=FALSE Then
		If SkillShotActive=TRUE Then
			SkillShotTimer.Enabled=1' - enabled with first switch hit
		End If
	End If
End Sub

Sub Skill3Trigger_Hit
	If bTilted=FALSE Then
		If SkillShotActive=TRUE Then
			If LastSwitchHit=1 Then 'Gate7
				LastSwitchHit=2 'Skill 3 Trigger for skill shot
				If Drop4.IsDropped Then:Drop4.IsDropped=0:PlaySound"fx_solenoid":End If 'raise the drop target and make a sound only if it is down
				Skill2.State=0 'Turn off skill shot 2 lamp
			End If
		End If
	End If
End Sub

Sub Gate6_Hit
	If bTilted=FALSE Then
		If SkillShotActive=TRUE Then
			If LastSwitchHit=2 Then 'Skill3Trigger
				Skill1.State=0 'Skill Shot 1 Lamp is off
				Skill3SignOff.TimerEnabled=0 'disable blinking skill shot sign
				Skill3SignOn.IsDropped=0 'Turn on Skill sign as this skill shot was made! 'ANIMATION would go here
				Skill3SignOff.IsDropped=1
				LightProgress6.State=0 'disable right ramp for skill shot
				SkillShotTimer.Enabled=0 'disable skill shot timer in case it was activated by something
				SkillShotActive=FALSE 'disable skill shots for the rest of this ball
				If LightTrapDoor1.State<>2 And LightTrapDoor2.State<>2 And LightTrapDoor3.State<>2 And Not BeginWizardShow.Enabled Then
					TrapDoorRamp.Collidable=1:TrapDoorWall.IsDropped=0:TrapDoorStop.IsDropped=1
				End If
				Gate6.TimerEnabled=1 'used to turn off lamp and restore playfield lights
				AddScore SkillValue3 'Add total Skill Shot points!!!
				AwardedSkills(CurrentPlayer,3)=AwardedSkills(CurrentPlayer,3)+1
				If WizardModeAvailable(CurrentPlayer)=1 Then
					If BeginWizardShow.Enabled=0 Then
						If BallsInSubway=0 Then
							PrepareWMStuff
							PlaceLamp=0
							BeginWizardShow.Enabled=1
						End If
					End If
				End If
			End If
		End If
	End If
End Sub

Sub Gate6_Timer
	Gate6.TimerEnabled=0:Skill3SignOn.IsDropped=1:Skill3SignOff.IsDropped=0
	If bTilted=FALSE Then
		HandlePFLamps
		If RRamps(CurrentPlayer)=18 Then:TrapDoorRamp.Collidable=0:TrapDoorWall.IsDropped=1:TrapDoorStop.IsDropped=0:End If 'open trapdoor
	End If
End Sub

Sub Skill3SignOff_Timer
	If bTilted=FALSE Then
		If Skill3SignOff.IsDropped Then
			Skill3SignOn.IsDropped=1:Skill3SignOff.IsDropped=0
			AnimTrapDoor2.IsDropped=1:AnimTrapDoor1.IsDropped=0
		Else
			Skill3SignOff.IsDropped=1:Skill3SignOn.IsDropped=0
			AnimTrapDoor1.IsDropped=1:AnimTrapDoor2.IsDropped=0
		End If
	Else
		Skill3SignOn.IsDropped=1:Skill3SignOff.IsDropped=0:Skill3SignOff.TimerEnabled=0
	End If
End Sub

Sub SkillTrigger_unHit
	If bTilted=FALSE Then
		If SkillShotActive=TRUE Then
			If LastSwitchHit=1 Then 'Gate7
				If Drop4.IsDropped Then:Drop4.IsDropped=0:PlaySound"fx_solenoid":End If 'raise the drop target and make a sound only if it is down
				Skill1.State=0 'Skill Shot 1 Lamp is off
				Skill2.State=1 'Turn on skill shot 2 lamp - this was made!
				Skill3SignOff.TimerEnabled=0 'disable blinking skill shot sign
				Skill3SignOn.IsDropped=1:Skill3SignOff.IsDropped=0 'turn off the skill shot sign
				LightProgress6.State=0 'disable right ramp for skill shot
				SkillShotTimer.Enabled=0 'disable skill shot timer in case it was activated by something
				SkillShotActive=FALSE 'disable skill shots for the rest of this ball
				If LightTrapDoor1.State<>2 And LightTrapDoor2.State<>2 And LightTrapDoor3.State<>2 And Not BeginWizardShow.Enabled Then
					TrapDoorRamp.Collidable=1:TrapDoorWall.IsDropped=0:TrapDoorStop.IsDropped=1
				End If
				SkillTrigger.TimerEnabled=1 'used to turn off lamp and restore playfield lights
				AddScore SkillValue2 'Add total Skill Shot points!!!
				AwardedSkills(CurrentPlayer,2)=AwardedSkills(CurrentPlayer,2)+1
				If WizardModeAvailable(CurrentPlayer)=1 Then
					If BeginWizardShow.Enabled=0 Then
						If BallsInSubway=0 Then
							PrepareWMStuff
							PlaceLamp=0
							BeginWizardShow.Enabled=1
						End If
					End If
				End If
			End If
		End If
	End If
End Sub

Sub SkillTrigger_Timer
	SkillTrigger.TimerEnabled=0:Skill2.State=0
	If bTilted=FALSE Then
		HandlePFLamps
		If RRamps(CurrentPlayer)=18 Then:TrapDoorRamp.Collidable=0:TrapDoorWall.IsDropped=1:TrapDoorStop.IsDropped=0:End If 'open trapdoor
	End If
End Sub

Sub Kicker1_Hit
StopSound"fx_metalrolling"
PlaySound"fx_kicker_enter"
	TrapDoorAnimationEnabled=FALSE
	If AnimTrapDoor1.IsDropped Then
		AnimTrapDoor2.IsDropped=1
		AnimTrapDoor1.IsDropped=0
	End If
	bBallInPlungerLane=FALSE
	gvFirstBallEjected=TRUE
	ClearBallID
	Kicker1.DestroyBall 'ball enters subway from here
	BallsInSubway=BallsInSubway+1
	If bTilted=FALSE Then
		If HurryUpSingleActive=TRUE Then
			RandomAward.State=2
			SubwayExit_Timer
			Exit Sub
		End If
		If bBallSaverActive=TRUE Then 'extend time for ball saver if ball is stuck in the subway
			Timer_BallSave.Enabled=0
		End If
		If PlayersPlayingGame=1 Then
			If gsBallsPerGame+1-BallsRemaining(0)=1 Then
				FlashInsertCoin
			End If
		End If
		If SkillShotActive=TRUE Then
			NoteCount(CurrentPlayer,1)=20'ensure that the notecount limit is high enough to force "Select a Song" for skill shot
			'clear displays
			For X=1 To 56
				DD X,0,1
			Next
			If Drop4.IsDropped Then:Drop4.IsDropped=0:PlaySound"fx_solenoid":End If 'raise the drop target and make a sound only if it is down
			Skill1.State=1 'Skill Shot 1 Lamp is solid on - skill shot 1 was made
			Skill2.State=0 'Turn off skill shot 2 lamp
			Skill3SignOff.TimerEnabled=0 'disable blinking skill shot sign
			Skill3SignOn.IsDropped=1:Skill3SignOff.IsDropped=0 'turn off the skill shot sign
			LightProgress6.State=0 'disable right ramp for skill shot
			SkillShotTimer.Enabled=0 'disable skill shot timer in case it was activated by something before kicker1 was hit
			SkillShotActive=FALSE 'disable skill shots for the rest of this ball
			Kicker1.TimerEnabled=1 'add a delay before song selection
			If RRamps(CurrentPlayer)=18 Then:LightTrapDoor1.State=2:LightTrapDoor2.State=2:LightTrapDoor3.State=2:TrapDoorRamp.Collidable=0:TrapDoorWall.IsDropped=1:TrapDoorStop.IsDropped=0:End If 'open trapdoor
			If RRamps(CurrentPlayer)=17 Then:LightTrapDoor1.State=2:LightTrapDoor2.State=1:LightTrapDoor3.State=1:End If
			If RRamps(CurrentPlayer)>11 And RRamps(CurrentPlayer)<17 Then:LightTrapDoor1.State=0:LightTrapDoor2.State=1:LightTrapDoor3.State=1:End If
			If RRamps(CurrentPlayer)=11 Then:LightTrapDoor1.State=0:LightTrapDoor2.State=2:LightTrapDoor3.State=1:End If
			If RRamps(CurrentPlayer)>5 And RRamps(CurrentPlayer)<11 Then:LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=1:End If
			If RRamps(CurrentPlayer)=5 Then:LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=2:End If
			If LightTrapDoor1.State<>2 And LightTrapDoor2.State<>2 And LightTrapDoor3.State<>2 And Not BeginWizardShow.Enabled Then
				TrapDoorRamp.Collidable=1:TrapDoorWall.IsDropped=0:TrapDoorStop.IsDropped=1
			End If
			If MusicNumber=12 Then
				MusicScore(CurrentPlayer,MusicNumber)=MusicScore(CurrentPlayer,MusicNumber)+SkillValue1*1.25 ' track it for total
				MusicScoreTemp(CurrentPlayer,MusicNumber)=MusicScoreTemp(CurrentPlayer,MusicNumber)+SkillValue1*1.25 ' track it for completion
				AddScore SkillValue1*1.25 'add it
				'check it for completion
				'This mode completes by achieving 1000000 points
				If MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 And OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0) Then 'hasn't been completed yet so give award
					PlaySound"AModeComplete"
					DOF 240, DOFPulse
					MusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					MusicCompleted2(MusicNumber,CurrentPlayer,0)=MusicCompleted2(MusicNumber,CurrentPlayer,0)+1 'increase completed count for this song
					LightTrack12.State=1
					CheckModeComplete
				End If
				AwardedSkills(CurrentPlayer,1)=AwardedSkills(CurrentPlayer,1)+1
			Else
				AddScore SkillValue1 'Add total Skill Shot points!!!  250,000 opening for this as it is rather easy
				AwardedSkills(CurrentPlayer,1)=AwardedSkills(CurrentPlayer,1)+1
			End If
		Else
			Skill1.State=0:TrapDoorRamp.Collidable=1:TrapDoorWall.IsDropped=0:TrapDoorStop.IsDropped=1
			'If Kicker1 is hit during a game but not as a skill shot then it doesn't award points
			If Not b2BallMultiMode And Not b3BallMultiMode And Not b4BallMultiMode Then
				If Not BeginWizardShow.Enabled Then
					If LightTrapDoor1.State=2 Or LightTrapDoor2.State=2 Or LightTrapDoor3.State=2 Then
						If LightTrapDoor1.State=2 Then LightTrapDoor1.State=1
						If LightTrapDoor2.State=2 Then LightTrapDoor2.State=1
						If LightTrapDoor3.State=2 Then LightTrapDoor3.State=1
						PlayerBallsInTopLock(CurrentPlayer)=PlayerBallsInTopLock(CurrentPlayer)+1
						Select Case PlayerBallsInTopLock(CurrentPlayer)
							Case 1:Kicker1.TimerEnabled=1:RandomAward.State=2
							Case 2:Kicker1.TimerEnabled=1:RandomAward.State=2
							Case 3:BallsInGame=BallsInGame-1:LastLockHit(CurrentPlayer)="TOP"
									Kicker1.TimerEnabled=0:BallsInSubway=BallsInSubway-1
									WizardModeDisplaysActive=TRUE
									LeftUp=FALSE:RightUp=FALSE:LeftFlipper.RotateToStart:TopLeftFlipper.RotateToStart:RightFlipper.RotateToStart:STimer.Enabled=0 'disable status timer
									gvStatusModeActive=FALSE:QTimer.Enabled=0:Timer1.Enabled=0
									PXO2=0:EndMusic:ThreeBallTimer.Enabled=1:ThreeBallTimer_Timer:Exit Sub 'start 3 ball multiball!
						End Select
					Else
						RandomAward.State=2
						Kicker1.TimerEnabled=1
					End If
				Else
					RandomAward.State=2
					Kicker1.TimerEnabled=1
				End If
			Else
				RandomAward.State=2
				Kicker1.TimerEnabled=1
			End If
		End If
	Else
		'machine is tilted, so simply move and kick out the ball
		SubwayExit.Enabled=1
		RandomAward.state = 2
	End If
End Sub

Sub SubwayExit_Timer
	BallsInSubway=BallsInSubway-1
	If BallsInSubway=0 Then SubwayExit.Enabled=0
	CreateBallID Kicker5
	Kicker5.Kick 177,8
	RandomAward.state = 0
	PlaySound SoundFXDOF("fx_Popper",221,DOFPulse,DOFContactors)
	DOF 242, DOFPulse
	If ExtendTime=TRUE Then
		ComboTimer.Enabled=1 'enable the combo timer as the ball is back in play
		ExtendTime=FALSE 'clears signal to delay starting the combo timer until the ball is back in play
	End If
	If bTilted=FALSE Then
		If HurryUpSingleActive=TRUE Then Exit Sub
		If RandomAwardAvailable(CurrentPlayer)=0 Or MusicNumber=11 Or LightWizardMode.State=1 Or LightMiniWM.State=1 Or b4BallMultiMode=TRUE Or b3BallMultiMode=TRUE Or b2BallMultiMode=TRUE Then RandomAward.State=0
		If LightMiniWM.State=1 Then 'Mini Wizard Mode Running
			HurryUpActive=TRUE
			If Hurry1.State=0 And Hurry2.State=0 And Hurry3.State=0 And Hurry4.State=0 And Hurry5.State=0 And Hurry6.State=0 And Hurry7.State=0 Then InitHurryUp 'something should be lit for hurryup
			HurrySmall.Enabled=1
			DisplayScore
		End If
		If bBallSaverActive=TRUE Then Timer_BallSave.Enabled=1
	End If
End Sub

Sub InitHurryUp
	MiniHPVal=2500000 'initialize value of first hurryup to 2 million points
	MiniHPValReal=2500000 'actual counter for hurryup timer
	'select first random hurryup shot location
	MiniHPCurrent=OldHurryUp
	Do Until MiniHPCurrent<>OldHurryUp
		MiniHPCurrent=INT(RND*8) 'value of 1-7
	Loop
	OldHurryUp=MiniHPCurrent
	Select Case MiniHPCurrent
		Case 1:Hurry1.State=2:LightProgress1.State=2
		Case 2:Hurry2.State=2:LightProgress2.State=2
		Case 3:Hurry3.State=2:LightProgress3.State=2
		Case 4:Hurry4.State=2:LightProgress4.State=2
		Case 5:Hurry5.State=2:LightProgress5.State=2
		Case 6:Hurry6.State=2:LightProgress6.State=2
		Case 7:Hurry7.State=2:LightProgress7.State=2
	End Select
End Sub

Sub DisplayPointsHurryUp
	If QTimer.Enabled Then QTimer.Enabled=0
	If Timer1.Enabled Then Timer1.Enabled=0
	Select Case CurrentPlayer
		Case 0:DisplayPoints(Score(0))
				DD 14,HS1N+37,1
				If HS3Z>1 Then:DD 13,HS1M+37,1:Else:DD 13,0,1:End If
				If HS3Z>2 Then:DD 12,HS1L+37,1:Else:DD 12,0,1:End If
				If HS3Z>3 Then:DD 11,HS1K+48,1:Else:DD 11,0,1:End If
				If HS3Z>4 Then:DD 10,HS1J+37,1:Else:DD 10,0,1:End If
				If HS3Z>5 Then:DD 9,HS1I+37,1:Else:DD 9,0,1:End If
				If HS3Z>6 Then:DD 8,HS1H+48,1:Else:DD 8,0,1:End If
				If HS3Z>7 Then:DD 7,HS1G+37,1:Else:DD 7,0,1:End If
				If HS3Z>8 Then:DD 6,HS1F+37,1:Else:DD 6,0,1:End If
				If HS3Z>9 Then:DD 5,HS1E+48,1:Else:DD 5,0,1:End If
				If HS3Z>10 Then:DD 4,HS1D+37,1:Else:DD 4,0,1:End If
				If HS3Z>11 Then:DD 3,HS1C+37,1:Else:DD 3,0,1:End If
				If HS3Z>12 Then:DD 2,HS1B+48,1:Else:DD 2,0,1:End If
				If HS3Z>13 Then:DD 1,HS1A+37,1:Else:DD 1,0,1:End If
		Case 1:DisplayPoints(Score(1))
				DD 42,HS1N+37,1
				If HS3Z>1 Then:DD 41,HS1M+37,1:Else:DD 41,0,1:End If
				If HS3Z>2 Then:DD 40,HS1L+37,1:Else:DD 40,0,1:End If
				If HS3Z>3 Then:DD 39,HS1K+48,1:Else:DD 39,0,1:End If
				If HS3Z>4 Then:DD 38,HS1J+37,1:Else:DD 38,0,1:End If
				If HS3Z>5 Then:DD 37,HS1I+37,1:Else:DD 37,0,1:End If
				If HS3Z>6 Then:DD 36,HS1H+48,1:Else:DD 36,0,1:End If
				If HS3Z>7 Then:DD 35,HS1G+37,1:Else:DD 35,0,1:End If
				If HS3Z>8 Then:DD 34,HS1F+37,1:Else:DD 34,0,1:End If
				If HS3Z>9 Then:DD 33,HS1E+48,1:Else:DD 33,0,1:End If
				If HS3Z>10 Then:DD 32,HS1D+37,1:Else:DD 32,0,1:End If
				If HS3Z>11 Then:DD 31,HS1C+37,1:Else:DD 31,0,1:End If
				If HS3Z>12 Then:DD 30,HS1B+48,1:Else:DD 30,0,1:End If
				If HS3Z>13 Then:DD 29,HS1A+37,1:Else:DD 29,0,1:End If
		Case 2:DisplayPoints(Score(2))
				DD 28,HS1N+37,1
				If HS3Z>1 Then:DD 27,HS1M+37,1:Else:DD 27,0,1:End If
				If HS3Z>2 Then:DD 26,HS1L+37,1:Else:DD 26,0,1:End If
				If HS3Z>3 Then:DD 25,HS1K+48,1:Else:DD 25,0,1:End If
				If HS3Z>4 Then:DD 24,HS1J+37,1:Else:DD 24,0,1:End If
				If HS3Z>5 Then:DD 23,HS1I+37,1:Else:DD 23,0,1:End If
				If HS3Z>6 Then:DD 22,HS1H+48,1:Else:DD 22,0,1:End If
				If HS3Z>7 Then:DD 21,HS1G+37,1:Else:DD 21,0,1:End If
				If HS3Z>8 Then:DD 20,HS1F+37,1:Else:DD 20,0,1:End If
				If HS3Z>9 Then:DD 19,HS1E+48,1:Else:DD 19,0,1:End If
				If HS3Z>10 Then:DD 18,HS1D+37,1:Else:DD 18,0,1:End If
				If HS3Z>11 Then:DD 17,HS1C+37,1:Else:DD 17,0,1:End If
				If HS3Z>12 Then:DD 16,HS1B+48,1:Else:DD 16,0,1:End If
				If HS3Z>13 Then:DD 15,HS1A+37,1:Else:DD 15,0,1:End If
		Case 3:DisplayPoints(Score(3))
				DD 56,HS1N+37,1
				If HS3Z>1 Then:DD 55,HS1M+37,1:Else:DD 55,0,1:End If
				If HS3Z>2 Then:DD 54,HS1L+37,1:Else:DD 54,0,1:End If
				If HS3Z>3 Then:DD 53,HS1K+48,1:Else:DD 53,0,1:End If
				If HS3Z>4 Then:DD 52,HS1J+37,1:Else:DD 52,0,1:End If
				If HS3Z>5 Then:DD 51,HS1I+37,1:Else:DD 51,0,1:End If
				If HS3Z>6 Then:DD 50,HS1H+48,1:Else:DD 50,0,1:End If
				If HS3Z>7 Then:DD 49,HS1G+37,1:Else:DD 49,0,1:End If
				If HS3Z>8 Then:DD 48,HS1F+37,1:Else:DD 48,0,1:End If
				If HS3Z>9 Then:DD 47,HS1E+48,1:Else:DD 47,0,1:End If
				If HS3Z>10 Then:DD 46,HS1D+37,1:Else:DD 46,0,1:End If
				If HS3Z>11 Then:DD 45,HS1C+37,1:Else:DD 45,0,1:End If
				If HS3Z>12 Then:DD 44,HS1B+48,1:Else:DD 44,0,1:End If
				If HS3Z>13 Then:DD 43,HS1A+37,1:Else:DD 43,0,1:End If
	End Select
End Sub

Sub HurrySmall_Timer
	MiniHPVal=MiniHPVal-10000
	MiniHPValReal=MiniHPValReal-10000
	If MiniHPVal<300000 Then MiniHPVal=300000 '300,000 minimum hurryup award for final three seconds
	If MiniHPValReal=300000 Then
		StopSound CurrentSound
		PlaySound"SHitIt" 'final 3 seconds
		CurrentSound="SHitIt"
		CredDelay.Enabled=1
	End If
	If MiniHPValReal<=0 Then 'actual timer value has now expired, hurryup is over
		HurryUpActive=FALSE
		HurrySmall.Enabled=0
		LightMiniWM.State=0
		PlayedMiniWizard(CurrentPlayer)=1
		LightTrack1.State=0:LightTrack2.State=0:LightTrack3.State=0:LightTrack4.State=0:LightTrack5.State=0:LightTrack6.State=0
		LightTrack7.State=0:LightTrack8.State=0:LightTrack9.State=0:LightTrack10.State=0:LightTrack11.State=0:LightTrack12.State=0
		UpdateMusicLamps 'fix playfield track lamps
		Hurry1.State=0:Hurry2.State=0:Hurry3.State=0:Hurry4.State=0:Hurry5.State=0:Hurry6.State=0:Hurry7.State=0
		LightProgress1.State=0:LightProgress2.State=0:LightProgress3.State=0:LightProgress4.State=0:LightProgress5.State=0:LightProgress6.State=0:LightProgress7.State=0
		LightProgress1.TimerEnabled=0:LightProgress2.TimerEnabled=0:LightProgress3.TimerEnabled=0:LightProgress4.TimerEnabled=0:LightProgress5.TimerEnabled=0:LightProgress6.TimerEnabled=0:LightProgress7.TimerEnabled=0
		DisplayScore
	Else
		Select Case CurrentPlayer
			Case 0:DisplayPoints(MiniHPVal)
					DD 28,HS1N+37,1
					If HS3Z>1 Then:DD 27,HS1M+37,1:Else:DD 27,0,1:End If
					If HS3Z>2 Then:DD 26,HS1L+37,1:Else:DD 26,0,1:End If
					If HS3Z>3 Then:DD 25,HS1K+48,1:Else:DD 25,0,1:End If
					If HS3Z>4 Then:DD 24,HS1J+37,1:Else:DD 24,0,1:End If
					If HS3Z>5 Then:DD 23,HS1I+37,1:Else:DD 23,0,1:End If
					If HS3Z>6 Then:DD 22,HS1H+48,1:Else:DD 22,0,1:End If
					If HS3Z>7 Then:DD 21,HS1G+37,1:Else:DD 21,0,1:End If
					If HS3Z>8 Then:DD 20,HS1F+37,1:Else:DD 20,0,1:End If
					If HS3Z>9 Then:DD 19,HS1E+48,1:Else:DD 19,0,1:End If
					If HS3Z>10 Then:DD 18,HS1D+37,1:Else:DD 18,0,1:End If
					If HS3Z>11 Then:DD 17,HS1C+37,1:Else:DD 17,0,1:End If
					If HS3Z>12 Then:DD 16,HS1B+48,1:Else:DD 16,0,1:End If
					If HS3Z>13 Then:DD 15,HS1A+37,1:Else:DD 15,0,1:End If
			Case 1:DisplayPoints(MiniHPVal)
					DD 56,HS1N+37,1
					If HS3Z>1 Then:DD 55,HS1M+37,1:Else:DD 55,0,1:End If
					If HS3Z>2 Then:DD 54,HS1L+37,1:Else:DD 54,0,1:End If
					If HS3Z>3 Then:DD 53,HS1K+48,1:Else:DD 53,0,1:End If
					If HS3Z>4 Then:DD 52,HS1J+37,1:Else:DD 52,0,1:End If
					If HS3Z>5 Then:DD 51,HS1I+37,1:Else:DD 51,0,1:End If
					If HS3Z>6 Then:DD 50,HS1H+48,1:Else:DD 50,0,1:End If
					If HS3Z>7 Then:DD 49,HS1G+37,1:Else:DD 49,0,1:End If
					If HS3Z>8 Then:DD 48,HS1F+37,1:Else:DD 48,0,1:End If
					If HS3Z>9 Then:DD 47,HS1E+48,1:Else:DD 47,0,1:End If
					If HS3Z>10 Then:DD 46,HS1D+37,1:Else:DD 46,0,1:End If
					If HS3Z>11 Then:DD 45,HS1C+37,1:Else:DD 45,0,1:End If
					If HS3Z>12 Then:DD 44,HS1B+48,1:Else:DD 44,0,1:End If
					If HS3Z>13 Then:DD 43,HS1A+37,1:Else:DD 43,0,1:End If
			Case 2:DisplayPoints(MiniHPVal)
					DD 14,HS1N+37,1
					If HS3Z>1 Then:DD 13,HS1M+37,1:Else:DD 13,0,1:End If
					If HS3Z>2 Then:DD 12,HS1L+37,1:Else:DD 12,0,1:End If
					If HS3Z>3 Then:DD 11,HS1K+48,1:Else:DD 11,0,1:End If
					If HS3Z>4 Then:DD 10,HS1J+37,1:Else:DD 10,0,1:End If
					If HS3Z>5 Then:DD 9,HS1I+37,1:Else:DD 9,0,1:End If
					If HS3Z>6 Then:DD 8,HS1H+48,1:Else:DD 8,0,1:End If
					If HS3Z>7 Then:DD 7,HS1G+37,1:Else:DD 7,0,1:End If
					If HS3Z>8 Then:DD 6,HS1F+37,1:Else:DD 6,0,1:End If
					If HS3Z>9 Then:DD 5,HS1E+48,1:Else:DD 5,0,1:End If
					If HS3Z>10 Then:DD 4,HS1D+37,1:Else:DD 4,0,1:End If
					If HS3Z>11 Then:DD 3,HS1C+37,1:Else:DD 3,0,1:End If
					If HS3Z>12 Then:DD 2,HS1B+48,1:Else:DD 2,0,1:End If
					If HS3Z>13 Then:DD 1,HS1A+37,1:Else:DD 1,0,1:End If
			Case 3:DisplayPoints(MiniHPVal)
					DD 42,HS1N+37,1
					If HS3Z>1 Then:DD 41,HS1M+37,1:Else:DD 41,0,1:End If
					If HS3Z>2 Then:DD 40,HS1L+37,1:Else:DD 40,0,1:End If
					If HS3Z>3 Then:DD 39,HS1K+48,1:Else:DD 39,0,1:End If
					If HS3Z>4 Then:DD 38,HS1J+37,1:Else:DD 38,0,1:End If
					If HS3Z>5 Then:DD 37,HS1I+37,1:Else:DD 37,0,1:End If
					If HS3Z>6 Then:DD 36,HS1H+48,1:Else:DD 36,0,1:End If
					If HS3Z>7 Then:DD 35,HS1G+37,1:Else:DD 35,0,1:End If
					If HS3Z>8 Then:DD 34,HS1F+37,1:Else:DD 34,0,1:End If
					If HS3Z>9 Then:DD 33,HS1E+48,1:Else:DD 33,0,1:End If
					If HS3Z>10 Then:DD 32,HS1D+37,1:Else:DD 32,0,1:End If
					If HS3Z>11 Then:DD 31,HS1C+37,1:Else:DD 31,0,1:End If
					If HS3Z>12 Then:DD 30,HS1B+48,1:Else:DD 30,0,1:End If
					If HS3Z>13 Then:DD 29,HS1A+37,1:Else:DD 29,0,1:End If
		End Select
	End If
End Sub

Sub InitHurryUpSingle
	HurryUpSingleActive=TRUE
	SaveLampStatesSmall
	MiniSingleVal=500000 'initialize value of first hurryup to 2 million points
	MiniSingleValReal=500000 'actual counter for hurryup timer
	'select first random hurryup shot location
	DisplayScore
	MiniSingleCurrent=OldSingleHurryUp
	Do Until MiniSingleCurrent<>OldSingleHurryUp
		MiniSingleCurrent=INT(RND*8) 'value of 1-7
	Loop
	OldSingleHurryUp=MiniSingleCurrent
	Select Case MiniSingleCurrent
		Case 1:Hurry1.State=2:LightProgress1.State=2
		Case 2:Hurry2.State=2:LightProgress2.State=2
		Case 3:Hurry3.State=2:LightProgress3.State=2
		Case 4:Hurry4.State=2:LightProgress4.State=2
		Case 5:Hurry5.State=2:LightProgress5.State=2
		Case 6:Hurry6.State=2:LightProgress6.State=2
		Case 7:Hurry7.State=2:LightProgress7.State=2
	End Select
	HurrySingle.Enabled=1
End Sub

Sub AwardHurryUpSingle
	AddScore MiniSingleVal 'award the hurryup points!
	HurrySingle.Enabled=0 'stop the small countdown timer as the award has been collected
	HurryUpSingleActive=FALSE
	Hurry1.State=0:Hurry2.State=0:Hurry3.State=0:Hurry4.State=0:Hurry5.State=0:Hurry6.State=0:Hurry7.State=0
	If MusicNumber=9 Then:For X=1 To 7:TWA(X)=2:Next
	RestoreLampStatesSmall
	DisplayScore
End Sub

Sub HurrySingle_Timer '250ms instead of 100ms provides 12.5 seconds to make the shot
	MiniSingleVal=MiniSingleVal-10000
	MiniSingleValReal=MiniSingleValReal-10000
	If MiniSingleVal<120000 Then MiniSingleVal=120000 '120,000 minimum hurryup award for final three seconds
	If MiniSingleValReal=120000 Then
		StopSound CurrentSound
		PlaySound"SHitIt" 'final 3 seconds
		CurrentSound="SHitIt"
		CredDelay.Enabled=1
	End If
	If MiniSingleValReal<=0 Then 'actual timer value has now expired, hurryup is over
		HurryUpSingleActive=FALSE
		HurrySingle.Enabled=0
		Hurry1.State=0:Hurry2.State=0:Hurry3.State=0:Hurry4.State=0:Hurry5.State=0:Hurry6.State=0:Hurry7.State=0
		If MusicNumber=9 Then:For X=1 To 7:TWA(X)=2:Next
		RestoreLampStatesSmall
		DisplayScore
	Else
		Select Case CurrentPlayer
			Case 0:DisplayPoints(MiniSingleVal)
					DD 28,HS1N+37,1
					If HS3Z>1 Then:DD 27,HS1M+37,1:Else:DD 27,0,1:End If
					If HS3Z>2 Then:DD 26,HS1L+37,1:Else:DD 26,0,1:End If
					If HS3Z>3 Then:DD 25,HS1K+48,1:Else:DD 25,0,1:End If
					If HS3Z>4 Then:DD 24,HS1J+37,1:Else:DD 24,0,1:End If
					If HS3Z>5 Then:DD 23,HS1I+37,1:Else:DD 23,0,1:End If
					If HS3Z>6 Then:DD 22,HS1H+48,1:Else:DD 22,0,1:End If
					If HS3Z>7 Then:DD 21,HS1G+37,1:Else:DD 21,0,1:End If
					If HS3Z>8 Then:DD 20,HS1F+37,1:Else:DD 20,0,1:End If
					If HS3Z>9 Then:DD 19,HS1E+48,1:Else:DD 19,0,1:End If
					If HS3Z>10 Then:DD 18,HS1D+37,1:Else:DD 18,0,1:End If
					If HS3Z>11 Then:DD 17,HS1C+37,1:Else:DD 17,0,1:End If
					If HS3Z>12 Then:DD 16,HS1B+48,1:Else:DD 16,0,1:End If
					If HS3Z>13 Then:DD 15,HS1A+37,1:Else:DD 15,0,1:End If
			Case 1:DisplayPoints(MiniSingleVal)
					DD 56,HS1N+37,1
					If HS3Z>1 Then:DD 55,HS1M+37,1:Else:DD 55,0,1:End If
					If HS3Z>2 Then:DD 54,HS1L+37,1:Else:DD 54,0,1:End If
					If HS3Z>3 Then:DD 53,HS1K+48,1:Else:DD 53,0,1:End If
					If HS3Z>4 Then:DD 52,HS1J+37,1:Else:DD 52,0,1:End If
					If HS3Z>5 Then:DD 51,HS1I+37,1:Else:DD 51,0,1:End If
					If HS3Z>6 Then:DD 50,HS1H+48,1:Else:DD 50,0,1:End If
					If HS3Z>7 Then:DD 49,HS1G+37,1:Else:DD 49,0,1:End If
					If HS3Z>8 Then:DD 48,HS1F+37,1:Else:DD 48,0,1:End If
					If HS3Z>9 Then:DD 47,HS1E+48,1:Else:DD 47,0,1:End If
					If HS3Z>10 Then:DD 46,HS1D+37,1:Else:DD 46,0,1:End If
					If HS3Z>11 Then:DD 45,HS1C+37,1:Else:DD 45,0,1:End If
					If HS3Z>12 Then:DD 44,HS1B+48,1:Else:DD 44,0,1:End If
					If HS3Z>13 Then:DD 43,HS1A+37,1:Else:DD 43,0,1:End If
			Case 2:DisplayPoints(MiniSingleVal)
					DD 14,HS1N+37,1
					If HS3Z>1 Then:DD 13,HS1M+37,1:Else:DD 13,0,1:End If
					If HS3Z>2 Then:DD 12,HS1L+37,1:Else:DD 12,0,1:End If
					If HS3Z>3 Then:DD 11,HS1K+48,1:Else:DD 11,0,1:End If
					If HS3Z>4 Then:DD 10,HS1J+37,1:Else:DD 10,0,1:End If
					If HS3Z>5 Then:DD 9,HS1I+37,1:Else:DD 9,0,1:End If
					If HS3Z>6 Then:DD 8,HS1H+48,1:Else:DD 8,0,1:End If
					If HS3Z>7 Then:DD 7,HS1G+37,1:Else:DD 7,0,1:End If
					If HS3Z>8 Then:DD 6,HS1F+37,1:Else:DD 6,0,1:End If
					If HS3Z>9 Then:DD 5,HS1E+48,1:Else:DD 5,0,1:End If
					If HS3Z>10 Then:DD 4,HS1D+37,1:Else:DD 4,0,1:End If
					If HS3Z>11 Then:DD 3,HS1C+37,1:Else:DD 3,0,1:End If
					If HS3Z>12 Then:DD 2,HS1B+48,1:Else:DD 2,0,1:End If
					If HS3Z>13 Then:DD 1,HS1A+37,1:Else:DD 1,0,1:End If
			Case 3:DisplayPoints(MiniSingleVal)
					DD 42,HS1N+37,1
					If HS3Z>1 Then:DD 41,HS1M+37,1:Else:DD 41,0,1:End If
					If HS3Z>2 Then:DD 40,HS1L+37,1:Else:DD 40,0,1:End If
					If HS3Z>3 Then:DD 39,HS1K+48,1:Else:DD 39,0,1:End If
					If HS3Z>4 Then:DD 38,HS1J+37,1:Else:DD 38,0,1:End If
					If HS3Z>5 Then:DD 37,HS1I+37,1:Else:DD 37,0,1:End If
					If HS3Z>6 Then:DD 36,HS1H+48,1:Else:DD 36,0,1:End If
					If HS3Z>7 Then:DD 35,HS1G+37,1:Else:DD 35,0,1:End If
					If HS3Z>8 Then:DD 34,HS1F+37,1:Else:DD 34,0,1:End If
					If HS3Z>9 Then:DD 33,HS1E+48,1:Else:DD 33,0,1:End If
					If HS3Z>10 Then:DD 32,HS1D+37,1:Else:DD 32,0,1:End If
					If HS3Z>11 Then:DD 31,HS1C+37,1:Else:DD 31,0,1:End If
					If HS3Z>12 Then:DD 30,HS1B+48,1:Else:DD 30,0,1:End If
					If HS3Z>13 Then:DD 29,HS1A+37,1:Else:DD 29,0,1:End If
		End Select
	End If
End Sub

Sub SkillShotTimer_Timer
	SkillShotTimer.Enabled=0 'took too long, so disable skill shots
	If SkillShotActive=TRUE Then
		If Drop4.IsDropped Then:Drop4.IsDropped=0:PlaySound"fx_solenoid":End If 'raise the drop target and make a sound only if it is down
		Skill1.State=0 'Skill Shot 1 Lamp is now off - skill shots were all missed
		Skill2.State=0 'Turn off skill shot 2 lamp
		Skill3SignOff.TimerEnabled=0 'disable blinking skill shot sign
		Skill3SignOn.IsDropped=1:Skill3SignOff.IsDropped=0 'turn off the skill shot sign
		LightProgress6.State=0 'disable right ramp for skill shot
		SkillShotActive=FALSE 'disable skill shots for the rest of this ball
		If LightTrapDoor1.State<>2 And LightTrapDoor2.State<>2 And LightTrapDoor3.State<>2 And Not BeginWizardShow.Enabled Then
			TrapDoorRamp.Collidable=1:TrapDoorWall.IsDropped=0:TrapDoorStop.IsDropped=1
		End If
		CheckNotes
		If bTilted=FALSE Then
			HandlePFLamps
			If RRamps(CurrentPlayer)=18 Then:TrapDoorRamp.Collidable=0:TrapDoorWall.IsDropped=1:TrapDoorStop.IsDropped=0:End If 'open trapdoor
			'check for wizard mode and mini wizard mode
			'failing those, fix up the rest of the playfield lamps
			If WizardModeAvailable(CurrentPlayer)=1 Then
				If BeginWizardShow.Enabled=0 Then
					If BallsInSubway=0 Then
						PrepareWMStuff
						PlaceLamp=0
						BeginWizardShow.Enabled=1
					End If
				End If
			Else
				HandleSongNumberLamps(LastSongPlayed(CurrentPlayer))
			End If
		End If
	End If
End Sub

Sub Kicker1_Timer
	Dim Z1,TempNoteCount
	TempNoteCount=0
	Kicker1.TimerEnabled=0
	If bTilted=FALSE Then
		'If All Songs have been completed, start Final Wizard Mode!
		If WizardModeAvailable(CurrentPlayer)=1 Or MiniWizardModeAvailable(CurrentPlayer)=1 Then
			If Not b2BallMultiMode Or b3BallMultiMode Or b4BallMultiMode Then
				Skill1.State=0 'turn off light
				LeftUp=FALSE
				RightUp=FALSE
				LeftFlipper.RotateToStart 'reset flippers
				TopLeftFlipper.RotateToStart
				RightFlipper.RotateToStart
				STimer.Enabled=0 'disable status timer
				gvStatusModeActive=FALSE 'disable status mode
				DisplayFlushQueue 'clear any display effects
				QTimer.Enabled=0
				Timer1.Enabled=0
				WizardModeDisplaysActive=TRUE 'kill flippers and keyboard input
				PXO3=0 'reset timer counter
				If WizardModeAvailable(CurrentPlayer)=1 Then
					If PlayedMiniWizard(CurrentPlayer)=0 Then
						If LightRightSpecial.State=0 Then LightRightSpecial.State=1 'lights right outlane special
						AddScore 12*1250000 'bypassing the mini wizard mode instantly awards half of the hurryup bonus for 12 music tracks = 15 million points
					End If
					'Start Wizard Mode for this ball
					If BeginWizardShow.Enabled=1 Then
						BeginWizardShow.Enabled=0
						LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=0
						LightNote6.State=0:LightProgress6.State=0:LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=0
					End If
					WMStartTimer.Enabled=1
					WizardModeAvailable(CurrentPlayer)=0 'wizard mode has started, so clear the available flag
				Else
					'If all songs have been started, but not completed, start mini Wizard Mode
					WMMiniTimer.Enabled=1
					'Start MiniWizard Mode for this ball
					MiniWizardModeAvailable(CurrentPlayer)=0 'mini wizard mode has started, so clear the available flag
				End If
			Else
				SubwayExit.Enabled=1
				RandomAward.state = 2
			End If
		Else
			If Not b2BallMultiMode And Not b3BallMultiMode And Not b4BallMultiMode Then
				For Z1=1 To 7
					TempNoteCount=TempNoteCount+NoteCount(CurrentPlayer,Z1)
				Next
				If TempNoteCount>=gsNotesForSongSelect Then 'enough notes have been hit to activate the change song kicker, open the trapdoor and blink the lamp for it
					BallSaved=FALSE
					'ensure that "Choose A Song" resets notecount values
					For Z1=1 To 7:NoteCount(CurrentPlayer,Z1)=0:Next
					'otherwise, give the player a choice to either continue playing the current song, or to start a never-started song
					PriorSong=LastSongPlayed(CurrentPlayer)
					TempSongSelect=PriorSong
					HandleSongNumberLamps(LastSongPlayed(CurrentPlayer))
					Skill1.State=0 'turn off light
					LeftUp=FALSE
					RightUp=FALSE
					LeftFlipper.RotateToStart 'reset flippers
					TopLeftFlipper.RotateToStart
					RightFlipper.RotateToStart
					STimer.Enabled=0 'disable status timer
					gvStatusModeActive=FALSE 'disable status mode
					DisplayFlushQueue 'clear any display effects
					SongSelectTimerCount=9
					SongSelectTimer_Timer
					SongSelectTimer.Enabled=1
					SongSelectInProgress=TRUE
					DisplayText"CHOOSE A SONG ",1,1:DMD_SetScoreboardBackground "chooseasong.png":DMD_DisplaySongSelect :PuPEvent 812
					QTimer.Enabled=0
					Timer1.Enabled=0
					TrackLights(PriorSong).State=0
					TrackLights(PriorSong).BlinkPattern="10"
					TrackLights(PriorSong).BlinkInterval=75
					TrackLights(PriorSong).State=0
					TrackLights(PriorSong).State=2
				Else
					BallSaved=FALSE
					Skill1.State=0 'not enough notes have been hit to activate the change song kicker
					TrapDoorStop.IsDropped=1:TrapDoorRamp.Collidable=1:TrapDoorWall.IsDropped=0
					SubwayExit.Enabled=1
					RandomAward.state = 2
				End If
			Else
				Skill1.State=0'return to playfield/kick ball back into play
				SubwayExit.Enabled=1
				RandomAward.state = 2
			End If
		End If
	Else
		'Skill1.State=0'return to playfield/kick ball back into play - multiball so don't tought the lamp
		SubwayExit.Enabled=1
		RandomAward.state = 2
	End If
End Sub

Sub HandlePFLamps
	If bTilted=FALSE Then
		If gsBallsPerGame+1-BallsRemaining(CurrentPlayer)=1 Then'set up initial playfield lamp states after skill shot
			LightNote1.State=2:LightNote2.State=2:LightNote3.State=2:LightNote4.State=2:LightNote5.State=2:LightNote6.State=2:LightNote7.State=2 'for now, turn on all the note shots
			SaveLampStates
		Else
			'restore playfield lights from their saved previous ball states
			RestoreLampStates
			'ensure left lock lights are correct
			If LRamps(CurrentPlayer)<5 Then:LightLock1.State=0:LightLock2.State=0:LightLock3.State=0:End If
			If LRamps(CurrentPlayer)=5 Then:LightLock1.State=2:LightLock2.State=0:LightLock3.State=0:End If
			If LRamps(CurrentPlayer)>5 And LRamps(CurrentPlayer)<11 Then:LightLock1.State=1:LightLock2.State=0:LightLock3.State=0:End If
			If LRamps(CurrentPlayer)=11 Then:LightLock1.State=1:LightLock2.State=2:LightLock3.State=0:End If
			If LRamps(CurrentPlayer)>11 And LRamps(CurrentPlayer)<17 Then:LightLock1.State=1:LightLock2.State=1:LightLock3.State=0:End If
			If LRamps(CurrentPlayer)=17 Then:LightLock1.State=1:LightLock2.State=1:LightLock3.State=2:End If
			If LRamps(CurrentPlayer)=18 Then:LightLock1.State=2:LightLock2.State=2:LightLock3.State=2:End If
			'ensure right lock lights are correct
			If RRamps(CurrentPlayer)<5 Then:LightTrapDoor3.State=0:LightTrapDoor2.State=0:LightTrapDoor1.State=0:End If
			If RRamps(CurrentPlayer)=5 Then:LightTrapDoor3.State=2:LightTrapDoor2.State=0:LightTrapDoor1.State=0:End If
			If RRamps(CurrentPlayer)>5 And RRamps(CurrentPlayer)<11 Then:LightTrapDoor3.State=1:LightTrapDoor2.State=0:LightTrapDoor1.State=0:End If
			If RRamps(CurrentPlayer)=11 Then:LightTrapDoor3.State=1:LightTrapDoor2.State=2:LightTrapDoor1.State=0:End If
			If RRamps(CurrentPlayer)>11 And RRamps(CurrentPlayer)<17 Then:LightTrapDoor3.State=1:LightTrapDoor2.State=1:LightTrapDoor1.State=0:End If
			If RRamps(CurrentPlayer)=17 Then:LightTrapDoor3.State=1:LightTrapDoor2.State=1:LightTrapDoor1.State=2:End If
			If RRamps(CurrentPlayer)=18 Then:LightTrapDoor3.State=2:LightTrapDoor2.State=2:LightTrapDoor1.State=2:End If
			'if there is a song left to start for the first time, then do the default handle mode
			HandleSongNumberLamps(LastSongPlayed(CurrentPlayer))
		End If
	End If
End Sub

Sub HandleSongNumberLamps(MN) 'used to initialize a new song setup
	If MN<>11 Then
		FollowMe.Enabled=0
		If RandomAwardAvailable(CurrentPlayer)>0 Then RandomAward.State=2 'If Random Award is available for this player, blink the light - only if it's not follow me mode
	End If
	Select Case MN
		Case 1:LightProgress2.State=2:LightProgress1.State=0:LightProgress3.State=0:LightProgress4.State=0:LightProgress5.State=0:LightProgress6.State=0:LightProgress7.State=0: 'Super Spinner
		Case 2:LightLeftInlane.State=2:LightRollover1.State=2:LightRollover2.State=2:LightRollover3.State=2:LightRightInlane1.State=2:LightRightInlane2.State=2'Albums Lanes
		Case 3:Bumper1L.State=2:Bumper2L.State=2:Bumper3L.State=2'Super Pops
		Case 4:LightProgress4.State=2:LightProgress6.State=2:LightProgress1.State=0:LightProgress2.State=0:LightProgress3.State=0:LightProgress5.State=0:LightProgress7.State=0 'Ramps
		Case 5:If Drop4.IsDropped=0 Then:LightBand4.State=2:Else:Lightband4.State=1:End If 'BAND Drop Targets
				If Drop3.IsDropped=0 Then:LightBand3.State=2:Else:Lightband3.State=1:End If
				If Drop2.IsDropped=0 Then:LightBand2.State=2:Else:Lightband2.State=1:End If
				If Drop1.IsDropped=0 Then:LightBand1.State=2:Else:Lightband1.State=1:End If
		Case 6:LightRStand1.State=2:LightRStand2.State=2:LightRStand3.State=2'Red Standups
		Case 7:LightProgress2.State=2:LightProgress7.State=2:LightProgress1.State=0:LightProgress3.State=0:LightProgress4.State=0:LightProgress5.State=0:LightProgress6.State=0:Gate.Open=TRUE 'Orbits
		Case 8:LightProgress3.State=2:LightProgress5.State=2:LightProgress1.State=0:LightProgress2.State=0:LightProgress4.State=0:LightProgress6.State=0:LightProgress7.State=0:Gate1.Open=TRUE:Gate4.Open=TRUE 'Super Loops
		Case 9:LightProgress1.State=2:LightProgress2.State=2:LightProgress3.State=2:LightProgress4.State=2:LightProgress5.State=2:LightProgress6.State=2:LightProgress7.State=2 'combos - all progress lamps
		Case 10:LightYStand1.State=2:LightYStand2.State=2:LightYStand3.State=2 'mini Yellow targets
		Case 11:LightProgress1.State=0:LightProgress2.State=0:LightProgress3.State=0:LightProgress4.State=0:LightProgress5.State=0:LightProgress6.State=0:LightProgress7.State=0:FollowMe.Enabled=1:FollowMe_Timer'song notes follow me mode
		Case 12:'frenzy mode
	End Select
End Sub

Sub HandleSongNumberLampsOff 'used to initialize a new song setup
	Gate.Open=FALSE:Gate1.Open=FALSE:Gate4.Open=FALSE
	FollowMe.Enabled=0:ComboTimer_Timer
	SP1=0:SP2=0:SP3=0:SP4=0:SP5=0:SP6=0:SP7=0
	LightJackpot1.State=0:LightJackpot2.State=0:LightJackpot3.State=0:LightJackpot4.State=0:LightJackpot5.State=0:LightJackpot6.State=0:LightJackpot7.State=0
	LightProgress1.State=0:LightProgress2.State=0:LightProgress3.State=0:LightProgress4.State=0:LightProgress5.State=0:LightProgress6.State=0:LightProgress7.State=0
	LightLeftInlane.State=0:LightRollover1.State=0:LightRollover2.State=0:LightRollover3.State=0:LightRightInlane1.State=0:LightRightInlane2.State=0
	Bumper1L.State=0:Bumper2L.State=0:Bumper3L.State=0
	If Drop1.IsDropped Or Drop2.IsDropped Or Drop3.IsDropped Or Drop4.IsDropped Then PlaySound SoundFXDOF("fx_target",228,DOFPulse,DOFContactors)
	LightBand1.State=0:LightBand2.State=0:LightBand3.State=0:LightBand4.State=0:Drop1.IsDropped=0:Drop2.IsDropped=0:Drop3.IsDropped=0:Drop4.IsDropped=0
	LightRStand1.State=0:LightRStand2.State=0:LightRStand3.State=0
	LightYStand1.State=0:LightYStand2.State=0:LightYStand3.State=0
End Sub

Sub SaveLampStates
	For X=1 To 35 'only save first 35 lamps
		LampState(X,CurrentPlayer)=LampList(X).State
	Next
	If bTilted=FALSE Then
		If LightKickback.State=1 Then
			LampState(36,CurrentPlayer)=1
		Else
			LampState(36,CurrentPlayer)=0
		End If
	End If
	LampState(25,CurrentPlayer)=0 'band1
	LampState(26,CurrentPlayer)=0 'band2
	LampState(27,CurrentPlayer)=0 'band3
	LampState(28,CurrentPlayer)=0 'band4
	For X=1 To 7
		JackpotState(X,CurrentPlayer)=0 'never save jackpot lamps
		ProgressState(X,CurrentPlayer)=ProgressLamps(X).State
	Next
End Sub

Sub RestoreLampStates
	For X=1 To 36
		LampList(X).State=LampState(X,CurrentPlayer)
	Next
	If MusicNumber=5 Then
		If Drop1.IsDropped=0 Then LightBand1.State=2
		If Drop2.IsDropped=0 Then LightBand2.State=2
		If Drop3.IsDropped=0 Then LightBand3.State=2
		If Drop4.IsDropped=0 Then LightBand4.State=2
	End If
	If Drop1.IsDropped=1 Then LightBand1.State=1
	If Drop2.IsDropped=1 Then LightBand2.State=1
	If Drop3.IsDropped=1 Then LightBand3.State=1
	If Drop4.IsDropped=1 Then LightBand4.State=1
	For X=1 To 7
		If ProgressState(X,CurrentPlayer)<>0 Then
			JackpotLamps(X).State=0
			ProgressLamps(X).State=0 'bring lamp to the front
			ProgressLamps(X).State=ProgressState(X,CurrentPlayer) 'set lamp to saved state
		End If
	Next
End Sub

Sub ChangeAllLamps(val,numoflamps) '0=off, 1=on, 2=blinking - this includes all lamps EXCEPT Jackpot lamps
	For X=1 To numoflamps
		LampList(X).State=val
	Next
	For X=1 To 7
		JackpotLamps(X).State=0 'ensure the jackpot lamps are off
		ProgressLamps(X).State=0 'bring to front
		ProgressLamps(X).State=val 'turn on or off
	Next
End Sub

Sub LightSeq1_PlayDone 'load or reload the light sequencer attract mode effects
	LightSeq1.StopPlay
	LightSeq1.UpdateInterval=5
	LightSeq1.Play SeqMiddleOutVertOn,75,4,200
	LightSeq1.UpdateInterval=10
	LightSeq1.Play SeqClockLeftOn,0,1,500
	LightSeq1.Play SeqClockLeftOff,0,1,500
	LightSeq1.Play SeqRightOn,50,1,100
	LightSeq1.Play SeqLeftOn,50,1,100
	LightSeq1.Play SeqRightOn,50,1,100
	LightSeq1.Play SeqLeftOn,50,1,100
	LightSeq1.Play SeqHatch1HorizOn,25,1,100
	LightSeq1.Play SeqHatch2HorizOn,25,1,100
	LightSeq1.Play SeqHatch1HorizOn,25,1,100
	LightSeq1.Play SeqHatch2HorizOn,25,1,100
	LightSeq1.Play SeqHatch1HorizOn,25,1,100
	LightSeq1.Play SeqHatch2HorizOn,25,1,100
	LightSeq1.Play SeqHatch1HorizOn,25,1,100
	LightSeq1.Play SeqHatch2HorizOn,25,1,100
	LightSeq1.Play SeqHatch1VertOn,25,1,100
	LightSeq1.Play SeqHatch2VertOn,25,1,100
	LightSeq1.Play SeqHatch1VertOn,25,1,100
	LightSeq1.Play SeqHatch2VertOn,25,1,100
	LightSeq1.Play SeqHatch1VertOn,25,1,100
	LightSeq1.Play SeqHatch2VertOn,25,1,100
	LightSeq1.Play SeqHatch1VertOn,25,1,100
	LightSeq1.Play SeqHatch2VertOn,25,1,100
End Sub

Sub LightSeq2_PlayDone 'load or reload the light sequencer attract mode effects
	LightSeq2.Play SeqRandom,15,0,4000
End Sub

'graphic equalizer coding for wizard mode
Sub GraphicEQ_Timer 'beats at 8 times/second (125ms)
	If bTilted=TRUE Then
		GraphicEQ.Enabled=0
		Exit Sub
	End If
	PXZ1=INT(RND*5) 'generate a value from 0 to 4
	Select Case PXZ1
		Case 0:For X=1 To 4:EQLights(X-1).State=0:Next 'turn all 4 track lights off
		Case 1:LightTrack4.State=1:For X=2 To 4:EQLights(X-1).State=0:Next 'turn off top 3 track lights
		Case 2:LightTrack4.State=1:LightTrack3.State=1:LightTrack2.State=0:LightTrack1.State=0 'half and half
		Case 3:For X=1 To 3:EQLights(X-1).State=1:Next:LightTrack1.State=0 'turn on first 3 track lights
		Case 4:For X=1 To 4:EQLights(X-1).State=1:Next 'turn on all 4 track lights
	End Select
	PXZ4=INT(RND*100)'generates value from 0 to 99
	If PXZ4<50 Then 'only update this one half of the time for mid-range
		PXZ2=INT(RND*5) 'generate a value from 0 to 4
		Select Case PXZ2
			Case 0:For X=5 To 8:EQLights(X-1).State=0:Next 'turn all 4 track lights off
			Case 1:LightTrack8.State=1:For X=6 To 8:EQLights(X-1).State=0:Next 'turn off top 3 track lights
			Case 2:LightTrack8.State=1:LightTrack7.State=1:LightTrack6.State=0:LightTrack5.State=0 'half and half
			Case 3:For X=5 To 7:EQLights(X-1).State=1:Next:LightTrack5.State=0 'turn on first 3 track lights
			Case 4:For X=5 To 8:EQLights(X-1).State=1:Next 'turn on all 4 track lights
		End Select
	End If
	PXZ3=INT(RND*5) 'generate a value from 0 to 4
	Select Case PXZ3
		Case 0:For X=9 To 12:EQLights(X-1).State=0:Next 'turn all 4 track lights off
		Case 1:LightTrack12.State=1:For X=10 To 12:EQLights(X-1).State=0:Next 'turn off top 3 track lights
		Case 2:LightTrack12.State=1:LightTrack11.State=1:LightTrack10.State=0:LightTrack9.State=0 'half and half
		Case 3:For X=9 To 11:EQLights(X-1).State=1:Next:LightTrack9.State=0 'turn on first 3 track lights
		Case 4:For X=9 To 12:EQLights(X-1).State=1:Next 'turn on all 4 track lights
	End Select
End Sub

'Turntable class taken from CORE.VBS
Class cTurntable
	Private cX,cY,cSize,cMaxSpeed,cSpinUp,cSpinDown
	Private cTempX,cTempY,cCurspeed,cMotorOn,cClockwise
	Private cBalls
	
	Private Sub Class_Initialize
		Set cBalls=CreateObject("Scripting.Dictionary")
		cMotorOn=FALSE
		cCurSpeed=0
		cClockwise=TRUE
		cSpinUp=10
		cSpinDown=4
	End Sub
	
	Public Sub InitTurntable(aTrigger,inSpeed,inCW)
		cX=aTrigger.X
		cY=aTrigger.Y
		cSize=aTrigger.Radius
		cMaxSpeed=ABS(inSpeed)
		cClockwise=(inCW=TRUE)
	End Sub
	
	Public Property Let Speed(inSpeed)
		cMaxSpeed=ABS(inSpeed)
		cClockwise=(inSpeed>=0)
	End Property
	
	Public Property Let Clockwise(inCW):cClockwise=(inCW=TRUE):End Property
	Public Property Let SpinUp(inSpinUp):cSpinUp=inSpinUp:End Property
	Public Property Let SpinDown(inSpinDown):cSpinDown=inSpinDown:End Property
	
	Public Property Get MaxSpeed:MaxSpeed=cSpeed:End Property
	Public Property Get Speed:Speed=cCurSpeed:End Property
	Public Property Get MotorState:MotorState=cMotorOn:End Property
	
	Public Sub MotorOn:cMotorOn=TRUE:End Sub
	Public Sub MotorOff:cMotorOn=FALSE:End Sub
	
	Public Sub AddBall(aBall):cBalls.Add aBall,0:End Sub
	Public Sub RemoveBall(aBall):cBalls.Remove(aBall):End Sub  
	
	Public Sub ProcessBalls()
		Dim tempObj
		For Each tempObj In cBalls.Keys:AffectBall tempObj:Next
	End Sub
	
	Public Sub ComputeSpin()
		If cMotorOn Then
			If cCurSpeed<cMaxSpeed Then
				cCurSpeed=cCurSpeed+cSpinUp/100
				If cCurSpeed>cMaxSpeed Then cCurSpeed=cMaxSpeed
			End If
		Else
			If cCurSpeed>0 Then
				cCurSpeed=cCurSpeed-cSpinDown/100
				If cCurSpeed<0 Then cCurSpeed=0
			End If
		End If
	End Sub
	
	Public Function GetDist(aBall)
		If aBall Is Nothing Then
			GetDist=100000
		Else
			cTempX=aBall.X-cX
			cTempY=aBall.Y-cY
			GetDist=Sqr(cTempX*cTempX+cTempY*cTempY)
		End If
	End Function
	
	Public Sub AffectBall(aBall)
		If aBall Is Nothing Then Exit Sub
		If cCurSpeed>0 Then
			Dim Dist
			Dist=GetDist(aBall)
			If Dist>cSize Then Exit Sub
			' Spin ball in direction of turntable motion.  Speed > 0 = clockwise.
			' Step 2: Determine amount of force to be applied to ball.
			' Determined by distance and current speed of turntable.
			Dim Force
			Force=(Dist*cCurSpeed/8000)
			If cClockWise Then Force=-Force
			aBall.VelX=aBall.VelX+(cTempY*Force/Dist)
			aBall.VelY=aBall.VelY-(cTempX*Force/Dist)
		End If
	End Sub
End Class

'CODE **************************************************************************************************************************************

Sub QTimer_Timer
	QTimer.Enabled=0:Timer1.Enabled=0
	DisplayScore
End Sub

Sub DisplayText(Nu,L,S) '2240 interval
	QTimer.Enabled=0:QTimer.Enabled=1
	LPA(1)=ASC(Mid(Nu,1,1)) 
	LPA(2)=ASC(Mid(Nu,2,1)) 
	LPA(3)=ASC(Mid(Nu,3,1)) 
	LPA(4)=ASC(Mid(Nu,4,1)) 
	LPA(5)=ASC(Mid(Nu,5,1)) 
	LPA(6)=ASC(Mid(Nu,6,1)) 
	LPA(7)=ASC(Mid(Nu,7,1)) 
	LPA(8)=ASC(Mid(Nu,8,1)) 
	LPA(9)=ASC(Mid(Nu,9,1)) 
	LPA(10)=ASC(Mid(Nu,10,1)) 
	LPA(11)=ASC(Mid(Nu,11,1)) 
	LPA(12)=ASC(Mid(Nu,12,1)) 
	LPA(13)=ASC(Mid(Nu,13,1)) 
	LPA(14)=ASC(Mid(Nu,14,1)) 
	SortNameD LPA(1),1 
	SortNameD LPA(2),2 
	SortNameD LPA(3),3 
	SortNameD LPA(4),4 
	SortNameD LPA(5),5 
	SortNameD LPA(6),6 
	SortNameD LPA(7),7 
	SortNameD LPA(8),8  
	SortNameD LPA(9),9 
	SortNameD LPA(10),10 
	SortNameD LPA(11),11 
	SortNameD LPA(12),12 
	SortNameD LPA(13),13 
	SortNameD LPA(14),14 
	For Q=1 To 14
		Select Case CurrentPlayer
			Case 0:DD Q+(L-1)*14,LPA(Q),S
			Case 1:DD 28+Q+(L-1)*14,LPA(Q),S
			Case 2:DD 14+Q-(L-1)*14,LPA(Q),S
			Case 3:DD 42+Q-(L-1)*14,LPA(Q),S
		End Select
	Next
End Sub

Sub DisplayPartialText(Nu,L,S) 'ONLY used in attract mode - currentplayer isn't affected
	QTimer.Enabled=0:QTimer.Enabled=1
	Dim PartLen
	PartLen=Len(Nu)
	LPA(1)=0
	LPA(2)=0 
	LPA(3)=0
	LPA(4)=0
	LPA(5)=0 
	LPA(6)=0 
	LPA(7)=0 
	LPA(8)=0
	LPA(9)=0 
	LPA(10)=0 
	LPA(11)=0 
	LPA(12)=0 
	LPA(13)=0 
	LPA(14)=0
	If PartLen>0 Then:LPA(1)=ASC(Mid(Nu,1,1)):SortNameD LPA(1),1:End If
	If PartLen>1 Then:LPA(2)=ASC(Mid(Nu,2,1)):SortNameD LPA(2),2:End If
	If PartLen>2 Then:LPA(3)=ASC(Mid(Nu,3,1)):SortNameD LPA(3),3:End If
	If PartLen>3 Then:LPA(4)=ASC(Mid(Nu,4,1)):SortNameD LPA(4),4:End If
	If PartLen>4 Then:LPA(5)=ASC(Mid(Nu,5,1)):SortNameD LPA(5),5:End If
	If PartLen>5 Then:LPA(6)=ASC(Mid(Nu,6,1)):SortNameD LPA(6),6:End If
	If PartLen>6 Then:LPA(7)=ASC(Mid(Nu,7,1)):SortNameD LPA(7),7:End If
	If PartLen>7 Then:LPA(8)=ASC(Mid(Nu,8,1)):SortNameD LPA(8),8:End If
	If PartLen>8 Then:LPA(9)=ASC(Mid(Nu,9,1)):SortNameD LPA(9),9:End If
	If PartLen>9 Then:LPA(10)=ASC(Mid(Nu,10,1)):SortNameD LPA(10),10:End If
	If PartLen>10 Then:LPA(11)=ASC(Mid(Nu,11,1)):SortNameD LPA(11),11:End If
	If PartLen>11 Then:LPA(12)=ASC(Mid(Nu,12,1)):SortNameD LPA(12),12:End If
	If PartLen>12 Then:LPA(13)=ASC(Mid(Nu,13,1)):SortNameD LPA(13),13:End If
	If PartLen>13 Then:LPA(14)=ASC(Mid(Nu,14,1)):SortNameD LPA(14),14:End If
	For Q=1 To 14:DD Q + ((L-1)*14),LPA(Q),S:Next
End Sub

Sub DisplayText3(Nu,L,S) 'requires manual redisplay of score
On ERROR Resume Next
	LPA(1)=ASC(Mid(Nu,1,1)) 
	LPA(2)=ASC(Mid(Nu,2,1)) 
	LPA(3)=ASC(Mid(Nu,3,1))
	LPA(4)=ASC(Mid(Nu,4,1))
	LPA(5)=ASC(Mid(Nu,5,1))
	LPA(6)=ASC(Mid(Nu,6,1))
	LPA(7)=ASC(Mid(Nu,7,1))
	LPA(8)=ASC(Mid(Nu,8,1))
	LPA(9)=ASC(Mid(Nu,9,1))
	LPA(10)=ASC(Mid(Nu,10,1))
	LPA(11)=ASC(Mid(Nu,11,1))
	LPA(12)=ASC(Mid(Nu,12,1))
	LPA(13)=ASC(Mid(Nu,13,1))
	LPA(14)=ASC(Mid(Nu,14,1))
	SortNameD LPA(1),1 
	SortNameD LPA(2),2
	SortNameD LPA(3),3
	SortNameD LPA(4),4
	SortNameD LPA(5),5
	SortNameD LPA(6),6
	SortNameD LPA(7),7
	SortNameD LPA(8),8
	SortNameD LPA(9),9
	SortNameD LPA(10),10
	SortNameD LPA(11),11
	SortNameD LPA(12),12
	SortNameD LPA(13),13
	SortNameD LPA(14),14
	For Q=1 To 14
		Select Case CurrentPlayer
			Case 0:DD Q+(L-1)*14,LPA(Q),S
			Case 1:DD 28+Q+(L-1)*14,LPA(Q),S
			Case 2:DD 14+Q-(L-1)*14,LPA(Q),S
			Case 3:DD 42+Q-(L-1)*14,LPA(Q),S
		End Select
	Next
On Error Goto 0
End Sub

Sub Table1_Exit

	SaveLUT

	If Not UltraDMD is Nothing Then
		If UltraDMD.IsRendering Then
			UltraDMD.CancelRendering
		End If	
		UltraDMD = NULL 
	End If

	Dim i
	For i=0 To 3
		SaveValue cGameSaveName,"HighScore"&i,gsHighScore(i)
		SaveValue cGameSaveName,"HighScoreName"&i,gsHighScoreName(i)
	Next
	For i=1 To 12 'individual track records
		SaveValue cGameSaveName,"Track"&i&"Score",gsTrackScore(i)
		SaveValue cGameSaveName,"Track"&i&"ScoreName",gsTrackScoreName(i)
	Next
	For i=0 To 3'last game scores
		SaveValue cGameSaveName,"OldScore"&i,Score(i)
	Next
	If gvCombosforComboChamp>999 Then gvCombosforComboChamp=999
	SaveValue cGameSaveName,"NotesForSongSelect",gsNotesForSongSelect
	SaveValue cGameSaveName,"ComboChamp",gvCombosForComboChamp
	SaveValue cGameSaveName,"ComboChampName",gsComboChampName
	SaveValue cGameSaveName,"GamesPlayed",gsGamesPlayed
	SaveValue cGameSaveName,"Credits",gsCredits
	SaveValue cGameSaveName,"GameDifficulty",gsGameDifficulty
	SaveValue cGameSaveName,"BallsPerGame",gsBallsPerGame
	SaveValue cGameSaveName,"FreePlay",gsFreePlay
	SaveValue cGameSaveName,"MatchPer",gsMatchPer
	SaveValue cGameSaveName,"TiltWarnings",gsTiltWarnings
	SaveValue cGameSaveName,"ReplayStart",gsReplayStart
	SaveValue cGameSaveName,"ReplayLevels",gsReplayLevels
	If B2SOn Then 
		Controller.Stop 
	End If
End Sub

Sub Table1_KeyDown(ByVal KeyCode)
'LUT controls
If keycode = LeftMagnaSave Then bLutActive = True
    If keycode = RightMagnaSave Then
            If bLutActive Then
                    if DisableLUTSelector = 0 then
                LUTSet = LUTSet  - 1
                if LutSet < 0 then LUTSet = 16
                SetLUT
                ShowLUT
            End If
        End If
        End If
	If WizardModeDisplaysActive Then Exit Sub
	If Not DelayTimer.Enabled Then
		If ATimer.Enabled Then 'Attract mode key handling
			If KeyCode=LeftFlipperKey Then UpdateCounters(FALSE)
			If KeyCode=RightFlipperKey Then UpdateCounters(TRUE)
		End If
	End If
	If SongSelectInProgress=TRUE Then
		TimerStatusDisplay.Enabled=0
		STimer.Enabled=0
		gvStatusModeActive=FALSE
		If KeyCode=StartGameKey Or KeyCode=PlungerKey Then 'Song has been chosen!
			If TempSongSelect<>PriorSong Then
				OldMusicCompleted(MusicNumber,CurrentPlayer,0)=MusicCompleted(TempSongSelect,CurrentPlayer,0)
				MusicScoreTemp(CurrentPlayer,TempSongSelect)=0 'reset temporary score for current player for this song - special case random award will not do this
				FollowMe.Interval=4000
				FollowMeVal=-1 'prepare to start with lamp 0 for strafe mode
				If PriorSong=11 And MusicCompleted2(11,CurrentPlayer,0)=2 Then
					FollowMeDifficulty(CurrentPlayer)=2' player completed song successfully, so increase difficulty for this one
					FollowMe.Interval=2000
					FollowMeCounter(CurrentPlayer)=0 'reset completion counter
				End If
				If PriorSong=11 And MusicCompleted2(11,CurrentPlayer,0)=1 Then
					FollowMeDifficulty(CurrentPlayer)=1
					FollowMeCounter(CurrentPlayer)=0 'reset completion counter
				End If
				'Song is changing - commit new data
				MusicPlayed(TempSongSelect,CurrentPlayer,0)=1
				LastSongPlayed(CurrentPlayer)=TempSongSelect
				PlayMusic TrackFilename(LastSongPlayed(CurrentPlayer))
				MusicNumber=LastSongPlayed(CurrentPlayer)
				DispText " "&TempSongSelect&" - "&TrackFilename(MusicNumber)
				'song was just 'changed', so clear out any note counts for the player
			End If
			TrackLights(TempSongSelect).State=0
			TrackLights(TempSongSelect).BlinkPattern="10"
			TrackLights(TempSongSelect).BlinkInterval=125
			TrackLights(TempSongSelect).State=0
			TrackLights(TempSongSelect).State=2
			For X=1 To 7
				NoteCount(CurrentPlayer,X)=0
			Next
			TrapDoorStop.IsDropped=1:TrapDoorRamp.Collidable=1:TrapDoorWall.IsDropped=0 'ensure entry is closed off for normal play
			LightNote1.State=2:LightNote2.State=2:LightNote3.State=2:LightNote4.State=2:LightNote5.State=2:LightNote6.State=2:LightNote7.State=2
			Skill1.State=0
			UpdateMusicLamps
			HandleSongNumberLamps(MusicNumber)
			'fix all lights for completion states
			If MusicNumber=1 And MusicScoreTemp(CurrentPlayer,MusicNumber)>=700000 Then LightTrack1.State=1
			If MusicNumber=2 And MusicScoreTemp(CurrentPlayer,MusicNumber)>=680000 Then LightTrack2.State=1
			If MusicNumber=3 And PopHitsTemp(CurrentPlayer)>99 Then LightTrack3.State=1
			If MusicNumber=4 And MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 Then LightTrack4.State=1
			If MusicNumber=5 And MusicScoreTemp(CurrentPlayer,MusicNumber)>=750000 Then LightTrack5.State=1
			If MusicNumber=6 And MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 Then LightTrack6.State=1
			If MusicNumber=7 And MusicScoreTemp(CurrentPlayer,MusicNumber)>=700000 Then LightTrack7.State=1
			If MusicNumber=8 And MusicScoreTemp(CurrentPlayer,MusicNumber)>=700000 Then LightTrack8.State=1
			If MusicNumber=9 And MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 Then LightTrack9.State=1
			If MusicNumber=10 And MusicScoreTemp(CurrentPlayer,MusicNumber)>=300000 Then LightTrack10.State=1
			If MusicNumber=11 And FollowMeCounter(CurrentPlayer)>6 Then LightTrack11.State=1
			If MusicNumber=12 And MusicScoreTemp(CurrentPlayer,MusicNumber)>=1000000 Then LightTrack12.State=1
			CheckModeComplete 'check for any applicable wizard modes
			SongSelectInProgress=FALSE
			DMD_SetScoreboardBackground ""
			AddScore 0
			RandomAward.State=2
			SubwayExit.Enabled=1
			Exit Sub
		End If
		If KeyCode=LeftFlipperKey Then 'cycle down through songs that have NOT been completed yet
			HandleSongNumberLampsOff
			TrackLights(TempSongSelect).State=0
			TrackLights(PriorSong).State=0
			TrackLights(PriorSong).BlinkInterval=125
			TrackLights(PriorSong).BlinkPattern="101100"
			TrackLights(PriorSong).State=2
			For X=1 To 12
				TempSongSelect=TempSongSelect-1 'decrement by 1
				If TempSongSelect<1 Then TempSongSelect=12 'rollover past upper bounds
				If MusicPlayed(TempSongSelect,CurrentPlayer,0)=0 Or TempSongSelect=PriorSong Then Exit For
			Next
			If TempSongSelect=11 Then FollowMeVal=-1 'prepare to start with lamp 0 for strafe mode
			DispText " "&TempSongSelect&" - "&TrackFilename(TempSongSelect):DMD_DisplaySongSelect
			HandleSongNumberLamps(TempSongSelect)
			TrackLights(TempSongSelect).State=0
			TrackLights(TempSongSelect).BlinkPattern="10"
			TrackLights(TempSongSelect).BlinkInterval=75
			TrackLights(TempSongSelect).State=2
			Exit Sub
		End If
		If KeyCode=RightFlipperKey Then 'cycle up through songs that have NOT been completed yet
			HandleSongNumberLampsOff
			TrackLights(TempSongSelect).State=0
			TrackLights(PriorSong).State=0
			TrackLights(PriorSong).BlinkInterval=125
			TrackLights(PriorSong).BlinkPattern="101100"
			TrackLights(PriorSong).State=2
			For X=1 To 12
				TempSongSelect=TempSongSelect+1 'increment by 1
				If TempSongSelect>12 Then TempSongSelect=1 'rollover past upper bounds
				If MusicPlayed(TempSongSelect,CurrentPlayer,0)=0 Or TempSongSelect=PriorSong Then Exit For
			Next
			If TempSongSelect=11 Then FollowMeVal=-1 'prepare to start with lamp 0 for strafe mode
			DispText " "&TempSongSelect&" - "&TrackFilename(TempSongSelect):DMD_DisplaySongSelect
			HandleSongNumberLamps(TempSongSelect)
			TrackLights(TempSongSelect).State=0
			TrackLights(TempSongSelect).BlinkPattern="10"
			TrackLights(TempSongSelect).BlinkInterval=75
			TrackLights(TempSongSelect).State=2
			Exit Sub
		End If
	End If
	If Not DelayTimer.Enabled And Not EnterMenuTimer.Enabled Then
		Dim TempState,RndNudge,i
		' If slam tilted then don't process any keys
		If Timer_SlamTilt.Enabled Then Exit Sub
		' can add a credit at any time
		If KeyCode=16777267 Or KeyCode=AddCreditKey Then
			PlaySound"fx_Coin"
			If gsCredits<40 Then
				gsCredits=gsCredits+1
				DOF 209, DOFOn
				If PlayersPlayingGame=1 And gsFreePlay=FALSE And gvFirstBallEjected=FALSE Then FlashInsertCoin
				' don't update display while entering in a high score
				If hsbModeActive=FALSE And bMenuModeActive=FALSE Then DisplayCredits
			End If
		End If
	End If
	' open or shut the door (When game is not active)
	If KeyCode=207 Then'END
		If bGameInPlay=FALSE Then
			If bDoorOpen=FALSE Then
				' flush the display queue
				DisplayFlushQueue
				DelayTimer.Enabled=0
				ATimer.Enabled=0
				RandomCallout.Enabled=0
				LightSeq1.StopPlay
				' and display door is open
				PlaySound"ZWilliamsMessage"
				DD 1,0,1:DD 2,0,1:DD 3,3,1:DD 4,15,1:DD 5,9,1:DD 6,14,1:DD 7,0,1:DD 8,4,1:DD 9,15,1:DD 10,15,1:DD 11,18,1:DD 12,0,1:DD 13,0,1:DD 14,0,1
				DD 29,0,1:DD 30,0,1:DD 31,3,1:DD 32,15,1:DD 33,9,1:DD 34,14,1:DD 35,0,1:DD 36,4,1:DD 37,15,1:DD 38,15,1:DD 39,18,1:DD 40,0,1:DD 41,0,1:DD 42,0,1
				DD 15,0,1:DD 16,0,1:DD 17,0,1:DD 18,9,1:DD 19,19,1:DD 20,0,1:DD 21,15,1:DD 22,16,1:DD 23,5,1:DD 24,14,1:DD 25,0,1:DD 26,0,1:DD 27,0,1:DD 28,0,1
				DD 43,0,1:DD 44,0,1:DD 45,0,1:DD 46,9,1:DD 47,19,1:DD 48,0,1:DD 49,15,1:DD 50,16,1:DD 51,5,1:DD 52,14,1:DD 53,0,1:DD 54,0,1:DD 55,0,1:DD 56,0,1
				bDoorOpen=TRUE
				DMD_DisplaySceneText "COIN DOOR", "IS OPEN"
			Else
				DisplayFlushQueue
				PH1=0
				ATimer.Enabled=1
				RandomCalloutCount=0
				RandomCallout.Enabled=1
				LightSeq1_PlayDone
				bDoorOpen=FALSE
			End If
		End If
	End If
	' operator ESC/Service key
	If KeyCode=constEscKey Then
		If EnterMenuTimer.Enabled=0 Then
			If bDoorOpen Then
				If bMenuModeActive Then
					' If in operator menu entry mode
					OpMenuProcessKey(KeyCode)
				End If
			End If
		End If
	End If
	' operator Previous
	If KeyCode=LeftFlipperKey Then
		If EnterMenuTimer.Enabled=0 Then
			If bDoorOpen Then
				If bMenuModeActive Then
					' If in operator menu entry mode
					OpMenuProcessKey(KeyCode)
					Exit Sub
				End If
			End If
		End If
	End If
	' operator Next
	If KeyCode=RightFlipperKey Then
		If EnterMenuTimer.Enabled=0 Then
			If bDoorOpen Then
				If bMenuModeActive Then
					' If in operator menu entry mode
					OpMenuProcessKey(KeyCode)
					Exit Sub
				End If
			End If
		End If
	End If
	' operator Enter/Menu key
	If KeyCode=StartGameKey Then
		If EnterMenuTimer.Enabled=0 Then
			If bDoorOpen Then
				If bMenuModeActive Then
					' If in operator menu entry mode
					OpMenuProcessKey(KeyCode)
				Else
					' Else go into operator menu mode
					PlayfieldScoreTimer_Timer
					ResetTable
					' ensure there is no game going
					bGameInPlay=FALSE
					' Set the tilt flag so nothing works on the playfield
					bTilted=TRUE
					EndMusic
					If MultiballSave.Enabled Then MultiballSave_Timer
					BGate.Open=TRUE:CGate.Open=TRUE
					If HurrySmall.Enabled Then
						MiniHPValReal=0
						HurrySmall_Timer
					End If
					DisplayFlushQueue
					If LightWizardMode.State>0 Then
						LightWizardMode.State=0
						BeginWizardShow.Enabled=0
					End If
					OpMenuInit
					Exit Sub
				End If
			End If
		End If
	End If
	If KeyCode=LeftTiltKey And bTilted=FALSE Then
		RndNudge=Rnd(1)/2 ' random number between .0 and .49
		RndNudge=RndNudge+2.25 ' make it between 2.25 and 2.749
		Nudge 75,RndNudge
		If hsbModeActive=FALSE And bGameInPlay Then AddToTilt(RndNudge)
	End If
	If KeyCode=RightTiltKey Then
		RndNudge=Rnd(1)/2
		RndNudge=RndNudge+2.25
		Nudge 285,RndNudge
		If hsbModeActive=FALSE And bGameInPlay Then AddToTilt(RndNudge)
	End If
	If KeyCode=CenterTiltKey Then
		RndNudge=Rnd(1)/2
		RndNudge=RndNudge+2.25
		Nudge 0,RndNudge
		If hsbModeActive=FALSE And bGameInPlay Then AddToTilt(RndNudge)
	End If
	If KeyCode=constBangBackKey Then
		RndNudge=Rnd(1)/2
		RndNudge=RndNudge+4.5
		Nudge 0,RndNudge
		If hsbModeActive=FALSE And bGameInPlay Then AddToTilt(RndNudge)
	End If
	' can slam tilt the machine at any time
	If KeyCode=199 Then   ' HOME (slam tilt)
		PlayfieldScoreTimer_Timer
		If LightKickback.State=1 Then
			LampState(36,CurrentPlayer)=1
		Else
			LampState(36,CurrentPlayer)=0
		End If
		ResetTable
		PlaySound"ZTilt"
		bTilted=TRUE
		EndMusic
		If MultiballSave.Enabled Then MultiballSave_Timer
		BGate.Open=TRUE:CGate.Open=TRUE
		If HurrySmall.Enabled Then
			MiniHPValReal=0
			HurrySmall_Timer
		End If
		If LightWizardMode.State>0 Then
			LightWizardMode.State=0
			BeginWizardShow.Enabled=0
		End If
		' flush the display queues
		For Q=1 To 56
			DD Q,117,1
		Next
		ATimer.Enabled=0
		RandomCallout.Enabled=0
		LightSeq1.StopPlay
		' ensure everything else is reset
		bGameInPlay=FALSE
		bMenuModeActive=FALSE
		hsbModeActive=FALSE
		' start the slam tilt timer which waits for all balls to return
		' before restarting the attract sequence
		Timer_SlamTilt.Interval=2000
		Timer_SlamTilt.Enabled=1
	End If
	' the next set of keys only work if there is a game in progress
	' is a game in progress?
	If bGameInPlay Then
		' not entering a high score
		If hsbModeActive=FALSE And bMenuModeActive=FALSE Then
			' another player starting?
			If KeyCode=StartGameKey And bTilted=FALSE Then    '1
				' free play or credits
				If gvFirstBallPlayed=FALSE Then
					If PlayersPlayingGame=1 Then
						For Q=29 To 56:DD Q,0,1:Next
					End If
					If gsFreePlay Then
						If PlayersPlayingGame<4 Then
							PlayersPlayingGame=PlayersPlayingGame+1
							' update the stats
							gsGamesPlayed=gsGamesPlayed+1
							' If the game is started force the new player's score to be displayed
							If gsBallsPerGame+1-BallsRemaining(0)=1 Then
								If PlayersPlayingGame=2 Then
									DisplayPoints(Score(1))
									DD 42,HS1N+37,1
									If HS3Z>1 Then:DD 41,HS1M+37,1:Else:DD 41,0,1:End If
									If HS3Z>2 Then:DD 40,HS1L+37,1:Else:DD 40,0,1:End If
									If HS3Z>3 Then:DD 39,HS1K+48,1:Else:DD 39,0,1:End If
									If HS3Z>4 Then:DD 38,HS1J+37,1:Else:DD 38,0,1:End If
									If HS3Z>5 Then:DD 37,HS1I+37,1:Else:DD 37,0,1:End If
									If HS3Z>6 Then:DD 36,HS1H+48,1:Else:DD 36,0,1:End If
									If HS3Z>7 Then:DD 35,HS1G+37,1:Else:DD 35,0,1:End If
									If HS3Z>8 Then:DD 34,HS1F+37,1:Else:DD 34,0,1:End If
									If HS3Z>9 Then:DD 33,HS1E+48,1:Else:DD 33,0,1:End If
									If HS3Z>10 Then:DD 32,HS1D+37,1:Else:DD 32,0,1:End If
									If HS3Z>11 Then:DD 31,HS1C+37,1:Else:DD 31,0,1:End If
									If HS3Z>12 Then:DD 30,HS1B+48,1:Else:DD 30,0,1:End If
									If HS3Z>13 Then:DD 29,HS1A+37,1:Else:DD 29,0,1:End If
								End If
								If PlayersPlayingGame=3 Then
									DisplayPoints(Score(2))
									DD 28,HS1N+37,1
									If HS3Z>1 Then:DD 41,HS1M+37,1:Else:DD 27,0,1:End If
									If HS3Z>2 Then:DD 40,HS1L+37,1:Else:DD 26,0,1:End If
									If HS3Z>3 Then:DD 39,HS1K+48,1:Else:DD 25,0,1:End If
									If HS3Z>4 Then:DD 38,HS1J+37,1:Else:DD 24,0,1:End If
									If HS3Z>5 Then:DD 37,HS1I+37,1:Else:DD 23,0,1:End If
									If HS3Z>6 Then:DD 36,HS1H+48,1:Else:DD 22,0,1:End If
									If HS3Z>7 Then:DD 35,HS1G+37,1:Else:DD 21,0,1:End If
									If HS3Z>8 Then:DD 34,HS1F+37,1:Else:DD 20,0,1:End If
									If HS3Z>9 Then:DD 33,HS1E+48,1:Else:DD 19,0,1:End If
									If HS3Z>10 Then:DD 32,HS1D+37,1:Else:DD 18,0,1:End If
									If HS3Z>11 Then:DD 31,HS1C+37,1:Else:DD 17,0,1:End If
									If HS3Z>12 Then:DD 30,HS1B+48,1:Else:DD 16,0,1:End If
									If HS3Z>13 Then:DD 29,HS1A+37,1:Else:DD 15,0,1:End If
								End If
								If PlayersPlayingGame=4 Then
									DisplayPoints(Score(3))
									DD 56,HS1N+37,1
									If HS3Z>1 Then:DD 41,HS1M+37,1:Else:DD 55,0,1:End If
									If HS3Z>2 Then:DD 40,HS1L+37,1:Else:DD 54,0,1:End If
									If HS3Z>3 Then:DD 39,HS1K+48,1:Else:DD 53,0,1:End If
									If HS3Z>4 Then:DD 38,HS1J+37,1:Else:DD 52,0,1:End If
									If HS3Z>5 Then:DD 37,HS1I+37,1:Else:DD 51,0,1:End If
									If HS3Z>6 Then:DD 36,HS1H+48,1:Else:DD 50,0,1:End If
									If HS3Z>7 Then:DD 35,HS1G+37,1:Else:DD 49,0,1:End If
									If HS3Z>8 Then:DD 34,HS1F+37,1:Else:DD 48,0,1:End If
									If HS3Z>9 Then:DD 33,HS1E+48,1:Else:DD 47,0,1:End If
									If HS3Z>10 Then:DD 32,HS1D+37,1:Else:DD 46,0,1:End If
									If HS3Z>11 Then:DD 31,HS1C+37,1:Else:DD 45,0,1:End If
									If HS3Z>12 Then:DD 30,HS1B+48,1:Else:DD 44,0,1:End If
									If HS3Z>13 Then:DD 29,HS1A+37,1:Else:DD 43,0,1:End If
								End If
							Else
								DisplayFlushQueue
								AddScore 0
							End If
							ATimer.Enabled=0
							RandomCallout.Enabled=0
							BTimer.Enabled=0
							MTimer.Enabled=0
							LightSeq1.StopPlay
							DMD_DisplayScoreboard
						End If
					Else  ' playing with credits
						If gsCredits>0 Then
							If PlayersPlayingGame<4 Then
								PlayersPlayingGame=PlayersPlayingGame+1
								' update the stats
								gsGamesPlayed=gsGamesPlayed+1
								gsCredits=gsCredits-1
								If gsCredits < 1 Then DOF 209, DOFPulse End If
								' If the game is started force the new players score to be displayed
								If gsBallsPerGame+1-BallsRemaining(0)=1 Then 'can only add players on the first player's first ball
									If PlayersPlayingGame=2 Then
										DisplayPoints(Score(1))
										DD 42,HS1N+37,1
										If HS3Z>1 Then:DD 41,HS1M+37,1:Else:DD 41,0,1:End If
										If HS3Z>2 Then:DD 40,HS1L+37,1:Else:DD 40,0,1:End If
										If HS3Z>3 Then:DD 39,HS1K+48,1:Else:DD 39,0,1:End If
										If HS3Z>4 Then:DD 38,HS1J+37,1:Else:DD 38,0,1:End If
										If HS3Z>5 Then:DD 37,HS1I+37,1:Else:DD 37,0,1:End If
										If HS3Z>6 Then:DD 36,HS1H+48,1:Else:DD 36,0,1:End If
										If HS3Z>7 Then:DD 35,HS1G+37,1:Else:DD 35,0,1:End If
										If HS3Z>8 Then:DD 34,HS1F+37,1:Else:DD 34,0,1:End If
										If HS3Z>9 Then:DD 33,HS1E+48,1:Else:DD 33,0,1:End If
										If HS3Z>10 Then:DD 32,HS1D+37,1:Else:DD 32,0,1:End If
										If HS3Z>11 Then:DD 31,HS1C+37,1:Else:DD 31,0,1:End If
										If HS3Z>12 Then:DD 30,HS1B+48,1:Else:DD 30,0,1:End If
										If HS3Z>13 Then:DD 29,HS1A+37,1:Else:DD 29,0,1:End If
									End If
									If PlayersPlayingGame=3 Then
										DisplayPoints(Score(2))
										DD 28,HS1N+37,1
										If HS3Z>1 Then:DD 41,HS1M+37,1:Else:DD 27,0,1:End If
										If HS3Z>2 Then:DD 40,HS1L+37,1:Else:DD 26,0,1:End If
										If HS3Z>3 Then:DD 39,HS1K+48,1:Else:DD 25,0,1:End If
										If HS3Z>4 Then:DD 38,HS1J+37,1:Else:DD 24,0,1:End If
										If HS3Z>5 Then:DD 37,HS1I+37,1:Else:DD 23,0,1:End If
										If HS3Z>6 Then:DD 36,HS1H+48,1:Else:DD 22,0,1:End If
										If HS3Z>7 Then:DD 35,HS1G+37,1:Else:DD 21,0,1:End If
										If HS3Z>8 Then:DD 34,HS1F+37,1:Else:DD 20,0,1:End If
										If HS3Z>9 Then:DD 33,HS1E+48,1:Else:DD 19,0,1:End If
										If HS3Z>10 Then:DD 32,HS1D+37,1:Else:DD 18,0,1:End If
										If HS3Z>11 Then:DD 31,HS1C+37,1:Else:DD 17,0,1:End If
										If HS3Z>12 Then:DD 30,HS1B+48,1:Else:DD 16,0,1:End If
										If HS3Z>13 Then:DD 29,HS1A+37,1:Else:DD 15,0,1:End If
									End If
									If PlayersPlayingGame=4 Then
										DisplayPoints(Score(3))
										DD 56,HS1N+37,1
										If HS3Z>1 Then:DD 41,HS1M+37,1:Else:DD 55,0,1:End If
										If HS3Z>2 Then:DD 40,HS1L+37,1:Else:DD 54,0,1:End If
										If HS3Z>3 Then:DD 39,HS1K+48,1:Else:DD 53,0,1:End If
										If HS3Z>4 Then:DD 38,HS1J+37,1:Else:DD 52,0,1:End If
										If HS3Z>5 Then:DD 37,HS1I+37,1:Else:DD 51,0,1:End If
										If HS3Z>6 Then:DD 36,HS1H+48,1:Else:DD 50,0,1:End If
										If HS3Z>7 Then:DD 35,HS1G+37,1:Else:DD 49,0,1:End If
										If HS3Z>8 Then:DD 34,HS1F+37,1:Else:DD 48,0,1:End If
										If HS3Z>9 Then:DD 33,HS1E+48,1:Else:DD 47,0,1:End If
										If HS3Z>10 Then:DD 32,HS1D+37,1:Else:DD 46,0,1:End If
										If HS3Z>11 Then:DD 31,HS1C+37,1:Else:DD 45,0,1:End If
										If HS3Z>12 Then:DD 30,HS1B+48,1:Else:DD 44,0,1:End If
										If HS3Z>13 Then:DD 29,HS1A+37,1:Else:DD 43,0,1:End If
									End If
								Else
									DisplayFlushQueue
									AddScore 0
								End If
								ATimer.Enabled=0
								RandomCallout.Enabled=0
								BTimer.Enabled=0
								MTimer.Enabled=0
								LightSeq1.StopPlay
								DMD_DisplayScoreboard
							End If
						Else
							' not enough credits
							NoCreditSound
							DisplayCredits
						End If
					End If
				End If
			End If
			If KeyCode=PlungerKey Then Plunger.Pullback:PTime.Enabled = 1
			If KeyCode=LeftFlipperKey And bTilted=FALSE Then
				LeftUp=TRUE
				LeftFlipper.RotateToEnd
				TopLeftFlipper.RotateToEnd
				PlaySound SoundFXDOF("fx_FlipperUp",201,DOFOn,DOFFlippers)
				' rotate the ALBUM lights to the left
				TempState=LightRollover1.State
				LightRollover1.State=LightRollover2.State
				LightRollover2.State=LightRollover3.State
				LightRollover3.State=LightRightInlane1.State
				LightRightInlane1.State=LightRightInlane2.State
				LightRightInlane2.State=LightLeftInlane.State
				LightLeftInlane.State=TempState
				' start status display timer
				If BallsInGame>0 And STimer.Enabled=0 Then TimerStatusDisplay.Enabled=1
			End If
			If KeyCode=RightFlipperKey And bTilted=FALSE Then
				RightUp=TRUE
				RightFlipper.RotateToEnd
				PlaySound SoundFXDOF("fx_FlipperUp",202,DOFOn,DOFFlippers)
				' rotate the ALBUM lights to the right
				TempState=LightRightInlane2.State
				LightRightInlane2.State=LightRightInlane1.State
				LightRightInlane1.State=LightRollover3.State
				LightRollover3.State=LightRollover2.State
				LightRollover2.State=LightRollover1.State
				LightRollover1.State=LightLeftInlane.State
				LightLeftInlane.State=TempState
				' start status display timer
				If BallsInGame>0 And STimer.Enabled=0 Then TimerStatusDisplay.Enabled=1
			End If
		Else  ' If (hsbModeActive)
			If bMenuModeActive=FALSE Then HighScoreProcessKeyDown(KeyCode)' Else in high score entry mode
		End If
	Else  ' If (bGameInPlay)
		' there isn't a game in play (we are in the attract mode or operator menu mode
		If bMenuModeActive=FALSE Then
			If Not DelayTimer.Enabled And bGameInPlay=FALSE Then
				If ATimer.Enabled Or BTimer.Enabled Then
					' wait for the start key to be pressed and start a new game
					If KeyCode=StartGameKey Then    '1
						If gsFreePlay Then
							If BallsInGame=0 Then
								ATimer.Enabled=0
								RandomCallout.Enabled=0
								BTimer.Enabled=0
								MTimer.Enabled=0
								LightSeq1.StopPlay
								bGameInPlay=TRUE
								ResetForNewGame
							End If
						Else
							If gsCredits>0 Then
								If BallsInGame=0 Then
									ATimer.Enabled=0
									RandomCallout.Enabled=0
									BTimer.Enabled=0
									MTimer.Enabled=0
									LightSeq1.StopPlay
									gsCredits=gsCredits-1
									If gsCredits < 1 Then DOF 209, DOFOff
									bGameInPlay=TRUE
									ResetForNewGame
								End If
							Else
								NoCreditSound
								DisplayCredits
							End If
						End If
					End If
				End If
			End If
		End If ' If (bGameInPlay)
	End If
End Sub

' A key (on the computer keyboard, not playfield) has been released
Sub Table1_KeyUp(ByVal KeyCode)
'LUT controls
If keycode = LeftMagnaSave Then bLutActive = False
	If WizardModeDisplaysActive Then Exit Sub
	If SongSelectInProgress=FALSE Then
		If KeyCode=PlungerKey Then
			Plunger.Fire:PTime.Enabled = 0:PTime2.Enabled = 1
			If bBallInPlungerLane=TRUE Then PlaySound"fx_Plunger" 'only play a sound effect for this if a ball is in the plunger lane
		End If
		If Not DelayTimer.Enabled Then' is a game in progress?
			If bGameInPlay Then
				If hsbModeActive=FALSE And bMenuModeActive=FALSE Then
					If KeyCode=LeftFlipperKey Then
						LeftUp=FALSE
						PlaySound SoundFXDOF("fx_flipperdown",201,DOFOff,DOFFlippers)
						LeftFlipper.RotateToStart
						TopLeftFlipper.RotateToStart
						' stop status display timer
						If RightUp=FALSE Then
							TimerStatusDisplay.Enabled=0
							If gvStatusModeActive Then
								STimer.Enabled=0
								gvStatusModeActive=FALSE
								DisplayFlushQueue
								AddScore 0
							End If
						End If
					End If
					If KeyCode=RightFlipperKey Then
						RightUp=FALSE
						PlaySound SoundFXDOF("fx_flipperdown",202,DOFOff,DOFFlippers)
						RightFlipper.RotateToStart
						' stop status display timer
						If LeftUp=FALSE Then
							TimerStatusDisplay.Enabled=0
							If gvStatusModeActive Then
								STimer.Enabled=0
								gvStatusModeActive=FALSE
								DisplayFlushQueue
								AddScore 0
							End If
						End If
					End If
				Else
					If bMenuModeActive=FALSE Then HighScoreProcessKeyUp(KeyCode)' Else in high score entry mode
				End If
			Else    ' If (bGameInPlay)
				If KeyCode=LeftFlipperKey Then
					LeftUp=FALSE
					LeftFlipper.RotateToStart
					TopLeftFlipper.RotateToStart
				End If
				If KeyCode=RightFlipperKey Then
					RightUp=FALSE
					RightFlipper.RotateToStart
				End If
			End If  ' If (bGameInPlay)
		End If
	End If
End Sub

Sub NoCreditSound
	If Not CredDelay.Enabled Then
		If ATimer.Enabled Or BTimer.Enabled Then
			NCSound=INT(RND*6) '0-5
			Select Case NCSound
				Case 0:PlaySound"SMustHave":CurrentSound="SMustHave"
				Case 1:PlaySound"SNeedTicket":CurrentSound="SNeedTicket"
				Case 2:PlaySound"SNothingFree":CurrentSound="SNothingFree"
				Case 3:PlaySound"SNoTicket":CurrentSound="SNoTicket"
				Case 4:PlaySound"SChild":CurrentSound="SChild"
				Case 5:PlaySound"SNotQuarter":CurrentSound="SNotQuarter"
			End Select
			CredDelay.Enabled=1
		End If
	End If
End Sub

Sub CredDelay_Timer
	CredDelay.Enabled=0
	CurrentSound=""
End Sub

Sub UpdateCounters(NewVal)
	RandomCalloutCount=RandomCalloutCount-5 'repeatedly pressing keys in attract mode increases time between callouts
	If RandomCalloutCount<0 Then RandomCalloutCount=0 'lower bounds value
	If RandomCalloutCount>24 Then 'requires a minimum of (25+ the 5 penalty reduction) 30 seconds to create a random callout, if available, it won't change the displays
		If Not CredDelay.Enabled Then
			If TotalCallouts=10 Then
				For X=0 To 9:TrackCallouts(X)=10:Next
				TotalCallouts=0
				rcallout=INT(RND*10) 'values 0-9
			End If
			If TotalCallouts>0 Then
				Do Until TrackCallouts(rcallout)=10
					rcallout=INT(RND*10) 'values 0-9
				Loop
			End If
			TotalCallouts=TotalCallouts+1
			TrackCallouts(rcallout)=0
			Select Case rcallout
				Case 0:PlaySound"SIsThatMyVoice":CurrentSound="SIsThatMyVoice"
				Case 1:PlaySound"SICantHearYou":CurrentSound="SICantHearYou"
				Case 2:PlaySound"SLittleNumber":CurrentSound="SLittleNumber"
				Case 3:PlaySound"SParty":CurrentSound="SParty"
				Case 4:PlaySound"SWillCall":CurrentSound="SWillCall"
				Case 5:PlaySound"SCamping":CurrentSound="SCamping"
				Case 6:PlaySound"SPrice":CurrentSound="SPrice"
				Case 7:PlaySound"SChange":CurrentSound="SChange"
				Case 8:PlaySound"SBlades":CurrentSound="SBlades"
				Case 9:PlaySound"SCopping":CurrentSound="SCopping"
			End Select
			CredDelay.Enabled=1
			RandomCalloutCount=25 'enforce upper bounds 25 is the hard cap - after the first callout is played, waiting five seconds will immediately play another callout
			'however, hitting flipper buttons after a callout is played before 5 seconds will increase the required wait time :)
		End If
	Else
		If NewVal=FALSE Then
			'display previous attract mode screen - scorpions is sometimes missed on reverse
			'this is probably because working out the display digits from a string takes longer
			If PH1<3000 Then
				PH1=68980
				For X=1 To 56:DD X,0,1:Next
			ElseIf PH1<14000 Then
				PH1=20
				For X=1 To 56:DD X,0,1:Next
			ElseIf PH1<17000 Then
				PH1=2080 'for some reason there is a delay if simply the counter is set - so manually draw the screen here and the timer will redraw the same thing - no perceptable change in timing, just in drawing
				DD 1,13,1:DD 2,1,1:DD 3,4,1:DD 4,5,1:DD 5,0,1:DD 6,16,1:DD 7,15,1:DD 8,19,1:DD 9,19,1:DD 10,9,1:DD 11,2,1:DD 12,12,1:DD 13,5,1:DD 14,0,1
				DD 15,0,0:DD 16,0,0:DD 17,0,0:DD 18,0,0:DD 19,0,0:DD 20,0,0:DD 21,2,1:DD 22,25,1:DD 23,0,0:DD 24,0,0:DD 25,0,0:DD 26,0,0:DD 27,0,0:DD 28,0,0
				DD 29,0,0:DD 30,0,0:DD 31,0,0:DD 32,0,0:DD 33,0,0:DD 34,0,0:DD 35,0,0:DD 36,0,0:DD 37,0,0:DD 38,0,0:DD 39,0,0:DD 40,0,0:DD 41,0,0:DD 42,0,0
				DD 43,0,0:DD 44,0,0:DD 45,0,0:DD 46,0,0:DD 47,0,0:DD 48,0,0:DD 49,0,0:DD 50,0,0:DD 51,0,0:DD 52,0,0:DD 53,0,0:DD 54,0,0:DD 55,0,0:DD 56,0,0
			ElseIf PH1<32000 Then
				PH1=13980
				For X=1 To 56:DD X,0,1:Next
			ElseIf PH1<35000 Then
				PH1=16980
				For X=1 To 56:DD X,0,1:Next
			ElseIf PH1<38000 Then
				PH1=31980
				For X=1 To 56:DD X,0,1:Next
			ElseIf PH1<41000 Then
				PH1=34980
				For X=1 To 56:DD X,0,1:Next
			ElseIf PH1<65000 Then
				PH1=37980
				For X=1 To 56:DD X,0,1:Next
			ElseIf PH1<67000 Then
				PH1=40980
				For X=1 To 56:DD X,0,1:Next
			ElseIf PH1<68980 Then
				PH1=64980
				For X=1 To 56:DD X,0,1:Next
			ElseIf PH1<71020 Then
				PH1=66980
				For X=1 To 56:DD X,0,1:Next
			End If
		Else
			'display next attract mode screen
			If PH1>68980 Then
				PH1=20
				For X=1 To 56:DD X,0,1:Next
			ElseIf PH1>66980 Then
				PH1=68980
				For X=1 To 56:DD X,0,1:Next
			ElseIf PH1>64980 Then
				PH1=66980
				For X=1 To 56:DD X,0,1:Next
			ElseIf PH1>40980 Then
				PH1=64980
				For X=1 To 56:DD X,0,1:Next
			ElseIf PH1>37980 Then
				PH1=40980
				For X=1 To 56:DD X,0,1:Next
			ElseIf PH1>34980 Then
				PH1=37980
				For X=1 To 56:DD X,0,1:Next
			ElseIf PH1>31980 Then
				PH1=34980
				For X=1 To 56:DD X,0,1:Next
			ElseIf PH1>16980 Then
				PH1=31980
				For X=1 To 56:DD X,0,1:Next
			ElseIf PH1>13980 Then
				PH1=16980
				For X=1 To 56:DD X,0,1:Next
			ElseIf PH1>2080 Then
				PH1=13980
			Else
				PH1=2080
				'for some reason there is a delay if simply the counter is set - so manually draw the screen here and the timer will redraw the same thing - no perceptable change in timing, just in drawing
				DD 1,13,1:DD 2,1,1:DD 3,4,1:DD 4,5,1:DD 5,0,1:DD 6,16,1:DD 7,15,1:DD 8,19,1:DD 9,19,1:DD 10,9,1:DD 11,2,1:DD 12,12,1:DD 13,5,1:DD 14,0,1
				DD 15,0,0:DD 16,0,0:DD 17,0,0:DD 18,0,0:DD 19,0,0:DD 20,0,0:DD 21,2,1:DD 22,25,1:DD 23,0,0:DD 24,0,0:DD 25,0,0:DD 26,0,0:DD 27,0,0:DD 28,0,0
				DD 29,0,0:DD 30,0,0:DD 31,0,0:DD 32,0,0:DD 33,0,0:DD 34,0,0:DD 35,0,0:DD 36,0,0:DD 37,0,0:DD 38,0,0:DD 39,0,0:DD 40,0,0:DD 41,0,0:DD 42,0,0
				DD 43,0,0:DD 44,0,0:DD 45,0,0:DD 46,0,0:DD 47,0,0:DD 48,0,0:DD 49,0,0:DD 50,0,0:DD 51,0,0:DD 52,0,0:DD 53,0,0:DD 54,0,0:DD 55,0,0:DD 56,0,0
			End If
		End If
	End If
End Sub

Sub TimerPlungerFire_Timer
	If bBallInPlungerLane Then 'if a ball is in the plunger lane
		If bPlungerFlag Then 'bplungerflag=true when pulled back
			AutoPlunger.Fire 'fire plunger
			PlaySound SoundFXDOF("fx_solenoid2",231,DOFPulse,DOFContactors)
			bPlungerFlag=FALSE 'plunger is now extended
		Else
			AutoPlunger.PullBack 'pull back plunger if currently extended
			bPlungerFlag=TRUE 'reset the flag
		End If
	Else
		TimerPlungerFire.Enabled=0 'disable the timer if no more balls to plunge
		AutoPlunger.PullBack 'pull back the plunger
		bPlungerFlag=TRUE 'reset the flag
	End If
End Sub

' Start, NewBall and EndOfGame Functions
'Initialize the variables/table for a new game
Sub ResetForNewGame
	Dim i
	' update game stats
	gsGamesPlayed=gsGamesPlayed+1
	gvStatusModeActive=FALSE
	SetGameDifficulty
	CurrentPlayer=0
	' single player (for now)
	PlayersPlayingGame=1
	StopAllMusic
	' turn off all the lights
	ChangeAllLamps 0,56
'	GIOFF
	' reset game values
	b2BallMultiMode=FALSE
	b3BallMultiMode=FALSE
	b4BallMultiMode=FALSE
	'Initialize for all 4 players
	Dim Z1,Z2,Z3
	For Z1=0 To 3
		LeftLockOpened(Z1)=FALSE
		Loops(Z1)=0
		LRamps(Z1)=0
		RRamps(Z1)=0
		PlayerBallsInLeftLock(Z1)=0
		PlayerBallsInTopLock(Z1)=0
		For Z2=1 To 12
			For Z3=0 To 1
				MusicPlayed(Z2,Z1,Z3)=0
				MusicCompleted(Z2,Z1,Z3)=0
				MusicCompleted2(Z2,Z1,Z3)=0
				OldMusicCompleted(Z2,Z1,Z3)=0
			Next
			MusicScore(Z1,Z2)=0
			MusicScoreTemp(Z1,Z2)=0
		Next
		ReplayStart(Z1,0)=gsReplayStart
		ReplayStart(Z1,1)=gsReplayLevels
		ExtraBallsAwards(Z1)=0
		BallsRemaining(Z1)=gsBallsPerGame
		BonusMultiplier(Z1)=1
		Score(Z1)=0
		For X=1 To 35:LampState(X,Z1)=0:Next'clear saved lamp states
		LampState(36,Z1)=1 'enable kickback for all players
		CombosThisGame(Z1)=0
		LastSongPlayed(Z1)=0
		For Z2=1 To 7
			NoteCount(Z1,Z2)=0
		Next
		For Z2=1 To 3
			AwardedSkills(Z1,Z2)=0 'clear out awarded skill shots for this game
		Next
		RandomAwardAvailable(Z1)=1
		RandomAwardShotsRequired(Z1)=gsRandomAwardShotsRequiredDefault
		For X=0 To 2
			RandomAwardsAwarded(Z1,X)=0
		Next
		FollowMeDifficulty(Z1)=-1
		FollowMeCounter(Z1)=0
		PopHits(Z1)=0
		PopHitsTemp(Z1)=0
		WizardModeAvailable(Z1)=0
		MiniWizardModeAvailable(Z1)=0
		PlayedMiniWizard(Z1)=0
	Next
	' turn off all the displays
	DisplayFlushQueue 'this also disables attract mode stuff
	For X=1 To 56
		DD X,0,1
	Next
	LightSeq1.StopPlay
	' Set up the start delay to handle the start of game attract sequence
	TimerFirstBallDelay.Enabled=1
End Sub

Sub StopAllMusic:EndMusic:MusicNumber=0:End Sub
Sub Table1_MusicDone
	'all music loops unless explicitly stopped
	If b2BallMultiMode=FALSE And b3BallMultiMode=FALSE Then
		PlayMusic TrackFilename(MusicNumber)
		DispText " "&MusicNumber&" - "&TrackFilename(MusicNumber)
		MusicTrackCheck
	Else
		If LightMB1.State=2 Then
			PlayMusic TrackFilename(15)
			DispText " "&"15 - "&TrackFilename(15)
			pupevent 601
		End If
		If LightMB2.State=2 Then
			PlayMusic TrackFilename(16)
			DispText " "&"16 - "&TrackFilename(16)
			pupevent 602
		End If
		If LightMB3.State=2 Then
			PlayMusic TrackFilename(17)
			DispText " "&"17 - "&TrackFilename(17)
			pupevent 603
		End If
		If LightMB4.State=2 Then
			PlayMusic TrackFilename(18)
			DispText " "&"18 - "&TrackFilename(18)
			pupevent 604
		End If
	End If
End Sub

' Initialize the variables/table for a new ball
Sub ResetForNewBall
	Dim i
	BallsInGame=0' no ball on the playfield yet (excluding locked balls)
	bTilted=FALSE' not tilted and no warnings given
	gpTiltWarningsGiven=0' (note the plumb might still be moving as I don't reset it)
	gvBallsToEject=0
	' cancel any special modes
	b2BallMultiMode=FALSE
	b3BallMultiMode=FALSE
	b4BallMultiMode=FALSE
	' Set the multiplier to 1x
	BonusMultiplier(CurrentPlayer)=1 'ensure bonus multiplier is 1X
	'done this way to accomodate quick/easy bonus held possibility
	Select Case BonusMultiplier(CurrentPlayer)
		Case 1:Light2X.State=0:Light3X.State=0:Light4X.State=0
		Case 2:Light2X.State=1:Light3X.State=0:Light4X.State=0
		Case 3:Light2X.State=0:Light3X.State=1:Light4X.State=0
		Case 4:Light2X.State=0:Light3X.State=0:Light4X.State=1
	End Select
	Bumper1L.State=0:Bumper2L.State=0:Bumper3L.State=0
	' turn on GI Lights
'	GION
	' enable slingshots and bumpers (in case we are recoving from a tilt)
	Bumper1.Threshold=1:Bumper2.Threshold=1:Bumper3.Threshold=1
	LeftSlingshot.SlingshotStrength=6:RightSlingshot.SlingshotStrength=6
	combovalue=25000 'initialize combo bonus to 25000 points
	gvCombosThisBall=0 'reset number of combos for bonus countup
	For X=1 To 12
		TrackLights(X).State=0'off
	Next
	If LastSongPlayed(CurrentPlayer)=13 Or LastSongPlayed(CurrentPlayer)=14 Or LastSongPlayed(CurrentPlayer)=0 Then
		If LastSongPlayed(CurrentPlayer)=13 Then 
			MiniWizardModeAvailable(CurrentPlayer)=0
			LightMiniWM.State=0
		End If
		If LastSongPlayed(CurrentPlayer)=14 Or LastSongPlayed(CurrentPlayer)=0 Then
			WizardModeAvailable(CurrentPlayer)=0
			MiniWizardModeAvailable(CurrentPlayer)=0
			LightWizardMode.State=0
		End If
		For X=1 To 12 'reset played counters and temporary completed counters
			MusicPlayed(X,CurrentPlayer,0)=0
			MusicCompleted(X,CurrentPlayer,0)=0
			OldMusicCompleted(X,CurrentPlayer,0)=0
		Next
		TempSongSelect=INT(RND*12)+1 'random song from 1-12
		MusicPlayed(TempSongSelect,CurrentPlayer,0)=1
		TrackLights(TempSongSelect).State=0
		TrackLights(TempSongSelect).BlinkPattern="10"
		TrackLights(TempSongSelect).BlinkInterval=125
		TrackLights(TempSongSelect).State=2
		LastSongPlayed(CurrentPlayer)=TempSongSelect
	End If
	UpdateMusicLamps
	DisplayScore
	PlayMusic TrackFilename(LastSongPlayed(CurrentPlayer))
	MusicNumber=LastSongPlayed(CurrentPlayer)
	DispText " "&TempSongSelect&" - "&TrackFilename(MusicNumber):DMD_DisplayScoreboard
	If RandomAwardAvailable(CurrentPlayer)>0 Then
		If MusicNumber<>11 Then RandomAward.State=2 'If Random Award is available for this player, blink the light unless we're in follow me mode
	Else
		RandomAward.State=0 'otherwise, turn it off
	End If
	'ensure drop targets are UP
	If Drop1.IsDropped Or Drop2.IsDropped Or Drop3.IsDropped Or Drop4.IsDropped Then
		Drop1.IsDropped=0:Drop2.IsDropped=0:Drop3.IsDropped=0:Drop4.IsDropped=0
		PlaySound SoundFXDOF("fx_solenoid",228,DOFPulse,DOFContactors)
	End If
	'Set up skill shot lamps if skill shot wasn't completed
	Drop4.IsDropped=1:PlaySound SoundFXDOF("fx_solenoidoff",228,DOFPulse,DOFContactors) 'Play a sound and drop the skill shot drop target
	Skill1.State=2 'Blink the skill shot 1 lamp
	Skill2.State=2 'Blink the skill shot 2 lamp
	Skill3SignOff.TimerEnabled=1 'enable blinking skill shot sign
	SkillShotActive=TRUE
	LightJackpot6.State=0:LightProgress6.State=0:LightProgress6.State=2 'blink right ramp for skill shot
	TrapDoorRamp.Collidable=0 'allow a right ramp skill shot shot to end up in the song select kicker
	TrapDoorWall.IsDropped=1:TrapDoorStop.IsDropped=0
	SkillValue1=250000+(AwardedSkills(CurrentPlayer,1)*50000) 'initial skill value for Skill Shot 1 times the number of completed skill shots of this type this game for this player
	SkillValue2=150000+(AwardedSkills(CurrentPlayer,2)*50000) 'initial skill value for Skill Shot 2 times the number of completed skill shots of this type this game for this player
	SkillValue3=400000+(AwardedSkills(CurrentPlayer,3)*50000) 'initial skill value for Skill Shot 3 times the number of completed skill shots of this type this game for this player
	gvFirstBallEjected=FALSE 'reset ball saver
	' If on the last ball
	If BallsRemaining(CurrentPlayer)=0 Then
		' display replay @ score
		If Not gsFreePlay And ReplayStart(CurrentPlayer,1)=0 Then
			DisplayText"  REPLAY AT   ",1,1:DMD_DisplaySceneText "REPLAY AT", ReplayStart(CurrentPlayer,0)
			DetermineJackpot ReplayStart(CurrentPlayer,0)
			If CurrentPlayer=0 Then
				DD 15,0,1
				DD 16,0,1
				DD 17,0,1
				If HS3Z>7 Then:DD 18,HS1O+37,1:Else:DD 18,0,1:End If
				If HS3Z>6 Then:DD 19,HS1H+48,1:Else:DD 19,0,1:End If
				If HS3Z>5 Then:DD 20,HS1I+37,1:Else:DD 20,0,1:End If
				If HS3Z>4 Then:DD 21,HS1J+37,1:Else:DD 21,0,1:End If
				If HS3Z>3 Then:DD 22,HS1K+48,1:Else:DD 22,0,1:End If
				If HS3Z>2 Then:DD 23,HS1L+37,1:Else:DD 23,0,1:End If
				If HS3Z>1 Then:DD 24,HS1M+37,1:Else:DD 24,0,1:End If
				DD 25,HS1N+37,1
				DD 26,0,1
				DD 27,0,1
				DD 28,0,1
			End If
			If CurrentPlayer=1 Then
				DD 43,0,1
				DD 44,0,1
				DD 45,0,1
				If HS3Z>7 Then:DD 46,HS1O+37,1:Else:DD 46,0,1:End If
				If HS3Z>6 Then:DD 47,HS1H+48,1:Else:DD 47,0,1:End If
				If HS3Z>5 Then:DD 48,HS1I+37,1:Else:DD 48,0,1:End If
				If HS3Z>4 Then:DD 49,HS1J+37,1:Else:DD 49,0,1:End If
				If HS3Z>3 Then:DD 50,HS1K+48,1:Else:DD 50,0,1:End If
				If HS3Z>2 Then:DD 51,HS1L+37,1:Else:DD 51,0,1:End If
				If HS3Z>1 Then:DD 52,HS1M+37,1:Else:DD 52,0,1:End If
				DD 53,HS1N+37,1
				DD 54,0,1
				DD 55,0,1
				DD 56,0,1
			End If
			If CurrentPlayer=2 Then
				DD 1,0,1
				DD 2,0,1
				DD 3,0,1
				If HS3Z>7 Then:DD 4,HS1O+37,1:Else:DD 4,0,1:End If
				If HS3Z>6 Then:DD 5,HS1H+48,1:Else:DD 5,0,1:End If
				If HS3Z>5 Then:DD 6,HS1I+37,1:Else:DD 6,0,1:End If
				If HS3Z>4 Then:DD 7,HS1J+37,1:Else:DD 7,0,1:End If
				If HS3Z>3 Then:DD 8,HS1K+48,1:Else:DD 8,0,1:End If
				If HS3Z>2 Then:DD 9,HS1L+37,1:Else:DD 9,0,1:End If
				If HS3Z>1 Then:DD 10,HS1M+37,1:Else:DD 10,0,1:End If
				DD 11,HS1N+37,1
				DD 12,0,1
				DD 13,0,1
				DD 14,0,1
			End If
			If CurrentPlayer=3 Then
				DD 29,0,1
				DD 30,0,1
				DD 31,0,1
				If HS3Z>7 Then:DD 32,HS1O+37,1:Else:DD 32,0,1:End If
				If HS3Z>6 Then:DD 33,HS1H+48,1:Else:DD 33,0,1:End If
				If HS3Z>5 Then:DD 34,HS1I+37,1:Else:DD 34,0,1:End If
				If HS3Z>4 Then:DD 35,HS1J+37,1:Else:DD 35,0,1:End If
				If HS3Z>3 Then:DD 36,HS1K+48,1:Else:DD 36,0,1:End If
				If HS3Z>2 Then:DD 37,HS1L+37,1:Else:DD 37,0,1:End If
				If HS3Z>1 Then:DD 38,HS1M+37,1:Else:DD 38,0,1:End If
				DD 39,HS1N+37,1
				DD 40,0,1
				DD 41,0,1
				DD 42,0,1
			End If
		End If
	End If
End Sub

Sub SongSelectTimer_Timer
	If SongSelectInProgress=TRUE Then
		SongSelectTimerCount=SongSelectTimerCount-1
		If SongSelectTimerCount>-1 Then
			DisplayText3"       "&SongSelectTimerCount&"      ",2,1:DMD_DisplaySongSelect
		End If
		If SongSelectTimerCount=-1 Then
			SongSelectTimer.Enabled=0
			Table1_KeyDown(StartGameKey)
			SongSelectTimer.Enabled=0
			DMD_SetScoreboardBackground ""
		End If
	Else
		SongSelectTimer.Enabled=0
		DMD_SetScoreboardBackground ""
	End If
End Sub

Sub UpdateMusicLamps
	If LastSongPlayed(CurrentPlayer)=13 Or LastSongPlayed(CurrentPlayer)=14 Then
		If LastSongPlayed(CurrentPlayer)=14 Then 'just finished wizard mode
			Dim Z1
			For Z1=1 To 7
				NoteCount(CurrentPlayer,Z1)=0
			Next
			LightNote1.State=0:LightNote2.State=0:LightNote3.State=0:LightNote4.State=0:LightNote5.State=0:LightNote6.State=0:LightNote7.State=0 'correct note lamps
			CheckNotes
		End If
		For X=1 To 12 'clear out played tracklights
			MusicPlayed(X,CurrentPlayer,0)=0 'clear played variables
			MusicCompleted(X,CurrentPlayer,0)=0 'clear temp completed variables
			OldMusicCompleted(X,CurrentPlayer,0)=0 'synchronize copy of temp completed variables
		Next
		StopAllMusic
		TempSongSelect=INT(RND*12)+1 'random song from 1-12
		MusicPlayed(TempSongSelect,CurrentPlayer,0)=1
		TrackLights(TempSongSelect).BlinkPattern="10"
		TrackLights(TempSongSelect).BlinkInterval=125
		TrackLights(TempSongSelect).State=2
		LastSongPlayed(CurrentPlayer)=TempSongSelect
		MusicNumber=LastSongPlayed(CurrentPlayer)
		DispText " "&TempSongSelect&" - "&TrackFilename(MusicNumber)
		If bTilted=FALSE Then PlayMusic TrackFilename(LastSongPlayed(CurrentPlayer))
		MusicScoreTemp(CurrentPlayer,TempSongSelect)=0 'reset temporary score for current player for this song - special case random award will not do this
		FollowMe.Interval=4000
		FollowMeVal=-1 'prepare to start with lamp 0 for strafe mode
		If TempSongSelect=11 And MusicCompleted2(11,CurrentPlayer,0)=2 Then
			FollowMeDifficulty(CurrentPlayer)=2' player completed song successfully, so increase difficulty for this one
			FollowMe.Interval=2000
			FollowMeCounter(CurrentPlayer)=0 'reset completion counter
		End If
		If TempSongSelect=11 And MusicCompleted2(11,CurrentPlayer,0)=1 Then
			FollowMeDifficulty(CurrentPlayer)=1
			FollowMeCounter(CurrentPlayer)=0 'reset completion counter
		End If
		HandleSongNumberLampsOff
		HandleSongNumberLamps(LastSongPlayed(CurrentPlayer))
	Else
		Dim TempW,TempX
		For X=1 To 12
			If MusicPlayed(X,CurrentPlayer,0)>0 Then 
				TrackLights(X).State=0'off
				TempW=""
				For TempX=1 To 12
					If TempX=X Then
						TempW=TempW&"1"
					Else
						TempW=TempW&"0"
					End If
				Next
				TrackLights(X).BlinkPattern=TempW 'change pattern
				TrackLights(X).BlinkInterval=125
				TrackLights(X).State=0'off
				TrackLights(X).State=2'blink
			End If
			If MusicCompleted(X,CurrentPlayer,0)>0 Then
				TrackLights(X).State=0'off
				TrackLights(X).BlinkPattern="10"
				TrackLights(X).BlinkInterval=125
				TrackLights(X).State=0'off
				TrackLights(X).State=1
			End If
		Next
		TrackLights(LastSongPlayed(CurrentPlayer)).State=0
		TrackLights(LastSongPlayed(CurrentPlayer)).BlinkPattern="10"
		TrackLights(LastSongPlayed(CurrentPlayer)).BlinkInterval=125
		TrackLights(LastSongPlayed(CurrentPlayer)).State=0'off
		TrackLights(LastSongPlayed(CurrentPlayer)).State=2
		DispText " "&LastSongPlayed(CurrentPlayer)&" - "&TrackFilename(LastSongPlayed(CurrentPlayer))
	End If
End Sub

' This function is called at the end of the game, it will reset all
' the drop targets, and eject any 'held' balls
Sub EndOfGame
	Dbg "End Of Game"
	if bOnTheFirstBallScorbit = False Then StopScorbit: Dbg "Stopping Scorbit"
	ResetTable
	' ensure everything else is reset
	bGameInPlay=FALSE
	bTilted=FALSE
	hsbModeActive=FALSE
	bMenuModeActive=FALSE
	gvFirstBallPlayed=FALSE
	gvFirstBallEjected=FALSE
	bBallSaverActive=FALSE
	' make sure the music has stoped
	StopAllMusic
	CurrentPlayer=0
End Sub

Sub ResetTable
	LockDiverter.IsDropped=0 'close the left ball lock
	LockDiverter2.IsDropped=1 'open passage to left inlane
	RandomAward.State=0 'turn off the random award light
	If Drop4.IsDropped Then:Drop4.IsDropped=0:PlaySound SoundFXDOF("fx_solenoid",228,DOFPulse,DOFContactors):End If 'raise the drop target and make a sound only if it is down
	Skill1.State=0
	Skill2.State=0
	If Skill3SignOff.TimerEnabled Then Skill3SignOff.TimerEnabled=0
	If Not Skill3SignOn.IsDropped Then Skill3SignOn.IsDropped=1
	If Skill3SignOff.IsDropped Then Skill3SignOff.IsDropped=0
	If SkillShotTimer.Enabled Then SkillShotTimer.Enabled=0
	SkillShotActive=FALSE
	TrapDoorRamp.Collidable=1:TrapDoorWall.IsDropped=0:TrapDoorStop.IsDropped=1
	LeftUp=FALSE
	RightUp=FALSE
	RightFlipper.RotateToStart
	TopLeftFlipper.RotateToStart
	RightFlipper.RotateToStart
	TimerStatusDisplay.Enabled=0
	ChangeAllLamps 0,56
	Bumper1.Threshold=2000:Bumper2.Threshold=2000:Bumper3.Threshold=2000
	LeftSlingshot.SlingshotStrength=.01:RightSlingshot.SlingshotStrength=.01
	' If there is a ball in the plunger, eject it
	If bBallInPlungerLane Then TimerPlungerFire.Enabled=1
	'turn off turntable and release balls into play
	TTPOST.IsDropped=1
	If CurrentMotorSound="fx_MotorOn" Then
		StopSound"fx_MotorOn"
		PlaySound"fx_MotorOff"
		CurrentMotorSound=""
	End If
	If CurrentMotorSound="fx_MotorRun" Then
		StopSound"fx_MotorRun"
		PlaySound"fx_MotorOff"
		CurrentMotorSound=""
	End If
	If MotorStart.Enabled Then MotorStart.Enabled=0
	TurnTable.MotorOff:lockl1.State=0: lockl2.State=0
	TurnTableUpdateNow.Enabled=0
	' flush the display queues
	If FirstRun=TRUE Then
		FirstRun=FALSE
	Else
		For Q=1 To 56
			DD Q,0,0
		Next
	End If
End Sub

' The start of game delay timer has expired so start the show
Sub TimerFirstBallDelay_Timer
	' stop the timer
	TimerFirstBallDelay.Enabled=0
	' reset the table for a new ball
	ResetForNewBall
	' create a new ball
	CreateNewBall
	if ScorbitActive = 1 And (Scorbit.bNeedsPairing) = False Then 
		Scorbit.StartSession()
		Dbg "Starting Scorbit Session"
	End If
	bOnTheFirstBallScorbit = True
End Sub

rem Set the game difficulty.  make timers either longer or shorter
Sub SetGameDifficulty
	Dim Z1
	' Set defaults
	gpBallSaveTime=8000  ' time ball is drain safe (ie If drain Then get a free ball)
	gsRandomAwardShotsRequiredDefault=5 '5 standups and/or drop target hits
	' If easy Then everything is 20% more
	If gsGameDifficulty=0 Then
		gpBallSaveTime=INT(gpBallSaveTime*1.2)
		gsRandomAwardShotsRequiredDefault=3 '3 standups and/or drop target hits
		For Z1=0 To 3
			NoteCount(Z1,1)=gsNotesForSongSelect 'each player has already hit the far left note lamp shot the required times to activate song selection
		Next
	End If
	' If tournament then individually handle other settings in other routines, ball save is 10% less, random award is not random and shots required are higher
	If gsGameDifficulty=2 Then
		gpBallSaveTime=INT(gpBallSaveTime*0.9)
		gsRandomAwardShotsRequiredDefault=7 '7 red standups and/or drop target hits
	End If
End Sub

' Handle the Plunger Lane
' create a new ball in the plunger lane
Sub CreateNewBall
	If ExtraBallsAwards(CurrentPlayer)>0 And bBallSaverActive=FALSE Then LightShootAgain.State=1
	LightKickback.State=LampState(36,CurrentPlayer) 'turn on the kickback light if the kickback is enabled for the current player
	CreateBallID BallRelease
	BallRelease.Kick 90,6
	PlaySound SoundFXDOF("fx_BallRel",219,DOFPulse,DOFContactors)
	BallsInGame=BallsInGame+1
	' mark a ball as being in the plunger lane
	bBallInPlungerLane=TRUE
	' turn on the ball save light
	Timer_BallSave.Enabled=0
	' display the score for the current player
	AddScore 0
End Sub

Sub CreateNewAutoBall(TimeDelay)
	gvBallsToEject=gvBallsToEject+1
	' If the eject timer is not running then start it
	' (this stops it from skipping)
	If Not Timer_AutoBallEject.Enabled Then
		Timer_AutoBallEject.Interval=TimeDelay+100
		Timer_AutoBallEject.Enabled=1
	End If
	TimerPlungerFire.Enabled=1
End Sub

Sub Timer_AutoBallEject_Timer
	Timer_AutoBallEject.Enabled=0
	' If there is a ball in the plunger lane
	If bBallInPlungerLane Then
		' Then try again in 1 second
		Timer_AutoBallEject.Interval=1000
		Timer_AutoBallEject.Enabled=1
	Else
		CreateBallID BallRelease
		BallRelease.Kick 90,6
		BallsInGame=BallsInGame+1
		PlaySound SoundFXDOF("fx_BallRel",219,DOFPulse,DOFContactors)
		' mark a ball as being in the plunger lane
		bBallInPlungerLane=TRUE
		' one less to create
		gvBallsToEject=gvBallsToEject-1
		' any more ?
		If gvBallsToEject<>0 Then
			' Then eject at 1 second intervals
			Timer_AutoBallEject.Interval=1000
			Timer_AutoBallEject.Enabled=1
		End If
	End If
End Sub

' the ball has been shot from the plunger
Sub Gate2_Hit
	If bTilted=FALSE Then
		If gvFirstBallEjected=FALSE Then
			gvFirstBallEjected=TRUE
			'start the ball saver
			If BallSaved=FALSE Then
				LightShootAgain.State=2
				Timer_BallSave.Enabled=0
				bBallSaverActive=TRUE
				Timer_BallSave.Interval=gpBallSaveTime
				Timer_BallSave.Enabled=0
				Timer_BallSave.Enabled=1
				'DisplayFlushQueue
				' ensure score is displayed
				'AddScore 0
			End If
		End If
	End If
	bBallInPlungerLane=FALSE
	' remember the switch
	LastSwitchHit=6 'Gate2
End Sub

rem The ball save timer has expired, turn off the light
Sub Timer_BallSave_Timer
	Timer_BallSave.Enabled=0
	' If the light is blinking, Then turn it off, but allow a little grace time
	' for the ball saver to still work
	Timer_BallSave.Interval=750
	Timer_BallSave.Enabled=1
	' the light is already off, grace time has expired
	bBallSaverActive=FALSE
	If ExtraBallsAwards(CurrentPlayer)>0 Then
		LightShootAgain.State=1
	Else
		LightShootAgain.State=0
	End If
End Sub

'               Handle the Trough/Drain
' lost a ball - check to see how many balls are on the playfield.
' If only one then decrement the remaining count and test for End of game
' If more than 1 ball (multi-ball) then kill off the ball but don't create
' a new one
Sub Drain_Hit
	DOF 226, DOFPulse
	BallSaved=FALSE 
	Dim i,RndSC
	' kill the ball anyway
	QTimer.Enabled=0
	Timer1.Enabled=0
	DisplayFlushQueue
	ClearBallID
	Drain.DestroyBall 
	BallsInGame=BallsInGame-1 
	' knock the ball into the ball storage mech
	PlaySound"fx_drain"
	If bGameInPlay And Not bTilted Then
		' is the ball saver active,
		If bBallSaverActive Then
			' yep then fire a new ball onto the playfield
			DisplayFlushQueue
			TimerStatusDisplay.Enabled=0
			DisplayText"  BALL SAVED  ",1,1 :PuPEvent 815
			DisplayText" AUTO RESTART ",2,1
			DMD_DisplaySceneTextWithPause "BALL SAVED", "AUTO RESTART", 3000
			'fix playfield lamps if needed
			BallSaved=TRUE
			Timer_BallSave_Timer
			If Drop4.IsDropped Then:Drop4.IsDropped=0:PlaySound"fx_solenoid":End If 'raise the drop target and make a sound only if it is down
			Skill1.State=0 'Turn Off Skill Shot 1 Lamp
			Skill2.State=0 'Turn off skill shot 2 lamp
			Skill3SignOff.TimerEnabled=0 'disable blinking skill shot sign
			Skill3SignOn.IsDropped=1:Skill3SignOff.IsDropped=0 'turn off the skill shot sign
			LightProgress6.State=0 'disable right ramp for skill shot
			SkillShotTimer.Enabled=0 'disable skill shot timer in case it was activated by something before kicker1 was hit
			SkillShotActive=FALSE 'disable skill shots for the rest of this ball
			If LightTrapDoor1.State<>2 And LightTrapDoor2.State<>2 And LightTrapDoor3.State<>2 And Not BeginWizardShow.Enabled Then
				TrapDoorRamp.Collidable=1:TrapDoorWall.IsDropped=0:TrapDoorStop.IsDropped=1
			End If
			If LightWizardMode.State=0 And LightMiniWM.State=0 And b4BallMultiMode=FALSE And b3BallMultiMode=FALSE And b2BallMultiMode=FALSE Then HandleSongNumberLamps(LastSongPlayed(CurrentPlayer))
			CheckNotes
			CreateNewAutoBall 0
			Exit Sub
		End If
		If MultiballSave.Enabled Then
			CreateNewAutoBall 0 'multiball ball save recreates balls for a set time limit of 20 seconds
			Exit Sub
		End If
		' cancel any multiball If on last ball (ie lost all other balls)
		If BallsInGame=1 And gvBallsToEject=0 Then
			' and in a multi-ball??
			If b2BallMultiMode Then
				' not in multiball mode any more
				b2BallMultiMode=FALSE
				If LightMB1.State=2 Then LightMB1.State=1
				If LightMB2.State=2 Then LightMB2.State=1
				If LightMB1.State=1 And LightMB2.State=1 Then
					LightMB1.State=0:LightMB2.State=0
				End If
				' change the music over
				LightJackpot1.State=0:LightJackpot2.State=0:LightJackpot3.State=0:LightJackpot4.State=0:LightJackpot5.State=0:LightJackpot6.State=0:LightJackpot7.State=0
				LightProgress1.State=0:LightProgress2.State=0:LightProgress3.State=0:LightProgress4.State=0:LightProgress5.State=0:LightProgress6.State=0:LightProgress7.State=0
				LightProgress1.State=1:LightProgress2.State=1:LightProgress3.State=1:LightProgress4.State=1:LightProgress5.State=1:LightProgress6.State=1:LightProgress7.State=1
				LightProgress1.State=0:LightProgress2.State=0:LightProgress3.State=0:LightProgress4.State=0:LightProgress5.State=0:LightProgress6.State=0:LightProgress7.State=0
				StopAllMusic
				PlayMusic TrackFilename(LastSongPlayed(CurrentPlayer))
				MusicNumber=LastSongPlayed(CurrentPlayer)
				DispText " "&TempSongSelect&" - "&TrackFilename(MusicNumber)
				If MusicNumber=9 Then:For X=1 To 7:TWA(X)=2:Next
				RestoreLampStatesSmall
				If Loops(CurrentPlayer)>23 Then Loops(CurrentPlayer)=0 'reset loop counter as player has enabled both 2-ball multiballs
				gsJackpot=100000 'reset jackpot value
				If bTilted=FALSE Then
					If RandomAwardAvailable(CurrentPlayer)>0 Then
						If MusicNumber<>11 Then RandomAward.State=2 'If Random Award is available for this player, blink the light - only if it's not follow me mode
					End If
				End If
				CheckNotes
				If Drop1.IsDropped=1 Then LightBand1.State=1
				If Drop2.IsDropped=1 Then LightBand2.State=1
				If Drop3.IsDropped=1 Then LightBand3.State=1
				If Drop4.IsDropped=1 Then LightBand4.State=1
			End If
			If b3BallMultiMode Then
				' 2 balls of the 3 ball multiball have been lost
				b3BallMultiMode=FALSE
				If LightMB3.State=2 Then LightMB3.State=1
				If LightMB4.State=2 Then LightMB4.State=1
				If LightMB3.State=1 And LightMB4.State=1 Then
					LightMB3.State=0:LightMB4.State=0
				End If
				' jet bumper bonus / extra ball remains the same
				DisplayFlushQueue
				' change the music over
				LightJackpot1.State=0:LightJackpot2.State=0:LightJackpot3.State=0:LightJackpot4.State=0:LightJackpot5.State=0:LightJackpot6.State=0:LightJackpot7.State=0
				LightProgress1.State=0:LightProgress2.State=0:LightProgress3.State=0:LightProgress4.State=0:LightProgress5.State=0:LightProgress6.State=0:LightProgress7.State=0
				LightProgress1.State=1:LightProgress2.State=1:LightProgress3.State=1:LightProgress4.State=1:LightProgress5.State=1:LightProgress6.State=1:LightProgress7.State=1
				LightProgress1.State=0:LightProgress2.State=0:LightProgress3.State=0:LightProgress4.State=0:LightProgress5.State=0:LightProgress6.State=0:LightProgress7.State=0
				StopAllMusic
				PlayMusic TrackFilename(LastSongPlayed(CurrentPlayer))
				MusicNumber=LastSongPlayed(CurrentPlayer)
				DispText " "&TempSongSelect&" - "&TrackFilename(MusicNumber)
				HandleSongNumberLampsOff
				HandleSongNumberLamps(MusicNumber)
				If LastLockHit(CurrentPlayer)="TOP" Then RRamps(CurrentPlayer)=0 'reset right ramp counter as three ball multiball just ended
				If LastLockHit(CurrentPlayer)="LEFT" Then LRamps(CurrentPlayer)=0 'reset left ramp counter as three ball multiball just ended
				gsJackpot=100000 'reset jackpot value
				If bTilted=FALSE Then
					If RandomAwardAvailable(CurrentPlayer)>0 Then
						If MusicNumber<>11 Then RandomAward.State=2 'If Random Award is available for this player, blink the light - only if it's not follow me mode
					End If
				End If
				CheckNotes
				If Drop1.IsDropped=1 Then LightBand1.State=1
				If Drop2.IsDropped=1 Then LightBand2.State=1
				If Drop3.IsDropped=1 Then LightBand3.State=1
				If Drop4.IsDropped=1 Then LightBand4.State=1
			End If
			If b4BallMultiMode Then 'Final Wizard Mode just ended
				b4BallMultiMode=FALSE
				GraphicEQ.Enabled=0
				LightWizardMode.State=0
				LightTrack1.State=0:LightTrack2.State=0:LightTrack3.State=0:LightTrack4.State=0:LightTrack5.State=0:LightTrack6.State=0
				LightTrack7.State=0:LightTrack8.State=0:LightTrack9.State=0:LightTrack10.State=0:LightTrack11.State=0:LightTrack12.State=0
				WizardModeAvailable(CurrentPlayer)=0
				MiniWizardModeAvailable(CurrentPlayer)=0
				PlayedMiniWizard(CurrentPlayer)=0 'reset flag for mini wizard mode as wizard mode was just completed
				LightSeq2.StopPlay
				
				Bumper1.State=0:Bumper2.State=0:Bumper3.State=0
				BeginWizardShow.Enabled=0
				LightTrapDoor1.State=0:LightTrapDoor2.State=0:LightTrapDoor3.State=0
				LightTrapDoor1.BlinkPattern="10"
				LightTrapDoor2.BlinkPattern="10"
				LightTrapDoor3.BlinkPattern="10"
				
				UpdateMusicLamps 'fix playfield track lamps
				DisplayFlushQueue
				If bTilted=FALSE Then
					If RandomAwardAvailable(CurrentPlayer)>0 And MusicNumber<>11 Then RandomAward.State=2 'If Random Award is available for this player, blink the light if not in follow me mode
					DisplayScore
				End If
				If Drop1.IsDropped=1 Then LightBand1.State=1
				If Drop2.IsDropped=1 Then LightBand2.State=1
				If Drop3.IsDropped=1 Then LightBand3.State=1
				If Drop4.IsDropped=1 Then LightBand4.State=1
			End If
		End If
		' was that the last ball on the playfield (not including locked balls)
		If BallsInGame=0 And gvBallsToEject=0 Then
			' end of ball display, change player, high score entry and match
			If HurrySmall.Enabled Then
				MiniHPValReal=0
				HurrySmall_Timer
			End If
			If HurrySingle.Enabled Then
				MiniSingleValReal=0
				HurrySingle_Timer
			End If
			If BeginWizardShow.Enabled=1 Then BeginWizardShow.Enabled=0
			RandomAward.State=0 'turn off random award flasher
			Skill1.State=0 'turn off choose a song flasher
			DoEndOfBallDisplay :PuPevent 806
		End If
	End If
End Sub

Sub MultiballSave_Timer
	MultiballSave.Enabled=0
	If ExtraBallsAwards(CurrentPlayer)>0 Then
		LightShootAgain.State=1
	Else
		LightShootAgain.State=0
	End If
End Sub

Sub MotorStart_Timer
	MotorStart.Enabled=0
	If bTilted=FALSE Then
		PlaySound"fx_MotorRun",-1
		CurrentMotorSound="fx_MotorRun"
	End If
End Sub

Sub ThreeBallTimer_Timer
	PXO2=PXO2+20
	If LastLockHit(CurrentPlayer)="TOP" Then
		Select Case PXO2
			Case 20:QTimer.Enabled=0:Timer1.Enabled=0 'disable all score display timers
						LightTrapDoor1.BlinkPattern="0010":LightTrapDoor2.BlinkPattern="0110":LightTrapDoor3.BlinkPattern="1110"
						LightTrapDoor1.State=2:LightTrapDoor2.State=2:LightTrapDoor3.State=2 'set up trap door lamps to cycle
						b3BallMultiMode=TRUE 'enable three ball multiball
						LightProgress1.TimerEnabled=0:LightProgress2.TimerEnabled=0:LightProgress3.TimerEnabled=0:LightProgress4.TimerEnabled=0:LightProgress5.TimerEnabled=0:LightProgress6.TimerEnabled=0:LightProgress7.TimerEnabled=0
						LightProgress1.State=0:LightProgress2.State=0:LightProgress3.State=0:LightProgress4.State=0:LightProgress5.State=0:LightProgress6.State=0:LightProgress7.State=0
						LightJackpot1.State=1:LightJackpot2.State=1:LightJackpot3.State=1:LightJackpot4.State=1:LightJackpot5.State=1:LightJackpot6.State=1:LightJackpot7.State=1
						LightJackpot1.State=0:LightJackpot2.State=0:LightJackpot3.State=0:LightJackpot4.State=0:LightJackpot5.State=0:LightJackpot6.State=0:LightJackpot7.State=0
						If MusicNumber<>9 Then
							FixLamps
						Else
							For X=1 To 7:TWA(X)=0:Next
						End If
						BallSaved=TRUE:bBallSaverActive=FALSE:Timer_BallSave.Enabled=0 'disable standard ballsaver
						RandomAward.State=0
						MultiballSave.Enabled=1:LightShootAgain.State=2 'enable multiball ballsaver for 20 seconds
						TTPOST.IsDropped=0:TurnTable.MotorOn:lockl1.State=2:lockl2.State=2:TurnTableUpdateNow.Enabled=1 'Turntable is starting up
						PlaySound"fx_MotorOn"
						CurrentMotorSound="fx_MotorOn"
						MotorStart.Enabled=1
						CreateBallID Kicker6:Kicker6.Kick 120,4:PlaySound SoundFXDOF("fx_popper",232,DOFPulse,DOFContactors) 'catch balls in the top ball lock and start spinning
						BallsInGame=BallsInGame+1:PlayerBallsInTopLock(CurrentPlayer)=PlayerBallsInTopLock(CurrentPlayer)-1
						If LightMB3.State=0 Then
							LightMB3.State=2
							Table1_MusicDone
						Else
							LightMB4.State=2
							Table1_MusicDone
						End If
						DisplayText3"   RECORD 1   ",1,1
						DisplayText3" SPINNING UP  ",2,1:DMD_DisplaySceneText "RECORD 1", "SPINNING UP"
			Case 1420:CreateBallID Kicker6:Kicker6.Kick 120,4:PlaySound SoundFXDOF("fx_popper",232,DOFPulse,DOFContactors)
						BallsInGame=BallsInGame+1:PlayerBallsInTopLock(CurrentPlayer)=PlayerBallsInTopLock(CurrentPlayer)-1
						LightTrapDoor3.State=0:LightTrapDoor3.BlinkPattern="10" 'turn off top lock lamp
						DisplayText3"   RECORD 2   ",1,1
						DisplayText3" SPINNING UP  ",2,1:DMD_DisplaySceneText "RECORD 2", "SPINNING UP"
			Case 2420:CreateBallID Kicker6:Kicker6.Kick 120,4:PlaySound SoundFXDOF("fx_popper",232,DOFPulse,DOFContactors)
						BallsInGame=BallsInGame+1:PlayerBallsInTopLock(CurrentPlayer)=PlayerBallsInTopLock(CurrentPlayer)-1
						LightTrapDoor2.State=0:LightTrapDoor2.BlinkPattern="10" 'turn off middle lock lamp
						DisplayText3"   RECORD 3   ",1,1
						DisplayText3" SPINNING UP  ",2,1:DMD_DisplaySceneText "RECORD 3", "SPINNING UP"
			Case 3420:LightTrapDoor1.State=0:LightTrapDoor1.BlinkPattern="10" 'turn off bottom lock lamp
						DisplayText3"   JACKPOTS   ",1,1
						DisplayText3"    READY     ",2,1:DMD_DisplaySceneText "JACKPOTS", "READY"
						LightJackpot1.State=0:LightJackpot2.State=0:LightJackpot3.State=0:LightJackpot4.State=0:LightJackpot5.State=0:LightJackpot6.State=0:LightJackpot7.State=0
						If INT(RND*10)<5 Then
							LightJackpot1.State=2:LightJackpot2.State=2:LightJackpot3.State=2:LightJackpot4.State=2:LightJackpot5.State=0:LightJackpot6.State=0:LightJackpot7.State=0
						Else
							LightJackpot1.State=0:LightJackpot2.State=0:LightJackpot3.State=0:LightJackpot4.State=0:LightJackpot5.State=2:LightJackpot6.State=2:LightJackpot7.State=2
						End If
						'half of jackpots are lit for this multiball and alternate when one is hit 4/7 split
		End Select
	Else
		Select Case PXO2
			Case 20:QTimer.Enabled=0:Timer1.Enabled=0 'disable all score display timers
						LightLock1.BlinkPattern="0010":LightLock2.BlinkPattern="0110":LightLock3.BlinkPattern="1110"
						LightLock1.State=2:LightLock2.State=2:LightLock3.State=2 'set up trap door lamps to cycle
						b3BallMultiMode=TRUE 'enable three ball multiball
						LightProgress1.TimerEnabled=0:LightProgress2.TimerEnabled=0:LightProgress3.TimerEnabled=0:LightProgress4.TimerEnabled=0:LightProgress5.TimerEnabled=0:LightProgress6.TimerEnabled=0:LightProgress7.TimerEnabled=0
						LightProgress1.State=0:LightProgress2.State=0:LightProgress3.State=0:LightProgress4.State=0:LightProgress5.State=0:LightProgress6.State=0:LightProgress7.State=0
						LightJackpot1.State=1:LightJackpot2.State=1:LightJackpot3.State=1:LightJackpot4.State=1:LightJackpot5.State=1:LightJackpot6.State=1:LightJackpot7.State=1
						LightJackpot1.State=0:LightJackpot2.State=0:LightJackpot3.State=0:LightJackpot4.State=0:LightJackpot5.State=0:LightJackpot6.State=0:LightJackpot7.State=0
						If MusicNumber<>9 Then
							FixLamps
						Else
							For X=1 To 7:TWA(X)=0:Next
						End If
						BallSaved=TRUE:bBallSaverActive=FALSE:Timer_BallSave.Enabled=0 'disable standard ballsaver
						RandomAward.State=0
						MultiballSave.Enabled=1:LightShootAgain.State=2 'enable multiball ballsaver for 20 seconds
						If LightMB3.State=0 Then
							LightMB3.State=2
							Table1_MusicDone
						Else
							LightMB4.State=2
							Table1_MusicDone
						End If
						DisplayText3"   BOOKING    ",1,1
						DisplayText3"   VENUES     ",2,1:DMD_DisplaySceneText "BOOKING", "VENUES"
			Case 1420:
						LightLock3.State=0 'turn off bottom lock lamp
						DisplayText3"   PRINTING   ",1,1
						DisplayText3"    FLYERS    ",2,1:DMD_DisplaySceneText "PRINTING", "FLYERS"
			Case 2420:
						LightLock2.State=0 'turn off middle lock lamp
						DisplayText3" TRIPLE PLAY  ",1,1
						DisplayText3"   CONCERTS   ",2,1:DMD_DisplaySceneText "TRIPLE PLAY", "CONCERTS"
			Case 3420:LightLock1.State=0 'turn off top lock lamp
						DisplayText3"   JACKPOTS   ",1,1
						DisplayText3"    READY     ",2,1:DMD_DisplaySceneText "JACKPOTS", "READY"
						LightJackpot1.State=0:LightJackpot2.State=0:LightJackpot3.State=0:LightJackpot4.State=0:LightJackpot5.State=0:LightJackpot6.State=0:LightJackpot7.State=0
						If INT(RND*10)<5 Then
							LightJackpot1.State=2:LightJackpot2.State=0:LightJackpot3.State=2:LightJackpot4.State=0:LightJackpot5.State=2:LightJackpot6.State=0:LightJackpot7.State=2
						Else
							LightJackpot1.State=0:LightJackpot2.State=2:LightJackpot3.State=0:LightJackpot4.State=2:LightJackpot5.State=0:LightJackpot6.State=2:LightJackpot7.State=0
						End If
						LightLock3.BlinkPattern="10"
						LightLock2.BlinkPattern="10"
						LightLock1.BlinkPattern="10"
						'half of jackpots are lit for this multiball and alternate when one is hit even/odd split
		End Select
	End If
	If PXO2>3420 Then
		If LightMB3.State=2 Then
			Select Case PXO2
				Case 4420:DisplayText3" PLAY LIKE A  ",1,1
							DisplayText3"              ",2,1:DMD_DisplaySceneText "PLAY LIKE A", "CHAMPION"
				Case 4480:DisplayText3"             C",2,1
				Case 4540:DisplayText3"            CH",2,1
				Case 4600:DisplayText3"           CHA",2,1
				Case 4660:DisplayText3"          CHAM",2,1
				Case 4720:DisplayText3"         CHAMP",2,1
				Case 4780:DisplayText3"        CHAMPI",2,1
				Case 4840:DisplayText3"       CHAMPIO",2,1
				Case 4900:DisplayText3"      CHAMPION",2,1
				Case 4960:DisplayText3"     CHAMPION ",2,1
				Case 5020:DisplayText3"    CHAMPION  ",2,1
				Case 5080:DisplayText3"   CHAMPION   ",2,1
				Case 5140:DisplayText3" CHAMPION     ",2,1
				Case 5200:DisplayText3"CHAMPION      ",2,1
				Case 5260:DisplayText3" CHAMPION     ",2,1
				Case 5320:DisplayText3"  CHAMPION    ",2,1
				Case 5380:DisplayText3"   CHAMPION   ",2,1
				Case 5440:DisplayText3"    CHAMPION  ",2,1
				Case 5500:DisplayText3"     CHAMPION ",2,1
				Case 5560:DisplayText3"      CHAMPION",2,1
				Case 5620:DisplayText3"     CHAMPION ",2,1
				Case 5680:DisplayText3"    CHAMPION  ",2,1
				Case 5740:DisplayText3"   CHAMPION   ",2,1
				Case 5800:DisplayText"   CHAMPION   ",2,1
							If LastLockHit(CurrentPlayer)="TOP" Then
								ThreeBallTimer.Enabled=0:PlaySound"AMBStart"
								TTPOST.IsDropped=1:TurnTable.MotorOff:lockl1.State=0: lockl2.State=0 'release balls into play now!
								TurnTableUpdateNow.Enabled=0 'Turntable is stopping
								StopSound"fx_MotorRun"
								PlaySound"fx_MotorOff"
								CurrentMotorSound=""
								If MotorStart.Enabled Then MotorStart.Enabled=0
							Else
								ThreeBallTimer.Enabled=0:PlaySound"AMBStart"
								BallsInGame=BallsInGame+2 'add 2 balls to current 1 ball in play
								Timer_BottomLockEject_Timer
								Timer_BottomLockEject.Enabled=1
							End If
							WizardModeDisplaysActive=FALSE
			End Select
		Else
			If PXO2=4420 Then
				DisplayText3"  WORLD TOUR  ",1,1
				DisplayText3"              ",2,1:DMD_SetScoreboardBackground "worldtour.png"
				For X=0 To 190:UsedCities(X)=0:Next
			End If
			If PXO2>4420 And PXO2<8740 And (PXO2-4420)/180=INT((PXO2-4420)/180) Then 'if divisible by 60 then display one of 23 possible capitol cities
				CityValidToUse=FALSE
				Do Until CityValidToUse=TRUE
					CityNameAttempt=INT(RND*190)+1 'generate random number from 1-190
					If UsedCities(CityNameAttempt)=0 Then
						UsedCities(CityNameAttempt)=1
						CityValidToUse=TRUE
					End If
				Loop
				If PXO2<8560 Then DisplayText3 Cities(CityNameAttempt),2,1
				If PXO2=8560 Then DisplayText Cities(CityNameAttempt),2,1
				DMD_DisplayWorldTour
				PlaySound"ZChange2"
			End If
			If PXO2=8740 Then
				If LastLockHit(CurrentPlayer)="TOP" Then
					ThreeBallTimer.Enabled=0:PlaySound"AMBStart"
					TTPOST.IsDropped=1:TurnTable.MotorOff:lockl1.State=0: lockl2.State=0 'release balls into play now!
					TurnTableUpdateNow.Enabled=0 'Turntable is stopping
					StopSound"fx_MotorRun"
					PlaySound"fx_MotorOff"
					CurrentMotorSound=""
					If MotorStart.Enabled Then MotorStart.Enabled=0
				Else
					ThreeBallTimer.Enabled=0:PlaySound"AMBStart"
					BallsInGame=BallsInGame+2 'add 2 balls to current 1 ball in play
					Timer_BottomLockEject_Timer
					Timer_BottomLockEject.Enabled=1
				End If
				DMD_SetScoreboardBackground ""
				WizardModeDisplaysActive=FALSE
			End If
		End If
	End If
End Sub

Dim Cities(190),UsedCities(190),CityNameAttempt,CityValidToUse ' There are ~196 countries in the world - only 190 capitol cities are used for 'World Tour' selection
Cities(1)="     APIA     "
Cities(2)="     BAKU     "
Cities(3)="     BERN     "
Cities(4)="     DILI     "
Cities(5)="     DOHA     "
Cities(6)="     JUBA     "
Cities(7)="     KYIV     "
Cities(8)="     LIMA     "
Cities(9)="     LOME     "
Cities(10)="     MALE     "
Cities(11)="     OSLO     "
Cities(12)="     RIGA     "
Cities(13)="     ROME     "
Cities(14)="     SUVA     "
Cities(15)="    ABUJA     "
Cities(16)="    ACCRA     "
Cities(17)="    AMMAN     "
Cities(18)="    CAIRO     "
Cities(19)="    DAKAR     "
Cities(20)="    DHAKA     "
Cities(21)="    HANOI     "
Cities(22)="    KABUL     "
Cities(23)="    MINSK     "
Cities(24)="    PARIS     "
Cities(25)="    PRAIA     "
Cities(26)="    QUITO     "
Cities(27)="    RABAT     "
Cities(28)="    SEOUL     "
Cities(29)="    SOFIA     "
Cities(30)="    TOKYO     "
Cities(31)="    TUNIS     "
Cities(32)="    VADUZ     "
Cities(33)="    ANKARA    "
Cities(34)="    ASMARA    "
Cities(35)="    ASTANA    "
Cities(36)="    ATHENS    "
Cities(37)="    BAMAKO    "
Cities(38)="    BANGUI    "
Cities(39)="    BANJUL    "
Cities(40)="    BEIRUT    "
Cities(41)="    BERLIN    "
Cities(42)="    BISSAU    "
Cities(43)="    BOGOTA    "
Cities(44)="    DUBLIN    "
Cities(45)="    HAVANA    "
Cities(46)="    KIGALI    "
Cities(47)="    LA PAZ    "
Cities(48)="    LISBON    "
Cities(49)="    LONDON    "
Cities(50)="    LUANDA    "
Cities(51)="    MADRID    "
Cities(52)="    MAJURO    "
Cities(53)="    MALABO    "
Cities(54)="    MANAMA    "
Cities(55)="    MANILA    "
Cities(56)="    MAPUTO    "
Cities(57)="    MASERU    "
Cities(58)="    MONACO    "
Cities(59)="    MORONI    "
Cities(60)="    MOSCOW    "
Cities(61)="    MUSCAT    "
Cities(62)="    NASSAU    "
Cities(63)="    NIAMEY    "
Cities(64)="    OTTAWA    "
Cities(65)="    PRAGUE    "
Cities(66)="    RIYADH    "
Cities(67)="    ROSEAU    "
Cities(68)="    SKOPJE    "
Cities(69)="    TAIPEI    "
Cities(70)="    TEHRAN    "
Cities(71)="    TIRANE    "
Cities(72)="    VIENNA    "
Cities(73)="    WARSAW    "
Cities(74)="    ZAGREB    "
Cities(75)="   ALGIERS    "
Cities(76)="   BAGHDAD    "
Cities(77)="   BANGKOK    "
Cities(78)="   BEIJING    "
Cities(79)="   BISHKEK    "
Cities(80)="   CARACAS    "
Cities(81)="   COLOMBO    "
Cities(82)="   CONAKRY    "
Cities(83)="   HONIARA    "
Cities(84)="   JAKARTA    "
Cities(85)="   KAMPALA    "
Cities(86)="   MANAGUA    "
Cities(87)="   MBABANE    "
Cities(88)="   NAIROBI    "
Cities(89)="   NICOSIA    "
Cities(90)="   PALIKIR    "
Cities(91)="   RANGOON    "
Cities(92)="   TALLINN    "
Cities(93)="   TBILISI    "
Cities(94)="   THIMPHU    "
Cities(95)="   TRIPOLI    "
Cities(96)="   VILNIUS    "
Cities(97)="   YAOUNDE    "
Cities(98)="   YEREVAN    "
Cities(99)="   ASHGABAT   "
Cities(100)="   ASUNCION   "
Cities(101)="   BELGRADE   "
Cities(102)="   BELMOPAN   "
Cities(103)="   BRASILIA   "
Cities(104)="   BRUSSELS   "
Cities(105)="   BUDAPEST   "
Cities(106)="   CANBERRA   "
Cities(107)="   CASTRIES   "
Cities(108)="   CHISINAU   "
Cities(109)="   DAMASCUS   "
Cities(110)="   DJIBOUTI   "
Cities(111)="   DUSHANBE   "
Cities(112)="   FREETOWN   "
Cities(113)="   GABORONE   "
Cities(114)="   HELSINKI   "
Cities(115)="   KHARTOUM   "
Cities(116)="   KINGSTON   "
Cities(117)="   KINSHASA   "
Cities(118)="   LILONGWE   "
Cities(119)="   MELEKEOK   "
Cities(120)="   MONROVIA   "
Cities(121)="   NDJAMENA   "
Cities(122)="   PRISTINA   "
Cities(123)="   SAN JOSE   "
Cities(124)="   SANTIAGO   "
Cities(125)="   SAO TOME   "
Cities(126)="   SARAJEVO   "
Cities(127)="   TASHKENT   "
Cities(128)="   VALLETTA   "
Cities(129)="   VICTORIA   "
Cities(130)="   WINDHOEK   "
Cities(131)="  ABU DHABI   "
Cities(132)="  AMSTERDAM   "
Cities(133)="  BUCHAREST   "
Cities(134)="  BUJUMBURA   "
Cities(135)="  CAPE TOWN   "
Cities(136)="  ISLAMABAD   "
Cities(137)="  JERUSALEM   "
Cities(138)="  KATHMANDU   "
Cities(139)="  KINGSTOWN   "
Cities(140)="  LJUBLJANA   "
Cities(141)="  MOGADISHU   "
Cities(142)="  NEW DELHI   "
Cities(143)="  PODGORICA   "
Cities(144)="  PORT-VILA   "
Cities(145)="  PYONGYANG   "
Cities(146)="  REYKJAVIK   "
Cities(147)="  SINGAPORE   "
Cities(148)="  STOCKHOLM   "
Cities(149)="  VIENTIANE   "
Cities(150)="  BASSETERRE  "
Cities(151)="  BRATISLAVA  "
Cities(152)="  BRIDGETOWN  "
Cities(153)="  COPENHAGEN  "
Cities(154)="  GEORGETOWN  "
Cities(155)="  LIBREVILLE  "
Cities(156)="  LUXEMBOURG  "
Cities(157)="  MONTEVIDEO  "
Cities(158)="  NOUAKCHOTT  "
Cities(159)="  NUKU ALOFA  "
Cities(160)="  PARAMARIBO  "
Cities(161)="  PHNOM PENH  "
Cities(162)="  PORT LOUIS  "
Cities(163)="  PORTO-NOVO  "
Cities(164)="  SAN MARINO  "
Cities(165)="  WELLINGTON  "
Cities(166)=" ADDIS ABABA  "
Cities(167)=" BRAZZAVILLE  "
Cities(168)=" KUWAIT CITY  "
Cities(169)=" MEXICO CITY  "
Cities(170)=" OUAGADOUGOU  "
Cities(171)=" PANAMA CITY  "
Cities(172)=" SAINT JOHNS  "
Cities(173)=" TEGUCIGALPA  "
Cities(174)=" ULAANBAATAR  "
Cities(175)=" ANTANANARIVO "
Cities(176)=" BUENOS AIRES "
Cities(177)=" KUALA LUMPUR "
Cities(178)=" PORT MORESBY "
Cities(179)=" SAINT GEORGE "
Cities(180)=" SAN SALVADOR "
Cities(181)=" TARAWA ATOLL "
Cities(182)=" VATICAN CITY "
Cities(183)=" YAMOUSSOUKRO "
Cities(184)="DAR ES SALAAM "
Cities(185)="PORT OF SPAIN "
Cities(186)="SANTO DOMINGO "
Cities(187)="WASHINGTON DC "
Cities(188)="GUATEMALA CITY"
Cities(189)="PORT-AU-PRINCE"
Cities(190)="VAIAKU VILLAGE"

Sub Kicker2_Hit
	PlaySound"fx_kicker_enter"
	ClearBallID
	Kicker2.DestroyBall
	PlayerBallsInLeftLock(CurrentPlayer)=PlayerBallsInLeftLock(CurrentPlayer)+1
	LockDiverter.IsDropped=0:LockDiverter2.IsDropped=1:LeftLockOpened(CurrentPlayer)=0
	Select Case PlayerBallsInLeftLock(CurrentPlayer)
		Case 1:LightLock1.State=1:LightLock2.State=0:LightLock3.State=0:CreateNewAutoBall(100):TimerPlungerFire.Enabled=1:BallsInGame=BallsInGame-1
		Case 2:LightLock1.State=1:LightLock2.State=1:LightLock3.State=0:CreateNewAutoBall(100):TimerPlungerFire.Enabled=1:BallsInGame=BallsInGame-1
		Case 3:LightLock1.State=1:LightLock2.State=1:LightLock3.State=1:LastLockHit(CurrentPlayer)="LEFT":PXO2=0:EndMusic
				WizardModeDisplaysActive=TRUE
				LeftUp=FALSE:RightUp=FALSE:LeftFlipper.RotateToStart:TopLeftFlipper.RotateToStart:RightFlipper.RotateToStart:STimer.Enabled=0 'disable status timer
				gvStatusModeActive=FALSE:QTimer.Enabled=0:Timer1.Enabled=0
				ThreeBallTimer.Enabled=1:ThreeBallTimer_Timer 'start 3 ball multiball! 'leave one ball in game so tilt doesn't instantly kill the table
	End Select
End Sub

Sub Timer_BottomLockEject_Timer
	If PlayerBallsInLeftLock(CurrentPlayer)>0 Then
		PlayerBallsInLeftLock(CurrentPlayer)=PlayerBallsInLeftLock(CurrentPlayer)-1
		CreateBallID Kicker2
		Kicker2.Kick 0,30
		PlaySound SoundFXDOF("fx_Popper",230,DOFPulse,DOFContactors)
		DOF 242, DOFPulse
	Else
		Timer_BottomLockEject.Enabled=0
	End If
End Sub


'---------------------------------------------------------------------------------------------------------
'                 Status Functions
' this timer is used to wait the defined period for when the status screen is displayed
Sub TimerStatusDisplay_Timer
	If ThreeBallTimer.Enabled Or RandomAwardTimer.Enabled Then:TimerStatusDisplay.Enabled=0:Exit Sub:End If
	TimerStatusDisplay.Enabled=0
	STimer.Enabled=1
	PXO4=0
	DisplayFlushQueue
	gvStatusModeActive=TRUE
	STimer_Timer
End Sub

Sub WMMiniTimer_Timer
	PXO3=PXO3+20
	Select Case PXO3
		Case 20:LightMiniWM.State=1
					QTimer.Enabled=0:QTimer.Enabled=0
					StopAllMusic
					PlayMusic TrackFilename(13)
					MusicNumber=13
					LastSongPlayed(CurrentPlayer)=13
					DispText " "&"13 - "&TrackFilename(MusicNumber)
					HandleSongNumberLampsOff 'disable old song lamps
					'open gates in case old song was track 3 (Bumpers)
					BGate.Open=TRUE:CGate.Open=TRUE
					DisplayText3" BATTLE OF THE",1,1
					DisplayText3"     BANDS    ",2,1:DMD_DisplaySceneText "BATTLE OF THE", "BANDS"
		Case 2020:DisplayText3"   HIT ALL    ",1,1
					DisplayText3"   HURRYUPS   ",2,1:DMD_DisplaySceneText "HIT ALL", "HURRYUPS"
		Case 4040:PlaySound"YCheer":DisplayText3"  25 SECONDS  ",1,1
					DisplayText3"       2500000",2,1:DMD_DisplaySceneText "25 SECONDS", "2500000"
					Select Case CurrentPlayer
						Case 0:Light395.State=1:Light396.State=1:Light449.State=1:Light450.State=1
						Case 1:Light899.State=1:Light900.State=1:Light953.State=1:Light954.State=1
						Case 2:Light143.State=1:Light144.State=1:Light197.State=1:Light198.State=1
						Case 3:Light647.State=1:Light648.State=1:Light701.State=1:Light702.State=1
					End Select
		Case 6060:DisplayText3"  GET READY   ",1,1
					DisplayText3"              ",2,1:DMD_DisplaySceneText " GET READY ", ""
					WizardModeDisplaysActive=FALSE
					WMMiniTimer.Enabled=0
					SubwayExit.Enabled=1
					RandomAward.state = 2
					MiniHPNum=0 'set number of hurryups made to 0
					OldHurryUp=0
					MiniHPTotal=0
					For X=1 To 12
						If TrackLights(X).State<>1 Then MiniHPTotal=MiniHPTotal+1 'count incomplete modes - this is the number of hurryups required to start full wizard mode
					Next
					InitHurryUp
					RandomAward.State=2
	End Select
End Sub

Sub WMStartTimer_Timer
	PXO3=PXO3+20
	Select Case PXO3
		Case 20:LightWizardMode.State=1:LightSeq2_PlayDone:PXZ1=0:PXZ2=0:PXZ3=0
				Light2X.State=0:Light3X.State=0:Light4X.State=2:BonusMultiplier(CurrentPlayer)=4 'maximize bonus multiplier and playfield multiplier
				LightKickback.State=1:KickBack.PullBack:KickbackSaver.Enabled=0:Kickback.TimerEnabled=0'Ensure kickback is lit and ready
				PopHits(CurrentPlayer)=120 'set pop bumper hit value to maximum 3210 points per hit
				PlayMusic TrackFilename(14) 'start music
				MusicNumber=14
				LastSongPlayed(CurrentPlayer)=14
				DispText " "&"14 - "&TrackFilename(MusicNumber)
				DisplayText3"  MUSICIANS   ",1,1
				DisplayText3" HALL OF FAME ",2,1:DMD_DisplaySceneText "MUSICIANS", "HALL OF FAME"
		Case 1420:DisplayText3" WIZARD MODE  ",1,1
					DisplayText3"  MULTI-BALL  ",2,1:DMD_DisplaySceneText "WIZARD MODE", "MULTI-BALL"
		Case 2820:DisplayText3"  EVERYTHING  ",1,1:PlaySound"YCheer"
					DisplayText3"    IS LIT    ",2,1:DMD_DisplaySceneText "EVERYTHING", "IS LIT"
		Case 4220:DisplayText3"  MAXIMIZED   ",1,1
					DisplayText3"   SCORING    ",2,1:DMD_DisplaySceneText "MAXIMIZED", "SCORING"
		Case 5620:DisplayText3"FOR MULTI-BALL",1,2
					DisplayText3"              ",2,1:DMD_DisplaySceneText "FOR MULTI-BALL", ""
					WizardModeDisplaysActive=FALSE
					SubwayExit.Enabled=1 'enables timer to release the ball in the subway system
					RandomAward.State=2
					GraphicEQ.Enabled=1 'engage graphic equalizer
					GraphicEQ_Timer
					LightExtraBall.State=2 'blink the extra ball lamp!
		Case 8620:'first ball has been ejected from the subway
					Bumper1.State=2:Bumper2.State=2:Bumper3.State=2
					b4BallMultiMode=TRUE 'sets wizard mode multiball to true
					WMStartTimer.Enabled=0 'disables wizard mode start timer
					CreateNewAutoBall(400) 'eject a ball from the plunger
					BallsInGame=BallsInGame+2
					CreateBallID Kicker2 'eject ball from bottom left lock
					Kicker2.Kick 0,30
					PlaySound SoundFXDOF("fx_solenoid2",230,DOFPulse,DOFContactors)
					CreateBallID Kicker6 'eject ball from turntable
					Kicker6.Kick 120,4
					PlaySound SoundFXDOF("fx_popper",232,DOFPulse,DOFContactors)
	End Select
End Sub

' Queue up all the status screen displays to the display driver ' no status text available during mini-wizard hurryups or full wizard mode
Sub STimer_Timer
	If LightMiniWM.State<>1 And LightWizardMode.State<>1 Then
		PXO4=PXO4+20
		Select Case PXO4
			Case 20:DisplayText3 DisplayName,1,1:DisplayText3"STATUS  REPORT",2,1:DMD_DisplaySceneText TableName, "STATUS REPORT"
			Case 2260:If MusicNumber<13 And MusicNumber>0 Then
							Dim DisplayProgress,FutureProgress,FutureSpaces
							DisplayProgress=FALSE:FutureSpaces=0
							Select Case MusicNumber
								Case 1:If MusicScoreTemp(CurrentPlayer,MusicNumber)<700000 Then
											DisplayProgress=TRUE
											FutureProgress=700000-MusicScoreTemp(CurrentPlayer,MusicNumber)
										End If
								Case 2:If MusicScoreTemp(CurrentPlayer,MusicNumber)<680000 Then
											DisplayProgress=TRUE
											FutureProgress=900000-MusicScoreTemp(CurrentPlayer,MusicNumber)
										End If
								Case 3:If PopHitsTemp(CurrentPlayer)<100 Then
											DisplayProgress=TRUE
											FutureProgress=100-PopHitsTemp(CurrentPlayer)
										End If
								Case 4:If MusicScoreTemp(CurrentPlayer,MusicNumber)<1000000 Then
											DisplayProgress=TRUE
											FutureProgress=900000-MusicScoreTemp(CurrentPlayer,MusicNumber)
										End If
								Case 5:If MusicScoreTemp(CurrentPlayer,MusicNumber)<800000 Then
											DisplayProgress=TRUE
											FutureProgress=900000-MusicScoreTemp(CurrentPlayer,MusicNumber)
										End If
								Case 6:If MusicScoreTemp(CurrentPlayer,MusicNumber)<1000000 Then
											DisplayProgress=TRUE
											FutureProgress=900000-MusicScoreTemp(CurrentPlayer,MusicNumber)
										End If
								Case 7:If MusicScoreTemp(CurrentPlayer,MusicNumber)<700000 Then
											DisplayProgress=TRUE
											FutureProgress=900000-MusicScoreTemp(CurrentPlayer,MusicNumber)
										End If
								Case 8:If MusicScoreTemp(CurrentPlayer,MusicNumber)<700000 Then
											DisplayProgress=TRUE
											FutureProgress=900000-MusicScoreTemp(CurrentPlayer,MusicNumber)
										End If
								Case 9:If MusicScoreTemp(CurrentPlayer,MusicNumber)<1000000 Then
											DisplayProgress=TRUE
											FutureProgress=900000-MusicScoreTemp(CurrentPlayer,MusicNumber)
										End If
								Case 10:If MusicScoreTemp(CurrentPlayer,MusicNumber)<300000 Then
											DisplayProgress=TRUE
											FutureProgress=900000-MusicScoreTemp(CurrentPlayer,MusicNumber)
										End If
								Case 11:If FollowMeCounter(CurrentPlayer)<7 Then
											DisplayProgress=TRUE
											FutureProgress=7-FollowMeCounter(CurrentPlayer)
										End If
								Case 12:If MusicScoreTemp(CurrentPlayer,MusicNumber)<1000000 Then
											DisplayProgress=TRUE
											FutureProgress=900000-MusicScoreTemp(CurrentPlayer,MusicNumber)
										End If
							End Select
							If FutureProgress<10000000 Then FutureSpaces=7
							If FutureProgress<1000000 Then FutureSpaces=6
							If FutureProgress<100000 Then FutureSpaces=5
							If FutureProgress<10000 Then FutureSpaces=4
							If FutureProgress<1000 Then FutureSpaces=3
							If FutureProgress<100 Then FutureSpaces=2
							If FutureProgress<10 Then FutureSpaces=1
							If DisplayProgress=TRUE Then
								If MusicNumber<10 Then
									DisplayText3"FINISH SONG  "&MusicNumber,1,1
								Else
									DisplayText3"FINISH SONG "&MusicNumber,1,1
								End If
								Select Case FutureSpaces
									Case 1:ComH="      "&FutureProgress&"       "
									Case 2:ComH="      "&FutureProgress&"      "
									Case 3:ComH="     "&FutureProgress&"      "
									Case 4:ComH="     "&FutureProgress&"     "
									Case 5:ComH="    "&FutureProgress&"     "
									Case 6:ComH="    "&FutureProgress&"    "
									Case 7:ComH="   "&FutureProgress&"    "
								End Select
								DisplayText3 ComH,2,1:DMD_DisplaySceneText "FINISH SONG "&MusicNumber, FutureProgress
	
								' some B2S changes ************************************************
								'If CurrentPlayer=0 Then
									'If FutureSpaces=7 Then:Light323.State=1:Light324.State=1:Light377.State=1:Light378.State=1:End If
									'If FutureSpaces=6 Then:Light377.State=1:Light378.State=1:End If
									'If FutureSpaces=5 Then:Light359.State=1:Light360.State=1:End If
									'If FutureSpaces=4 Then:Light359.State=1:Light360.State=1:End If
								'End If
								'If CurrentPlayer=1 Then
									'If FutureSpaces=7 Then:Light827.State=1:Light828.State=1:Light881.State=1:Light882.State=1:End If
									'If FutureSpaces=6 Then:Light881.State=1:Light882.State=1:End If
									'If FutureSpaces=5 Then:Light863.State=1:Light864.State=1:End If
									'If FutureSpaces=4 Then:Light863.State=1:Light864.State=1:End If
								'End If
								'If CurrentPlayer=2 Then
									'If FutureSpaces=7 Then:Light71.State=1:Light72.State=1:Light125.State=1:Light126.State=1:End If
									'If FutureSpaces=6 Then:Light125.State=1:Light126.State=1:End If
									'If FutureSpaces=5 Then:Light107.State=1:Light108.State=1:End If
									'If FutureSpaces=4 Then:Light107.State=1:Light108.State=1:End If
								'End If
								'If CurrentPlayer=3 Then
									'If FutureSpaces=7 Then:Light575.State=1:Light576.State=1:Light629.State=1:Light630.State=1:End If
									'If FutureSpaces=6 Then:Light629.State=1:Light630.State=1:End If
									'If FutureSpaces=5 Then:Light611.State=1:Light612.State=1:End If
									'If FutureSpaces=4 Then:Light611.State=1:Light612.State=1:End If
								'End If
								' some B2S changes ************************************************
								
							Else
								PXO4=4500
							End If
						Else
							PXO4=4500
						End If
			Case 4520:If CombosThisGame(CurrentPlayer)=0 Then DisplayText3"  NO COMBOS   ",1,1:DMD_DisplaySceneText " NO COMBOS ", ""
							If CombosThisGame(CurrentPlayer)=1 Then DisplayText3"   1 COMBO    ",1,1:DMD_DisplaySceneText "  1 COMBO  ", ""
							If CombosThisGame(CurrentPlayer)>1 And CombosThisGame(CurrentPlayer)<10 Then:ComH="   "&CombosThisGame(CurrentPlayer)&" COMBOS   ":DisplayText3 ComH,1,1:DMD_DisplaySceneText ComH, "":End If
							If CombosThisGame(CurrentPlayer)>9 And CombosThisGame(CurrentPlayer)<100 Then:ComH="  "&CombosThisGame(CurrentPlayer)&" COMBOS   ":DisplayText3 ComH,1,1:DMD_DisplaySceneText ComH, "":End If
							If CombosThisGame(CurrentPlayer)>99 And CombosThisGame(CurrentPlayer)<1000 Then:ComH="  "&CombosThisGame(CurrentPlayer)&" COMBOS  ":DisplayText3 ComH,1,1:DMD_DisplaySceneText ComH, "":End If
							DisplayText3"              ",2,1
			Case 6740:DisplayText3"COMBO CHAMPION",1,1
							DetermineComboChamp
							DisplayText3"              ",2,1:DMD_DisplaySceneText "COMBO CHAMPION", gvCombosForComboChamp
							Select Case CurrentPlayer
								Case 0:DD 15,LPA(1),1:DD 16,LPA(2),1:DD 17,LPA(3),1:DD 18,LPA(4),1:DD 19,LPA(5),1
											If gvCombosforComboChamp<10 Then DD 21,gvCombosforComboChamp+37,1
											If gvCombosforComboChamp>9 And gvCombosforComboChamp<100 Then:DD 21,INT(gvCombosforComboChamp/10)+37,1:DD 22,(gvCombosforComboChamp-INT(gvCombosforComboChamp/10)*10)+37,1:End If
											If gvCombosforComboChamp>99 And gvCombosforComboChamp<1000 Then
												AQWE=gvCombosforComboChamp
												AQ=INT(AQWE/100)
												AQWE=AQWE-AQ*100
												AQW=INT(AQWE/10)
												AQWE=AQWE-AQW*10
												DD 20,AQ+37,1
												DD 21,AQW+37,1
												DD 22,AQWE+37,1
											End If
								Case 1:DD 43,LPA(1),1:DD 44,LPA(2),1:DD 45,LPA(3),1:DD 46,LPA(4),1:DD 47,LPA(5),1
											If gvCombosforComboChamp<10 Then DD 49,gvCombosforComboChamp+37,1
											If gvCombosforComboChamp>9 And gvCombosforComboChamp<100 Then:DD 49,INT(gvCombosforComboChamp/10)+37,1:DD 50,(gvCombosforComboChamp-INT(gvCombosforComboChamp/10)*10)+37,1:End If
											If gvCombosforComboChamp>99 And gvCombosforComboChamp<1000 Then
												AQWE=gvCombosforComboChamp
												AQ=INT(AQWE/100)
												AQWE=AQWE-AQ*100
												AQW=INT(AQWE/10)
												AQWE=AQWE-AQW*10
												DD 48,AQ+37,1
												DD 49,AQW+37,1
												DD 50,AQWE+37,1
											End If
								Case 2:DD 1,LPA(1),1:DD 2,LPA(2),1:DD 3,LPA(3),1:DD 4,LPA(4),1:DD 5,LPA(5),1
											If gvCombosforComboChamp<10 Then DD 7,gvCombosforComboChamp+37,1
											If gvCombosforComboChamp>9 And gvCombosforComboChamp<100 Then:DD 7,INT(gvCombosforComboChamp/10)+37,1:DD 8,(gvCombosforComboChamp-INT(gvCombosforComboChamp/10)*10)+37,1:End If
											If gvCombosforComboChamp>99 And gvCombosforComboChamp<1000 Then
												AQWE=gvCombosforComboChamp
												AQ=INT(AQWE/100)
												AQWE=AQWE-AQ*100
												AQW=INT(AQWE/10)
												AQWE=AQWE-AQW*10
												DD 6,AQ+37,1
												DD 7,AQW+37,1
												DD 8,AQWE+37,1
											End If
								Case 3:DD 29,LPA(1),1:DD 30,LPA(2),1:DD 31,LPA(3),1:DD 32,LPA(4),1:DD 33,LPA(5),1
											If gvCombosforComboChamp<10 Then DD 35,gvCombosforComboChamp+37,1
											If gvCombosforComboChamp>9 And gvCombosforComboChamp<100 Then:DD 35,INT(gvCombosforComboChamp/10)+37,1:DD 36,(gvCombosforComboChamp-INT(gvCombosforComboChamp/10)*10)+37,1:End If
											If gvCombosforComboChamp>99 And gvCombosforComboChamp<1000 Then
												AQWE=gvCombosforComboChamp
												AQ=INT(AQWE/100)
												AQWE=AQWE-AQ*100
												AQW=INT(AQWE/10)
												AQWE=AQWE-AQW*10
												DD 34,AQ+37,1
												DD 35,AQW+37,1
												DD 36,AQWE+37,1
											End If
							End Select
			Case 8980:If ReplayStart(CurrentPlayer,1)<>0 Then
							If gsFreePlay Then
								DisplayText3"EXTRA BALL AT ",1,1:DMD_DisplaySceneText "EXTRA BALL AT", ReplayStart(CurrentPlayer,0)
							Else
								DisplayText3"  REPLAY AT   ",1,1:DMD_DisplaySceneText "REPLAY AT", ReplayStart(CurrentPlayer,0)
							End If
							ComH="   "&ReplayStart(CurrentPlayer,0)&"    "
							DisplayText3 ComH,2,1

							' some B2S changes ************************************************
							'If CurrentPlayer=0 Then:Light323.State=1:Light324.State=1:Light377.State=1:Light378.State=1:End If
							'If CurrentPlayer=1 Then:Light827.State=1:Light828.State=1:Light881.State=1:Light882.State=1:End If
							'If CurrentPlayer=2 Then:Light71.State=1:Light72.State=1:Light125.State=1:Light126.State=1:End If
							'If CurrentPlayer=3 Then:Light575.State=1:Light576.State=1:Light629.State=1:Light630.State=1:End If
							' some B2S changes ************************************************

						End If
			Case 11220:If gsFreePlay Then
							DisplayText3"  FREE  PLAY  ",1,1:DMD_DisplaySceneText " FREE PLAY ", ""
						Else
							If gsCredits>9 Then
								ComH="  CREDITS "&gsCredits&"  "
							Else
								ComH="  CREDITS  "&gsCredits&"  "
							End If
							DisplayText3 ComH,1,1:DMD_DisplaySceneText "CREDITS", gsCredits
						End If
						DisplayText3"              ",2,1
			Case 13460:PXO4=2240
		End Select
	End If
End Sub

'                 Score Functions
' add points to the score and update the score board
Sub AddScore(Points)
	If bTilted=FALSE Then
		Dim tCount
		tCount=1
		If Light2X.State=2 Then tCount=2 'account for playfield score multiplier
		If Light3X.State=2 Then tCount=3 'account for playfield score multiplier
		If Light4X.State=2 Then tCount=4 'account for playfield score multiplier
		If Score(CurrentPlayer)+Points<100000000000000 Then Score(CurrentPlayer)=Score(CurrentPlayer)+(Points*tCount)
		If RandomAwardTimer.Enabled=0 Then DisplayScore
	End If
	' check to see If the player is to be awarded a special (extra ball)
	If Score(CurrentPlayer)>=ReplayStart(CurrentPlayer,0) Then
		' and not run out of levels
		If ReplayStart(CurrentPlayer,1)<>0 Then
			ReplayStart(CurrentPlayer,0)=ReplayStart(CurrentPlayer,0)*2
			ReplayStart(CurrentPlayer,1)=ReplayStart(CurrentPlayer,1)-1
			' If free play award a extra ball, Else a free credit
			If gsFreePlay Then
				' award and extra ball to the player
				AwardExtraBall CurrentPlayer,1,TRUE
			Else
				' award a credit
				AwardFreeGame 1
			End If
		End If
	End If
End Sub

' Add points to the jackpot.  checks the mode of play and the max limit of the jackpot
Sub AddJackpot(points)
	' jackpot only increments in multiball mode and not tilted
	If bTilted=FALSE Then
		If b2BallMultiMode=TRUE Or b3BallMultiMode=TRUE Or b4BallMultiMode=TRUE Then
			gsJackpot=gsJackpot+points
			' check we havn't excedded the jackpot limit (10M)
			If gsJackpot>10000000 Then gsJackpot=10000000
		End If
	End If
End Sub

' the player won an extra ball
Sub AwardExtraBall(Player,eb,Replay)
	' give it to 'em
	ExtraBallsAwards(Player)=ExtraBallsAwards(Player)+eb
	' and light the light if it's not already on
	If ExtraBallsAwards(Player)>0 Then
		If LightShootAgain.State=0 Then LightShootAgain.State=1 'if any ballsaver isn't active, turn on the shoot again lamp
	End If
	If Replay Then
		' award a credit
		AwardFreeGame 1
	Else
		' blink (alternativly between the 2 lines)
		PlaySound"AExtraBall"
		If HurryUpActive=FALSE And HurryUpSingleActive=FALSE Then
			DisplayText"  EXTRA BALL  ",1,2:DMD_DisplaySceneTextWithPause "EXTRA BALL", "", 4000
			Timer1.Enabled=0
			Timer1.Enabled=1
		End If
		AwardedEB=TRUE
	End If
End Sub

Sub Timer1_Timer:Timer1.Enabled=0:DisplayText"  EXTRA BALL  ",2,2:End Sub


Sub Trigger5_Hit
	If bTilted=FALSE And AwardedEB=TRUE Then
		QTimer.Enabled=0
		DisplayScore
		AwardedEB=FALSE
	End If
End Sub

' award a few (or several) game by increasing the credits
Sub AwardFreeGame(Games)
	' Set the amount of games (credits) to award (cumulative)
	gvCreditsToAward=gvCreditsToAward+Games
	If Timer_AwardKnocker.Enabled=FALSE Then
		Timer_AwardKnocker.Interval=500
		Timer_AwardKnocker.Enabled=1
	End If
End Sub

' the award kocker timer has expired, make a noise and award a credit
Sub Timer_AwardKnocker_Timer
	Timer_AwardKnocker.Enabled=0
	If gsCredits<40 Then gsCredits=gsCredits+1
	DOF 209, DOFOn
	PlaySound SoundFXDOF("fx_Knocker",208,DOFPulse,DOFKnocker)
	DOF 242, DOFPulse
	gvCreditsToAward=gvCreditsToAward-1
	If gvCreditsToAward>0 Then
		Timer_AwardKnocker.Interval=500
		Timer_AwardKnocker.Enabled=1
	End If
End Sub

Sub IncreasePlayfieldScoring
	PlayfieldScoreTimer.Enabled=0
	PlayfieldScoreTimer.Enabled=1
	If Light3X.State=2 Then
		Light4X.State=2
	ElseIf Light2X.State=2 Then
		Light3X.State=2
	Else
		Light2X.State=2
	End If
End Sub

Sub PlayfieldScoreTimer_Timer
	'If playfield score multiplier is active, disable it
	Light2X.State=0
	Light3X.State=0
	Light4X.State=0
	Select Case BonusMultiplier(CurrentPlayer)
		Case 2:Light2X.State=1
		Case 3:Light3X.State=1
		Case 4:Light4X.State=1
	End Select
	PlayfieldScoreTimer.Enabled=0
End Sub


' build up the displays which handle the bonus screens when a ball is lost
Sub DoEndOfBallDisplay
	FollowMe.Enabled=0
	ComboTimer_Timer
	SP1=0:SP2=0:SP3=0:SP4=0:SP5=0:SP6=0:SP7=0
	LightProgress1.State=0:LightProgress2.State=0:LightProgress3.State=0:LightProgress4.State=0:LightProgress5.State=0:LightProgress6.State=0:LightProgress7.State=0
	SaveLampStates
	ChangeAllLamps 0,38
	PlayfieldScoreTimer_Timer 'disable playfield score timer
	BonusTime=0:CombosStatusText="              ":SongsStatusText="              "
	' ensure the timer is stopped
	TimerBonusDisplay.Enabled=0
	TimerStatusDisplay.Enabled=0
	STimer.Enabled=0
	' first ball is played
	gvFirstBallPlayed=TRUE
	gvBonusAmount=0
	gvBonusLoops=0
	If bTilted=FALSE Then
		BonusTime=4800
		Dim XVal
		XVal=0
		For X=1 To 12
			XVal=XVal+MusicCompleted2(X,CurrentPlayer,0)
		Next
		If XVal=0 Then SongsStatusText="   NO SONGS   "
		If XVal=1 Then SongsStatusText="    1 SONG    "
		If XVal>1 And XVal<10 Then SongsStatusText="   "&XVal&" SONGS    "
		If XVal>9 And XVal<100 Then SongsStatusText="  "&XVAL&" SONGS    "
		If XVal>99 And XVal<1000 Then SongsStatusText="  "&XVal&" SONGS   "
		If XVal>999 And XVal<10000 Then SongsStatusText=" "&XVal&" SONGS   "
		If XVal>9999 Then SongsStatusText=" 9999 SONGS   "
		gvBonusAmount=gvBonusAmount+XVal*20000
		If gvCombosThisBall=0 Then CombosStatusText="  NO COMBOS   "
		If gvCombosThisBall=1 Then CombosStatusText="   1 COMBO    "
		If gvCombosThisBall>1 And gvCombosThisBall<10 Then CombosStatusText="   "&gvCombosThisBall&" COMBOS   "
		If gvCombosThisBall>9 And gvCombosThisBall<100 Then CombosStatusText="  "&gvCombosThisBall&" COMBOS   "
		If gvCombosThisBall>99 And gvCombosThisBall<1000 Then CombosStatusText="  "&gvCombosThisBall&" COMBOS  "
		If gvCombosThisBall>999 And gvCombosThisBall<10000 Then CombosStatusText=" "&gvCombosThisBall&" COMBOS  "
		gvBonusAmount=gvBonusAmount+gvCombosThisBall*5000
		If gvBonusAmount>99999999 Then gvBonusAmount=99999999
		If BonusMultiplier(CurrentPlayer)>1 Then BonusTime=BonusTime+1600
		DisplayFlushQueue
		EOBA=0:EOBT.Enabled=0:EOBT.Enabled=1:EOBT_Timer
	Else
		BonusTime=20
	End If
	StopAllMusic
	If PlayersPlayingGame=1 Then
		If CurrentPlayer=0 Then
			DD 29,0,1:DD 30,0,1:DD 31,0,1:DD 32,0,1:DD 33,0,1:DD 34,0,1:DD 35,0,1:DD 36,0,1:DD 37,0,1:DD 38,0,1:DD 39,0,1:DD 40,0,1:DD 41,0,1:DD 42,0,1
			DD 43,0,1:DD 44,0,1:DD 45,0,1:DD 46,0,1:DD 47,0,1:DD 48,0,1:DD 49,0,1:DD 50,0,1:DD 51,0,1:DD 52,0,1:DD 53,0,1:DD 54,0,1:DD 55,0,1:DD 56,0,1
		End If
	End If
	' start the bonus timer
	TimerBonusDisplay.Interval=BonusTime
	TimerBonusDisplay.Enabled=1
End Sub

Sub EOBT_Timer
	EOBA=EOBA+20
	Select Case EOBA
		Case 20:JTS:PlaySound"A1CombosBonus":DisplayText3 CombosStatusText,2,1:DMD_DisplaySceneText CombosStatusText, ""
		Case 1620:If SongsStatusText<>"   NO SONGS   " Then:PlaySound"A2SongsBonus":End If:DisplayText3 SongsStatusText,2,1:DMD_DisplaySceneText SongsStatusText, ""
		Case 3220:If gvBonusAmount>999999 Then:PlaySound"A4Awesome":DOF 239, DOFPulse:End If:DisplayText3"BONUS         ",2,1:JTT:DMD_DisplaySceneText "BONUS", gvBonusAmount
		Case 4820:If BonusMultiplier(CurrentPlayer)>1 Then
						PlaySound"YCheer"
						gvBonusAmount=gvBonusAmount*BonusMultiplier(CurrentPlayer)
						If gvBonusAmount>99999999 Then gvBonusAmount=100000000
						DisplayText3"    X         ",2,1:JTT:DMD_DisplaySceneText "BONUS", " x"&BonusMultiplier(CurrentPlayer)&"  "&gvBonusAmount
						Select Case CurrentPlayer
							Case 0:DD 20,BonusMultiplier(CurrentPlayer)+37,1
							Case 1:DD 48,BonusMultiplier(CurrentPlayer)+37,1
							Case 2:DD 6,BonusMultiplier(CurrentPlayer)+37,1
							Case 3:DD 34,BonusMultiplier(CurrentPlayer)+37,1
						End Select
					Else
						EOBT.Enabled=0:EOBA=0
					End If
	End Select
End Sub

Sub JTT
	If gvBonusAmount>99999999 Then gvBonusAmount=100000000
	If CurrentPlayer=0 Then
		DisplayPoints(gvBonusAmount)
		DD 28,HS1N+37,1
		If HS3Z>1 Then:DD 27,HS1M+37,1:Else:DD 27,0,1:End If
		If HS3Z>2 Then:DD 26,HS1L+37,1:Else:DD 26,0,1:End If
		If HS3Z>3 Then:DD 25,HS1K+48,1:Else:DD 25,0,1:End If
		If HS3Z>4 Then:DD 24,HS1J+37,1:Else:DD 24,0,1:End If
		If HS3Z>5 Then:DD 23,HS1I+37,1:Else:DD 23,0,1:End If
		If HS3Z>6 Then:DD 22,HS1H+48,1:Else:DD 22,0,1:End If
		If HS3Z>7 Then:DD 21,HS1H+37,1:Else:DD 21,0,1:End If
	End If
	If CurrentPlayer=1 Then
		DisplayPoints(gvBonusAmount)
		DD 56,HS1N+37,1
		If HS3Z>1 Then:DD 55,HS1M+37,1:Else:DD 55,0,1:End If
		If HS3Z>2 Then:DD 54,HS1L+37,1:Else:DD 54,0,1:End If
		If HS3Z>3 Then:DD 53,HS1K+48,1:Else:DD 53,0,1:End If
		If HS3Z>4 Then:DD 52,HS1J+37,1:Else:DD 52,0,1:End If
		If HS3Z>5 Then:DD 51,HS1I+37,1:Else:DD 51,0,1:End If
		If HS3Z>6 Then:DD 50,HS1H+48,1:Else:DD 50,0,1:End If
		If HS3Z>7 Then:DD 49,HS1H+37,1:Else:DD 49,0,1:End If
	End If
	If CurrentPlayer=2 Then
		DisplayPoints(gvBonusAmount)
		DD 14,HS1N+37,1
		If HS3Z>1 Then:DD 13,HS1M+37,1:Else:DD 13,0,1:End If
		If HS3Z>2 Then:DD 12,HS1L+37,1:Else:DD 12,0,1:End If
		If HS3Z>3 Then:DD 11,HS1K+48,1:Else:DD 11,0,1:End If
		If HS3Z>4 Then:DD 10,HS1J+37,1:Else:DD 10,0,1:End If
		If HS3Z>5 Then:DD 9,HS1I+37,1:Else:DD 9,0,1:End If
		If HS3Z>6 Then:DD 8,HS1H+48,1:Else:DD 8,0,1:End If
		If HS3Z>7 Then:DD 7,HS1H+37,1:Else:DD 7,0,1:End If
	End If
	If CurrentPlayer=3 Then
		DisplayPoints(gvBonusAmount)
		DD 42,HS1N+37,1
		If HS3Z>1 Then:DD 41,HS1M+37,1:Else:DD 41,0,1:End If
		If HS3Z>2 Then:DD 40,HS1L+37,1:Else:DD 40,0,1:End If
		If HS3Z>3 Then:DD 39,HS1K+48,1:Else:DD 39,0,1:End If
		If HS3Z>4 Then:DD 38,HS1J+37,1:Else:DD 38,0,1:End If
		If HS3Z>5 Then:DD 37,HS1I+37,1:Else:DD 37,0,1:End If
		If HS3Z>6 Then:DD 36,HS1H+48,1:Else:DD 36,0,1:End If
		If HS3Z>7 Then:DD 35,HS1H+37,1:Else:DD 35,0,1:End If
	End If
End Sub

Sub JTS
	If CurrentPlayer=0 Then
		For Q=1 To 28:DD Q,0,1:Next
		DisplayPoints(Score(CurrentPlayer))
		DD 14,HS1N+37,1
		If HS3Z>1 Then:DD 13,HS1M+37,1:Else:DD 13,0,1:End If
		If HS3Z>2 Then:DD 12,HS1L+37,1:Else:DD 12,0,1:End If
		If HS3Z>3 Then:DD 11,HS1K+48,1:Else:DD 11,0,1:End If
		If HS3Z>4 Then:DD 10,HS1J+37,1:Else:DD 10,0,1:End If
		If HS3Z>5 Then:DD 9,HS1I+37,1:Else:DD 9,0,1:End If
		If HS3Z>6 Then:DD 8,HS1H+48,1:Else:DD 8,0,1:End If
		If HS3Z>7 Then:DD 7,HS1G+37,1:Else:DD 7,0,1:End If
		If HS3Z>8 Then:DD 6,HS1F+37,1:Else:DD 6,0,1:End If
		If HS3Z>9 Then:DD 5,HS1E+48,1:Else:DD 5,0,1:End If
		If HS3Z>10 Then:DD 4,HS1D+37,1:Else:DD 4,0,1:End If
		If HS3Z>11 Then:DD 3,HS1C+37,1:Else:DD 3,0,1:End If
		If HS3Z>12 Then:DD 2,HS1B+48,1:Else:DD 2,0,1:End If
		If HS3Z>13 Then:DD 1,HS1A+37,1:Else:DD 1,0,1:End If
		DD 15,2,1:DD 16,15,1:DD 17,14,1:DD 18,21,1:DD 19,19,1
		DisplayPoints(gvBonusAmount)
		DD 28,HS1N+37,1
		If HS3Z>1 Then:DD 27,HS1M+37,1:Else:DD 27,0,1:End If
		If HS3Z>2 Then:DD 26,HS1L+37,1:Else:DD 26,0,1:End If
		If HS3Z>3 Then:DD 25,HS1K+48,1:Else:DD 25,0,1:End If
		If HS3Z>4 Then:DD 24,HS1J+37,1:Else:DD 24,0,1:End If
		If HS3Z>5 Then:DD 23,HS1I+37,1:Else:DD 23,0,1:End If
		If HS3Z>6 Then:DD 22,HS1H+48,1:Else:DD 22,0,1:End If
		If HS3Z>7 Then:DD 21,HS1H+37,1:Else:DD 21,0,1:End If
	End If
	If CurrentPlayer=1 Then
		For Q=29 To 56:DD Q,0,1:Next
		DisplayPoints(Score(CurrentPlayer))
		DD 42,HS1N+37,1
		If HS3Z>1 Then:DD 41,HS1M+37,1:Else:DD 41,0,1:End If
		If HS3Z>2 Then:DD 40,HS1L+37,1:Else:DD 40,0,1:End If
		If HS3Z>3 Then:DD 39,HS1K+48,1:Else:DD 39,0,1:End If
		If HS3Z>4 Then:DD 38,HS1J+37,1:Else:DD 38,0,1:End If
		If HS3Z>5 Then:DD 37,HS1I+37,1:Else:DD 37,0,1:End If
		If HS3Z>6 Then:DD 36,HS1H+48,1:Else:DD 36,0,1:End If
		If HS3Z>7 Then:DD 35,HS1G+37,1:Else:DD 35,0,1:End If
		If HS3Z>8 Then:DD 34,HS1F+37,1:Else:DD 34,0,1:End If
		If HS3Z>9 Then:DD 33,HS1E+48,1:Else:DD 33,0,1:End If
		If HS3Z>10 Then:DD 32,HS1D+37,1:Else:DD 32,0,1:End If
		If HS3Z>11 Then:DD 31,HS1C+37,1:Else:DD 31,0,1:End If
		If HS3Z>12 Then:DD 30,HS1B+48,1:Else:DD 30,0,1:End If
		If HS3Z>13 Then:DD 29,HS1A+37,1:Else:DD 29,0,1:End If
		DD 43,2,1:DD 44,15,1:DD 45,14,1:DD 46,21,1:DD 47,19,1
		DisplayPoints(gvBonusAmount)
		DD 56,HS1N+37,1
		If HS3Z>1 Then:DD 55,HS1M+37,1:Else:DD 55,0,1:End If
		If HS3Z>2 Then:DD 54,HS1L+37,1:Else:DD 54,0,1:End If
		If HS3Z>3 Then:DD 53,HS1K+48,1:Else:DD 53,0,1:End If
		If HS3Z>4 Then:DD 52,HS1J+37,1:Else:DD 52,0,1:End If
		If HS3Z>5 Then:DD 51,HS1I+37,1:Else:DD 51,0,1:End If
		If HS3Z>6 Then:DD 50,HS1H+48,1:Else:DD 50,0,1:End If
		If HS3Z>7 Then:DD 49,HS1H+37,1:Else:DD 49,0,1:End If
	End If
	If CurrentPlayer=2 Then
		For Q=1 To 28:DD Q,0,1:Next
		DisplayPoints(Score(CurrentPlayer))
		DD 28,HS1N+37,1
		If HS3Z>1 Then:DD 27,HS1M+37,1:Else:DD 27,0,1:End If
		If HS3Z>2 Then:DD 26,HS1L+37,1:Else:DD 26,0,1:End If
		If HS3Z>3 Then:DD 25,HS1K+48,1:Else:DD 25,0,1:End If
		If HS3Z>4 Then:DD 24,HS1J+37,1:Else:DD 24,0,1:End If
		If HS3Z>5 Then:DD 23,HS1I+37,1:Else:DD 23,0,1:End If
		If HS3Z>6 Then:DD 22,HS1H+48,1:Else:DD 22,0,1:End If
		If HS3Z>7 Then:DD 21,HS1G+37,1:Else:DD 21,0,1:End If
		If HS3Z>8 Then:DD 20,HS1F+37,1:Else:DD 20,0,1:End If
		If HS3Z>9 Then:DD 19,HS1E+48,1:Else:DD 19,0,1:End If
		If HS3Z>10 Then:DD 18,HS1D+37,1:Else:DD 18,0,1:End If
		If HS3Z>11 Then:DD 17,HS1C+37,1:Else:DD 17,0,1:End If
		If HS3Z>12 Then:DD 16,HS1B+48,1:Else:DD 16,0,1:End If
		If HS3Z>13 Then:DD 15,HS1A+37,1:Else:DD 15,0,1:End If
		DD 1,2,1:DD 2,15,1:DD 3,14,1:DD 4,21,1:DD 5,19,1
		DisplayPoints(gvBonusAmount)
		DD 14,HS1N+37,1
		If HS3Z>1 Then:DD 13,HS1M+37,1:Else:DD 13,0,1:End If
		If HS3Z>2 Then:DD 12,HS1L+37,1:Else:DD 12,0,1:End If
		If HS3Z>3 Then:DD 11,HS1K+48,1:Else:DD 11,0,1:End If
		If HS3Z>4 Then:DD 10,HS1J+37,1:Else:DD 10,0,1:End If
		If HS3Z>5 Then:DD 9,HS1I+37,1:Else:DD 9,0,1:End If
		If HS3Z>6 Then:DD 8,HS1H+48,1:Else:DD 8,0,1:End If
		If HS3Z>7 Then:DD 7,HS1H+37,1:Else:DD 7,0,1:End If
	End If
	If CurrentPlayer=3 Then
		For Q=29 To 56:DD Q,0,1:Next
		DisplayPoints(Score(CurrentPlayer))
		DD 56,HS1N+37,1
		If HS3Z>1 Then:DD 55,HS1M+37,1:Else:DD 55,0,1:End If
		If HS3Z>2 Then:DD 54,HS1L+37,1:Else:DD 54,0,1:End If
		If HS3Z>3 Then:DD 53,HS1K+48,1:Else:DD 53,0,1:End If
		If HS3Z>4 Then:DD 52,HS1J+37,1:Else:DD 52,0,1:End If
		If HS3Z>5 Then:DD 51,HS1I+37,1:Else:DD 51,0,1:End If
		If HS3Z>6 Then:DD 50,HS1H+48,1:Else:DD 50,0,1:End If
		If HS3Z>7 Then:DD 49,HS1G+37,1:Else:DD 49,0,1:End If
		If HS3Z>8 Then:DD 48,HS1F+37,1:Else:DD 48,0,1:End If
		If HS3Z>9 Then:DD 47,HS1E+48,1:Else:DD 47,0,1:End If
		If HS3Z>10 Then:DD 46,HS1D+37,1:Else:DD 46,0,1:End If
		If HS3Z>11 Then:DD 45,HS1C+37,1:Else:DD 45,0,1:End If
		If HS3Z>12 Then:DD 44,HS1B+48,1:Else:DD 44,0,1:End If
		If HS3Z>13 Then:DD 43,HS1A+37,1:Else:DD 43,0,1:End If
		DD 29,2,1:DD 30,15,1:DD 31,14,1:DD 32,21,1:DD 33,19,1
		DisplayPoints(gvBonusAmount)
		DD 42,HS1N+37,1
		If HS3Z>1 Then:DD 41,HS1M+37,1:Else:DD 41,0,1:End If
		If HS3Z>2 Then:DD 40,HS1L+37,1:Else:DD 40,0,1:End If
		If HS3Z>3 Then:DD 39,HS1K+48,1:Else:DD 39,0,1:End If
		If HS3Z>4 Then:DD 38,HS1J+37,1:Else:DD 38,0,1:End If
		If HS3Z>5 Then:DD 37,HS1I+37,1:Else:DD 37,0,1:End If
		If HS3Z>6 Then:DD 36,HS1H+48,1:Else:DD 36,0,1:End If
		If HS3Z>7 Then:DD 35,HS1H+37,1:Else:DD 35,0,1:End If
	End If
End Sub

' The bonus timer has expired, decrease the bonus amount.
' When finished check to see If we need to swap players
' or is it the end of the game ?
Sub TimerBonusDisplay_Timer
	Dim Timer,Amount
	TimerBonusDisplay.Enabled=0
	DisplayFlushQueue
	'just in case some smart arse tilts it while the bonus is ticking down
	If bTilted Then gvBonusAmount=0
	If gvBonusAmount>99999999 Then gvBonusAmount=99999999
	If gvBonusAmount>0 Then
		gvBonusLoops=gvBonusLoops+1
		If CurrentSound="" Then
			PlaySound"A3BonusCount",-1
			CurrentSound="A3BonusCount"
		End If
		If gvBonusAmount>10000 Then
			Amount=10000
		Else
			If gvBonusAmount>1000 Then
				Amount=1000
			Else
				Amount=gvBonusAmount
			End If
		End If
		AddScore(Amount)
		gvBonusAmount=gvBonusAmount-Amount
		If CurrentPlayer=0 Then
			For Q=1 To 28:DD Q,0,1:Next
			DisplayPoints(Score(CurrentPlayer))
			DD 14,HS1N+37,1
			If HS3Z>1 Then:DD 13,HS1M+37,1:Else:DD 13,0,1:End If
			If HS3Z>2 Then:DD 12,HS1L+37,1:Else:DD 12,0,1:End If
			If HS3Z>3 Then:DD 11,HS1K+48,1:Else:DD 11,0,1:End If
			If HS3Z>4 Then:DD 10,HS1J+37,1:Else:DD 10,0,1:End If
			If HS3Z>5 Then:DD 9,HS1I+37,1:Else:DD 9,0,1:End If
			If HS3Z>6 Then:DD 8,HS1H+48,1:Else:DD 8,0,1:End If
			If HS3Z>7 Then:DD 7,HS1G+37,1:Else:DD 7,0,1:End If
			If HS3Z>8 Then:DD 6,HS1F+37,1:Else:DD 6,0,1:End If
			If HS3Z>9 Then:DD 5,HS1E+48,1:Else:DD 5,0,1:End If
			If HS3Z>10 Then:DD 4,HS1D+37,1:Else:DD 4,0,1:End If
			If HS3Z>11 Then:DD 3,HS1C+37,1:Else:DD 3,0,1:End If
			If HS3Z>12 Then:DD 2,HS1B+48,1:Else:DD 2,0,1:End If
			If HS3Z>13 Then:DD 1,HS1A+37,1:Else:DD 1,0,1:End If
			DD 15,2,1:DD 16,15,1:DD 17,14,1:DD 18,21,1:DD 19,19,1
			If gvBonusAmount>99999999 Then gvBonusAmount=99999999
			DisplayPoints(gvBonusAmount)
			DD 28,HS1N+37,1
			If HS3Z>1 Then:DD 27,HS1M+37,1:Else:DD 27,0,1:End If
			If HS3Z>2 Then:DD 26,HS1L+37,1:Else:DD 26,0,1:End If
			If HS3Z>3 Then:DD 25,HS1K+48,1:Else:DD 25,0,1:End If
			If HS3Z>4 Then:DD 24,HS1J+37,1:Else:DD 24,0,1:End If
			If HS3Z>5 Then:DD 23,HS1I+37,1:Else:DD 23,0,1:End If
			If HS3Z>6 Then:DD 22,HS1H+48,1:Else:DD 22,0,1:End If
			If HS3Z>7 Then:DD 21,HS1H+37,1:Else:DD 21,0,1:End If
		End If
		If CurrentPlayer=1 Then
			For Q=29 To 56:DD Q,0,1:Next
			DisplayPoints(Score(CurrentPlayer))
			DD 42,HS1N+37,1
			If HS3Z>1 Then:DD 41,HS1M+37,1:Else:DD 41,0,1:End If
			If HS3Z>2 Then:DD 40,HS1L+37,1:Else:DD 40,0,1:End If
			If HS3Z>3 Then:DD 39,HS1K+48,1:Else:DD 39,0,1:End If
			If HS3Z>4 Then:DD 38,HS1J+37,1:Else:DD 38,0,1:End If
			If HS3Z>5 Then:DD 37,HS1I+37,1:Else:DD 37,0,1:End If
			If HS3Z>6 Then:DD 36,HS1H+48,1:Else:DD 36,0,1:End If
			If HS3Z>7 Then:DD 35,HS1G+37,1:Else:DD 35,0,1:End If
			If HS3Z>8 Then:DD 34,HS1F+37,1:Else:DD 34,0,1:End If
			If HS3Z>9 Then:DD 33,HS1E+48,1:Else:DD 33,0,1:End If
			If HS3Z>10 Then:DD 32,HS1D+37,1:Else:DD 32,0,1:End If
			If HS3Z>11 Then:DD 31,HS1C+37,1:Else:DD 31,0,1:End If
			If HS3Z>12 Then:DD 30,HS1B+48,1:Else:DD 30,0,1:End If
			If HS3Z>13 Then:DD 29,HS1A+37,1:Else:DD 29,0,1:End If
			DD 43,2,1:DD 44,15,1:DD 45,14,1:DD 46,21,1:DD 47,19,1
			If gvBonusAmount>99999999 Then gvBonusAmount=99999999
			DisplayPoints(gvBonusAmount)
			DD 56,HS1N+37,1
			If HS3Z>1 Then:DD 55,HS1M+37,1:Else:DD 55,0,1:End If
			If HS3Z>2 Then:DD 54,HS1L+37,1:Else:DD 54,0,1:End If
			If HS3Z>3 Then:DD 53,HS1K+48,1:Else:DD 53,0,1:End If
			If HS3Z>4 Then:DD 52,HS1J+37,1:Else:DD 52,0,1:End If
			If HS3Z>5 Then:DD 51,HS1I+37,1:Else:DD 51,0,1:End If
			If HS3Z>6 Then:DD 50,HS1H+48,1:Else:DD 50,0,1:End If
			If HS3Z>7 Then:DD 49,HS1H+37,1:Else:DD 49,0,1:End If
		End If
		If CurrentPlayer=2 Then
			For Q=1 To 28:DD Q,0,1:Next
			DisplayPoints(Score(CurrentPlayer))
			DD 28,HS1N+37,1
			If HS3Z>1 Then:DD 27,HS1M+37,1:Else:DD 27,0,1:End If
			If HS3Z>2 Then:DD 26,HS1L+37,1:Else:DD 26,0,1:End If
			If HS3Z>3 Then:DD 25,HS1K+48,1:Else:DD 25,0,1:End If
			If HS3Z>4 Then:DD 24,HS1J+37,1:Else:DD 24,0,1:End If
			If HS3Z>5 Then:DD 23,HS1I+37,1:Else:DD 23,0,1:End If
			If HS3Z>6 Then:DD 22,HS1H+48,1:Else:DD 22,0,1:End If
			If HS3Z>7 Then:DD 21,HS1G+37,1:Else:DD 21,0,1:End If
			If HS3Z>8 Then:DD 20,HS1F+37,1:Else:DD 20,0,1:End If
			If HS3Z>9 Then:DD 19,HS1E+48,1:Else:DD 19,0,1:End If
			If HS3Z>10 Then:DD 18,HS1D+37,1:Else:DD 18,0,1:End If
			If HS3Z>11 Then:DD 17,HS1C+37,1:Else:DD 17,0,1:End If
			If HS3Z>12 Then:DD 16,HS1B+48,1:Else:DD 16,0,1:End If
			If HS3Z>13 Then:DD 15,HS1A+37,1:Else:DD 15,0,1:End If
			DD 1,2,1:DD 2,15,1:DD 3,14,1:DD 4,21,1:DD 5,19,1
			If gvBonusAmount>99999999 Then gvBonusAmount=99999999
			DisplayPoints(gvBonusAmount)
			DD 14,HS1N+37,1
			If HS3Z>1 Then:DD 13,HS1M+37,1:Else:DD 13,0,1:End If
			If HS3Z>2 Then:DD 12,HS1L+37,1:Else:DD 12,0,1:End If
			If HS3Z>3 Then:DD 11,HS1K+48,1:Else:DD 11,0,1:End If
			If HS3Z>4 Then:DD 10,HS1J+37,1:Else:DD 10,0,1:End If
			If HS3Z>5 Then:DD 9,HS1I+37,1:Else:DD 9,0,1:End If
			If HS3Z>6 Then:DD 8,HS1H+48,1:Else:DD 8,0,1:End If
			If HS3Z>7 Then:DD 7,HS1H+37,1:Else:DD 7,0,1:End If
		End If
		If CurrentPlayer=3 Then
			For Q=29 To 56:DD Q,0,1:Next
			DisplayPoints(Score(CurrentPlayer))
			DD 56,HS1N+37,1
			If HS3Z>1 Then:DD 55,HS1M+37,1:Else:DD 55,0,1:End If
			If HS3Z>2 Then:DD 54,HS1L+37,1:Else:DD 54,0,1:End If
			If HS3Z>3 Then:DD 53,HS1K+48,1:Else:DD 53,0,1:End If
			If HS3Z>4 Then:DD 52,HS1J+37,1:Else:DD 52,0,1:End If
			If HS3Z>5 Then:DD 51,HS1I+37,1:Else:DD 51,0,1:End If
			If HS3Z>6 Then:DD 50,HS1H+48,1:Else:DD 50,0,1:End If
			If HS3Z>7 Then:DD 49,HS1G+37,1:Else:DD 49,0,1:End If
			If HS3Z>8 Then:DD 48,HS1F+37,1:Else:DD 48,0,1:End If
			If HS3Z>9 Then:DD 47,HS1E+48,1:Else:DD 47,0,1:End If
			If HS3Z>10 Then:DD 46,HS1D+37,1:Else:DD 46,0,1:End If
			If HS3Z>11 Then:DD 45,HS1C+37,1:Else:DD 45,0,1:End If
			If HS3Z>12 Then:DD 44,HS1B+48,1:Else:DD 44,0,1:End If
			If HS3Z>13 Then:DD 43,HS1A+37,1:Else:DD 43,0,1:End If
			DD 29,2,1:DD 30,15,1:DD 31,14,1:DD 32,21,1:DD 33,19,1
			If gvBonusAmount>99999999 Then gvBonusAmount=99999999
			DisplayPoints(gvBonusAmount)
			DD 42,HS1N+37,1
			If HS3Z>1 Then:DD 41,HS1M+37,1:Else:DD 41,0,1:End If
			If HS3Z>2 Then:DD 40,HS1L+37,1:Else:DD 40,0,1:End If
			If HS3Z>3 Then:DD 39,HS1K+48,1:Else:DD 39,0,1:End If
			If HS3Z>4 Then:DD 38,HS1J+37,1:Else:DD 38,0,1:End If
			If HS3Z>5 Then:DD 37,HS1I+37,1:Else:DD 37,0,1:End If
			If HS3Z>6 Then:DD 36,HS1H+48,1:Else:DD 36,0,1:End If
			If HS3Z>7 Then:DD 35,HS1H+37,1:Else:DD 35,0,1:End If
		End If
		If gvBonusLoops<25 Then
			Timer=60-gvBonusLoops
		Else
			Timer=25
		End If
		TimerBonusDisplay.Interval=Timer
		TimerBonusDisplay.Enabled=1
	Else
		StopSound"A3BonusCount"
		CurrentSound=""
		' bonus count down has finished
		If ExtraBallsAwards(CurrentPlayer)=0 Then BallsRemaining(CurrentPlayer)=BallsRemaining(CurrentPlayer)-1
		' has the player won an extra-ball ? (might be multiple outstanding)
		If ExtraBallsAwards(CurrentPlayer)>0 Then
			ExtraBallsAwards(CurrentPlayer)=ExtraBallsAwards(CurrentPlayer)-1
			If ExtraBallsAwards(CurrentPlayer)=0 Then LightShootAgain.State=0
			' display shoot again on the screen
			DisplayFlushQueue
			PlaySound"YCheer"
			DisplayText" SAME PLAYER  ",1,1
			DisplayText" SHOOTS AGAIN ",2,2
			DMD_DisplaySceneText "SAME PLAYER", "SHOOTS AGAIN"
			' create a new ball
			ResetForNewBall
			CreateNewBall
		Else
			' no extra ball
			' are there any more balls?
			If BallsRemaining(CurrentPlayer)<=0 Then
				CheckTrackScoresVal=FALSE
				For Z4=1 To 12
					If MusicScore(CurrentPlayer,Z4)>=gsTrackScore(Z4) Then
						CheckTrackScoresVal=TRUE
						Exit For
					End If
				Next
				' has the player beaten any of the high scores or made combo champ, or broken any of the 12 track scores?
				If Score(CurrentPlayer)>=gsHighScore(3) Or CombosThisGame(CurrentPlayer)>=gvCombosforComboChamp Or CheckTrackScoresVal=TRUE Then
					' you are champion
					Dim RandomClip
					RandomClip=INT(RND*4)
					Select Case RandomClip
						Case 0:PlaySound"AStickAround"
						Case 1:PlaySound"ABrokenGlass"
						Case 2:PlaySound"ACongrats"
						Case 3:PlaySound"ADrums"
					End Select
					' start the high score music
					StopAllMusic
					PlaySound"AHSMusic",-1
					HighScoreEntryInit
				Else
					' no more balls (and no HS), but maybe another player ?
					DoEndOfBallComplete
				End If
			Else
				' more balls, move on
				DoEndOfBallComplete
			End If
		End If    ' If (ExtraBallsAwards)
	End If    ' If (gvBonusAmount)
End Sub

' this function is called at the end of bonus display
' (or high score entry finished) and it either ends the game or
' moves onto the next player (or the next ball of the same player)
Sub DoEndOfBallComplete
	Dim NextPlayer
	' work out the next player (If any)
	NextPlayer=CurrentPlayer+1
	If NextPlayer>PlayersPlayingGame-1 Then NextPlayer=0
	' is it the end of the game ? (all balls been lost for all players)
	If BallsRemaining(CurrentPlayer)<=0 And BallsRemaining(NextPlayer)<=0 Then
		bEndOfGame=TRUE
		MatchRunning=TRUE :PuPEvent 817
	Else
		bEndOfGame=FALSE
	End If
	DisplayFlushQueue
	If bEndOfGame Then
		' If playing for credits Then do the end of game match which can give a free credit
		If gsFreePlay=FALSE And gsMatchPer>0 Then
			DD 1,0,1:DD 2,0,1:DD 3,0,1:DD 4,0,1:DD 5,0,1:DD 6,0,1:DD 7,0,1:DD 8,0,1:DD 9,0,1:DD 10,0,1:DD 11,0,1:DD 12,0,1:DD 13,0,1:DD 14,0,1
			DD 15,0,1:DD 16,0,1:DD 17,0,1:DD 18,0,1:DD 19,0,1:DD 20,0,1:DD 21,0,1:DD 22,0,1:DD 23,0,1:DD 24,0,1:DD 25,0,1:DD 26,0,1:DD 27,0,1:DD 28,0,1
			DD 29,0,1:DD 30,0,1:DD 31,0,1:DD 32,0,1:DD 33,0,1:DD 34,0,1:DD 35,0,1:DD 36,0,1:DD 37,0,1:DD 38,0,1:DD 39,0,1:DD 40,0,1:DD 41,0,1:DD 42,0,1
			DD 43,0,1:DD 44,0,1:DD 45,0,1:DD 46,0,1:DD 47,0,1:DD 48,0,1:DD 49,0,1:DD 50,0,1:DD 51,0,1:DD 52,0,1:DD 53,0,1:DD 54,0,1:DD 55,0,1:DD 56,0,1
			DoMatchDisplay
		Else
			DelayTimer.Enabled=1
			DisplayPoints(Score(0))
			DD 14,HS1N+37,1
			If HS3Z>1 Then:DD 13,HS1M+37,1:Else:DD 13,0,1:End If
			If HS3Z>2 Then:DD 12,HS1L+37,1:Else:DD 12,0,1:End If
			If HS3Z>3 Then:DD 11,HS1K+48,1:Else:DD 11,0,1:End If
			If HS3Z>4 Then:DD 10,HS1J+37,1:Else:DD 10,0,1:End If
			If HS3Z>5 Then:DD 9,HS1I+37,1:Else:DD 9,0,1:End If
			If HS3Z>6 Then:DD 8,HS1H+48,1:Else:DD 8,0,1:End If
			If HS3Z>7 Then:DD 7,HS1G+37,1:Else:DD 7,0,1:End If
			If HS3Z>8 Then:DD 6,HS1F+37,1:Else:DD 6,0,1:End If
			If HS3Z>9 Then:DD 5,HS1E+48,1:Else:DD 5,0,1:End If
			If HS3Z>10 Then:DD 4,HS1D+37,1:Else:DD 4,0,1:End If
			If HS3Z>11 Then:DD 3,HS1C+37,1:Else:DD 3,0,1:End If
			If HS3Z>12 Then:DD 2,HS1B+48,1:Else:DD 2,0,1:End If
			If HS3Z>13 Then:DD 1,HS1A+37,1:Else:DD 1,0,1:End If
			DD 15,0,1:DD 16,0,1:DD 17,7,1:DD 18,1,1:DD 19,13,1:DD 20,5,1:DD 21,0,1:DD 22,0,1:DD 23,15,1:DD 24,22,1:DD 25,5,1:DD 26,18,1:DD 27,0,1:DD 28,0,1
			If PlayersPlayingGame>1 Then
				DisplayPoints(Score(1))
				DD 42,HS1N+37,1
				If HS3Z>1 Then:DD 41,HS1M+37,1:Else:DD 41,0,1:End If
				If HS3Z>2 Then:DD 40,HS1L+37,1:Else:DD 40,0,1:End If
				If HS3Z>3 Then:DD 39,HS1K+48,1:Else:DD 39,0,1:End If
				If HS3Z>4 Then:DD 38,HS1J+37,1:Else:DD 38,0,1:End If
				If HS3Z>5 Then:DD 37,HS1I+37,1:Else:DD 37,0,1:End If
				If HS3Z>6 Then:DD 36,HS1H+48,1:Else:DD 36,0,1:End If
				If HS3Z>7 Then:DD 35,HS1G+37,1:Else:DD 35,0,1:End If
				If HS3Z>8 Then:DD 34,HS1F+37,1:Else:DD 34,0,1:End If
				If HS3Z>9 Then:DD 33,HS1E+48,1:Else:DD 33,0,1:End If
				If HS3Z>10 Then:DD 32,HS1D+37,1:Else:DD 32,0,1:End If
				If HS3Z>11 Then:DD 31,HS1C+37,1:Else:DD 31,0,1:End If
				If HS3Z>12 Then:DD 30,HS1B+48,1:Else:DD 30,0,1:End If
				If HS3Z>13 Then:DD 29,HS1A+37,1:Else:DD 29,0,1:End If
				DD 15,0,1:DD 16,0,1:DD 17,7,1:DD 18,1,1:DD 19,13,1:DD 20,5,1:DD 21,0,1:DD 22,0,1:DD 23,15,1:DD 24,22,1:DD 25,5,1:DD 26,18,1:DD 27,0,1:DD 28,0,1
				DD 43,0,1:DD 44,0,1:DD 45,7,1:DD 46,1,1:DD 47,13,1:DD 48,5,1:DD 49,0,1:DD 50,0,1:DD 51,15,1:DD 52,22,1:DD 53,5,1:DD 54,18,1:DD 55,0,1:DD 56,0,1
			End If
			If PlayersPlayingGame>2 Then
				DisplayPoints(Score(2))
				DD 28,HS1N+37,1
				If HS3Z>1 Then:DD 27,HS1M+37,1:Else:DD 41,0,1:End If
				If HS3Z>2 Then:DD 26,HS1L+37,1:Else:DD 40,0,1:End If
				If HS3Z>3 Then:DD 25,HS1K+48,1:Else:DD 39,0,1:End If
				If HS3Z>4 Then:DD 24,HS1J+37,1:Else:DD 38,0,1:End If
				If HS3Z>5 Then:DD 23,HS1I+37,1:Else:DD 37,0,1:End If
				If HS3Z>6 Then:DD 22,HS1H+48,1:Else:DD 36,0,1:End If
				If HS3Z>7 Then:DD 21,HS1G+37,1:Else:DD 35,0,1:End If
				If HS3Z>8 Then:DD 20,HS1F+37,1:Else:DD 34,0,1:End If
				If HS3Z>9 Then:DD 19,HS1E+48,1:Else:DD 33,0,1:End If
				If HS3Z>10 Then:DD 18,HS1D+37,1:Else:DD 32,0,1:End If
				If HS3Z>11 Then:DD 17,HS1C+37,1:Else:DD 31,0,1:End If
				If HS3Z>12 Then:DD 16,HS1B+48,1:Else:DD 30,0,1:End If
				If HS3Z>13 Then:DD 15,HS1A+37,1:Else:DD 29,0,1:End If
				DD 43,0,1:DD 44,0,1:DD 45,7,1:DD 46,1,1:DD 47,13,1:DD 48,5,1:DD 49,0,1:DD 50,0,1:DD 51,15,1:DD 52,22,1:DD 53,5,1:DD 54,18,1:DD 55,0,1:DD 56,0,1
			End If
			If PlayersPlayingGame>3 Then
				DisplayPoints(Score(3))
				DD 56,HS1N+37,1
				If HS3Z>1 Then:DD 55,HS1M+37,1:Else:DD 41,0,1:End If
				If HS3Z>2 Then:DD 54,HS1L+37,1:Else:DD 40,0,1:End If
				If HS3Z>3 Then:DD 53,HS1K+48,1:Else:DD 39,0,1:End If
				If HS3Z>4 Then:DD 52,HS1J+37,1:Else:DD 38,0,1:End If
				If HS3Z>5 Then:DD 51,HS1I+37,1:Else:DD 37,0,1:End If
				If HS3Z>6 Then:DD 50,HS1H+48,1:Else:DD 36,0,1:End If
				If HS3Z>7 Then:DD 49,HS1G+37,1:Else:DD 35,0,1:End If
				If HS3Z>8 Then:DD 48,HS1F+37,1:Else:DD 34,0,1:End If
				If HS3Z>9 Then:DD 47,HS1E+48,1:Else:DD 33,0,1:End If
				If HS3Z>10 Then:DD 46,HS1D+37,1:Else:DD 32,0,1:End If
				If HS3Z>11 Then:DD 45,HS1C+37,1:Else:DD 31,0,1:End If
				If HS3Z>12 Then:DD 44,HS1B+48,1:Else:DD 30,0,1:End If
				If HS3Z>13 Then:DD 43,HS1A+37,1:Else:DD 29,0,1:End If
			End If
		End If
	Else
		' Set the next player
		CurrentPlayer=NextPlayer
		DisplayFlushQueue
		' create a new ball
		ResetForNewBall
		CreateNewBall


	End If
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' Setup the match feature (at the end of the game)
Sub DoMatchDisplay
	' reset the flippers
	LeftUp=FALSE
	RightUp=FALSE
	RightFlipper.RotateToStart
	TopLeftFlipper.RotateToStart
	RightFlipper.RotateToStart
	LockDiverter.IsDropped=0 'close off lock access
	LockDiverter2.IsDropped=1
	' Set the last matched value
	gvLastMatchValue=100     ' Set it to a invalid value so all combinations
	gvLastMatchPosition=0
	StopAllMusic
	DMD_SetScoreboardBackground "match.png"
	' Set the total amount of loops to do (5 per player)
	gvMatchLoops=10
	' Set the loop interval
	Timer_MatchDisplay.Enabled=0
	Timer_MatchDisplay.Interval=80
	Timer_MatchDisplay.Enabled=1
End Sub

Sub Timer_MatchDisplay_Timer
	Dim Last2DigitsA,Last2DigitsB,Last2DigitsC,Last2DigitsD,RndMatch,BotText,RndPosition,Length,TMatchVal
	Timer_MatchDisplay.Enabled=0
	' If the loop count=0 Then we use that to end the game and return to attract mode
	If gvMatchLoops>0 Then
		If PlayersPlayingGame>0 Then
			Last2DigitsA=Score(0) MOD 100
			DD 1,13,1:DD 2,1,1:DD 3,20,1:DD 4,3,1:DD 5,8,1:DD 6,0,1:DD 7,0,1:DD 8,0,1:DD 9,0,1:DD 10,0,1:DD 11,0,1:DD 12,0,1:DD 13,Last2DigitsA/10+37,1:DD 14,37,1
		End If
		If PlayersPlayingGame>1 Then
			Last2DigitsB=Score(1) MOD 100
			DD 6,0,1:DD 7,0,1:DD 8,0,1:DD 9,Last2DigitsA/10+37,1:DD 10,37,1:DD 11,0,1:DD 12,0,1:DD 13,Last2DigitsB/10+37,1:DD 14,37,1
		End If
		If PlayersPlayingGame>2 Then
			Last2DigitsC=Score(2) MOD 100
			DD 29,13,1:DD 30,1,1:DD 31,20,1:DD 32,3,1:DD 33,8,1:DD 34,0,1:DD 35,0,1:DD 36,0,1:DD 37,0,1:DD 38,0,1:DD 39,0,1:DD 40,0,1:DD 41,Last2DigitsC/10+37,1:DD 42,37,1
		End If
		If PlayersPlayingGame>3 Then
			Last2DigitsD=Score(3) MOD 100
			DD 34,0,1:DD 35,0,1:DD 36,0,1:DD 37,Last2DigitsC/10+37,1:DD 38,37,1:DD 39,0,1:DD 40,0,1:DD 41,Last2DigitsD/10+37,1:DD 42,37,1
		End If
		' get a random match that doesn't equal the last random number picked
		If gvMatchLoops>2 Then
			Do
				RndMatch=INT(Rnd(1)*10)   ' pick a number between 0 and 9
				RndMatch=RndMatch*10      ' convert it to 00 -> 90
			Loop Until RndMatch<>gvLastMatchValue
			' remember this value for next time
			gvLastMatchValue=RndMatch
			DD 15+gvLastMatchPosition,0,1:DD 16+gvLastMatchPosition,0,1
			DD 43+gvLastMatchPosition,0,1:DD 44+gvLastMatchPosition,0,1
			Do
				RndPosition=INT(Rnd(1)*5) ' pick a number between 0 and 5
				RndPosition=RndPosition*2
				RndPosition=RndPosition+2
			Loop Until RndPosition<>gvLastMatchPosition
			gvLastMatchPosition=RndPosition
			DD 15+RndPosition,RndMatch/10+37,1:DD 16+RndPosition,37,1:DMD_DisplayMatch RndPosition, RndMatch
			If PlayersPlayingGame>1 Then:DD 43+RndPosition,RndMatch/10+37,1:DD 44+RndPosition,37,1:End If
		Else
			If PlayersPlayingGame=1 Then
				Do
					RndMatch=INT(Rnd(1)*10)   ' pick a number between 0 and 9
					RndMatch=RndMatch*10      ' convert it to 00 -> 90
				Loop Until RndMatch<>gvLastMatchValue And RndMatch<>Last2DigitsA
			End If
			If PlayersPlayingGame=2 Then
				Do
					RndMatch=INT(Rnd(1)*10)   ' pick a number between 0 and 9
					RndMatch=RndMatch*10      ' convert it to 00 -> 90
				Loop Until RndMatch<>gvLastMatchValue And RndMatch<>Last2DigitsA And RndMatch<>Last2DigitsB
			End If
			If PlayersPlayingGame=3 Then
				Do
					RndMatch=INT(Rnd(1)*10)   ' pick a number between 0 and 9
					RndMatch=RndMatch*10      ' convert it to 00 -> 90
				Loop Until RndMatch<>gvLastMatchValue And RndMatch<>Last2DigitsA And RndMatch<>Last2DigitsB And RndMatch<>Last2DigitsC
			End If
			If PlayersPlayingGame=4 Then
				Do
					RndMatch=INT(Rnd(1)*10)   ' pick a number between 0 and 9
					RndMatch=RndMatch*10      ' convert it to 00 -> 90
				Loop Until RndMatch<>gvLastMatchValue And RndMatch<>Last2DigitsA And RndMatch<>Last2DigitsB And RndMatch<>Last2DigitsC And RndMatch<>Last2DigitsD
			End If
			' remember this value for next time
			gvLastMatchValue=RndMatch
			DD 15+gvLastMatchPosition,0,1:DD 16+gvLastMatchPosition,0,1
			DD 43+gvLastMatchPosition,0,1:DD 44+gvLastMatchPosition,0,1
			Do
				RndPosition=INT(Rnd(1)*5) ' pick a number between 0 and 5
				RndPosition=RndPosition*2
				RndPosition=RndPosition+2
			Loop Until RndPosition<>gvLastMatchPosition
			gvLastMatchPosition=RndPosition
			DD 15+RndPosition,RndMatch/10+37,1:DD 16+RndPosition,37,1:DMD_DisplayMatch RndPosition, RndMatch
			If PlayersPlayingGame>1 Then:DD 43+RndPosition,RndMatch/10+37,1:DD 44+RndPosition,37,1:End If
			' If on the last match loop (for the player) Then we check if there actually is a match
			' If under the match percentage Then
			If INT(Rnd(1)*100)<gsMatchPer Then
				TMatchVal=INT(Rnd(1)*4)
				If TMatchVal=0 Then RndMatch=Last2DigitsA ' force it to equal player 1's score
				If TMatchVal=1 Then RndMatch=Last2DigitsB ' force it to equal player 2's score
				If TMatchVal=2 Then RndMatch=Last2DigitsC ' force it to equal player 3's score
				If TMatchVal=3 Then RndMatch=Last2DigitsD ' force it to equal player 4's score
			End If
			' If a match then award a match
			Dim XRAZ
			XRAZ=0
			If Last2DigitsA=RndMatch Or (PlayersPlayingGame>1 And Last2DigitsB=RndMatch) Or (PlayersPlayingGame>2 And Last2DigitsC=RndMatch) Or (PlayersPlayingGame And Last2DigitsD=RndMatch) Then
				If Last2DigitsA=RndMatch Then
					XRAZ=XRAZ+1
					DD 15+RndPosition,RndMatch/10+37,2:DD 16+RndPosition,37,2
					PlaySound"YCheer":MTM=0:MTimer.Enabled=1
				End If
				If PlayersPlayingGame>1 And Last2DigitsB=RndMatch Then
					XRAZ=XRAZ+1
					DD 43+RndPosition,RndMatch/10+37,2:DD 44+RndPosition,37,2
					PlaySound"YCheer":MTM=0:MTimer.Enabled=1
				End If
				If PlayersPlayingGame>2 And Last2DigitsC=RndMatch Then
					XRAZ=XRAZ+1
					DD 15+RndPosition,RndMatch/10+37,2:DD 16+RndPosition,37,2
					PlaySound"YCheer":MTM=0:MTimer.Enabled=1
				End If
				If PlayersPlayingGame>3 And Last2DigitsD=RndMatch Then
					XRAZ=XRAZ+1
					DD 43+RndPosition,RndMatch/10+37,2:DD 44+RndPosition,37,2
					PlaySound"YCheer":MTM=0:MTimer.Enabled=1
				End If
				If XRAZ>0 Then AwardFreeGame XRAZ
			Else
				If PlayersPlayingGame=1 Or PlayersPlayingGame=3 Then
					DD 15+RndPosition,RndMatch/10+37,1:DD 16+RndPosition,37,1
					MTM=0:MTimer.Enabled=1
				End If
				If PlayersPlayingGame=2 Or PlayersPlayingGame=4 Then
					DD 43+RndPosition,RndMatch/10+37,1:DD 44+RndPosition,37,1
					MTM=0:MTimer.Enabled=1
				End If
			End If
		End If
		' one less loop to do
		gvMatchLoops=gvMatchLoops-1
		' If still more to do Then restart the timer
		If gvMatchLoops>0 Then
			Timer_MatchDisplay.Enabled=1
		Else
			' no more loops, hold the start for a bit longer (keeps the last screen on the display)
			gvMatchLoops=0
			Timer_MatchDisplay.Interval=1600
			Timer_MatchDisplay.Enabled=1
		End If
	Else  ' If (gvMatchLoops)
		' end of game
		DMD_SetScoreboardBackground ""
		EndOfGame
	End If
End Sub

Sub MTimer_Timer
	MTM=MTM+1
	If MTM=2 Then
		DisplayFlushQueue
		DisplayPoints(Score(0)):DMD_DisplayScores
		DD 14,HS1N+37,1
		If HS3Z>1 Then:DD 13,HS1M+37,1:Else:DD 13,0,1:End If
		If HS3Z>2 Then:DD 12,HS1L+37,1:Else:DD 12,0,1:End If
		If HS3Z>3 Then:DD 11,HS1K+48,1:Else:DD 11,0,1:End If
		If HS3Z>4 Then:DD 10,HS1J+37,1:Else:DD 10,0,1:End If
		If HS3Z>5 Then:DD 9,HS1I+37,1:Else:DD 9,0,1:End If
		If HS3Z>6 Then:DD 8,HS1H+48,1:Else:DD 8,0,1:End If
		If HS3Z>7 Then:DD 7,HS1G+37,1:Else:DD 7,0,1:End If
		If HS3Z>8 Then:DD 6,HS1F+37,1:Else:DD 6,0,1:End If
		If HS3Z>9 Then:DD 5,HS1E+48,1:Else:DD 5,0,1:End If
		If HS3Z>10 Then:DD 4,HS1D+37,1:Else:DD 4,0,1:End If
		If HS3Z>11 Then:DD 3,HS1C+37,1:Else:DD 3,0,1:End If
		If HS3Z>12 Then:DD 2,HS1B+48,1:Else:DD 2,0,1:End If
		If HS3Z>13 Then:DD 1,HS1A+37,1:Else:DD 1,0,1:End If
		DD 15,0,1:DD 16,0,1:DD 17,7,1:DD 18,1,1:DD 19,13,1:DD 20,5,1:DD 21,0,1:DD 22,0,1:DD 23,15,1:DD 24,22,1:DD 25,5,1:DD 26,18,1:DD 27,0,1:DD 28,0,1
		If PlayersPlayingGame>1 Then
			DisplayPoints(Score(1))
			DD 42,HS1N+37,1
			If HS3Z>1 Then:DD 41,HS1M+37,1:Else:DD 41,0,1:End If
			If HS3Z>2 Then:DD 40,HS1L+37,1:Else:DD 40,0,1:End If
			If HS3Z>3 Then:DD 39,HS1K+48,1:Else:DD 39,0,1:End If
			If HS3Z>4 Then:DD 38,HS1J+37,1:Else:DD 38,0,1:End If
			If HS3Z>5 Then:DD 37,HS1I+37,1:Else:DD 37,0,1:End If
			If HS3Z>6 Then:DD 36,HS1H+48,1:Else:DD 36,0,1:End If
			If HS3Z>7 Then:DD 35,HS1G+37,1:Else:DD 35,0,1:End If
			If HS3Z>8 Then:DD 34,HS1F+37,1:Else:DD 34,0,1:End If
			If HS3Z>9 Then:DD 33,HS1E+48,1:Else:DD 33,0,1:End If
			If HS3Z>10 Then:DD 32,HS1D+37,1:Else:DD 32,0,1:End If
			If HS3Z>11 Then:DD 31,HS1C+37,1:Else:DD 31,0,1:End If
			If HS3Z>12 Then:DD 30,HS1B+48,1:Else:DD 30,0,1:End If
			If HS3Z>13 Then:DD 29,HS1A+37,1:Else:DD 29,0,1:End If
			DD 15,0,1:DD 16,0,1:DD 17,7,1:DD 18,1,1:DD 19,13,1:DD 20,5,1:DD 21,0,1:DD 22,0,1:DD 23,15,1:DD 24,22,1:DD 25,5,1:DD 26,18,1:DD 27,0,1:DD 28,0,1
			DD 43,0,1:DD 44,0,1:DD 45,7,1:DD 46,1,1:DD 47,13,1:DD 48,5,1:DD 49,0,1:DD 50,0,1:DD 51,15,1:DD 52,22,1:DD 53,5,1:DD 54,18,1:DD 55,0,1:DD 56,0,1
		End If
		If PlayersPlayingGame>2 Then
			DisplayPoints(Score(2))
			DD 28,HS1N+37,1
			If HS3Z>1 Then:DD 27,HS1M+37,1:Else:DD 27,0,1:End If
			If HS3Z>2 Then:DD 26,HS1L+37,1:Else:DD 26,0,1:End If
			If HS3Z>3 Then:DD 25,HS1K+48,1:Else:DD 25,0,1:End If
			If HS3Z>4 Then:DD 24,HS1J+37,1:Else:DD 24,0,1:End If
			If HS3Z>5 Then:DD 23,HS1I+37,1:Else:DD 23,0,1:End If
			If HS3Z>6 Then:DD 22,HS1H+48,1:Else:DD 22,0,1:End If
			If HS3Z>7 Then:DD 21,HS1G+37,1:Else:DD 21,0,1:End If
			If HS3Z>8 Then:DD 20,HS1F+37,1:Else:DD 20,0,1:End If
			If HS3Z>9 Then:DD 19,HS1E+48,1:Else:DD 19,0,1:End If
			If HS3Z>10 Then:DD 18,HS1D+37,1:Else:DD 18,0,1:End If
			If HS3Z>11 Then:DD 17,HS1C+37,1:Else:DD 17,0,1:End If
			If HS3Z>12 Then:DD 16,HS1B+48,1:Else:DD 16,0,1:End If
			If HS3Z>13 Then:DD 15,HS1A+37,1:Else:DD 15,0,1:End If
			DD 43,0,1:DD 44,0,1:DD 45,7,1:DD 46,1,1:DD 47,13,1:DD 48,5,1:DD 49,0,1:DD 50,0,1:DD 51,15,1:DD 52,22,1:DD 53,5,1:DD 54,18,1:DD 55,0,1:DD 56,0,1
		End If
		If PlayersPlayingGame>3 Then
			DisplayPoints(Score(3))
			DD 56,HS1N+37,1
			If HS3Z>1 Then:DD 55,HS1M+37,1:Else:DD 55,0,1:End If
			If HS3Z>2 Then:DD 54,HS1L+37,1:Else:DD 54,0,1:End If
			If HS3Z>3 Then:DD 53,HS1K+48,1:Else:DD 53,0,1:End If
			If HS3Z>4 Then:DD 52,HS1J+37,1:Else:DD 52,0,1:End If
			If HS3Z>5 Then:DD 51,HS1I+37,1:Else:DD 51,0,1:End If
			If HS3Z>6 Then:DD 50,HS1H+48,1:Else:DD 50,0,1:End If
			If HS3Z>7 Then:DD 49,HS1G+37,1:Else:DD 49,0,1:End If
			If HS3Z>8 Then:DD 48,HS1F+37,1:Else:DD 48,0,1:End If
			If HS3Z>9 Then:DD 47,HS1E+48,1:Else:DD 47,0,1:End If
			If HS3Z>10 Then:DD 46,HS1D+37,1:Else:DD 46,0,1:End If
			If HS3Z>11 Then:DD 45,HS1C+37,1:Else:DD 45,0,1:End If
			If HS3Z>12 Then:DD 44,HS1B+48,1:Else:DD 44,0,1:End If
			If HS3Z>13 Then:DD 43,HS1A+37,1:Else:DD 43,0,1:End If
		End If
	End If
	If MTM>2 Then
		MTimer.Enabled=0
		DisplayFlushQueue
		PH1=0
		ATimer.Enabled=1
		RandomCalloutCount=0
		RandomCallout.Enabled=1
		LightSeq1_PlayDone
		MatchRunning=FALSE
	End If
End Sub

Sub TimerTiltReduce_Timer
	If gvTiltLevel>0 Then gvTiltLevel=gvTiltLevel-(gvTiltLevel*.05)
	If gvTiltLevel<0 Then gvTiltLevel=0
End Sub

Sub AddToTilt(byVal Power)
	Dim i
	gvTiltLevel=gvTiltLevel+(50*Power)
	If gvTiltLevel>200 And bTilted=FALSE Then
		gpTiltWarningsGiven=gpTiltWarningsGiven+1
		If gvTiltLevel>400 Then gpTiltWarningsGiven=gpTiltWarningsGiven+1
		If gpTiltWarningsGiven>=gsTiltWarnings Then
			PlayfieldScoreTimer_Timer
			SaveLampStates
			ResetTable
			QTimer.Enabled=0
			If STimer.Enabled Then:STimer.Enabled=0:DisplayFlushQueue:End If
			DisplayText"TILT TILT TILT",1,1
			DisplayText"TILT TILT TILT",2,1:DMD_DisplaySceneText "TILT TILT TILT", ""
			PlaySound"ZTilt"
			bTilted=TRUE
			EndMusic
			If MultiballSave.Enabled Then MultiballSave_Timer
			BGate.Open=TRUE:CGate.Open=TRUE
			If HurrySmall.Enabled Then
				MiniHPValReal=0
				HurrySmall_Timer
			End If
			If LightWizardMode.State>0 Then
				If Bumper1.State=2 Then Bumper1.State=0
				If Bumper2.State=2 Then Bumper2.State=0
				If Bumper3.State=2 Then Bumper3.State=0
				LightWizardMode.State=0
				BeginWizardShow.Enabled=0
				LightTrapDoor1.State=0
				LightTrapDoor2.State=0
				LightTrapDoor3.State=0
				LightTrapDoor1.BlinkPattern="10"
				LightTrapDoor2.BlinkPattern="10"
				LightTrapDoor3.BlinkPattern="10"
			End If
			RandomAward.State=0
			Timer_Tilted.Interval=2000
			Timer_Tilted.Enabled=1
		Else
			PlaySound"ZTilt2"
			If gpTiltWarningsGiven=1 Then
				DisplayText"  * DANGER *  ",1,1
				DisplayText"              ",2,1:DMD_DisplaySceneText " - DANGER - ", ""
			Else
				DisplayFlushQueue
				DisplayText"  * DANGER *  ",1,1
				DisplayText"  * DANGER *  ",2,1:DMD_DisplaySceneText " - DANGER - ", " - DANGER - "
			End If
		End If
	End If
End Sub

Sub Timer_Tilted_Timer
	Timer_Tilted.Enabled=0
	If BallsInGame=0 Then
		DoEndOfBallDisplay
	Else
		Timer_Tilted.Interval=1000
		Timer_Tilted.Enabled=1
	End If
End Sub

Sub Timer_SlamTilt_Timer
	Timer_SlamTilt.Enabled=0
	If BallsInGame=0 Then
		EndOfGame
		DisplayFlushQueue
		DelayTimer.Enabled=0
		Table1_Init
	Else
		For Q=1 To 56
			DD Q,117,1
		Next
		Timer_SlamTilt.Interval=1000
		Timer_SlamTilt.Enabled=1
	End If
End Sub

Sub ResetHSTD
	gsHighScore(0)     =40000000 ' top high score
	gsHighScoreName(0) ="BRIAN"
	gsHighScore(1)     =35000000 ' second
	gsHighScoreName(1) ="BLACK"
	gsHighScore(2)     =30000000 ' third
	gsHighScoreName(2) ="SHIVA"
	gsHighScore(3)     =25000000 ' and fourth
	gsHighScoreName(3) ="RANDY"
	gvCombosForComboChamp=4     ' combo champ
	gsComboChampName   ="CHORD"
	gsTrackScore(1)    =1000000 'Spinner
	gsTrackScoreName(1)="BONO "
	gsTrackScore(2)    =1000000 'Albums
	gsTrackScoreName(2)="SLASH"
	gsTrackScore(3)    =1000000 'Bumpers
	gsTrackScoreName(3)="ZOMBI"
	gsTrackScore(4)    =1000000 'Ramps
	gsTrackScoreName(4)="ERIK "
	gsTrackScore(5)    =1000000 'Band
	gsTrackScoreName(5)="BRENT"
	gsTrackScore(6)    =1000000 'Standups
	gsTrackScoreName(6)="STEVE"
	gsTrackScore(7)    =1000000 'Orbits
	gsTrackScoreName(7)="JAMES"
	gsTrackScore(8)    =1000000 'Loops
	gsTrackScoreName(8)="POPPY"
	gsTrackScore(9)    =1000000 'Combos
	gsTrackScoreName(9)="DALE "
	gsTrackScore(10)    =1000000 'Minis
	gsTrackScoreName(10)="DIXIE"
	gsTrackScore(11)    =1000000 'Notes
	gsTrackScoreName(11)="WAYNE"
	gsTrackScore(12)    =1000000 'Frenzy
	gsTrackScoreName(12)="FRED "
End Sub

Sub DisplayCredits
	If bGameInPlay=FALSE Then
		ATimer.Enabled=0
		RandomCallout.Enabled=0
		DD 1,0,0:DD 2,0,0:DD 3,0,0:DD 4,0,0:DD 5,0,0:DD 6,0,0:DD 7,0,0:DD 8,0,0:DD 9,0,0:DD 10,0,0:DD 11,0,0:DD 12,0,0:DD 13,0,0:DD 14,0,0
		DD 15,0,0:DD 16,0,0:DD 17,0,0:DD 18,0,0:DD 19,0,0:DD 20,0,0:DD 21,0,0:DD 22,0,0:DD 23,0,0:DD 24,0,0:DD 25,0,0:DD 26,0,0:DD 27,0,0:DD 28,0,0
		DD 29,0,0:DD 30,0,0:DD 31,0,0:DD 32,0,0:DD 33,0,0:DD 34,0,0:DD 35,0,0:DD 36,0,0:DD 37,0,0:DD 38,0,0:DD 39,0,0:DD 40,0,0:DD 41,0,0:DD 42,0,0
		DD 43,0,0:DD 44,0,0:DD 45,0,0:DD 46,0,0:DD 47,0,0:DD 48,0,0:DD 49,0,0:DD 50,0,0:DD 51,0,0:DD 52,0,0:DD 53,0,0:DD 54,0,0:DD 55,0,0:DD 56,0,0
		DMD_ClearScene
		If gsFreePlay Then
			DD 1,0,0:DD 2,0,0:DD 3,6,1:DD 4,18,1:DD 5,5,1:DD 6,5,1:DD 7,0,0:DD 8,0,0:DD 9,16,1:DD 10,12,1:DD 11,1,1:DD 12,25,1:DD 13,0,0:DD 14,0,0
			DD 15,0,0:DD 16,16,2:DD 17,18,2:DD 18,5,2:DD 19,19,2:DD 20,19,2:DD 21,0,0:DD 22,0,0:DD 23,19,2:DD 24,20,2:DD 25,1,2:DD 26,18,2:DD 27,20,2:DD 28,0,0
			DD 29,0,0:DD 30,0,0:DD 31,6,1:DD 32,18,1:DD 33,5,1:DD 34,5,1:DD 35,0,0:DD 36,0,0:DD 37,16,1:DD 38,12,1:DD 39,1,1:DD 40,25,1:DD 41,0,0:DD 42,0,0
			DD 43,0,0:DD 44,16,2:DD 45,18,2:DD 46,5,2:DD 47,19,2:DD 48,19,2:DD 49,0,0:DD 50,0,0:DD 51,19,2:DD 52,20,2:DD 53,1,2:DD 54,18,2:DD 55,20,2:DD 56,0,0
			DMD_DisplaySceneText "FREE PLAY", "PRESS START"
		Else
			DD 1,3,1:DD 2,18,1:DD 3,5,1:DD 4,4,1:DD 5,9,1:DD 6,20,1:DD 7,19,1:DD 8,0,0:DD 9,0,0:DD 10,0,0:DD 11,0,0:DD 12,0,0
			phCredits1=0:phCredits2=0
			If gsCredits>9 Then phCredits1=INT(gsCredits/10)
			phCredits2=gsCredits-phCredits1*10
			If phCredits1>0 Then DD 13,phCredits1+37,1
			DD 14,phCredits2+37,1
			If gsCredits>0 Then
				DD 15,0,0:DD 16,16,2:DD 17,18,2:DD 18,5,2:DD 19,19,2:DD 20,19,2:DD 21,0,0:DD 22,0,2:DD 23,19,2:DD 24,20,2:DD 25,1,2:DD 26,18,2:DD 27,20,2:DD 28,0,0
				If phCredits1>0 Then 	
					DMD_DisplaySceneText "CREDITS   "&phCredits1&phCredits2, "PRESS START"
				Else
					DMD_DisplaySceneText "CREDITS    "&phCredits2, "PRESS START"
				End If
			Else
				DD 15,0,0:DD 16,9,2:DD 17,14,2:DD 18,19,2:DD 19,5,2:DD 20,18,2:DD 21,20,2:DD 22,0,0:DD 23,0,0:DD 24,3,2:DD 25,15,2:DD 26,9,2:DD 27,14,2:DD 28,0,0
				DMD_DisplaySceneText "CREDITS    0", "INSERT COIN"
			End If
			DD 29,0,0:DD 30,38,1:DD 31,0,0:DD 32,3,1:DD 33,18,1:DD 34,5,1:DD 35,4,1:DD 36,9,1:DD 37,20,1:DD 38,0,0:DD 39,16,1:DD 40,5,1:DD 41,18,1:DD 42,0,0
			DD 43,0,0:DD 44,0,0:DD 45,0,0:DD 46,0,0:DD 47,16,1:DD 48,12,1:DD 49,1,1:DD 50,25,1:DD 51,5,1:DD 52,18,1:DD 53,0,0:DD 54,0,0:DD 55,0,0:DD 56,0,0
		End If
		BTimer.Enabled=1
	End If
End Sub

Sub BTimer_Timer
	PH1=0
	DD 1,0,0:DD 2,0,0:DD 3,0,0:DD 4,0,0:DD 5,0,0:DD 6,0,0:DD 7,0,0:DD 8,0,0:DD 9,0,0:DD 10,0,0:DD 11,0,0:DD 12,0,0:DD 13,0,0:DD 14,0,0
	DD 15,0,0:DD 16,0,0:DD 17,0,0:DD 18,0,0:DD 19,0,0:DD 20,0,0:DD 21,0,0:DD 22,0,0:DD 23,0,0:DD 24,0,0:DD 25,0,0:DD 26,0,0:DD 27,0,0:DD 28,0,0
	DD 29,0,0:DD 30,0,0:DD 31,0,0:DD 32,0,0:DD 33,0,0:DD 34,0,0:DD 35,0,0:DD 36,0,0:DD 37,0,0:DD 38,0,0:DD 39,0,0:DD 40,0,0:DD 41,0,0:DD 42,0,0
	DD 43,0,0:DD 44,0,0:DD 45,0,0:DD 46,0,0:DD 47,0,0:DD 48,0,0:DD 49,0,0:DD 50,0,0:DD 51,0,0:DD 52,0,0:DD 53,0,0:DD 54,0,0:DD 55,0,0:DD 56,0,0
	DMD_ClearScene
	BTimer.Enabled=0
	ATimer.Enabled=1
	RandomCalloutCount=0
	RandomCallout.Enabled=1
	LightSeq1_PlayDone
End Sub

Sub ATimer_Timer 'Attract mode display update routine
	If PH1=0 Or PH1=71000 Then 'clear displays
		DD 1,0,0:DD 2,0,0:DD 3,0,0:DD 4,0,0:DD 5,0,0:DD 6,0,0:DD 7,0,0:DD 8,0,0:DD 9,0,0:DD 10,0,0:DD 11,0,0:DD 12,0,0:DD 13,0,0:DD 14,0,0
		DD 15,0,0:DD 16,0,0:DD 17,0,0:DD 18,0,0:DD 19,0,0:DD 20,0,0:DD 21,0,0:DD 22,0,0:DD 23,0,0:DD 24,0,0:DD 25,0,0:DD 26,0,0:DD 27,0,0:DD 28,0,0
		DD 29,0,0:DD 30,0,0:DD 31,0,0:DD 32,0,0:DD 33,0,0:DD 34,0,0:DD 35,0,0:DD 36,0,0:DD 37,0,0:DD 38,0,0:DD 39,0,0:DD 40,0,0:DD 41,0,0:DD 42,0,0
		DD 43,0,0:DD 44,0,0:DD 45,0,0:DD 46,0,0:DD 47,0,0:DD 48,0,0:DD 49,0,0:DD 50,0,0:DD 51,0,0:DD 52,0,0:DD 53,0,0:DD 54,0,0:DD 55,0,0:DD 56,0,0
		PH1=0
		DMD_ClearScene
	End If
	PH1=PH1+20
	Select Case PH1
		'INITIAL GAME NAME DISPLAY
		Case 40:DisplayPartialText Left(DisplayName,1),1,1:DisplayPartialText Left(DisplayName,1),3,1:DMD_DisplayLogo
		Case 80:DisplayPartialText Left(DisplayName,2),1,1:DisplayPartialText Left(DisplayName,2),3,1
		Case 120:DisplayPartialText Left(DisplayName,3),1,1:DisplayPartialText Left(DisplayName,3),3,1
		Case 160:DisplayPartialText Left(DisplayName,4),1,1:DisplayPartialText Left(DisplayName,4),3,1
		Case 200:DisplayPartialText Left(DisplayName,5),1,1:DisplayPartialText Left(DisplayName,5),3,1
		Case 240:DisplayPartialText Left(DisplayName,6),1,1:DisplayPartialText Left(DisplayName,6),3,1
		Case 280:DisplayPartialText Left(DisplayName,7),1,1:DisplayPartialText Left(DisplayName,7),3,1
		Case 320:DisplayPartialText Left(DisplayName,8),1,1:DisplayPartialText Left(DisplayName,8),3,1
		Case 360:DisplayPartialText Left(DisplayName,9),1,1:DisplayPartialText Left(DisplayName,9),3,1
		Case 400:DisplayPartialText Left(DisplayName,10),1,1:DisplayPartialText Left(DisplayName,10),3,1
		Case 440:DisplayPartialText Left(DisplayName,11),1,1:DisplayPartialText Left(DisplayName,11),3,1
		Case 480:DisplayPartialText Left(DisplayName,12),1,1:DisplayPartialText Left(DisplayName,12),3,1
		Case 520:DisplayPartialText Left(DisplayName,13),1,1:DisplayPartialText Left(DisplayName,13),3,1
		Case 560:DisplayPartialText Left(DisplayName,14),1,1:DisplayPartialText Left(DisplayName,14),3,1
		Case 3000:DD 1,13,1:DD 2,1,1:DD 3,4,1:DD 4,5,1:DD 5,0,1:DD 6,16,1:DD 7,15,1:DD 8,19,1:DD 9,19,1:DD 10,9,1:DD 11,2,1:DD 12,12,1:DD 13,5,1:DD 14,0,1
					DD 15,0,0:DD 16,0,0:DD 17,0,0:DD 18,0,0:DD 19,0,0:DD 20,0,0:DD 21,2,1:DD 22,25,1:DD 23,0,0:DD 24,0,0:DD 25,0,0:DD 26,0,0:DD 27,0,0:DD 28,0,0
					DD 29,0,0:DD 30,0,0:DD 31,0,0:DD 32,0,0:DD 33,0,0:DD 34,0,0:DD 35,0,0:DD 36,0,0:DD 37,0,0:DD 38,0,0:DD 39,0,0:DD 40,0,0:DD 41,0,0:DD 42,0,0
					DD 43,0,0:DD 44,0,0:DD 45,0,0:DD 46,0,0:DD 47,0,0:DD 48,0,0:DD 49,0,0:DD 50,0,0:DD 51,0,0:DD 52,0,0:DD 53,0,0:DD 54,0,0:DD 55,0,0:DD 56,0,0
					DMD_DisplaySceneText "TABLE MOD BY", "X13 MODS"
		Case 4000:Clock=0
					DD 29,0,0:DD 30,0,0:DD 31,0,0:DD 32,0,0:DD 33,0,0:DD 34,0,0:DD 35,0,0:DD 36,0,0:DD 37,0,0:DD 38,0,0:DD 39,0,0:DD 40,0,0:DD 41,0,0:DD 42,0,0
					DD 43,0,0:DD 44,0,0:DD 45,0,0:DD 46,0,0:DD 47,0,0:DD 48,0,0:DD 49,0,0:DD 50,0,0:DD 51,0,0:DD 52,0,0:DD 53,0,0:DD 54,0,0:DD 55,0,0:DD 56,0,0
		Case 4040:  DD 44,2,1
		Case 4120:  DD 45,18,1
		Case 4200:  DD 46,9,1
		Case 4280:  DD 47,1,1
		Case 4360:  DD 48,14,1
		Case 4440:  DD 51,19,1
		Case 4520:  DD 52,13,1
		Case 4600:  DD 53,9,1
		Case 4680:  DD 54,20,1
		Case 4720:  DD 29,0,0:DD 30,0,0:DD 31,0,0:DD 32,4,1:DD 33,5,1:DD 34,19,1:DD 35,20,1:DD 36,18,1:DD 37,21,1:DD 38,11,1:DD 39,0,0:DD 40,0,0:DD 41,0,0:DD 42,0,0
		Case 4760:  DD 55,8,1
		Case 8000:  DD 29,0,0:DD 30,0,0:DD 31,0,0:DD 32,0,0:DD 33,0,0:DD 34,0,0:DD 35,0,0:DD 36,0,0:DD 37,0,0:DD 38,0,0:DD 39,0,0:DD 40,0,0:DD 41,0,0:DD 42,0,0
					DD 43,0,0:DD 44,0,0:DD 45,0,0:DD 46,0,0:DD 47,0,0:DD 48,0,0:DD 49,0,0:DD 50,0,0:DD 51,0,0:DD 52,0,0:DD 53,0,0:DD 54,0,0:DD 55,0,0:DD 56,0,0
		
		PH1=10500
		' some B2S changes ************************************************

		Case 11000:DisplayText" GRAPHICS BY  ",1,1
						DisplayText" AARON  JAMES ",2,1:DMD_DisplaySceneText "WITH HELP FROM", "SUPER TILTED GROUP"
						DisplayText" BETA TESTING ",3,1
						DisplayText"  SHOOBY DOO  ",4,1
		Case 12000:DMD_DisplaySceneText "PUPPACK BY", "X13_Pinner"
        Case 12500:DMD_DisplaySceneText "X13 Mod Group", "Discord"
		Case 13500:DMD_DisplaySceneText "MOTLEY CRUE", "Solid State"
		Case 14000:DD 13,0,0
					If gsFreePlay Then
						DD 1,0,0:DD 2,0,0:DD 3,6,1:DD 4,18,1:DD 5,5,1:DD 6,5,1:DD 7,0,0:DD 8,0,0:DD 9,16,1:DD 10,12,1:DD 11,1,1:DD 12,25,1:DD 13,0,0:DD 14,0,0
						DD 15,0,0:DD 16,16,2:DD 17,18,2:DD 18,5,2:DD 19,19,2:DD 20,19,2:DD 21,0,0:DD 22,0,0:DD 23,19,2:DD 24,20,2:DD 25,1,2:DD 26,18,2:DD 27,20,2:DD 28,0,0
						DD 29,0,0:DD 30,0,0:DD 31,6,1:DD 32,18,1:DD 33,5,1:DD 34,5,1:DD 35,0,0:DD 36,0,0:DD 37,16,1:DD 38,12,1:DD 39,1,1:DD 40,25,1:DD 41,0,0:DD 42,0,0
						DD 43,0,0:DD 44,16,2:DD 45,18,2:DD 46,5,2:DD 47,19,2:DD 48,19,2:DD 49,0,0:DD 50,0,0:DD 51,19,2:DD 52,20,2:DD 53,1,2:DD 54,18,2:DD 55,20,2:DD 56,0,0
						DMD_DisplaySceneText "FREE PLAY", "PRESS START"
					Else
						DD 1,3,1:DD 2,18,1:DD 3,5,1:DD 4,4,1:DD 5,9,1:DD 6,20,1:DD 7,19,1:DD 8,0,0:DD 9,0,0:DD 10,0,0:DD 11,0,0:DD 12,0,0
						phCredits1=0:phCredits2=0
						If gsCredits>9 Then phCredits1=INT(gsCredits/10)
						phCredits2=gsCredits-phCredits1*10
						If phCredits1>0 Then DD 13,phCredits1+37,1
						DD 14,phCredits2+37,1
						If gsCredits>0 Then
							DD 15,0,0:DD 16,16,2:DD 17,18,2:DD 18,5,2:DD 19,19,2:DD 20,19,2:DD 21,0,0:DD 22,0,2:DD 23,19,2:DD 24,20,2:DD 25,1,2:DD 26,18,2:DD 27,20,2:DD 28,0,0
							If phCredits1>0 Then 	
								DMD_DisplaySceneText "CREDITS   "&phCredits1&phCredits2, "PRESS START"
							Else
								DMD_DisplaySceneText "CREDITS    "&phCredits2, "PRESS START"
							End If
						Else
							DD 15,0,0:DD 16,9,2:DD 17,14,2:DD 18,19,2:DD 19,5,2:DD 20,18,2:DD 21,20,2:DD 22,0,0:DD 23,0,0:DD 24,3,2:DD 25,15,2:DD 26,9,2:DD 27,14,2:DD 28,0,0
							DMD_DisplaySceneText "CREDITS    0", "NEED COIN DUDE"
						End If
						DD 29,0,0:DD 30,38,1:DD 31,0,0:DD 32,3,1:DD 33,18,1:DD 34,5,1:DD 35,4,1:DD 36,9,1:DD 37,20,1:DD 38,0,0:DD 39,16,1:DD 40,5,1:DD 41,18,1:DD 42,0,0
						DD 43,0,0:DD 44,0,0:DD 45,0,0:DD 46,0,0:DD 47,16,1:DD 48,12,1:DD 49,1,1:DD 50,25,1:DD 51,5,1:DD 52,18,1:DD 53,0,0:DD 54,0,0:DD 55,0,0:DD 56,0,0
					End If
		Case 17000:DD 1,8,1:DD 2,9,1:DD 3,7,1:DD 4,8,1:DD 5,5,1:DD 6,19,1:DD 7,20,1:DD 8,0,0:DD 9,19,1:DD 10,3,1:DD 11,15,1:DD 12,18,1:DD 13,5,1:DD 14,19,1
					DD 15,0,0:DD 16,0,0:DD 17,0,0:DD 18,0,0:DD 19,0,0:DD 20,0,0:DD 21,0,0:DD 22,0,0:DD 23,0,0:DD 24,0,0:DD 25,0,0:DD 26,0,0:DD 27,0,0:DD 28,0,0
					DD 29,0,0:DD 30,0,0:DD 31,0,0:DD 32,0,0:DD 33,0,0:DD 34,0,0:DD 35,0,0:DD 36,0,0:DD 37,0,0:DD 38,0,0:DD 39,0,0:DD 40,0,0:DD 41,0,0:DD 42,0,0
					DD 43,0,0:DD 44,0,0:DD 45,0,0:DD 46,0,0:DD 47,0,0:DD 48,0,0:DD 49,0,0:DD 50,0,0:DD 51,0,0:DD 52,0,0:DD 53,0,0:DD 54,0,0:DD 55,0,0:DD 56,0,0
					DMD_DisplaySceneText "HIGHEST SCORES", ""
		Case 20000:DetermineHighScore 0
					DetermineHSName 0: DMD_DisplaySceneText "1> "+gsHighScoreName(0), gsHighScore(0)
					DD 43,76,1
					DD 42,73,1'Race Car Right to left
		Case 20020:DD 44,76,1:DD 43,77,1
					DD 41,73,1:DD 42,74,1'Race Car Right to left
		Case 20040:DD 45,76,1:DD 44,77,1:DD 43,78,1
					DD 40,73,1:DD 41,74,1:DD 42,75,1 'Race Car Right to left
		Case 20060:If HS3Z>13 Then:DD 43,HS1A+37,1:Else:DD 43,0,1:End If
					DD 42,0,1
					DD 46,76,1:DD 45,77,1:DD 44,78,1
					DD 39,73,1:DD 40,74,1:DD 41,75,1 'Race Car Right to left
		Case 20080:If HS3Z>12 Then:DD 44,HS1B+48,1:Else:DD 44,0,1:End If
					DD 41,0,1
					DD 47,76,1:DD 46,77,1:DD 45,78,1
					DD 38,73,1:DD 39,74,1:DD 40,75,1 'Race Car Right to left
		Case 20100:If HS3Z>11 Then:DD 45,HS1C+37,1:Else:DD 45,0,1:End If
					DD 40,0,1
					DD 48,76,1:DD 47,77,1:DD 46,78,1
					DD 37,73,1:DD 38,74,1:DD 39,75,1 'Race Car Right to left
		Case 20120:If HS3Z>10 Then:DD 46,HS1D+37,1:Else:DD 46,0,1:End If
					DD 39,0,1
					DD 49,76,1:DD 48,77,1:DD 47,78,1
					DD 36,73,1:DD 37,74,1:DD 38,75,1 'Race Car Right to left
		Case 20140:If HS3Z>9 Then:DD 47,HS1E+48,1:Else:DD 47,0,1:End If
					DD 38,0,1
					DD 50,76,1:DD 49,77,1:DD 48,78,1
					DD 35,73,1:DD 36,74,1:DD 37,75,1 'Race Car Right to left
		Case 20160:If HS3Z>8 Then:DD 48,HS1F+37,1:Else:DD 48,0,1:End If
					DD 37,0,1
					DD 51,76,1:DD 50,77,1:DD 49,78,1
					DD 34,73,1:DD 35,74,1:DD 36,75,1 'Race Car Right to left
		Case 20180:If HS3Z>7 Then:DD 49,HS1G+37,1:Else:DD 49,0,1:End If
					DD 36,LPA(5),1
					DD 52,76,1:DD 51,77,1:DD 50,78,1
					DD 33,73,1:DD 34,74,1:DD 35,75,1 'Race Car Right to left
		Case 20200:If HS3Z>6 Then:DD 50,HS1H+48,1:Else:DD 50,0,1:End If
					DD 35,LPA(4),1
					DD 53,76,1:DD 52,77,1:DD 51,78,1
					DD 32,73,1:DD 33,74,1:DD 34,75,1 'Race Car Right to left
		Case 20220:If HS3Z>5 Then:DD 51,HS1I+37,1:Else:DD 51,0,1:End If
					DD 34,LPA(3),1
					DD 54,76,1:DD 53,77,1:DD 52,78,1
					DD 31,73,1:DD 32,74,1:DD 33,75,1 'Race Car Right to left
		Case 20240:If HS3Z>4 Then:DD 52,HS1J+37,1:Else:DD 52,0,1:End If
					DD 33,LPA(2),1
					DD 55,76,1:DD 54,77,1:DD 53,78,1
					DD 30,73,1:DD 31,74,1:DD 32,75,1 'Race Car Right to left
		Case 20260:If HS3Z>3 Then:DD 53,HS1K+48,1:Else:DD 53,0,1:End If
					DD 32,LPA(1),1
					DD 56,76,1:DD 55,77,1:DD 54,78,1
					DD 29,73,1:DD 30,74,1:DD 31,75,1 'Race Car Right to left
		Case 20280:If HS3Z>2 Then:DD 54,HS1L+37,1:Else:DD 54,0,1:End If
					DD 31,0,1
					DD 56,77,1:DD 55,78,1
					DD 29,74,1:DD 30,75,1 'Race Car Right to left
		Case 20300:If HS3Z>1 Then:DD 55,HS1M+37,1:Else:DD 55,0,1:End If
					DD 56,78,1
					DD 30,29,1
					DD 29,75,1 'Race Car Right to left
		Case 20320:If HS3Z>0 Then:DD 56,HS1N+37,1:Else:DD 56,0,1:End If
					DD 29,38,1
		Case 23000:DetermineHighScore 1 
					DetermineHSName 1:DMD_DisplaySceneText "2> "+gsHighScoreName(1), gsHighScore(1) 
					DD 43,76,1
					DD 42,73,1'Race Car Right to left
		Case 23020:DD 44,76,1:DD 43,77,1
					DD 41,73,1:DD 42,74,1'Race Car Right to left
		Case 23040:DD 45,76,1:DD 44,77,1:DD 43,78,1
					DD 40,73,1:DD 41,74,1:DD 42,75,1 'Race Car Right to left
		Case 23060:If HS3Z>13 Then:DD 43,HS1A+37,1:Else:DD 43,0,1:End If
					DD 42,0,1
					DD 46,76,1:DD 45,77,1:DD 44,78,1
					DD 39,73,1:DD 40,74,1:DD 41,75,1 'Race Car Right to left
		Case 23080:If HS3Z>12 Then:DD 44,HS1B+48,1:Else:DD 44,0,1:End If
					DD 41,0,1
					DD 47,76,1:DD 46,77,1:DD 45,78,1
					DD 38,73,1:DD 39,74,1:DD 40,75,1 'Race Car Right to left
		Case 23100:If HS3Z>11 Then:DD 45,HS1C+37,1:Else:DD 45,0,1:End If
					DD 40,0,1
					DD 48,76,1:DD 47,77,1:DD 46,78,1
					DD 37,73,1:DD 38,74,1:DD 39,75,1 'Race Car Right to left
		Case 23120:If HS3Z>10 Then:DD 46,HS1D+37,1:Else:DD 46,0,1:End If
					DD 39,0,1
					DD 49,76,1:DD 48,77,1:DD 47,78,1
					DD 36,73,1:DD 37,74,1:DD 38,75,1 'Race Car Right to left
		Case 23140:If HS3Z>9 Then:DD 47,HS1E+48,1:Else:DD 47,0,1:End If
					DD 38,0,1
					DD 50,76,1:DD 49,77,1:DD 48,78,1
					DD 35,73,1:DD 36,74,1:DD 37,75,1 'Race Car Right to left
		Case 23160:If HS3Z>8 Then:DD 48,HS1F+37,1:Else:DD 48,0,1:End If
					DD 37,0,1
					DD 51,76,1:DD 50,77,1:DD 49,78,1
					DD 34,73,1:DD 35,74,1:DD 36,75,1 'Race Car Right to left
		Case 23180:If HS3Z>7 Then:DD 49,HS1G+37,1:Else:DD 49,0,1:End If
					DD 36,LPA(5),1
					DD 52,76,1:DD 51,77,1:DD 50,78,1
					DD 33,73,1:DD 34,74,1:DD 35,75,1 'Race Car Right to left
		Case 23200:If HS3Z>6 Then:DD 50,HS1H+48,1:Else:DD 50,0,1:End If
					DD 35,LPA(4),1
					DD 53,76,1:DD 52,77,1:DD 51,78,1
					DD 32,73,1:DD 33,74,1:DD 34,75,1 'Race Car Right to left
		Case 23220:If HS3Z>5 Then:DD 51,HS1I+37,1:Else:DD 51,0,1:End If
					DD 34,LPA(3),1
					DD 54,76,1:DD 53,77,1:DD 52,78,1
					DD 31,73,1:DD 32,74,1:DD 33,75,1 'Race Car Right to left
		Case 23240:If HS3Z>4 Then:DD 52,HS1J+37,1:Else:DD 52,0,1:End If
					DD 33,LPA(2),1
					DD 55,76,1:DD 54,77,1:DD 53,78,1
					DD 30,73,1:DD 31,74,1:DD 32,75,1 'Race Car Right to left
		Case 23260:If HS3Z>3 Then:DD 53,HS1K+48,1:Else:DD 53,0,1:End If
					DD 32,LPA(1),1
					DD 56,76,1:DD 55,77,1:DD 54,78,1
					DD 29,73,1:DD 30,74,1:DD 31,75,1 'Race Car Right to left
		Case 23280:If HS3Z>2 Then:DD 54,HS1L+37,1:Else:DD 54,0,1:End If
					DD 31,0,1
					DD 56,77,1:DD 55,78,1
					DD 29,74,1:DD 30,75,1 'Race Car Right to left
		Case 23300:If HS3Z>1 Then:DD 55,HS1M+37,1:Else:DD 55,0,1:End If
					DD 56,78,1
					DD 30,29,1
					DD 29,75,1 'Race Car Right to left
		Case 23320:If HS3Z>0 Then:DD 56,HS1N+37,1:Else:DD 56,0,1:End If
					DD 29,39,1
		Case 26000:DetermineHighScore 2 
					DetermineHSName 2:DMD_DisplaySceneText "3> "+gsHighScoreName(2), gsHighScore(2)
					DD 43,76,1
					DD 42,73,1'Race Car Right to left
		Case 26020:DD 44,76,1:DD 43,77,1
					DD 41,73,1:DD 42,74,1'Race Car Right to left
		Case 26040:DD 45,76,1:DD 44,77,1:DD 43,78,1
					DD 40,73,1:DD 41,74,1:DD 42,75,1 'Race Car Right to left
		Case 26060:If HS3Z>13 Then:DD 43,HS1A+37,1:Else:DD 43,0,1:End If
					DD 42,0,1
					DD 46,76,1:DD 45,77,1:DD 44,78,1
					DD 39,73,1:DD 40,74,1:DD 41,75,1 'Race Car Right to left
		Case 26080:If HS3Z>12 Then:DD 44,HS1B+48,1:Else:DD 44,0,1:End If
					DD 41,0,1
					DD 47,76,1:DD 46,77,1:DD 45,78,1
					DD 38,73,1:DD 39,74,1:DD 40,75,1 'Race Car Right to left
		Case 26100:If HS3Z>11 Then:DD 45,HS1C+37,1:Else:DD 45,0,1:End If
					DD 40,0,1
					DD 48,76,1:DD 47,77,1:DD 46,78,1
					DD 37,73,1:DD 38,74,1:DD 39,75,1 'Race Car Right to left
		Case 26120:If HS3Z>10 Then:DD 46,HS1D+37,1:Else:DD 46,0,1:End If
					DD 39,0,1
					DD 49,76,1:DD 48,77,1:DD 47,78,1
					DD 36,73,1:DD 37,74,1:DD 38,75,1 'Race Car Right to left
		Case 26140:If HS3Z>9 Then:DD 47,HS1E+48,1:Else:DD 47,0,1:End If
					DD 38,0,1
					DD 50,76,1:DD 49,77,1:DD 48,78,1
					DD 35,73,1:DD 36,74,1:DD 37,75,1 'Race Car Right to left
		Case 26160:If HS3Z>8 Then:DD 48,HS1F+37,1:Else:DD 48,0,1:End If
					DD 37,0,1
					DD 51,76,1:DD 50,77,1:DD 49,78,1
					DD 34,73,1:DD 35,74,1:DD 36,75,1 'Race Car Right to left
		Case 26180:If HS3Z>7 Then:DD 49,HS1G+37,1:Else:DD 49,0,1:End If
					DD 36,LPA(5),1
					DD 52,76,1:DD 51,77,1:DD 50,78,1
					DD 33,73,1:DD 34,74,1:DD 35,75,1 'Race Car Right to left
		Case 26200:If HS3Z>6 Then:DD 50,HS1H+48,1:Else:DD 50,0,1:End If
					DD 35,LPA(4),1
					DD 53,76,1:DD 52,77,1:DD 51,78,1
					DD 32,73,1:DD 33,74,1:DD 34,75,1 'Race Car Right to left
		Case 26220:If HS3Z>5 Then:DD 51,HS1I+37,1:Else:DD 51,0,1:End If
					DD 34,LPA(3),1
					DD 54,76,1:DD 53,77,1:DD 52,78,1
					DD 31,73,1:DD 32,74,1:DD 33,75,1 'Race Car Right to left
		Case 26240:If HS3Z>4 Then:DD 52,HS1J+37,1:Else:DD 52,0,1:End If
					DD 33,LPA(2),1
					DD 55,76,1:DD 54,77,1:DD 53,78,1
					DD 30,73,1:DD 31,74,1:DD 32,75,1 'Race Car Right to left
		Case 26260:If HS3Z>3 Then:DD 53,HS1K+48,1:Else:DD 53,0,1:End If
					DD 32,LPA(1),1
					DD 56,76,1:DD 55,77,1:DD 54,78,1
					DD 29,73,1:DD 30,74,1:DD 31,75,1 'Race Car Right to left
		Case 26280:If HS3Z>2 Then:DD 54,HS1L+37,1:Else:DD 54,0,1:End If
					DD 31,0,1
					DD 56,77,1:DD 55,78,1
					DD 29,74,1:DD 30,75,1 'Race Car Right to left
		Case 26300:If HS3Z>1 Then:DD 55,HS1M+37,1:Else:DD 55,0,1:End If
					DD 56,78,1
					DD 30,29,1
					DD 29,75,1 'Race Car Right to left
		Case 26320:If HS3Z>0 Then:DD 56,HS1N+37,1:Else:DD 56,0,1:End If
					DD 29,40,1
		Case 29000:DetermineHighScore 3 
					DetermineHSName 3:DMD_DisplaySceneText "4> "+gsHighScoreName(3), gsHighScore(3)
					DD 43,76,1
					DD 42,73,1'Race Car Right to left
		Case 29020:DD 44,76,1:DD 43,77,1
					DD 41,73,1:DD 42,74,1'Race Car Right to left
		Case 29040:DD 45,76,1:DD 44,77,1:DD 43,78,1
					DD 40,73,1:DD 41,74,1:DD 42,75,1 'Race Car Right to left
		Case 29060:If HS3Z>13 Then:DD 43,HS1A+37,1:Else:DD 43,0,1:End If
					DD 42,0,1
					DD 46,76,1:DD 45,77,1:DD 44,78,1
					DD 39,73,1:DD 40,74,1:DD 41,75,1 'Race Car Right to left
		Case 29080:If HS3Z>12 Then:DD 44,HS1B+48,1:Else:DD 44,0,1:End If
					DD 41,0,1
					DD 47,76,1:DD 46,77,1:DD 45,78,1
					DD 38,73,1:DD 39,74,1:DD 40,75,1 'Race Car Right to left
		Case 29100:If HS3Z>11 Then:DD 45,HS1C+37,1:Else:DD 45,0,1:End If
					DD 40,0,1
					DD 48,76,1:DD 47,77,1:DD 46,78,1
					DD 37,73,1:DD 38,74,1:DD 39,75,1 'Race Car Right to left
		Case 29120:If HS3Z>10 Then:DD 46,HS1D+37,1:Else:DD 46,0,1:End If
					DD 39,0,1
					DD 49,76,1:DD 48,77,1:DD 47,78,1
					DD 36,73,1:DD 37,74,1:DD 38,75,1 'Race Car Right to left
		Case 29140:If HS3Z>9 Then:DD 47,HS1E+48,1:Else:DD 47,0,1:End If
					DD 38,0,1
					DD 50,76,1:DD 49,77,1:DD 48,78,1
					DD 35,73,1:DD 36,74,1:DD 37,75,1 'Race Car Right to left
		Case 29160:If HS3Z>8 Then:DD 48,HS1F+37,1:Else:DD 48,0,1:End If
					DD 37,0,1
					DD 51,76,1:DD 50,77,1:DD 49,78,1
					DD 34,73,1:DD 35,74,1:DD 36,75,1 'Race Car Right to left
		Case 29180:If HS3Z>7 Then:DD 49,HS1G+37,1:Else:DD 49,0,1:End If
					DD 36,LPA(5),1
					DD 52,76,1:DD 51,77,1:DD 50,78,1
					DD 33,73,1:DD 34,74,1:DD 35,75,1 'Race Car Right to left
		Case 29200:If HS3Z>6 Then:DD 50,HS1H+48,1:Else:DD 50,0,1:End If
					DD 35,LPA(4),1
					DD 53,76,1:DD 52,77,1:DD 51,78,1
					DD 32,73,1:DD 33,74,1:DD 34,75,1 'Race Car Right to left
		Case 29220:If HS3Z>5 Then:DD 51,HS1I+37,1:Else:DD 51,0,1:End If
					DD 34,LPA(3),1
					DD 54,76,1:DD 53,77,1:DD 52,78,1
					DD 31,73,1:DD 32,74,1:DD 33,75,1 'Race Car Right to left
		Case 29240:If HS3Z>4 Then:DD 52,HS1J+37,1:Else:DD 52,0,1:End If
					DD 33,LPA(2),1
					DD 55,76,1:DD 54,77,1:DD 53,78,1
					DD 30,73,1:DD 31,74,1:DD 32,75,1 'Race Car Right to left
		Case 29260:If HS3Z>3 Then:DD 53,HS1K+48,1:Else:DD 53,0,1:End If
					DD 32,LPA(1),1
					DD 56,76,1:DD 55,77,1:DD 54,78,1
					DD 29,73,1:DD 30,74,1:DD 31,75,1 'Race Car Right to left
		Case 29280:If HS3Z>2 Then:DD 54,HS1L+37,1:Else:DD 54,0,1:End If
					DD 31,0,1
					DD 56,77,1:DD 55,78,1
					DD 29,74,1:DD 30,75,1 'Race Car Right to left
		Case 29300:If HS3Z>1 Then:DD 55,HS1M+37,1:Else:DD 55,0,1:End If
					DD 56,78,1
					DD 30,29,1
					DD 29,75,1 'Race Car Right to left
		Case 29320:If HS3Z>0 Then:DD 56,HS1N+37,1:Else:DD 56,0,1:End If
					DD 29,41,1
		Case 32000:DisplayText"COMBO CHAMPION",1,1
					DetermineComboChamp:DMD_DisplaySceneText "COMBO CHAMPION", Trim(gsComboChampName)&" - "&gvCombosForComboChamp
					DD 29,29,1:DD 30,0,0:DD 31,0,0:DD 32,0,0:DD 33,LPA(1),2:DD 34,LPA(2),2:DD 35,LPA(3),2:DD 36,LPA(4),2:DD 37,LPA(5),2:DD 38,0,0:DD 39,0,0:DD 40,0,0:DD 41,0,0:DD 42,28,1
					DD 43,0,0:DD 44,0,0:DD 45,0,0:DD 46,0,0:DD 47,0,0: DD 48,0,0:DD 51,0,0:DD 52,0,0:DD 53,0,0:DD 54,0,0:DD 55,0,0:DD 56,0,0
					If gvCombosForComboChamp<10 Then DD 49,37,1:DD 50,gvCombosForComboChamp+37,1
					If gvCombosForComboChamp>9 And gvCombosForComboChamp<100 Then
						DD 49,INT(gvCombosForComboChamp/10)+37,1
						DD 50,gvCombosForComboChamp-(INT(gvCombosForComboChamp/10)*10)+37,1
					End If
					If gvCombosForComboChamp>99 And gvCombosForComboChamp<1000 Then
						AQWE=gvCombosforComboChamp
						AQ=INT(AQWE/100)
						AQWE=AQWE-AQ*100
						AQW=INT(AQWE/10)
						AQWE=AQWE-AQW*10
						DD 48,AQ+37,1
						DD 49,AQW+37,1
						DD 50,AQWE+37,1
					End If
		Case 35000:For Q=1 To 56
						DD Q,0,0
					Next
					DD 1,0,0:DD 2,0,0:DD 3,0,0:DD 4,10,1:DD 5,1,1:DD 6,3,1:DD 7,11,1:DD 8,16,1:DD 9,15,1:DD 10,20,1:DD 11,0,0:DD 12,0,0:DD 13,0,0:DD 14,0,0
					If gsFreePlay Then
						DD 29,0,0:DD 30,0,0:DD 31,5,1:DD 32,24,1:DD 33,20,1:DD 34,18,1:DD 35,1,1:DD 36,0,0:DD 37,2,1:DD 38,1,1:DD 39,12,1:DD 40,12,1:DD 41,0,0:DD 42,0,0
					Else
						DD 29,0,0:DD 30,0,0:DD 31,18,1:DD 32,5,1:DD 33,16,1:DD 34,12,1:DD 35,1,1:DD 36,25,1:DD 37,0,0:DD 38,1,1:DD 39,20,1:DD 40,0,0:DD 41,0,0:DD 42,0,0
					End If
					DetermineJackpot gsJackpot :DMD_DisplaySceneText "JACKPOT", gsJackpot
					If HS3Z>6 Then:DD 18,HS1H+48,2:Else:DD 18,0,1:End If
					If HS3Z>5 Then:DD 19,HS1I+37,2:Else:DD 19,0,1:End If
					If HS3Z>4 Then:DD 20,HS1J+37,2:Else:DD 20,0,1:End If
					If HS3Z>3 Then:DD 21,HS1K+48,2:Else:DD 21,0,1:End If
					If HS3Z>2 Then:DD 22,HS1L+37,2:Else:DD 22,0,1:End If
					If HS3Z>1 Then:DD 23,HS1M+37,2:Else:DD 23,0,1:End If
					DD 24,HS1N+37,2
					DetermineJackpot gsReplayStart
					If HS3Z>7 Then:DD 46,HS1O+37,1:Else:DD 46,0,1:End If
					If HS3Z>6 Then:DD 47,HS1H+48,1:Else:DD 47,0,1:End If
					If HS3Z>5 Then:DD 48,HS1I+37,1:Else:DD 48,0,1:End If
					If HS3Z>4 Then:DD 49,HS1J+37,1:Else:DD 49,0,1:End If
					If HS3Z>3 Then:DD 50,HS1K+48,1:Else:DD 50,0,1:End If
					If HS3Z>2 Then:DD 51,HS1L+37,1:Else:DD 51,0,1:End If
					If HS3Z>1 Then:DD 52,HS1M+37,1:Else:DD 52,0,1:End If
					DD 53,HS1N+37,1
		Case 36500:	If gsFreePlay Then
						DMD_DisplaySceneText "EXTRA BALL", gsReplayStart
					Else
						DMD_DisplaySceneText "REPLAY AT", gsReplayStart
					End If
		Case 38000:For Q=1 To 56
						DD Q,0,0
					Next
					S1A=0:S1B=0:S1C=0:S1D=0:S1E=0:S1F=0:S1G=0:S1H=0:S1I=0:S1J=0:S1K=0:S1L=0:S1M=0:S1N=0:S1Z=0:S3Z=0
					S2A=0:S2B=0:S2C=0:S2D=0:S2E=0:S2F=0:S2G=0:S2H=0:S2I=0:S2J=0:S2K=0:S2L=0:S2M=0:S2N=0:S2Z=0:S4Z=0
					S5A=0:S5B=0:S5C=0:S5D=0:S5E=0:S5F=0:S5G=0:S5H=0:S5I=0:S5J=0:S5K=0:S5L=0:S5M=0:S5N=0:S5Z=0:S7Z=0
					S6A=0:S6B=0:S6C=0:S6D=0:S6E=0:S6F=0:S6G=0:S6H=0:S6I=0:S6J=0:S6K=0:S6L=0:S6M=0:S6N=0:S6Z=0:S8Z=0
					S1Z=Score(0)
					S2Z=Score(1)
					S5Z=Score(2)
					S6Z=Score(3)
					If S1Z>9999999999999 Then
						S3Z=14
						S1A=INT(S1Z/10000000000000)
						S1Z=S1Z-S1A*10000000000000
					End If
					If S1Z>999999999999 Then
						If S3Z=0 Then S3Z=13
						S1B=INT(S1Z/1000000000000)
						S1Z=S1Z-S1B*1000000000000
					End If
					If S1Z>99999999999 Then
						If S3Z=0 Then S3Z=12
						S1C=INT(S1Z/100000000000)
						S1Z=S1Z-S1C*100000000000
					End If
					If S1Z>9999999999 Then
						If S3Z=0 Then S3Z=11
						S1D=INT(S1Z/10000000000)
						S1Z=S1Z-S1D*10000000000
					End If
					If S1Z>999999999 Then
						If S3Z=0 Then S3Z=10
						S1E=INT(S1Z/1000000000)
						S1Z=S1Z-S1E*1000000000
					End If
					If S1Z>99999999 Then
						If S3Z=0 Then S3Z=9
						S1F=INT(S1Z/100000000)
						S1Z=S1Z-S1F*100000000
					End If
					If S1Z>9999999 Then
						If S3Z=0 Then S3Z=8
						S1G=INT(S1Z/10000000)
						S1Z=S1Z-S1G*10000000
					End If
					If S1Z>999999 Then
						If S3Z=0 Then S3Z=7
						S1H=INT(S1Z/1000000)
						S1Z=S1Z-S1H*1000000
					End If
					If S1Z>99999 Then
						If S3Z=0 Then S3Z=6
						S1I=INT(S1Z/100000)
						S1Z=S1Z-S1I*100000
					End If
					If S1Z>9999 Then
						If S3Z=0 Then S3Z=5
						S1J=INT(S1Z/10000)
						S1Z=S1Z-S1J*10000
					End If
					If S1Z>999 Then
						If S3Z=0 Then S3Z=4
						S1K=INT(S1Z/1000)
						S1Z=S1Z-S1K*1000
					End If
					If S1Z>99 Then
						If S3Z=0 Then S3Z=3
						S1L=INT(S1Z/100)
						S1Z=S1Z-S1L*100
					End If
					If S1Z>9 Then
						If S3Z=0 Then S3Z=2
						S1M=INT(S1Z/10)
						S1Z=S1Z-S1M*10
					End If
					S1N=S1Z
					If S3Z=0 Then S3Z=1
					DD 14,S1N+37,1
					If S3Z>1 Then DD 13,S1M+37,1
					If S3Z>2 Then DD 12,S1L+37,1
					If S3Z>3 Then DD 11,S1K+48,1
					If S3Z>4 Then DD 10,S1J+37,1
					If S3Z>5 Then DD 9,S1I+37,1
					If S3Z>6 Then DD 8,S1H+48,1
					If S3Z>7 Then DD 7,S1G+37,1
					If S3Z>8 Then DD 6,S1F+37,1
					If S3Z>9 Then DD 5,S1E+48,1
					If S3Z>10 Then DD 4,S1D+37,1
					If S3Z>11 Then DD 3,S1C+37,1
					If S3Z>12 Then DD 2,S1B+48,1
					If S3Z>13 Then DD 1,S1A+37,1
					If S2Z>0 Then
						If S2Z>9999999999999 Then
							S4Z=14
							S2A=INT(S2Z/10000000000000)
							S2Z=S2Z-S2A*10000000000000
						End If
						If S2Z>999999999999 Then
							If S4Z=0 Then S4Z=13
							S2B=INT(S2Z/1000000000000)
							S2Z=S2Z-S2B*1000000000000
						End If
						If S2Z>99999999999 Then
							If S4Z=0 Then S4Z=12
							S2C=INT(S2Z/100000000000)
							S2Z=S2Z-S2C*100000000000
						End If
						If S2Z>9999999999 Then
							If S4Z=0 Then S4Z=11
							S2D=INT(S2Z/10000000000)
							S2Z=S2Z-S2D*10000000000
						End If
						If S2Z>999999999 Then
							If S4Z=0 Then S4Z=10
							S2E=INT(S2Z/1000000000)
							S2Z=S2Z-S2E*1000000000
						End If
						If S2Z>99999999 Then
							If S4Z=0 Then S4Z=9
							S2F=INT(S2Z/100000000)
							S2Z=S2Z-S2F*100000000
						End If
						If S2Z>9999999 Then
							If S4Z=0 Then S4Z=8
							S2G=INT(S2Z/10000000)
							S2Z=S2Z-S2G*10000000
						End If
						If S2Z>999999 Then
							If S4Z=0 Then S4Z=7
							S2H=INT(S2Z/1000000)
							S2Z=S2Z-S2H*1000000
						End If
						If S2Z>99999 Then
							If S4Z=0 Then S4Z=6
							S2I=INT(S2Z/100000)
							S2Z=S2Z-S2I*100000
						End If
						If S2Z>9999 Then
							If S4Z=0 Then S4Z=5
							S2J=INT(S2Z/10000)
							S2Z=S2Z-S2J*10000
						End If
						If S2Z>999 Then
							If S4Z=0 Then S4Z=4
							S2K=INT(S2Z/1000)
							S2Z=S2Z-S2K*1000
						End If
						If S2Z>99 Then
							If S4Z=0 Then S4Z=3
							S2L=INT(S2Z/100)
							S2Z=S2Z-S2L*100
						End If
						If S2Z>9 Then
							If S4Z=0 Then S4Z=2
							S2M=INT(S2Z/10)
							S2Z=S2Z-S2M*10
						End If
						S2N=S2Z
						If S4Z=0 Then S4Z=1
						DD 42,S2N+37,1
						If S4Z>1 Then DD 41,S2M+37,1
						If S4Z>2 Then DD 40,S2L+37,1
						If S4Z>3 Then DD 39,S2K+48,1
						If S4Z>4 Then DD 38,S2J+37,1
						If S4Z>5 Then DD 37,S2I+37,1
						If S4Z>6 Then DD 36,S2H+48,1
						If S4Z>7 Then DD 35,S2G+37,1
						If S4Z>8 Then DD 34,S2F+37,1
						If S4Z>9 Then DD 33,S2E+48,1
						If S4Z>10 Then DD 32,S2D+37,1
						If S4Z>11 Then DD 31,S2C+37,1
						If S4Z>12 Then DD 30,S2B+48,1
						If S4Z>13 Then DD 29,S2A+37,1
					End If
					If S5Z>0 Then
						If S5Z>9999999999999 Then
							S7Z=14
							S5A=INT(S5Z/10000000000000)
							S5Z=S5Z-S5A*10000000000000
						End If
						If S5Z>999999999999 Then
							If S7Z=0 Then S7Z=13
							S5B=INT(S5Z/1000000000000)
							S5Z=S5Z-S5B*1000000000000
						End If
						If S5Z>99999999999 Then
							If S7Z=0 Then S7Z=12
							S5C=INT(S5Z/100000000000)
							S5Z=S5Z-S5C*100000000000
						End If
						If S5Z>9999999999 Then
							If S7Z=0 Then S7Z=11
							S5D=INT(S5Z/10000000000)
							S5Z=S5Z-S5D*10000000000
						End If
						If S5Z>999999999 Then
							If S7Z=0 Then S7Z=10
							S5E=INT(S5Z/1000000000)
							S5Z=S5Z-S5E*1000000000
						End If
						If S5Z>99999999 Then
							If S7Z=0 Then S7Z=9
							S5F=INT(S5Z/100000000)
							S5Z=S5Z-S5F*100000000
						End If
						If S5Z>9999999 Then
							If S7Z=0 Then S7Z=8
							S5G=INT(S5Z/10000000)
							S5Z=S5Z-S5G*10000000
						End If
						If S5Z>999999 Then
							If S7Z=0 Then S7Z=7
							S5H=INT(S5Z/1000000)
							S5Z=S5Z-S5H*1000000
						End If
						If S5Z>99999 Then
							If S7Z=0 Then S7Z=6
							S5I=INT(S5Z/100000)
							S5Z=S5Z-S5I*100000
						End If
						If S5Z>9999 Then
							If S7Z=0 Then S7Z=5
							S5J=INT(S5Z/10000)
							S5Z=S5Z-S5J*10000
						End If
						If S5Z>999 Then
							If S7Z=0 Then S7Z=4
							S5K=INT(S5Z/1000)
							S5Z=S5Z-S5K*1000
						End If
						If S5Z>99 Then
							If S7Z=0 Then S7Z=3
							S5L=INT(S5Z/100)
							S5Z=S5Z-S5L*100
						End If
						If S5Z>9 Then
							If S7Z=0 Then S7Z=2
							S5M=INT(S5Z/10)
							S5Z=S5Z-S5M*10
						End If
						S5N=S5Z
						If S7Z=0 Then S7Z=1
						DD 28,S5N+37,1
						If S7Z>1 Then DD 27,S5M+37,1
						If S7Z>2 Then DD 26,S5L+37,1
						If S7Z>3 Then DD 25,S5K+48,1
						If S7Z>4 Then DD 24,S5J+37,1
						If S7Z>5 Then DD 23,S5I+37,1
						If S7Z>6 Then DD 22,S5H+48,1
						If S7Z>7 Then DD 21,S5G+37,1
						If S7Z>8 Then DD 20,S5F+37,1
						If S7Z>9 Then DD 19,S5E+48,1
						If S7Z>10 Then DD 18,S5D+37,1
						If S7Z>11 Then DD 17,S5C+37,1
						If S7Z>12 Then DD 16,S5B+48,1
						If S7Z>13 Then DD 15,S5A+37,1
					End If
					If S6Z>0 Then
						If S6Z>9999999999999 Then
							S8Z=14
							S6A=INT(S6Z/10000000000000)
							S6Z=S6Z-S6A*10000000000000
						End If
						If S6Z>999999999999 Then
							If S8Z=0 Then S8Z=13
							S6B=INT(S6Z/1000000000000)
							S6Z=S6Z-S6B*1000000000000
						End If
						If S6Z>99999999999 Then
							If S8Z=0 Then S8Z=12
							S6C=INT(S6Z/100000000000)
							S6Z=S6Z-S6C*100000000000
						End If
						If S6Z>9999999999 Then
							If S8Z=0 Then S8Z=11
							S6D=INT(S6Z/10000000000)
							S6Z=S6Z-S6D*10000000000
						End If
						If S6Z>999999999 Then
							If S8Z=0 Then S8Z=10
							S6E=INT(S6Z/1000000000)
							S6Z=S6Z-S6E*1000000000
						End If
						If S6Z>99999999 Then
							If S8Z=0 Then S8Z=9
							S6F=INT(S6Z/100000000)
							S6Z=S6Z-S6F*100000000
						End If
						If S6Z>9999999 Then
							If S8Z=0 Then S8Z=8
							S6G=INT(S6Z/10000000)
							S6Z=S6Z-S6G*10000000
						End If
						If S6Z>999999 Then
							If S8Z=0 Then S8Z=7
							S6H=INT(S6Z/1000000)
							S6Z=S6Z-S6H*1000000
						End If
						If S6Z>99999 Then
							If S8Z=0 Then S8Z=6
							S6I=INT(S6Z/100000)
							S6Z=S6Z-S6I*100000
						End If
						If S6Z>9999 Then
							If S8Z=0 Then S8Z=5
							S6J=INT(S6Z/10000)
							S6Z=S6Z-S6J*10000
						End If
						If S6Z>999 Then
							If S8Z=0 Then S8Z=4
							S6K=INT(S6Z/1000)
							S6Z=S6Z-S6K*1000
						End If
						If S6Z>99 Then
							If S8Z=0 Then S8Z=3
							S6L=INT(S6Z/100)
							S6Z=S6Z-S6L*100
						End If
						If S6Z>9 Then
							If S8Z=0 Then S8Z=2
							S6M=INT(S6Z/10)
							S6Z=S6Z-S6M*10
						End If
						S6N=S6Z
						If S8Z=0 Then S8Z=1
						DD 56,S6N+37,1
						If S8Z>1 Then DD 55,S6M+37,1
						If S8Z>2 Then DD 54,S6L+37,1
						If S8Z>3 Then DD 53,S6K+48,1
						If S8Z>4 Then DD 52,S6J+37,1
						If S8Z>5 Then DD 51,S6I+37,1
						If S8Z>6 Then DD 50,S6H+48,1
						If S8Z>7 Then DD 49,S6G+37,1
						If S8Z>8 Then DD 48,S6F+37,1
						If S8Z>9 Then DD 47,S6E+48,1
						If S8Z>10 Then DD 46,S6D+37,1
						If S8Z>11 Then DD 45,S6C+37,1
						If S8Z>12 Then DD 44,S6B+48,1
						If S8Z>13 Then DD 43,S6A+37,1
					End If
					DMD_DisplayScores
		Case 41000:DisplayText"SPINNER  CHAMP",1,1:DisplayText"              ",2,1						 :DMD_DisplaySceneText "SPINNER CHAMP", Trim(gsTrackScoreName(1))&" - "&gsTrackScore(1)
					DisplayText" 1> "&gsTrackScoreName(1)&"     ",3,1:DisplayText"              ",4,1
					ShowTrackScore 1
		Case 43000:DisplayText"ALBUMS   CHAMP",1,1:DisplayText"              ",2,1						 :DMD_DisplaySceneText "ALBUMS CHAMP", Trim(gsTrackScoreName(2))&" - "&gsTrackScore(2)
					DisplayText" 2> "&gsTrackScoreName(2)&"     ",3,1:DisplayText"              ",4,1    
					ShowTrackScore 2
		Case 45000:DisplayText"BUMPERS  CHAMP",1,1:DisplayText"              ",2,1						 :DMD_DisplaySceneText "BUMPERS CHAMP", Trim(gsTrackScoreName(3))&" - "&gsTrackScore(3)
					DisplayText" 3> "&gsTrackScoreName(3)&"     ",3,1:DisplayText"              ",4,1    
					ShowTrackScore 3
		Case 47000:DisplayText"RAMPS    CHAMP",1,1:DisplayText"              ",2,1						 :DMD_DisplaySceneText "RAMPS CHAMP", Trim(gsTrackScoreName(4))&" - "&gsTrackScore(4)
					DisplayText" 4> "&gsTrackScoreName(4)&"     ",3,1:DisplayText"              ",4,1    
					ShowTrackScore 4
		Case 49000:DisplayText"BAND     CHAMP",1,1:DisplayText"              ",2,1						 :DMD_DisplaySceneText "BAND CHAMP", Trim(gsTrackScoreName(5))&" - "&gsTrackScore(5)
					DisplayText" 5> "&gsTrackScoreName(5)&"     ",3,1:DisplayText"              ",4,1    
					ShowTrackScore 5
		Case 51000:DisplayText"STANDUPS CHAMP",1,1:DisplayText"              ",2,1						 :DMD_DisplaySceneText "STANDUPS CHAMP", Trim(gsTrackScoreName(6))&" - "&gsTrackScore(6)
					DisplayText" 6> "&gsTrackScoreName(6)&"     ",3,1:DisplayText"              ",4,1    
					ShowTrackScore 6
		Case 53000:DisplayText"ORBITS   CHAMP",1,1:DisplayText"              ",2,1						 :DMD_DisplaySceneText "ORBITS CHAMP", Trim(gsTrackScoreName(7))&" - "&gsTrackScore(7)
					DisplayText" 7> "&gsTrackScoreName(7)&"     ",3,1:DisplayText"              ",4,1    
					ShowTrackScore 7
		Case 55000:DisplayText"LOOPS    CHAMP",1,1:DisplayText"              ",2,1						 :DMD_DisplaySceneText "LOOPS CHAMP", Trim(gsTrackScoreName(8))&" - "&gsTrackScore(8)
					DisplayText" 8> "&gsTrackScoreName(8)&"     ",3,1:DisplayText"              ",4,1    
					ShowTrackScore 8
		Case 57000:DisplayText"COMBOS   CHAMP",1,1:DisplayText"              ",2,1						 :DMD_DisplaySceneText "COMBOS CHAMP", Trim(gsTrackScoreName(9))&" - "&gsTrackScore(9)
					DisplayText" 9> "&gsTrackScoreName(9)&"     ",3,1:DisplayText"              ",4,1    
					ShowTrackScore 9 
		Case 59000:DisplayText"MINIS    CHAMP",1,1:DisplayText"              ",2,1						 :DMD_DisplaySceneText "MINIS CHAMP", Trim(gsTrackScoreName(10))&" - "&gsTrackScore(10)
					DisplayText"10> "&gsTrackScoreName(10)&"     ",3,1:DisplayText"              ",4,1   
					ShowTrackScore 10
		Case 61000:DisplayText"FOLLOW   CHAMP",1,1:DisplayText"              ",2,1						 :DMD_DisplaySceneText "FOLLOW CHAMP", Trim(gsTrackScoreName(11))&" - "&gsTrackScore(11)
					DisplayText"11> "&gsTrackScoreName(11)&"     ",3,1:DisplayText"              ",4,1   
					ShowTrackScore 11
		Case 63000:DisplayText"FRENZY   CHAMP",1,1:DisplayText"              ",2,1						 :DMD_DisplaySceneText "FRENZY CHAMP", Trim(gsTrackScoreName(12))&" - "&gsTrackScore(12)
					DisplayText"12> "&gsTrackScoreName(12)&"     ",3,1:DisplayText"              ",4,1   
					ShowTrackScore 12
		Case 65000:For Q=1 To 56
						DD Q,0,0
					Next
					DD 14,S1N+37,1
					If S3Z>1 Then DD 13,S1M+37,1
					If S3Z>2 Then DD 12,S1L+37,1
					If S3Z>3 Then DD 11,S1K+48,1
					If S3Z>4 Then DD 10,S1J+37,1
					If S3Z>5 Then DD 9,S1I+37,1
					If S3Z>6 Then DD 8,S1H+48,1
					If S3Z>7 Then DD 7,S1G+37,1
					If S3Z>8 Then DD 6,S1F+37,1
					If S3Z>9 Then DD 5,S1E+48,1
					If S3Z>10 Then DD 4,S1D+37,1
					If S3Z>11 Then DD 3,S1C+37,1
					If S3Z>12 Then DD 2,S1B+48,1
					If S3Z>13 Then DD 1,S1A+37,1
					If S2Z>0 Then
						DD 42,S2N+37,1
						If S4Z>1 Then DD 41,S2M+37,1
						If S4Z>2 Then DD 40,S2L+37,1
						If S4Z>3 Then DD 39,S2K+48,1
						If S4Z>4 Then DD 38,S2J+37,1
						If S4Z>5 Then DD 37,S2I+37,1
						If S4Z>6 Then DD 36,S2H+48,1
						If S4Z>7 Then DD 35,S2G+37,1
						If S4Z>8 Then DD 34,S2F+37,1
						If S4Z>9 Then DD 33,S2E+48,1
						If S4Z>10 Then DD 32,S2D+37,1
						If S4Z>11 Then DD 31,S2C+37,1
						If S4Z>12 Then DD 30,S2B+48,1
						If S4Z>13 Then DD 29,S2A+37,1
					End If
					If S5Z>0 Then
						DD 28,S5N+37,1
						If S7Z>1 Then DD 27,S5M+37,1
						If S7Z>2 Then DD 26,S5L+37,1
						If S7Z>3 Then DD 25,S5K+48,1
						If S7Z>4 Then DD 24,S5J+37,1
						If S7Z>5 Then DD 23,S5I+37,1
						If S7Z>6 Then DD 22,S5H+48,1
						If S7Z>7 Then DD 21,S5G+37,1
						If S7Z>8 Then DD 20,S5F+37,1
						If S7Z>9 Then DD 19,S5E+48,1
						If S7Z>10 Then DD 18,S5D+37,1
						If S7Z>11 Then DD 17,S5C+37,1
						If S7Z>12 Then DD 16,S5B+48,1
						If S7Z>13 Then DD 15,S5A+37,1
					End If
					If S6Z>0 Then
						DD 56,S6N+37,1
						If S8Z>1 Then DD 55,S6M+37,1
						If S8Z>2 Then DD 54,S6L+37,1
						If S8Z>3 Then DD 53,S6K+48,1
						If S8Z>4 Then DD 52,S6J+37,1
						If S8Z>5 Then DD 51,S6I+37,1
						If S8Z>6 Then DD 50,S6H+48,1
						If S8Z>7 Then DD 49,S6G+37,1
						If S8Z>8 Then DD 48,S6F+37,1
						If S8Z>9 Then DD 47,S6E+48,1
						If S8Z>10 Then DD 46,S6D+37,1
						If S8Z>11 Then DD 45,S6C+37,1
						If S8Z>12 Then DD 44,S6B+48,1
						If S8Z>13 Then DD 43,S6A+37,1
					End If
					DMD_DisplayScores
		Case 67000:DD 1,0,0:DD 2,0,0:DD 3,7,2:DD 4,1,2:DD 5,13,2:DD 6,5,2:DD 7,0,0:DD 8,0,0:DD 9,15,2:DD 10,22,2:DD 11,5,2:DD 12,18,2:DD 13,0,0:DD 14,0,0
					DD 15,0,0:DD 16,0,0:DD 17,0,0:DD 18,0,0:DD 19,0,0:DD 20,0,0:DD 21,0,0:DD 22,0,0:DD 23,0,0:DD 24,0,0:DD 25,0,0:DD 26,0,0:DD 27,0,0:DD 28,0,0
					DD 29,0,0:DD 30,0,0:DD 31,7,2:DD 32,1,2:DD 33,13,2:DD 34,5,2:DD 35,0,0:DD 36,0,0:DD 37,15,2:DD 38,22,2:DD 39,5,2:DD 40,18,2:DD 41,0,0:DD 42,0,0
					DD 43,0,0:DD 44,0,0:DD 45,0,0:DD 46,0,0:DD 47,0,0:DD 48,0,0:DD 49,0,0:DD 50,0,0:DD 51,0,0:DD 52,0,0:DD 53,0,0:DD 54,0,0:DD 55,0,0:DD 56,0,0
					DMD_DisplaySceneText "   GAME OVER   ", ""
		Case 69000:If gsFreePlay Then
						DD 1,0,0:DD 2,0,0:DD 3,6,1:DD 4,18,1:DD 5,5,1:DD 6,5,1:DD 7,0,0:DD 8,0,0:DD 9,16,1:DD 10,12,1:DD 11,1,1:DD 12,25,1:DD 13,0,0:DD 14,0,0
						DD 15,0,0:DD 16,16,2:DD 17,18,2:DD 18,5,2:DD 19,19,2:DD 20,19,2:DD 21,0,0:DD 22,0,0:DD 23,19,2:DD 24,20,2:DD 25,1,2:DD 26,18,2:DD 27,20,2:DD 28,0,0
						DD 29,0,0:DD 30,0,0:DD 31,6,1:DD 32,18,1:DD 33,5,1:DD 34,5,1:DD 35,0,0:DD 36,0,0:DD 37,16,1:DD 38,12,1:DD 39,1,1:DD 40,25,1:DD 41,0,0:DD 42,0,0
						DD 43,0,0:DD 44,16,2:DD 45,18,2:DD 46,5,2:DD 47,19,2:DD 48,19,2:DD 49,0,0:DD 50,0,0:DD 51,19,2:DD 52,20,2:DD 53,1,2:DD 54,18,2:DD 55,20,2:DD 56,0,0
						DMD_DisplaySceneText "FREE PLAY", "PRESS START"
					Else
						DD 1,3,1:DD 2,18,1:DD 3,5,1:DD 4,4,1:DD 5,9,1:DD 6,20,1:DD 7,19,1:DD 8,0,0:DD 9,0,0:DD 10,0,0:DD 11,0,0:DD 12,0,0
						phCredits1=0:phCredits2=0
						If gsCredits>9 Then phCredits1=INT(gsCredits/10)
						phCredits2=gsCredits-phCredits1*10
						If phCredits1>0 Then DD 13,phCredits1+37,1
						DD 14,phCredits2+37,1
						If gsCredits>0 Then
							DD 15,0,0:DD 16,16,2:DD 17,18,2:DD 18,5,2:DD 19,19,2:DD 20,19,2:DD 21,0,0:DD 22,0,2:DD 23,19,2:DD 24,20,2:DD 25,1,2:DD 26,18,2:DD 27,20,2:DD 28,0,0
							If phCredits1>0 Then 	
								DMD_DisplaySceneText "CREDITS   "&phCredits1&phCredits2, "PRESS START"
							Else
								DMD_DisplaySceneText "CREDITS    "&phCredits2, "PRESS START"
							End If
						Else
							DD 15,0,0:DD 16,9,2:DD 17,14,2:DD 18,19,2:DD 19,5,2:DD 20,18,2:DD 21,20,2:DD 22,0,0:DD 23,0,0:DD 24,3,2:DD 25,15,2:DD 26,9,2:DD 27,14,2:DD 28,0,0
							DMD_DisplaySceneText "CREDITS    0", "INSERT COIN"
						End If
						DD 29,0,0:DD 30,38,1:DD 31,0,0:DD 32,3,1:DD 33,18,1:DD 34,5,1:DD 35,4,1:DD 36,9,1:DD 37,20,1:DD 38,0,0:DD 39,16,1:DD 40,5,1:DD 41,18,1:DD 42,0,0
						DD 43,0,0:DD 44,0,0:DD 45,0,0:DD 46,0,0:DD 47,16,1:DD 48,12,1:DD 49,1,1:DD 50,25,1:DD 51,5,1:DD 52,18,1:DD 53,0,0:DD 54,0,0:DD 55,0,0:DD 56,0,0
					End If
	End Select
End Sub

Sub ShowTrackScore(TrackNo)
	T2A=0:T2B=0:T2C=0:T2D=0:T2E=0:T2F=0:T2G=0:T2H=0:T2I=0:T2J=0:T2K=0:T2L=0:T2M=0:T2N=0:T2Z=0:T4Z=0
	T2Z=gsTrackScore(TrackNo)
	If T2Z>9999999999999 Then
		T4Z=14
		T2A=INT(T2Z/10000000000000)
		T2Z=T2Z-T2A*10000000000000
	End If
	If T2Z>999999999999 Then
		If T4Z=0 Then T4Z=13
		T2B=INT(T2Z/1000000000000)
		T2Z=T2Z-T2B*1000000000000
	End If
	If T2Z>99999999999 Then
		If T4Z=0 Then T4Z=12
		T2C=INT(T2Z/100000000000)
		T2Z=T2Z-T2C*100000000000
	End If
	If T2Z>9999999999 Then
		If T4Z=0 Then T4Z=11
		T2D=INT(T2Z/10000000000)
		T2Z=T2Z-T2D*10000000000
	End If
	If T2Z>999999999 Then
		If T4Z=0 Then T4Z=10
		T2E=INT(T2Z/1000000000)
		T2Z=T2Z-T2E*1000000000
	End If
	If T2Z>99999999 Then
		If T4Z=0 Then T4Z=9
		T2F=INT(T2Z/100000000)
		T2Z=T2Z-T2F*100000000
	End If
	If T2Z>9999999 Then
		If T4Z=0 Then T4Z=8
		T2G=INT(T2Z/10000000)
		T2Z=T2Z-T2G*10000000
	End If
	If T2Z>999999 Then
		If T4Z=0 Then T4Z=7
		T2H=INT(T2Z/1000000)
		T2Z=T2Z-T2H*1000000
	End If
	If T2Z>99999 Then
		If T4Z=0 Then T4Z=6
		T2I=INT(T2Z/100000)
		T2Z=T2Z-T2I*100000
	End If
	If T2Z>9999 Then
		If T4Z=0 Then T4Z=5
		T2J=INT(T2Z/10000)
		T2Z=T2Z-T2J*10000
	End If
	If T2Z>999 Then
		If T4Z=0 Then T4Z=4
		T2K=INT(T2Z/1000)
		T2Z=T2Z-T2K*1000
	End If
	If T2Z>99 Then
		If T4Z=0 Then T4Z=3
		T2L=INT(T2Z/100)
		T2Z=T2Z-T2L*100
	End If
	If T2Z>9 Then
		If T4Z=0 Then T4Z=2
		T2M=INT(T2Z/10)
		T2Z=T2Z-T2M*10
	End If
	T2N=T2Z
	If T4Z=0 Then T4Z=1
	DD 56,T2N+37,1
	If T4Z>1 Then DD 55,T2M+37,1
	If T4Z>2 Then DD 54,T2L+37,1
	If T4Z>3 Then DD 53,T2K+48,1
	If T4Z>4 Then DD 52,T2J+37,1
	If T4Z>5 Then DD 51,T2I+37,1
	If T4Z>6 Then DD 50,T2H+48,1
	If T4Z>7 Then DD 49,T2G+37,1
	If T4Z>8 Then DD 48,T2F+37,1
	If T4Z>9 Then DD 47,T2E+48,1
	If T4Z>10 Then DD 46,T2D+37,1
	If T4Z>11 Then DD 45,T2C+37,1
	If T4Z>12 Then DD 44,T2B+48,1
	If T4Z>13 Then DD 43,T2A+37,1
End Sub

Sub DetermineComboChamp
	LPA(1)=ASC(Mid(gsComboChampName,1,1))
	LPA(2)=ASC(Mid(gsComboChampName,2,1))
	LPA(3)=ASC(Mid(gsComboChampName,3,1))
	LPA(4)=ASC(Mid(gsComboChampName,4,1))
	LPA(5)=ASC(Mid(gsComboChampName,5,1))
	SortNameD LPA(1),1
	SortNameD LPA(2),2
	SortNameD LPA(3),3
	SortNameD LPA(4),4
	SortNameD LPA(5),5
End Sub

Sub DetermineHSName(Nu)
	LPA(1)=ASC(Mid(gsHighScoreName(Nu),1,1))
	LPA(2)=ASC(Mid(gsHighScoreName(Nu),2,1))
	LPA(3)=ASC(Mid(gsHighScoreName(Nu),3,1))
	LPA(4)=ASC(Mid(gsHighScoreName(Nu),4,1))
	LPA(5)=ASC(Mid(gsHighScoreName(Nu),5,1))
	SortNameD LPA(1),1
	SortNameD LPA(2),2
	SortNameD LPA(3),3
	SortNameD LPA(4),4
	SortNameD LPA(5),5
End Sub

Sub SortNameD(char,Nu)
	Select Case char
		Case 32:LPA(Nu)=0
		Case 39:LPA(Nu)=118
		Case 42:LPA(Nu)=30
		Case 43:LPA(Nu)=31
		Case 45:LPA(Nu)=32
		Case 47:LPA(Nu)=33
		Case 48:LPA(Nu)=37
		Case 49:LPA(Nu)=38
		Case 50:LPA(Nu)=39
		Case 51:LPA(Nu)=40
		Case 52:LPA(Nu)=41
		Case 53:LPA(Nu)=42
		Case 54:LPA(Nu)=43
		Case 55:LPA(Nu)=44
		Case 56:LPA(Nu)=45
		Case 57:LPA(Nu)=46
		Case 60:LPA(Nu)=28
		Case 61:LPA(Nu)=34
		Case 62:LPA(Nu)=29
		Case 65:LPA(Nu)=1
		Case 66:LPA(Nu)=2
		Case 67:LPA(Nu)=3
		Case 68:LPA(Nu)=4
		Case 69:LPA(Nu)=5
		Case 70:LPA(Nu)=6
		Case 71:LPA(Nu)=7
		Case 72:LPA(Nu)=8
		Case 73:LPA(Nu)=9
		Case 74:LPA(Nu)=10
		Case 75:LPA(Nu)=11
		Case 76:LPA(Nu)=12
		Case 77:LPA(Nu)=13
		Case 78:LPA(Nu)=14
		Case 79:LPA(Nu)=15
		Case 80:LPA(Nu)=16
		Case 81:LPA(Nu)=17
		Case 82:LPA(Nu)=18
		Case 83:LPA(Nu)=19
		Case 84:LPA(Nu)=20
		Case 85:LPA(Nu)=21
		Case 86:LPA(Nu)=22
		Case 87:LPA(Nu)=23
		Case 88:LPA(Nu)=24
		Case 89:LPA(Nu)=25
		Case 90:LPA(Nu)=26
		Case 92:LPA(Nu)=35
		Case 94:LPA(Nu)=36
		Case 95:LPA(Nu)=116
		Case 96:LPA(Nu)=47
	End Select
End Sub

Sub DetermineHighScore(Nu)
	HS1A=0:HS1B=0:HS1C=0:HS1D=0:HS1E=0:HS1F=0:HS1G=0:HS1H=0:HS1I=0:HS1J=0:HS1K=0:HS1L=0:HS1M=0:HS1N=0:HS1Z=0:HS3Z=0
	HS1Z=gsHighScore(Nu)
	If HS1Z>9999999999999 Then
		HS3Z=14
		HS1A=INT(HS1Z/10000000000000)
		HS1Z=HS1Z-HS1A*10000000000000
	End If
	If HS1Z>999999999999 Then
		If HS3Z=0 Then HS3Z=13
		HS1B=INT(HS1Z/1000000000000)
		HS1Z=HS1Z-HS1B*1000000000000
	End If
	If HS1Z>99999999999 Then
		If HS3Z=0 Then HS3Z=12
		HS1C=INT(HS1Z/100000000000)
		HS1Z=HS1Z-HS1C*100000000000
	End If
	If HS1Z>9999999999 Then
		If HS3Z=0 Then HS3Z=11
		HS1D=INT(HS1Z/10000000000)
		HS1Z=HS1Z-HS1D*10000000000
	End If
	If HS1Z>999999999 Then
		If HS3Z=0 Then HS3Z=10
		HS1E=INT(HS1Z/1000000000)
		HS1Z=HS1Z-HS1E*1000000000
	End If
	If HS1Z>99999999 Then
		If HS3Z=0 Then HS3Z=9
		HS1F=INT(HS1Z/100000000)
		HS1Z=HS1Z-HS1F*100000000
	End If
	If HS1Z>9999999 Then
		If HS3Z=0 Then HS3Z=8
		HS1G=INT(HS1Z/10000000)
		HS1Z=HS1Z-HS1G*10000000
	End If
	If HS1Z>999999 Then
		If HS3Z=0 Then HS3Z=7
		HS1H=INT(HS1Z/1000000)
		HS1Z=HS1Z-HS1H*1000000
	End If
	If HS1Z>99999 Then
		If HS3Z=0 Then HS3Z=6
		HS1I=INT(HS1Z/100000)
		HS1Z=HS1Z-HS1I*100000
	End If
	If HS1Z>9999 Then
		If HS3Z=0 Then HS3Z=5
		HS1J=INT(HS1Z/10000)
		HS1Z=HS1Z-HS1J*10000
	End If
	If HS1Z>999 Then
		If HS3Z=0 Then HS3Z=4
		HS1K=INT(HS1Z/1000)
		HS1Z=HS1Z-HS1K*1000
	End If
	If HS1Z>99 Then
		If HS3Z=0 Then HS3Z=3
		HS1L=INT(HS1Z/100)
		HS1Z=HS1Z-HS1L*100
	End If
	If HS1Z>9 Then
		If HS3Z=0 Then HS3Z=2
		HS1M=INT(HS1Z/10)
		HS1Z=HS1Z-HS1M*10
	End If
	HS1N=HS1Z
	If HS3Z=0 Then HS3Z=1
End Sub

Sub DisplayPoints(Nu)
	HS1A=0:HS1B=0:HS1C=0:HS1D=0:HS1E=0:HS1F=0:HS1G=0:HS1H=0:HS1I=0:HS1J=0:HS1K=0:HS1L=0:HS1M=0:HS1N=0:HS1Z=0:HS3Z=0
	HS1Z=Nu
	If HS1Z>9999999999999 Then
		HS3Z=14
		HS1A=INT(HS1Z/10000000000000)
		HS1Z=HS1Z-HS1A*10000000000000
	End If
	If HS1Z>999999999999 Then
		If HS3Z=0 Then HS3Z=13
		HS1B=INT(HS1Z/1000000000000)
		HS1Z=HS1Z-HS1B*1000000000000
	End If
	If HS1Z>99999999999 Then
		If HS3Z=0 Then HS3Z=12
		HS1C=INT(HS1Z/100000000000)
		HS1Z=HS1Z-HS1C*100000000000
	End If
	If HS1Z>9999999999 Then
		If HS3Z=0 Then HS3Z=11
		HS1D=INT(HS1Z/10000000000)
		HS1Z=HS1Z-HS1D*10000000000
	End If
	If HS1Z>999999999 Then
		If HS3Z=0 Then HS3Z=10
		HS1E=INT(HS1Z/1000000000)
		HS1Z=HS1Z-HS1E*1000000000
	End If
	If HS1Z>99999999 Then
		If HS3Z=0 Then HS3Z=9
		HS1F=INT(HS1Z/100000000)
		HS1Z=HS1Z-HS1F*100000000
	End If
	If HS1Z>9999999 Then
		If HS3Z=0 Then HS3Z=8
		HS1G=INT(HS1Z/10000000)
		HS1Z=HS1Z-HS1G*10000000
	End If
	If HS1Z>999999 Then
		If HS3Z=0 Then HS3Z=7
		HS1H=INT(HS1Z/1000000)
		HS1Z=HS1Z-HS1H*1000000
	End If
	If HS1Z>99999 Then
		If HS3Z=0 Then HS3Z=6
		HS1I=INT(HS1Z/100000)
		HS1Z=HS1Z-HS1I*100000
	End If
	If HS1Z>9999 Then
		If HS3Z=0 Then HS3Z=5
		HS1J=INT(HS1Z/10000)
		HS1Z=HS1Z-HS1J*10000
	End If
	If HS1Z>999 Then
		If HS3Z=0 Then HS3Z=4
		HS1K=INT(HS1Z/1000)
		HS1Z=HS1Z-HS1K*1000
	End If
	If HS1Z>99 Then
		If HS3Z=0 Then HS3Z=3
		HS1L=INT(HS1Z/100)
		HS1Z=HS1Z-HS1L*100
	End If
	If HS1Z>9 Then
		If HS3Z=0 Then HS3Z=2
		HS1M=INT(HS1Z/10)
		HS1Z=HS1Z-HS1M*10
	End If
	HS1N=HS1Z
	If HS3Z=0 Then HS3Z=1
End Sub

Sub DetermineJackpot(Nu)
	HS1A=0:HS1B=0:HS1C=0:HS1D=0:HS1E=0:HS1F=0:HS1G=0:HS1H=0:HS1I=0:HS1J=0:HS1K=0:HS1L=0:HS1M=0:HS1N=0:HS1O=0:HS1Z=0:HS3Z=0
	HS1Z=Nu
	If HS1Z>9999999 Then
		If HS3Z=0 Then HS3Z=8
		HS1O=INT(HS1Z/10000000)
		HS1Z=HS1Z-HS1O*10000000
	End If
	If HS1Z>999999 Then
		If HS3Z=0 Then HS3Z=7
		HS1H=INT(HS1Z/1000000)
		HS1Z=HS1Z-HS1H*1000000
	End If
	If HS1Z>99999 Then
		If HS3Z=0 Then HS3Z=6
		HS1I=INT(HS1Z/100000)
		HS1Z=HS1Z-HS1I*100000
	End If
	If HS1Z>9999 Then
		If HS3Z=0 Then HS3Z=5
		HS1J=INT(HS1Z/10000)
		HS1Z=HS1Z-HS1J*10000
	End If
	If HS1Z>999 Then
		If HS3Z=0 Then HS3Z=4
		HS1K=INT(HS1Z/1000)
		HS1Z=HS1Z-HS1K*1000
	End If
	If HS1Z>99 Then
		If HS3Z=0 Then HS3Z=3
		HS1L=INT(HS1Z/100)
		HS1Z=HS1Z-HS1L*100
	End If
	If HS1Z>9 Then
		If HS3Z=0 Then HS3Z=2
		HS1M=INT(HS1Z/10)
		HS1Z=HS1Z-HS1M*10
	End If
	HS1N=HS1Z
	If HS3Z=0 Then HS3Z=1
End Sub

' this function queues up all the displays used in attract mode
Sub StartAttractModeDisplays
	For Q=1 To 56:DD Q,0,0:Next
	BTimer.Enabled=0
	PH1=0 'reset placeholder queue to start
	ATimer.Enabled=0
	ATimer.Enabled=1
	RandomCalloutCount=0
	RandomCallout.Enabled=0
	RandomCallout.Enabled=1
	LightSeq1.UpdateInterval=10:LightSeq1.Play SeqUpOn,0,1:LightSeq1.Play SeqDownOff,0,1,500
	LightSeq1_PlayDone
End Sub

'     Display Driver for Multiplayer/Segment Use
' If the current players displays are free (nothing in the queue)
' Then draw the score and ballsremaining
Sub DisplayScore
	If Not QTimer.Enabled And HurryUpActive=FALSE And HurryUpSingleActive=FALSE Then
		If bGameInPlay=TRUE Then
			If PlayersPlayingGame>0 Then
				DisplayPoints(Score(0))
				DD 14,HS1N+37,1
				If HS3Z>1 Then:DD 13,HS1M+37,1:Else:DD 13,0,1:End If
				If HS3Z>2 Then:DD 12,HS1L+37,1:Else:DD 12,0,1:End If
				If HS3Z>3 Then:DD 11,HS1K+48,1:Else:DD 11,0,1:End If
				If HS3Z>4 Then:DD 10,HS1J+37,1:Else:DD 10,0,1:End If
				If HS3Z>5 Then:DD 9,HS1I+37,1:Else:DD 9,0,1:End If
				If HS3Z>6 Then:DD 8,HS1H+48,1:Else:DD 8,0,1:End If
				If HS3Z>7 Then:DD 7,HS1G+37,1:Else:DD 7,0,1:End If
				If HS3Z>8 Then:DD 6,HS1F+37,1:Else:DD 6,0,1:End If
				If HS3Z>9 Then:DD 5,HS1E+48,1:Else:DD 5,0,1:End If
				If HS3Z>10 Then:DD 4,HS1D+37,1:Else:DD 4,0,1:End If
				If HS3Z>11 Then:DD 3,HS1C+37,1:Else:DD 3,0,1:End If
				If HS3Z>12 Then:DD 2,HS1B+48,1:Else:DD 2,0,1:End If
				If HS3Z>13 Then:DD 1,HS1A+37,1:Else:DD 1,0,1:End If
				' If on the first ball (and still in plunger) then entice a second player - flashing lights this way takes a lot of cpu power, so stop enticement once ball is in play
				If gvFirstBallEjected=FALSE And PlayersPlayingGame=1 Then
					If gsBallsPerGame+1-BallsRemaining(0)=1 Then
						If gsFreePlay=TRUE Then
							FlashInsertCoin
							DD 29,0,1:DD 30,16,2:DD 31,18,2:DD 32,5,2:DD 33,19,2:DD 34,19,2:DD 35,0,1:DD 36,0,1:DD 37,19,2:DD 38,20,2:DD 39,1,2:DD 40,18,2:DD 41,20,2:DD 42,0,1
							DD 43,0,1:DD 44,6,1:DD 45,15,1:DD 46,18,1:DD 47,0,1:DD 48,39,1:DD 49,0,1:DD 50,16,1:DD 51,12,1:DD 52,1,1:DD 53,25,1:DD 54,5,1:DD 55,18,1:DD 56,0,1
						Else  ' playing with credits
							If gsCredits>0 Then
								FlashInsertCoin
								DD 29,0,1:DD 30,16,2:DD 31,18,2:DD 32,5,2:DD 33,19,2:DD 34,19,2:DD 35,0,1:DD 36,0,1:DD 37,19,2:DD 38,20,2:DD 39,1,2:DD 40,18,2:DD 41,20,2:DD 42,0,1
								DD 43,0,1:DD 44,6,1:DD 45,15,1:DD 46,18,1:DD 47,0,1:DD 48,39,1:DD 49,0,1:DD 50,16,1:DD 51,12,1:DD 52,1,1:DD 53,25,1:DD 54,5,1:DD 55,18,1:DD 56,0,1
							Else
								FlashInsertCoin
								DD 29,0,1:DD 30,9,2:DD 31,14,2:DD 32,19,2:DD 33,5,2:DD 34,18,2:DD 35,20,2:DD 36,0,1:DD 37,0,1:DD 38,3,2:DD 39,15,2:DD 40,9,2:DD 41,14,2:DD 42,0,1
								DD 43,0,1:DD 44,6,1:DD 45,15,1:DD 46,18,1:DD 47,0,1:DD 48,39,1:DD 49,0,1:DD 50,16,1:DD 51,12,1:DD 52,1,1:DD 53,25,1:DD 54,5,1:DD 55,18,1:DD 56,0,1
							End If
						End If
					End If
				End If
				If PlayersPlayingGame>1 Then
					DisplayPoints(Score(1))
					DD 42,HS1N+37,1
					If HS3Z>1 Then:DD 41,HS1M+37,1:Else:DD 41,0,1:End If
					If HS3Z>2 Then:DD 40,HS1L+37,1:Else:DD 40,0,1:End If
					If HS3Z>3 Then:DD 39,HS1K+48,1:Else:DD 39,0,1:End If
					If HS3Z>4 Then:DD 38,HS1J+37,1:Else:DD 38,0,1:End If
					If HS3Z>5 Then:DD 37,HS1I+37,1:Else:DD 37,0,1:End If
					If HS3Z>6 Then:DD 36,HS1H+48,1:Else:DD 36,0,1:End If
					If HS3Z>7 Then:DD 35,HS1G+37,1:Else:DD 35,0,1:End If
					If HS3Z>8 Then:DD 34,HS1F+37,1:Else:DD 34,0,1:End If
					If HS3Z>9 Then:DD 33,HS1E+48,1:Else:DD 33,0,1:End If
					If HS3Z>10 Then:DD 32,HS1D+37,1:Else:DD 32,0,1:End If
					If HS3Z>11 Then:DD 31,HS1C+37,1:Else:DD 31,0,1:End If
					If HS3Z>12 Then:DD 30,HS1B+48,1:Else:DD 30,0,1:End If
					If HS3Z>13 Then:DD 29,HS1A+37,1:Else:DD 29,0,1:End If
				End If
				If PlayersPlayingGame>2 Then
					DisplayPoints(Score(2))
					DD 28,HS1N+37,1
					If HS3Z>1 Then:DD 27,HS1M+37,1:Else:DD 27,0,1:End If
					If HS3Z>2 Then:DD 26,HS1L+37,1:Else:DD 26,0,1:End If
					If HS3Z>3 Then:DD 25,HS1K+48,1:Else:DD 25,0,1:End If
					If HS3Z>4 Then:DD 24,HS1J+37,1:Else:DD 24,0,1:End If
					If HS3Z>5 Then:DD 23,HS1I+37,1:Else:DD 23,0,1:End If
					If HS3Z>6 Then:DD 22,HS1H+48,1:Else:DD 22,0,1:End If
					If HS3Z>7 Then:DD 21,HS1G+37,1:Else:DD 21,0,1:End If
					If HS3Z>8 Then:DD 20,HS1F+37,1:Else:DD 20,0,1:End If
					If HS3Z>9 Then:DD 19,HS1E+48,1:Else:DD 19,0,1:End If
					If HS3Z>10 Then:DD 18,HS1D+37,1:Else:DD 18,0,1:End If
					If HS3Z>11 Then:DD 17,HS1C+37,1:Else:DD 17,0,1:End If
					If HS3Z>12 Then:DD 16,HS1B+48,1:Else:DD 16,0,1:End If
					If HS3Z>13 Then:DD 15,HS1A+37,1:Else:DD 15,0,1:End If
				End If
				If PlayersPlayingGame>3 Then
					DisplayPoints(Score(3))
					DD 56,HS1N+37,1
					If HS3Z>1 Then:DD 55,HS1M+37,1:Else:DD 55,0,1:End If
					If HS3Z>2 Then:DD 54,HS1L+37,1:Else:DD 54,0,1:End If
					If HS3Z>3 Then:DD 53,HS1K+48,1:Else:DD 53,0,1:End If
					If HS3Z>4 Then:DD 52,HS1J+37,1:Else:DD 52,0,1:End If
					If HS3Z>5 Then:DD 51,HS1I+37,1:Else:DD 51,0,1:End If
					If HS3Z>6 Then:DD 50,HS1H+48,1:Else:DD 50,0,1:End If
					If HS3Z>7 Then:DD 49,HS1G+37,1:Else:DD 49,0,1:End If
					If HS3Z>8 Then:DD 48,HS1F+37,1:Else:DD 48,0,1:End If
					If HS3Z>9 Then:DD 47,HS1E+48,1:Else:DD 47,0,1:End If
					If HS3Z>10 Then:DD 46,HS1D+37,1:Else:DD 46,0,1:End If
					If HS3Z>11 Then:DD 45,HS1C+37,1:Else:DD 45,0,1:End If
					If HS3Z>12 Then:DD 44,HS1B+48,1:Else:DD 44,0,1:End If
					If HS3Z>13 Then:DD 43,HS1A+37,1:Else:DD 43,0,1:End If
				End If
				If CurrentPlayer=0 And PlayersPlayingGame<>3 Then
					DD 15,0,1:DD 16,0,1:DD 17,0,1:DD 18,0,1:DD 19,0,1:DD 20,0,1:DD 21,0,1:DD 22,0,1:DD 23,2,1:DD 24,1,1:DD 25,12,1:DD 26,12,1:DD 27,0,1:DD 28,gsBallsPerGame+1-BallsRemaining(0)+37,1
				End If
				If CurrentPlayer=0 And PlayersPlayingGame=3 Then
					DD 43,0,1:DD 44,0,1:DD 45,0,1:DD 46,0,1:DD 47,0,1:DD 48,0,1:DD 49,0,1:DD 50,0,1:DD 51,2,1:DD 52,1,1:DD 53,12,1:DD 54,12,1:DD 55,0,1:DD 56,gsBallsPerGame+1-BallsRemaining(0)+37,1
				End If
				If CurrentPlayer=1 Then
					DD 43,0,1:DD 44,0,1:DD 45,0,1:DD 46,0,1:DD 47,0,1:DD 48,0,1:DD 49,0,1:DD 50,0,1:DD 51,2,1:DD 52,1,1:DD 53,12,1:DD 54,12,1:DD 55,0,1:DD 56,gsBallsPerGame+1-BallsRemaining(1)+37,1
				End If
				If CurrentPlayer=2 Then
					DD 1,0,1:DD 2,0,1:DD 3,0,1:DD 4,0,1:DD 5,0,1:DD 6,0,1:DD 7,0,1:DD 8,0,1:DD 9,2,1:DD 10,1,1:DD 11,12,1:DD 12,12,1:DD 13,0,1:DD 14,gsBallsPerGame+1-BallsRemaining(2)+37,1
				End If
				If CurrentPlayer=3 Then
					DD 29,0,1:DD 30,0,1:DD 31,0,1:DD 32,0,1:DD 33,0,1:DD 34,0,1:DD 35,0,1:DD 36,0,1:DD 37,2,1:DD 38,1,1:DD 39,12,1:DD 40,12,1:DD 41,0,1:DD 42,gsBallsPerGame+1-BallsRemaining(3)+37,1
				End If
			End If
		End If
	End If
	If HurryUpActive=TRUE Or HurryUpSingleActive=TRUE Then DisplayPointsHurryUp 'while mini wizard mode is running, simply add points but don't draw the ball numbers

	DMD_DisplayScoreBoard
End Sub

Sub FlashInsertCoin
	If PlayersPlayingGame=1 Then
		If bBallInPlungerLane=FALSE And gvFirstBallEjected=TRUE Then
			If gsBallsPerGame+1-BallsRemaining(CurrentPlayer)=1 Then

			' some B2S changes ************************************************
                B2SStopFlash 29,42
			'	For X=504 To 755
			'		If BGDisplays(X).State=2 Then BGDisplays(X).State=1
			'	Next
				Exit Sub
			Else
                DD 29,56,0
			'	For X=504 To 1007
			'		BGDisplays(X).State=0
			'	Next
			End If
			' some B2S changes ************************************************

		End If
	Else
		Exit Sub
	End If
	If gsFreePlay=TRUE Then
		DD 29,0,1:DD 30,16,2:DD 31,18,2:DD 32,5,2:DD 33,19,2:DD 34,19,2:DD 35,0,1:DD 36,0,1:DD 37,19,2:DD 38,20,2:DD 39,1,2:DD 40,18,2:DD 41,20,2:DD 42,0,1
		DD 43,0,1:DD 44,6,1:DD 45,15,1:DD 46,18,1:DD 47,0,1:DD 48,39,1:DD 49,0,1:DD 50,16,1:DD 51,12,1:DD 52,1,1:DD 53,25,1:DD 54,5,1:DD 55,18,1:DD 56,0,1
	Else  ' playing with credits
		If gsCredits>0 Then
			DD 29,0,1:DD 30,16,2:DD 31,18,2:DD 32,5,2:DD 33,19,2:DD 34,19,2:DD 35,0,1:DD 36,0,1:DD 37,19,2:DD 38,20,2:DD 39,1,2:DD 40,18,2:DD 41,20,2:DD 42,0,1
			DD 43,0,1:DD 44,6,1:DD 45,15,1:DD 46,18,1:DD 47,0,1:DD 48,39,1:DD 49,0,1:DD 50,16,1:DD 51,12,1:DD 52,1,1:DD 53,25,1:DD 54,5,1:DD 55,18,1:DD 56,0,1
		Else
			DD 29,0,1:DD 30,9,2:DD 31,14,2:DD 32,19,2:DD 33,5,2:DD 34,18,2:DD 35,20,2:DD 36,0,1:DD 37,0,1:DD 38,3,2:DD 39,15,2:DD 40,9,2:DD 41,14,2:DD 42,0,1
			DD 43,0,1:DD 44,6,1:DD 45,15,1:DD 46,18,1:DD 47,0,1:DD 48,39,1:DD 49,0,1:DD 50,16,1:DD 51,12,1:DD 52,1,1:DD 53,25,1:DD 54,5,1:DD 55,18,1:DD 56,0,1
		End If
	End If
End Sub

Sub DisplayFlushQueue
	If PlayersPlayingGame>0 Then
		If CurrentPlayer=0 Or CurrentPlayer=2 Then
			For Q=1 To 28
				DD Q,0,0
			Next
		End If
		If CurrentPlayer=1 Or CurrentPlayer=3 Then
			For Q=29 To 56
				DD Q,0,0
			Next
		End If
	Else
		For Q=1 To 56
			DD Q,0,0
		Next
	End If
	ATimer.Enabled=0
	RandomCallout.Enabled=0
	BTimer.Enabled=0
	QTimer.Enabled=0
	DMD_ClearScene
End Sub

'****************************************************************************************************************************************************
'**********************************************  HIGH SCORE ENTRY ROUTINES  *************************************************************************
'****************************************************************************************************************************************************
Dim hsbModeActive
Dim hsEnteredName
Dim hsEnteredDigits(5)
Dim hsCurrentDigit
Dim hsValidLetters
Dim hsCurrentLetter
Dim hsLetterFlash
Dim DCA
DCA=0

Sub HighScoreEntryInit
	hsbModeActive=TRUE
	hsLetterFlash=0
	hsEnteredDigits(0)=" "  ' blank out the name
	hsEnteredDigits(1)=" "
	hsEnteredDigits(2)=" "
	hsEnteredDigits(3)=" "
	hsEnteredDigits(4)=" "
	hsCurrentDigit=0      ' start with first digit
	' define the valid characters allowed in the high score name
	hsValidLetters=" ABCDEFGHIJKLMNOPQRSTUVWXYZ'<>*+-/=\^0123456789`"   ' ` is back arrow
	' letter display starts with the first character (space)
	hsCurrentLetter=1
	' flush any other displays queued up and call the display name function
	DisplayFlushQueue
	DCA=0
	DCycle.Enabled=0
	DCycle.Enabled=1

	If Score(CurrentPlayer)>=gsHighScore(3) Then
		DMD_SetScoreboardBackground "highscore.png"
	ElseIf CombosThisGame(CurrentPlayer)>=gvCombosforComboChamp Then
		DMD_SetScoreboardBackground "combochamp.png"
	ElseIf CheckTrackScoresVal Then
		Select Case Z4
			Case 1:DMD_SetScoreboardBackground "spinnerchamp.png"
			Case 2:DMD_SetScoreboardBackground "albumschamp.png"
			Case 3:DMD_SetScoreboardBackground "bumperschamp.png"
			Case 4:DMD_SetScoreboardBackground "rampschamp.png"
			Case 5:DMD_SetScoreboardBackground "bandchamp.png"
			Case 6:DMD_SetScoreboardBackground "standupschamp.png"
			Case 7:DMD_SetScoreboardBackground "orbitschamp.png"
			Case 8:DMD_SetScoreboardBackground "loopschamp.png"
			Case 9:DMD_SetScoreboardBackground "comboschamp.png"
			Case 10:DMD_SetScoreboardBackground "minischamp.png"
			Case 11:DMD_SetScoreboardBackground "followchamp.png"
			Case 12:DMD_SetScoreboardBackground "frenzychamp.png"
		End Select
	End If

	DCycle_Timer
	HighScoreDisplayNameNow
	' Set the flash rate/update rate of the high score entry
	Timer_HighScoreFlash.Interval=250
	Timer_HighScoreFlash.Enabled=TRUE
	Timer_HighScoreTimeout.Interval=20000     ' 20 seconds
	Timer_HighScoreTimeout.Enabled=TRUE
End Sub

Sub DCycle_Timer
	DCA=DCA+20
	Dim XYZ
	Select Case CurrentPlayer
		Case 0:XYZ=0
		Case 1:XYZ=28
		Case 2:XYZ=14
		Case 3:XYZ=42
	End Select
	' are we entering for a HS or a Combo Champ ?
	If Score(CurrentPlayer)>=gsHighScore(3) Then
		' these displays will just cycle over and over
		Select Case DCA
			Case 20:DisplayText3"  HIGH SCORE  ",1,1
			Case 1140:DisplayPoints(Score(CurrentPlayer))
						DD XYZ+14,HS1N+37,1
						If HS3Z>1 Then:DD XYZ+13,HS1M+37,1:Else:DD XYZ+13,0,1:End If
						If HS3Z>2 Then:DD XYZ+12,HS1L+37,1:Else:DD XYZ+12,0,1:End If
						If HS3Z>3 Then:DD XYZ+11,HS1K+48,1:Else:DD XYZ+11,0,1:End If
						If HS3Z>4 Then:DD XYZ+10,HS1J+37,1:Else:DD XYZ+10,0,1:End If
						If HS3Z>5 Then:DD XYZ+9,HS1I+37,1:Else:DD XYZ+9,0,1:End If
						If HS3Z>6 Then:DD XYZ+8,HS1H+48,1:Else:DD XYZ+8,0,1:End If
						If HS3Z>7 Then:DD XYZ+7,HS1G+37,1:Else:DD XYZ+7,0,1:End If
						If HS3Z>8 Then:DD XYZ+6,HS1F+37,1:Else:DD XYZ+6,0,1:End If
						If HS3Z>9 Then:DD XYZ+5,HS1E+48,1:Else:DD XYZ+5,0,1:End If
						If HS3Z>10 Then:DD XYZ+4,HS1D+37,1:Else:DD XYZ+4,0,1:End If
						If HS3Z>11 Then:DD XYZ+3,HS1C+37,1:Else:DD XYZ+3,0,1:End If
						If HS3Z>12 Then:DD XYZ+2,HS1B+48,1:Else:DD XYZ+2,0,1:End If
						If HS3Z>13 Then:DD XYZ+1,HS1A+37,1:Else:DD XYZ+1,0,1:End If
			Case 2260:DCA=0
		End Select
	Else
		If CombosThisGame(CurrentPlayer)>=gvCombosforComboChamp Then
			Select Case DCA
				Case 20:DisplayText3"COMBO CHAMPION",1,1
				Case 1140:DisplayText3"      1 COMBOS",1,1
							If gvCombosforComboChamp<10 Then DD XYZ+7,gvCombosforComboChamp+37,1
							If gvCombosforComboChamp>9 And gvCombosforComboChamp<100 Then:DD XYZ+7,INT(gvCombosforComboChamp/10)+37,1:DD XYZ+6,(gvCombosforComboChamp-INT(gvCombosforComboChamp/10)*10)+37,1:End If
							If gvCombosforComboChamp>99 And gvCombosforComboChamp<1000 Then
								AQWE=gvCombosforComboChamp
								AQ=INT(AQWE/100)
								AQWE=AQWE-AQ*100
								AQW=INT(AQWE/10)
								AQWE=AQWE-AQW*10
								DD XYZ+5,AQ+37,1
								DD XYZ+6,AQW+37,1
								DD XYZ+7,AQWE+37,1
							End If
				Case 2260:DCA=0
			End Select
		Else
			If CheckTrackScoresVal Then
				Select Case DCA
					Case 20:Select Case Z4
								Case 1:DisplayText3"SPINNER  CHAMP",1,1
								Case 2:DisplayText3"ALBUMS   CHAMP",1,1
								Case 3:DisplayText3"BUMPERS  CHAMP",1,1
								Case 4:DisplayText3"RAMPS    CHAMP",1,1
								Case 5:DisplayText3"BAND     CHAMP",1,1
								Case 6:DisplayText3"STANDUPS CHAMP",1,1
								Case 7:DisplayText3"ORBITS   CHAMP",1,1
								Case 8:DisplayText3"LOOPS    CHAMP",1,1
								Case 9:DisplayText3"COMBOS   CHAMP",1,1
								Case 10:DisplayText3"MINIS    CHAMP",1,1
								Case 11:DisplayText3"FOLLOW   CHAMP",1,1
								Case 12:DisplayText3"FRENZY   CHAMP",1,1
							End Select
					Case 1140:DisplayPoints(MusicScore(CurrentPlayer,Z4))
								DD XYZ+14,HS1N+37,1
								If HS3Z>1 Then:DD XYZ+13,HS1M+37,1:Else:DD XYZ+13,0,1:End If
								If HS3Z>2 Then:DD XYZ+12,HS1L+37,1:Else:DD XYZ+12,0,1:End If
								If HS3Z>3 Then:DD XYZ+11,HS1K+48,1:Else:DD XYZ+11,0,1:End If
								If HS3Z>4 Then:DD XYZ+10,HS1J+37,1:Else:DD XYZ+10,0,1:End If
								If HS3Z>5 Then:DD XYZ+9,HS1I+37,1:Else:DD XYZ+9,0,1:End If
								If HS3Z>6 Then:DD XYZ+8,HS1H+48,1:Else:DD XYZ+8,0,1:End If
								If HS3Z>7 Then:DD XYZ+7,HS1G+37,1:Else:DD XYZ+7,0,1:End If
								If HS3Z>8 Then:DD XYZ+6,HS1F+37,1:Else:DD XYZ+6,0,1:End If
								If HS3Z>9 Then:DD XYZ+5,HS1E+48,1:Else:DD XYZ+5,0,1:End If
								If HS3Z>10 Then:DD XYZ+4,HS1D+37,1:Else:DD XYZ+4,0,1:End If
								If HS3Z>11 Then:DD XYZ+3,HS1C+37,1:Else:DD XYZ+3,0,1:End If
								If HS3Z>12 Then:DD XYZ+2,HS1B+48,1:Else:DD XYZ+2,0,1:End If
								If HS3Z>13 Then:DD XYZ+1,HS1A+37,1:Else:DD XYZ+1,0,1:End If
					Case 2260:DCA=0
				End Select
			End If
		End If
	End If
End Sub

'handle the high score entry keys
Sub HighScoreProcessKeyDown(KeyCode)
	' previous letter
	If KeyCode=LeftFlipperKey Then
		PlaySound"ZChange"
		hsCurrentLetter=hsCurrentLetter-1
		If hsCurrentLetter=0 Then hsCurrentLetter=LEN(hsValidLetters)
		HighScoreDisplayNameNow
		RightFlipTimer.Enabled=0
		RightFlipTimerFaster.Enabled=0
		LeftFlipTimer.Enabled=0
		LeftFlipTimerFaster.Enabled=0
		LeftFlipTimer.Enabled=1
	End If
	' next letter
	If KeyCode=RightFlipperKey Then
		PlaySound"ZChange"
		hsCurrentLetter=hsCurrentLetter+1
		If hsCurrentLetter>LEN(hsValidLetters) Then hsCurrentLetter=1
		HighScoreDisplayNameNow
		LeftFlipTimer.Enabled=0
		LeftFlipTimerFaster.Enabled=0
		RightFlipTimer.Enabled=0
		RightFlipTimerFaster.Enabled=0
		RightFlipTimer.Enabled=1
	End If
	' confirm letter
	If KeyCode=StartGameKey Or KeyCode=PlungerKey Then
		LeftFlipTimer.Enabled=0
		LeftFlipTimerFaster.Enabled=0
		RightFlipTimer.Enabled=0
		RightFlipTimerFaster.Enabled=0
		' If not the backarrow Then commit the letter
		If mid(hsValidLetters,hsCurrentLetter,1)<>"`" Then
			PlaySound"ZWilliamsEnter"
			hsEnteredDigits(hsCurrentDigit)=mid(hsValidLetters,hsCurrentLetter,1)
			hsCurrentDigit=hsCurrentDigit+1
			' If that was the last digit, Then commit the name
			If hsCurrentDigit=5 Then
				PlaySound"AHSComplete"
				DOF 241, DOFPulse
				HighScoreCommitName
			Else
				HighScoreDisplayNameNow
			End If
		Else
			PlaySound"ZWilliamsPrevious"
			hsEnteredDigits(hsCurrentDigit)=" "
			If hsCurrentDigit>0 Then hsCurrentDigit=hsCurrentDigit-1
			HighScoreDisplayNameNow
		End If
	End If
End Sub

Sub HighScoreProcessKeyUp(KeyCode)
	' previous letter
	If KeyCode=LeftFlipperKey Then
		LeftFlipTimer.Enabled=0
		LeftFlipTimerFaster.Enabled=0
	End If
	' next letter
	If KeyCode=RightFlipperKey Then
		RightFlipTimer.Enabled=0
		RightFlipTimerFaster.Enabled=0
	End If
End Sub

Sub LeftFlipTimer_Timer:LeftFlipTimer.Enabled=0:LeftFlipTimerFaster.Enabled=1:LeftFlipTimerFaster_Timer:End Sub
Sub RightFlipTimer_Timer:RightFlipTimer.Enabled=0:RightFlipTimerFaster.Enabled=1:RightFlipTimerFaster_Timer:End Sub

Sub LeftFlipTimerFaster_Timer
	PlaySound"ZChange"
	hsCurrentLetter=hsCurrentLetter-1
	If hsCurrentLetter=0 Then hsCurrentLetter=LEN(hsValidLetters)
	HighScoreDisplayNameNow
End Sub

Sub RightFlipTimerFaster_Timer
	PlaySound"ZChange"
	hsCurrentLetter=hsCurrentLetter+1
	If hsCurrentLetter>LEN(hsValidLetters) Then hsCurrentLetter=1
	HighScoreDisplayNameNow
End Sub

'display the currently entered initals right now with no flashing letter
Sub HighScoreDisplayNameNow
	' turn off the timers
	Timer_HighScoreFlash.Enabled=0
	Timer_HighScoreTimeout.Enabled=0
	' do an update now (by setting hsLetterFlash to 0 it ensures that the letter is
	' displayed and not the flash character "_"
	hsLetterFlash=0
	HighScoreDisplayName
	' reset the timer (also resets the interval)
	Timer_HighScoreFlash.Enabled=1
	' reset the timout timer
	Timer_HighScoreTimeout.Enabled=1
End Sub

'Display the currently entered name on the screen, with the current letter flashing
Sub HighScoreDisplayName
	Dim i
	Dim TempBotStr
	' now do the bottom line (a little more complex)
	TempBotStr="  > "
	' If the first digit has been entered then display that
	If hsCurrentDigit>0 Then TempBotStr=TempBotStr&hsEnteredDigits(0)
	' dito for second digit
	If hsCurrentDigit>1 Then TempBotStr=TempBotStr&hsEnteredDigits(1)
	' and last third digit
	If hsCurrentDigit>2 Then TempBotStr=TempBotStr&hsEnteredDigits(2)
	' and last forth digit
	If hsCurrentDigit>3 Then TempBotStr=TempBotStr&hsEnteredDigits(3)
	' and last digit
	If hsCurrentDigit>4 Then TempBotStr=TempBotStr&hsEnteredDigits(4)
	' If not all the digits entered then make the current one flash
	If hsCurrentDigit<>5 Then
		If hsLetterFlash<>0 Then
			TempBotStr=TempBotStr&"_"
		Else
			TempBotStr=TempBotStr&mid(hsValidLetters,hsCurrentLetter,1)
		End If
	End If
	' pad out the rest of the string If not on the last letter
	If hsCurrentDigit<1 Then TempBotStr=TempBotStr&hsEnteredDigits(1)
	If hsCurrentDigit<2 Then TempBotStr=TempBotStr&hsEnteredDigits(2)
	If hsCurrentDigit<3 Then TempBotStr=TempBotStr&hsEnteredDigits(3)
	If hsCurrentDigit<4 Then TempBotStr=TempBotStr&hsEnteredDigits(4)
	TempBotStr=TempBotStr&" <   "
	DisplayText3 TempBotStr,2,1:DMD_DisplayHighScore TempBotStr
End Sub

'the timer which refreshes the high score entry screen has expired, refresh the display and start again
Sub Timer_HighScoreFlash_Timer
	Timer_HighScoreFlash.Enabled=FALSE
	hsLetterFlash=hsLetterFlash+1
	If hsLetterFlash=2 Then hsLetterFlash=0
	HighScoreDisplayName
	Timer_HighScoreFlash.Enabled=TRUE
End Sub

'the governing timeout timer has expired, commit what ever the current player has entered
Sub Timer_HighScoreTimeout_Timer:Timer_HighScoreTimeout.Enabled=FALSE:HighScoreCommitName:End Sub

'The High Score name has been entered
Sub HighScoreCommitName
	' turn off the timers
	DCycle.Enabled=0
	Timer_HighScoreFlash.Enabled=FALSE
	Timer_HighScoreTimeout.Enabled=FALSE
	' turn of the mode flasg
	hsbModeActive=FALSE
	' save the entered name
	hsEnteredName=hsEnteredDigits(0)&hsEnteredDigits(1)&hsEnteredDigits(2)&hsEnteredDigits(3)&hsEnteredDigits(4)
	If hsEnteredName="     " Then hsEnteredName="DISCO"
	' have we made the top score
	If Score(CurrentPlayer)>=gsHighScore(0) Then
		' move the scores down
		gsHighScore(3)=gsHighScore(2)
		gsHighScoreName(3)=gsHighScoreName(2)
		gsHighScore(2)=gsHighScore(1)
		gsHighScoreName(2)=gsHighScoreName(1)
		gsHighScore(1)=gsHighScore(0)
		gsHighScoreName(1)=gsHighScoreName(0)
		' put in the new one
		gsHighScore(0)=Score(CurrentPlayer)
		gsHighScoreName(0)=hsEnteredName
		' award 3 credits if not in free play
		If gsFreePlay=FALSE Then AwardFreeGame 3
	Else
		If Score(CurrentPlayer)>=gsHighScore(1) Then
			' move the scores down
			gsHighScore(3)=gsHighScore(2)
			gsHighScoreName(3)=gsHighScoreName(2)
			gsHighScore(2)=gsHighScore(1)
			gsHighScoreName(2)=gsHighScoreName(1)
			' put in the new one
			gsHighScore(1)=Score(CurrentPlayer)
			gsHighScoreName(1)=hsEnteredName
		Else
			If Score(CurrentPlayer)>=gsHighScore(2) Then
				' move the scores down
				gsHighScore(3)=gsHighScore(2)
				gsHighScoreName(3)=gsHighScoreName(2)
				' put in the new one
				gsHighScore(2)=Score(CurrentPlayer)
				gsHighScoreName(2)=hsEnteredName
			Else
				If Score(CurrentPlayer)>=gsHighScore(3) Then
					' put in the new one
					gsHighScore(3)=Score(CurrentPlayer)
					gsHighScoreName(3)=hsEnteredName
				End If
			End If
		End If
	End If
	' did the player make a high score and/or combo champ
	If CombosThisGame(CurrentPlayer)>=gvCombosforComboChamp Then
		gvCombosforComboChamp=CombosThisGame(CurrentPlayer)
		gsComboChampName=hsEnteredName
		' award an extra credit If not in free play
		If gsFreePlay=FALSE Then AwardFreeGame 1
	End If
	For X=1 To 12
		If MusicScore(CurrentPlayer,X)>=gsTrackScore(X) Then
			gsTrackScore(X)=MusicScore(CurrentPlayer,X)
			gsTrackScoreName(X)=hsEnteredName
			If gsFreePlay=FALSE Then AwardFreeGame 1
		End If
	Next
	' complete the end of ball sequence
	StopSound"AHSMusic"
	DMD_SetScoreboardBackground ""
	DoEndOfBallComplete
End Sub

'****************************************************************************************************************************************************
'**************************************************  OPERATOR MENU AND ADJUSTMENTS  *****************************************************************
'****************************************************************************************************************************************************
Sub OpMenuInit
	bMenuModeActive=TRUE
	' play the enter sound
	PlaySound"ZWilliamsEnter"
	' MS displays the rom version
	For Q=1 To 56:DD Q,0,1:Next
	ATimer.Enabled=0
	RandomCallout.Enabled=0
	LightSeq1.StopPlay
	CurrentPlayer=0
	DisplayText3 DisplayName,1,1
	DisplayText3" ROM VER  L-1 ",2,1:DMD_DisplaySceneText DisplayName, " ROM VER  L-1 "
	EnterMenuTimer.Enabled=1
End Sub

Sub EnterMenuTimer_Timer
	EnterMenuTimer.Enabled=0
	' start at the beginning (top level menu, first item)
	For Q=1 To 56:DD Q,0,1:Next
	opMenuIndex=0
	opMenuDepth=0
	opSubMenuIndex=0
	' Set the current menu display
	OpMenuSetupDisplays
End Sub

Sub OpMenuProcessKey(KeyCode)
	' operator escape key
	If KeyCode=constEscKey Then
		PlaySound"ZWilliamsPrevious"
		' If at the root then exit
		If opMenuDepth=0 Then
			bMenuModeActive=FALSE
			' reset the table
			EndOfGame
			DisplayFlushQueue
			LightSeq1.UpdateInterval=10
			LightSeq1.Play SeqUpOn,0,1
			LightSeq1.Play SeqDownOff,0,1,500
			LightSeq1_PlayDone'start attract mode light sequencer on a loop
			bDoorOpen=FALSE'coin door is closed
			StartAttractModeDisplays
		Else
			If opMenuDepth=1 Then
				opMenuDepth=0   ' back to root
			Else
				' in the lowest level (ie modifying values)
				' esc resets to default
				If opMenuIndex=0 Then
					Select Case opSubMenuIndex
						Case 0:gsBallsPerGame=3
						Case 1:gsGameDifficulty=1
						Case 2:gsNotesForSongSelect=5
						Case 3:gsReplayStart=50000000
						Case 4:gsReplayLevels=1
						Case 5:gsMatchPer=10
						Case 6:gsTiltWarnings=3
						Case 7:gsFreePlay=FALSE
					End Select
				End If
				opMenuDepth=1
			End If
			OpMenuSetupDisplays
		End If
	End If
	' operator previous key
	If KeyCode=LeftFlipperKey Then
		' If at the root Then
		If opMenuDepth=0 Then
			opMenuIndex=opMenuIndex-1
			' check the bounds of the menu
			If opMenuIndex<0 Then opMenuIndex=1     ' only 2 items at root
		Else
			If opMenuDepth=1 Then
				opSubMenuIndex=opSubMenuIndex-1
				' adjustments
				If opMenuIndex=0 Then
					If opSubMenuIndex<0 Then opSubMenuIndex=7      ' 8 items in utils menu
				Else
					' utilities
					If opSubMenuIndex<0 Then opSubMenuIndex=1      ' 2 items in utils menu
				End If
			Else
				' at the lowest level (ie change data)
				OpMenuAdjustSubItems(KeyCode)
			End If
		End If
		PlaySound"ZWilliamsPrevious"
		OpMenuSetupDisplays
		If opMenuDepth=2 Then

			' some B2S changes ************************************************
            For i=15 to 28: B2SFlashBuff(i,1)=1: Next
			'For X=324 To 502
			'	If BGDisplays(X).State=1 Then BGDisplays(X).State=2
			'Next
            For i=43 to 56: B2SFlashBuff(i,1)=1: Next
			'For X=830 To 1007
			'	If BGDisplays(X).State=1 Then BGDisplays(X).State=2
			'Next
			' some B2S changes ************************************************

		End If
	End If
	' operator next key
	If KeyCode=RightFlipperKey Then
		' If at the root Then
		If opMenuDepth=0 Then
			opMenuIndex=opMenuIndex+1
			' check the bounds of the menu
			If opMenuIndex>1 Then opMenuIndex=0     ' only 2 items at root
		Else
			If opMenuDepth=1 Then
				opSubMenuIndex=opSubMenuIndex+1
				' adjustments
				If opMenuIndex=0 Then
					If opSubMenuIndex>7 Then opSubMenuIndex=0      ' 8 items in utils menu
				Else
					' utilities
					If opSubMenuIndex>1 Then opSubMenuIndex=0      ' 2 items in utils menu
				End If
			Else
				' at the lowest level (ie change data)
				OpMenuAdjustSubItems(KeyCode)
			End If
		End If
		PlaySound"ZWilliamsNext"
		OpMenuSetupDisplays
		If opMenuDepth=2 Then

			' some B2S changes ************************************************
            For i=15 to 28: B2SFlashBuff(i,1)=1: Next
			'For X=324 To 502
			'	If BGDisplays(X).State=1 Then BGDisplays(X).State=2
			'Next
            For i=43 to 56: B2SFlashBuff(i,1)=1: Next
			'For X=830 To 1007
			'	If BGDisplays(X).State=1 Then BGDisplays(X).State=2
			'Next
			' some B2S changes ************************************************

		End If
	End If
	' operator enter key
	If KeyCode=StartGameKey Then
		If opMenuDepth=0 Then
			If EnterMenuTimer.Enabled Then
				For Q=1 To 56:DD Q,0,1:Next
				EnterMenuTimer.Enabled=0
				opMenuIndex=0
				opMenuDepth=0
				opSubMenuIndex=0
			Else
				opMenuDepth=1
			End If
			opSubMenuIndex=0
			OpMenuSetupDisplays
		Else
			If opMenuDepth=1 Then
				' If on adjustments, Then go down lower where we can actually adjust the values
				If opMenuIndex=0 Then
					opMenuDepth=2
					OpMenuSetupDisplays

					' some B2S changes ************************************************
                    For i=15 to 28: B2SFlashBuff(i,1)=1: Next
					'For X=324 To 502
					'	If BGDisplays(X).State=1 Then BGDisplays(X).State=2
					'Next
                    For i=43 to 56: B2SFlashBuff(i,1)=1: Next
					'For X=830 To 1007
					'	If BGDisplays(X).State=1 Then BGDisplays(X).State=2
					'Next
					' some B2S changes ************************************************

				Else
					' utilities
					Select Case (opSubMenuIndex)
						Case 0:gsCredits=0
								DisplayText3" ALL CREDITS  ",1,1:DMD_DisplaySceneText "ALL CREDITS", "CLEARED"
						Case 1:ResetHSTD
								DisplayText3" HIGH SCORES  ",1,1:DMD_DisplaySceneText "HIGH SCORES", "CLEARED"
					End Select
					DisplayText3"   CLEARED    ",2,1
				End If
			Else  ' depth=2
				' go back to selection menu
				opMenuDepth=1
				OpMenuSetupDisplays
			End If
		End If
		PlaySound"ZWilliamsEnter"
	End If
End Sub

Sub OpMenuAdjustSubItems(KeyCode)
	Select Case opSubMenuIndex
		Case 0:If KeyCode=LeftFlipperKey Then
					gsBallsPerGame=gsBallsPerGame-1
					If gsBallsPerGame<1 Then gsBallsPerGame=9   ' must have one ball
				Else
					gsBallsPerGame=gsBallsPerGame+1
					If gsBallsPerGame>9 Then gsBallsPerGame=1   ' and a maximum of 9
				End If
		Case 1:If KeyCode=LeftFlipperKey Then
					gsGameDifficulty=gsGameDifficulty-1
					If gsGameDifficulty<0 Then gsGameDifficulty=2
				Else
					gsGameDifficulty=gsGameDifficulty+1
					If gsGameDifficulty>2 Then gsGameDifficulty=0
				End If
		Case 2:If KeyCode=LeftFlipperKey Then
					gsNotesForSongSelect=gsNotesForSongSelect-1
					If gsNotesForSongSelect<1 Then gsNotesForSongSelect=7
				Else
					gsNotesForSongSelect=gsNotesForSongSelect+1
					If gsNotesForSongSelect>7 Then gsNotesForSongSelect=1
				End If
		Case 3:If KeyCode=LeftFlipperKey Then
					gsReplayStart=gsReplayStart-500000
					If gsReplayStart<10000000 Then gsReplayStart=90000000
				Else
					gsReplayStart=gsReplayStart+500000
					If gsReplayStart>90000000 Then gsReplayStart=10000000
				End If
		Case 4:If KeyCode=LeftFlipperKey Then
					gsReplayLevels=gsReplayLevels-1
					If gsReplayLevels<1 Then gsReplayLevels=4
				Else
					gsReplayLevels=gsReplayLevels+1
					If gsReplayLevels>4 Then gsReplayLevels=1
				End If
		Case 5:If KeyCode=LeftFlipperKey Then
					gsMatchPer=gsMatchPer-2
					If gsMatchPer<0 Then gsMatchPer=50
				Else
					gsMatchPer=gsMatchPer+2
					If gsMatchPer>50 Then gsMatchPer=0
				End If
		Case 6:If KeyCode=LeftFlipperKey Then
					gsTiltWarnings=gsTiltWarnings-1
					If gsTiltWarnings<1 Then gsTiltWarnings=9
				Else
					gsTiltWarnings=gsTiltWarnings+1
					If gsTiltWarnings>9 Then gsTiltWarnings=1
				End If
		Case 7:If gsFreePlay=FALSE Then:gsFreePlay=TRUE:Else:gsFreePlay=FALSE:End If
	End Select
End Sub

'this function builds up the menu displays depending on what menu/submenu it is in.
Sub OpMenuSetupDisplays
	Dim TopText
	Dim BottomText
	If opMenuDepth=0 Then
		DisplayText3"MAIN MENU     ",1,1
		Select Case opMenuIndex
			Case 0:DisplayText3"A  ADJUSTMENTS",2,1:DMD_DisplaySceneText "MAIN MENU", "A  ADJUSTMENTS"
			Case 1:DisplayText3"U  UTILITIES  ",2,1:DMD_DisplaySceneText "MAIN MENU", "U  UTILITIES  "
		End Select
		'If CurrentPlayer=0 Or CurrentPlayer=2 Then:Light269.State=1:Else:Light773.State=1:End If
	Else  ' If (MenuDepth=1 or 2)
		' what parent menu are we in ?
		If opMenuIndex=0 Then
			Select Case opSubMenuIndex
				Case 0:DisplayText3"BALLS PER GAME",1,1
							ComH="A1           "&gsBallsPerGame:DisplayText3 ComH,2,1:DMD_DisplaySceneText "BALLS PER GAME", ComH
							'If CurrentPlayer=0 Or CurrentPlayer=2 Then:Light287.State=1:Else:Light791.State=1:End If
				Case 1:DisplayText3"DIFFICULTY    ",1,1
							Select Case gsGameDifficulty
								Case 0:DisplayText3"A2        EASY",2,1:DMD_DisplaySceneText "DIFFICULTY", "A2        EASY"
								Case 1:DisplayText3"A2     FACTORY",2,1:DMD_DisplaySceneText "DIFFICULTY", "A2     FACTORY"
								Case 2:DisplayText3"A2  TOURNAMENT",2,1:DMD_DisplaySceneText "DIFFICULTY", "A2  TOURNAMENT"
							End Select
							'If CurrentPlayer=0 Or CurrentPlayer=2 Then:Light287.State=1:Else:Light791.State=1:End If
				Case 2:DisplayText3"NOTE SELECTION",1,1
							ComH="A3           "&gsNotesForSongSelect:DisplayText3 ComH,2,1:DMD_DisplaySceneText "NOTE SELECTION", ComH
							'If CurrentPlayer=0 Or CurrentPlayer=2 Then:Light287.State=1:Else:Light791.State=1:End If
				Case 3:DisplayText3"REPLAY START  ",1,1
							ComH="A4    "&gsReplayStart:DisplayText3 ComH,2,1:DMD_DisplaySceneText "REPLAY START", ComH
							If CurrentPlayer=0 Or CurrentPlayer=2 Then
							'	Light287.State=1
							'	Light449.State=1:Light450.State=1:Light395.State=1:Light396.State=1
							Else
							'	Light791.State=1
							'	Light953.State=1:Light954.State=1:Light899.State=1:Light900.State=1
							End If
				Case 4:DisplayText3"REPLAY LEVELS ",1,1
							ComH="A5           "&gsReplayLevels:DisplayText3 ComH,2,1:DMD_DisplaySceneText "REPLAY LEVELS", ComH
							'If CurrentPlayer=0 Or CurrentPlayer=2 Then:Light287.State=1:Else:Light791.State=1:End If
				Case 5:DisplayText3"MATCH PERCENT ",1,1
							If gsMatchPer=0 Then DisplayText3"A6         OFF",2,1:DMD_DisplaySceneText "MATCH PERCENT", "A6         OFF"
							If gsMatchPer>0 And gsMatchPer<10 Then
								ComH="A6       0"&gsMatchPer&"   ":DisplayText3 ComH,2,1:DMD_DisplaySceneText "MATCH PERCENT", "A6          0"&gsMatchPer
								DD 26,70,1:DD 27,33,1:DD 28,71,1
							End If
							If gsMatchPer>8 Then
								ComH="A6       "&gsMatchPer&"   ":DisplayText3 ComH,2,1:DMD_DisplaySceneText "MATCH PERCENT", "A6          "&gsMatchPer
								DD 26,70,1:DD 27,33,1:DD 28,71,1
							End If
							'If CurrentPlayer=0 Or CurrentPlayer=2 Then:Light287.State=1:Else:Light791.State=1:End If
				Case 6:DisplayText3"TILT WARNINGS ",1,1
							ComH="A7           "&gsTiltWarnings:DisplayText3 ComH,2,1:DMD_DisplaySceneText "TILT WARNINGS", ComH
							'If CurrentPlayer=0 Or CurrentPlayer=2 Then:Light287.State=1:Else:Light791.State=1:End If
				Case 7:DisplayText3"FREE PLAY     ",1,1
							Select Case gsFreePlay
								Case TRUE:DisplayText3"A8         YES",2,1:DMD_DisplaySceneText "FREE PLAY", "A8         YES"
								Case FALSE:DisplayText3"A8          NO",2,1:DMD_DisplaySceneText "FREE PLAY", "A8          NO"
							End Select
							'If CurrentPlayer=0 Or CurrentPlayer=2 Then:Light287.State=1:Else:Light791.State=1:End If
			End Select
		Else
			DisplayText3"UTILITIES     ",1,1
			Select Case opSubMenuIndex
				Case 0:DisplayText3"1 CLEAR CREDIT",2,1:DMD_DisplaySceneText "UTILITIES", "1 CLEAR CREDIT"
							'If CurrentPlayer=0 Or CurrentPlayer=2 Then:Light269.State=1:Else:Light773.State=1:End If
				Case 1:DisplayText3"2 RESET SCORES",2,1:DMD_DisplaySceneText "UTILITIES", "2 RESET SCORES"
							If CurrentPlayer=0 Or CurrentPlayer=2 Then
							'	Light269.State=1:Light413.State=1:Light431.State=1:Light449.State=1:Light467.State=1
							Else
							'	Light773.State=1:Light917.State=1:Light935.State=1:Light953.State=1:Light971.State=1
							End If
			End Select
		End If
	End If
End Sub

Dim D(56)


' new B2S stuff ************************************************

Dim B2SFlashBuff(56,2),B2SF1
Dim B2SSEG,B2SFlash
B2SSEG=Array(0,2167,10831,57,8719,2169,2161,2109,2166,8713,30,5232,56,1334,4406,63,2163,4159,6259,2157,8705,62,17456,20534,21760,9472,17417,1024,5120,16640,32576,10816,2112,17408,2120,4352,20480,63,6,2139,2127,2150,2157,2173,7,2175,2151,7168,32959,32902,35035,35023,35046,35053,35069,32903,35071,35047,4957,8713,8713,3438,5192,4680,1,2121,2121,1,49,8753,2563,8280,8759,26636,585,11609,12376,2569,11597,128,2295,10959,185,8847,2297,2289,2237,2294,8841,158,5360,184,1462,4534,191,2291,4287,6387,2285,8833,190,17584,20662,21888,9600,17545,191,134,2267,2255,2278,2285,2301,135,2303,2279,8,32639,1024,0)
For B2SF1=1 to 56:B2SFlashBuff(B2SF1,1)=0:B2SFlashBuff(B2SF1,2)=0:Next

Sub BallRelease_timer()
   'Flashing Text timer
    B2SFlash=Abs(B2SFlash-1)
	If B2SOn Then
    For B2SF1=1 to 56
       If B2SFlashBuff(B2SF1,1)=1 Then
          If B2SFlash=1 then
             Controller.B2SSetLED B2SF1,B2SFlashBuff(B2SF1,2)
          Else
             Controller.B2SSetLED B2SF1,0
          End If
       End If
    Next
	End If
End Sub

Sub B2SStopFlash (ByVal sta,ByVal sto)
   Dim i
   For i=sto to sto
      B2SFlashBuff(i,1)=0
	  If B2SOn Then Controller.B2SSetLED i,B2SFlashBuff(i,2) End If
   Next
End Sub

' *************************************************************


Sub DD(n,CharacterNumber,S)
	'Dig(1), Character list, On/Off/Blinking

	' new B2S stuff ************************************************

    If B2SOn Then Controller.B2SSetLED n,B2SSEG (CharacterNumber) End If 
    If S=2 then
        B2SFlashBuff(n,1)=1:B2SFlashBuff(n,2)=B2SSEG (CharacterNumber)
    ELSE
        B2SFlashBuff(n,1)=0:B2SFlashBuff(n,2)=B2SSEG (CharacterNumber)
    End If

	' **************************************************************


End Sub

'---------------------------------------------------------------------------------------------------------------------------------------------- 
'ATIMER=Attract Mode Timer
'BTIMER=Coin delay display when in attract mode

'1-- DMD Segment displays
'DispDC1 -- lit P1=32/32/255 ----- unlit P1=8/8/96
'DispDC2
'DispMS1 -- lit P2=255/0/0 ----- unlit P2=96/0/0
'DispMS2

'14 digits *2 per player = 17x27 vp units/digit   2 vp unit spacing between verical display sets

'56 total digits

'slow blink=160=timer interval
'fast blink=80=blink interval
'update interval=20=displaytimer

'displayflushqueue

'displaytext"string",1,1 2240 delay
'displaytext3"string",1,1 Instant

'-----------------
'doendofballdisplay



' Added by JPSalas
' Replaced LightGI1.State=0:LightGI2.State=0:LightGI3.State=0:LightGI4.State=0 with GIOFF
' and
' LightGI1.State=1:LightGI2.State=1:LightGI3.State=1:LightGI4.State=1 with GION

'Sub GION
'	LightGI1.State=1:LightGI2.State=1:LightGI3.State=1:LightGI4.State=1
'	GI1.State=1:GI2.State=1:GI3.State=1:GI4.State=1:GI5.State=1:GI6.State=1:GI7.State=1:GI8.State=1:GI9.State=1:GI10.State=1:GI11.State=1:GI12.State=1:GI13.State=1
'End Sub

'SUb GIOFF
'	LightGI1.State=0:LightGI2.State=0:LightGI3.State=0:LightGI4.State=0
'	GI1.State=0:GI2.State=0:GI3.State=0:GI4.State=0:GI5.State=0:GI6.State=0:GI7.State=0:GI8.State=0:GI9.State=0:GI10.State=0:GI11.State=0:GI12.State=0:GI13.State=0
'End Sub



' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX, Rothbauerw, Thalamus and Herweh
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

' set position as table object and Vol + RndPitch manually

Sub PlaySoundAtVolPitch(sound, tableobj, Vol, RndPitch)
  PlaySound sound, 1, Vol, AudioPan(tableobj), RndPitch, 0, 0, 1, AudioFade(tableobj)
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

Sub PlaySoundAtBallAbsVol(sound, VolMult)
  PlaySound sound, 0, VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

' requires rampbump1 to 7 in Sound Manager

Sub RandomBump(voladj, freq)
  Dim BumpSnd:BumpSnd= "rampbump" & CStr(Int(Rnd*7)+1)
  PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, AudioPan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

' set position as bumperX and Vol manually. Allows rapid repetition/overlaying sound

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
  tmp = tableobj.y * 2 / table1.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = tableobj.x * 2 / table1.width-1
  If tmp > 0 Then
    AudioPan = Csng(tmp ^10)
  Else
    AudioPan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = ball.x * 2 / table1.width-1
  If tmp > 0 Then
    Pan = Csng(tmp ^10)
  Else
    Pan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
  Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function VolMulti(ball,Multiplier) ' Calculates the Volume of the sound based on the ball speed
  VolMulti = Csng(BallVel(ball) ^2 / 150 ) * Multiplier
End Function

Function DVolMulti(ball,Multiplier) ' Calculates the Volume of the sound based on the ball speed
  DVolMulti = Csng(BallVel(ball) ^2 / 150 ) * Multiplier
  debug.print DVolMulti
End Function

Function BallRollVol(ball) ' Calculates the Volume of the sound based on the ball speed
  BallRollVol = Csng(BallVel(ball) ^2 / (80000 - (79900 * Log(RollVol) / Log(100))))
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


'**********************************************
'    Flipper adjustments - enable tricks
'             by JLouLouLou
'**********************************************

Dim FlipperPower
Dim FlipperElasticity
Dim EOSTorque, EOSAngle
Dim SOSTorque, SOSAngle
Dim FullStrokeEOS_Torque

Dim LLiveCatchTimer
Dim RLiveCatchTimer
Dim LiveCatchSensivity

FlipperPower = 5000
FlipperElasticity = 0.85
EOSTorque = 0.2
EOSAngle = 6
SOSTorque = 0.1
SOSAngle = 6
FullStrokeEOS_Torque = 0.5
LiveCatchSensivity = 25 'adjust as you prefer
LLiveCatchTimer = 0
RLiveCatchTimer = 0

Sub RealTimeFast_Timer 'flipper's tricks timer
    'Flipper Stroke Routine
    If LeftFlipper.CurrentAngle => LeftFlipper.StartAngle - SOSAngle Then LeftFlipper.Strength = FlipperPower * SOSTorque:End If                                                       'Start of Stroke for Tap pass and Tap shoot
    If LeftFlipper.CurrentAngle < LeftFlipper.StartAngle - SOSAngle and LeftFlipper.CurrentAngle > LeftFlipper.EndAngle + EOSAngle Then LeftFlipper.Strength = FlipperPower:End If      'Full Stroke
    If LeftFlipper.CurrentAngle < LeftFlipper.EndAngle + EOSAngle and LeftFlipper.CurrentAngle > LeftFlipper.EndAngle Then LeftFlipper.Strength = FlipperPower * EOSTorque:End If       'EOS Stroke
    If LeftFlipper.CurrentAngle = LeftFlipper.EndAngle Then LeftFlipper.Strength = FlipperPower * FullStrokeEOS_Torque:End If                                                           'Bunny Bump Remover

    If RightFlipper.CurrentAngle <= RightFlipper.StartAngle + SOSAngle Then RightFlipper.Strength = FlipperPower * SOSTorque:End If                                                     'Start of Stroke for Tap pass and Tap shoot
    If RightFlipper.CurrentAngle > RightFlipper.StartAngle + SOSAngle and RightFlipper.CurrentAngle < RightFlipper.EndAngle - EOSAngle Then RightFlipper.Strength = FlipperPower:End If 'Full Stroke
    If RightFlipper.CurrentAngle > RightFlipper.EndAngle - EOSAngle and RightFlipper.CurrentAngle < RightFlipper.EndAngle Then RightFlipper.Strength = FlipperPower * EOSTorque:End If  'EOS Stroke
    If RightFlipper.CurrentAngle = RightFlipper.EndAngle Then RightFlipper.Strength = FlipperPower * FullStrokeEOS_Torque:End If                                                        'Bunny Bump Remover

    'Live Catch Routine
    If LeftFlipper.CurrentAngle <= LeftFlipper.EndAngle + EOSAngle and LeftFlipper.CurrentAngle => LeftFlipper.EndAngle Then
        LLiveCatchTimer = LLiveCatchTimer + 1
        If LLiveCatchTimer < LiveCatchSensivity Then
            LeftFlipper.Elasticity = 0.2
        Else
            LeftFlipper.Elasticity = FlipperElasticity
            LLiveCatchTimer = LiveCatchSensivity
        End If
    Else
        LLiveCatchTimer = 0
    End If
    If RightFlipper.CurrentAngle => RightFlipper.EndAngle - EOSAngle and RightFlipper.CurrentAngle <= RightFlipper.EndAngle Then
        RLiveCatchTimer = RLiveCatchTimer + 1
        If RLiveCatchTimer < LiveCatchSensivity Then
            RightFlipper.Elasticity = 0.2
        Else
            RightFlipper.Elasticity = FlipperElasticity
            RLiveCatchTimer = LiveCatchSensivity
        End If
    Else
        RLiveCatchTimer = 0
    End If
End Sub

'********************************************************************
'      JP's VP10 Rolling Sounds (+rothbauerw's Dropping Sounds)
'********************************************************************

Const tnob = 5 ' total number of balls
Const maxvel = 50 'max ball velocity
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
        StopSound "fx_ballrolling" & b
        StopSound "fx_plasticrolling" & b
        StopSound "fx_metalrolling" & b
    Next

	' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
		If BallVel(BOT(b) ) > 1 Then
            rolling(b) = True
			If BOT(b).Z > 30 Then
				' ball on plastic ramp
				StopSound "fx_ballrolling" & b
				StopSound "fx_metalrolling" & b
				PlaySound "fx_plasticrolling" & b, -1, Vol(BOT(b)) / 2, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
			Else
				' ball on playfield
				StopSound "fx_plasticrolling" & b
				StopSound "fx_metalrolling" & b
				PlaySound "fx_ballrolling" & b, -1, Vol(BOT(b)) / 2, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
			End If
		Else
			If rolling(b) Then
                StopSound "fx_ballrolling" & b
				StopSound "fx_plasticrolling" & b
				StopSound "fx_metalrolling" & b
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

'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle

End Sub

'*****************************************
'	ninuzzu's	BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)

Sub BallShadowUpdate_timer()
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
        If BOT(b).X < Table1.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 6
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 6
        End If
        ballShadow(b).Y = BOT(b).Y + 12
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub


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


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
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
	PlaySound "gate", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub



'****************************************
' B2B Collision by Steely & Pinball Ken
' added destruk changes
'****************************************

Dim tnopb, nosf
'
tnopb = 10
nosf = 10

Dim currentball(10), ballStatus(10)
Dim iball, cnt, coff, errMessage

XYdata.interval = 1
coff = False

For cnt = 0 to ubound(ballStatus)
    ballStatus(cnt) = 0
Next

' Create ball in kicker and assign a Ball ID used mostly in non-vpm tables
Sub CreateBallID(Kickername)
    For cnt = 1 to ubound(ballStatus)
        If ballStatus(cnt) = 0 Then
            Set currentball(cnt) = Kickername.createball
            currentball(cnt).uservalue = cnt
            ballStatus(cnt) = 1
            ballStatus(0) = ballStatus(0) + 1
            If coff = False Then
                If ballStatus(0)> 1 and XYdata.enabled = False Then XYdata.enabled = True
            End If
            Exit For
        End If
    Next
End Sub

Sub ClearBallID
    On Error Resume Next
    iball = ActiveBall.uservalue
    currentball(iball).UserValue = 0
    If Err Then Msgbox Err.description & vbCrLf & iball
    ballStatus(iBall) = 0
    ballStatus(0) = ballStatus(0) -1
    On Error Goto 0
End Sub

' Ball data collection and B2B Collision detection
ReDim baX(tnopb, 4), baY(tnopb, 4), bVx(tnopb, 4), bVy(tnopb, 4), TotalVel(tnopb, 4)
Dim cForce, bDistance, xyTime, cFactor, id, id2, id3, B1, B2

Sub XYdata_Timer()
    xyTime = Timer + (XYdata.interval * .001)
    If id2 >= 4 Then id2 = 0
    id2 = id2 + 1
    For id = 1 to ubound(ballStatus)
        If ballStatus(id) = 1 Then
            baX(id, id2) = round(currentball(id).x, 2)
            baY(id, id2) = round(currentball(id).y, 2)
            bVx(id, id2) = round(currentball(id).velx, 2)
            bVy(id, id2) = round(currentball(id).vely, 2)
            TotalVel(id, id2) = (bVx(id, id2) ^2 + bVy(id, id2) ^2)
            If TotalVel(id, id2)> TotalVel(0, 0) Then TotalVel(0, 0) = int(TotalVel(id, id2) )
        End If
    Next

    id3 = id2:B2 = 2:B1 = 1
    Do
        If ballStatus(B1) = 1 and ballStatus(B2) = 1 Then
            bDistance = int((TotalVel(B1, id3) + TotalVel(B2, id3) ) ^1.04)
            If((baX(B1, id3) - baX(B2, id3) ) ^2 + (baY(B1, id3) - baY(B2, id3) ) ^2) <2800 + bDistance Then collide B1, B2:Exit Sub
        End If
        B1 = B1 + 1
        If B1 >= ballStatus(0) Then Exit Do
        If B1 >= B2 then B1 = 1:B2 = B2 + 1
    Loop

    If ballStatus(0) <= 1 Then XYdata.enabled = False

    If XYdata.interval >= 40 Then coff = True:XYdata.enabled = False
    If Timer> xyTime * 3 Then coff = True:XYdata.enabled = False
    If Timer> xyTime Then XYdata.interval = XYdata.interval + 1
End Sub

'Calculate the collision force and play sound
Dim cTime, cb1, cb2, avgBallx, cAngle, bAngle1, bAngle2

Sub Collide(cb1, cb2)
    If TotalVel(0, 0) / 1.8> cFactor Then cFactor = int(TotalVel(0, 0) / 1.8)
    avgBallx = (bvX(cb2, 1) + bvX(cb2, 2) + bvX(cb2, 3) + bvX(cb2, 4) ) / 4
    If avgBallx <bvX(cb2, id2) + .1 and avgBallx> bvX(cb2, id2) -.1 Then
        If ABS(TotalVel(cb1, id2) - TotalVel(cb2, id2) ) <.000005 Then Exit Sub
    End If
    If Timer <cTime Then Exit Sub
    cTime = Timer + .1
    GetAngle baX(cb1, id3) - baX(cb2, id3), baY(cb1, id3) - baY(cb2, id3), cAngle
    id3 = id3 - 1:If id3 = 0 Then id3 = 4
    GetAngle bVx(cb1, id3), bVy(cb1, id3), bAngle1
    GetAngle bVx(cb2, id3), bVy(cb2, id3), bAngle2
    cForce = Cint((abs(TotalVel(cb1, id3) * Cos(cAngle-bAngle1) ) + abs(TotalVel(cb2, id3) * Cos(cAngle-bAngle2) ) ) )
    If cForce <4 Then Exit Sub
    cForce = Cint((cForce) / (cFactor / nosf) )
    If cForce> nosf-1 Then cForce = nosf-1
    PlaySound("fx_collide" & cForce)
End Sub

Dim Xin, Yin, rAngle, Radit, wAngle, Pi
Pi = Round(4 * Atn(1), 6) '3.1415926535897932384626433832795

' Get angle
Sub GetAngle(Xin, Yin, wAngle)
    If Sgn(Xin) = 0 Then
        If Sgn(Yin) = 1 Then rAngle = 3 * Pi / 2 Else rAngle = Pi / 2
        If Sgn(Yin) = 0 Then rAngle = 0
        Else
            rAngle = atn(- Yin / Xin)
    End If
    If sgn(Xin) = -1 Then Radit = Pi Else Radit = 0
    If sgn(Xin) = 1 and sgn(Yin) = 1 Then Radit = 2 * Pi
    wAngle = round((Radit + rAngle), 4)
End Sub


'***********************
'   JP LED's Mini DMD
' Displays the song name
'***********************
Dim Digits(32)

DMDInit

Sub DMDInit

End Sub

Sub DispText(astr)
	Dim digit
    astr = mid(astr,1,LEN(astr) -4) 'remove the termination .mp3
	astr = UCASE(astr) 'to uppercase
	For digit = 0 to 31
		DMDDisplayChar mid(astr, digit + 1, 1), digit
	Next
End Sub


' new B2S stuff ************************************************

Dim B2SLS
Dim B2SL(16)
B2SL(0)=1
B2SL(1)=2
B2SL(2)=4
B2SL(3)=8
B2SL(4)=16
B2SL(5)=32
B2SL(6)=64
B2SL(7)=128
B2SL(8)=256
B2SL(9)=512
B2SL(10)=1024
B2SL(11)=2048
B2SL(12)=4096
B2SL(13)=8192
B2SL(14)=16384
B2SL(15)=32768


Sub B2SLed(ByVal dig, ByVal seg, ByVal b2sst)
  If b2sst=1 Then B2SLS=B2SLS+B2SL(seg)
  If seg=15 Then
     'WriteLed dig, B2SLS
     If B2SON Then Controller.B2SSetLED dig+56,B2SLS End If
     B2SLS=0
  End If
End Sub

Sub DMDDisplayChar(achar, adigit)
    If achar = "" Then achar = " "
    Select Case ASC(achar)
        Case 34:
            B2SLed adigit,0,0
            B2SLed adigit,1, 1
            B2SLed adigit,2, 0
            B2SLed adigit,3, 0
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 1
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 36:
            B2SLed adigit,0,1
            B2SLed adigit,1, 2
            B2SLed adigit,2, 1
            B2SLed adigit,3, 1
            B2SLed adigit,4, 0
            B2SLed adigit,5, 1
            B2SLed adigit,6, 1
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 1
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 1
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
       Case 39:
            B2SLed adigit,0,0
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 0
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 1
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 42:
            B2SLed adigit,0,0
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 0
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 1
            B2SLed adigit,7, 0
            B2SLed adigit,8, 1
            B2SLed adigit,9, 1
            B2SLed adigit,10, 1
            B2SLed adigit,11, 1
            B2SLed adigit,12, 1
            B2SLed adigit,13, 1
            B2SLed adigit,14, 1
            B2SLed adigit,15, 0
        Case 43:
            B2SLed adigit,0,0
            B2SLed adigit,1, 1
            B2SLed adigit,2, 0
            B2SLed adigit,3, 0
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 1
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 1
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 1
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 45:
            B2SLed adigit,0,0
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 0
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 1
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 47:
            B2SLed adigit,0,0
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 0
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 1
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 1
            B2SLed adigit,15, 0
        Case 48:
            B2SLed adigit,0,1
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 1
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 49:
            B2SLed adigit,0,0
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 0
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 50:
            B2SLed adigit,0,1
            B2SLed adigit,1, 1
            B2SLed adigit,2, 0
            B2SLed adigit,3, 1
            B2SLed adigit,4, 1
            B2SLed adigit,5, 0
            B2SLed adigit,6, 1
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 51:
            B2SLed adigit,0,1
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 1
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 1
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 52:
            B2SLed adigit,0,0
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 0
            B2SLed adigit,4, 0
            B2SLed adigit,5, 1
            B2SLed adigit,6, 1
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 53:
            B2SLed adigit,0,1
            B2SLed adigit,1, 0
            B2SLed adigit,2, 1
            B2SLed adigit,3, 1
            B2SLed adigit,4, 0
            B2SLed adigit,5, 1
            B2SLed adigit,6, 1
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 54:
            B2SLed adigit,0,1
            B2SLed adigit,1, 0
            B2SLed adigit,2, 1
            B2SLed adigit,3, 1
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 1
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 55:
            B2SLed adigit,0,1
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 0
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 56:
            B2SLed adigit,0,1
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 1
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 1
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 57:
            B2SLed adigit,0,1
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 1
            B2SLed adigit,4, 0
            B2SLed adigit,5, 1
            B2SLed adigit,6, 1
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 60:
            B2SLed adigit,0,0
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 0
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 1
            B2SLed adigit,11, 0
            B2SLed adigit,12, 1
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 61:
            B2SLed adigit,0,0
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 1
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 1
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 62:
            B2SLed adigit,0,0
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 0
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 1
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 1
            B2SLed adigit,15, 0
        Case 64:
            B2SLed adigit,0,1
            B2SLed adigit,1, 1
            B2SLed adigit,2, 0
            B2SLed adigit,3, 1
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 65:
            B2SLed adigit,0,1
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 0
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 1
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 66:
            B2SLed adigit,0,1
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 1
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 1
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 1
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 67:
            B2SLed adigit,0,1
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 1
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 68:
            B2SLed adigit,0,1
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 1
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 1
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 1
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 69:
            B2SLed adigit,0,1
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 1
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 1
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 70:
            B2SLed adigit,0,1
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 0
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 1
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 71:
            B2SLed adigit,0,1
            B2SLed adigit,1, 0
            B2SLed adigit,2, 1
            B2SLed adigit,3, 1
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 72:
            B2SLed adigit,0,0
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 0
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 1
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 73:
            B2SLed adigit,0,1
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 1
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 1
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 1
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 74:
            B2SLed adigit,0,0
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 1
            B2SLed adigit,4, 1
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 75:
            B2SLed adigit,0,0
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 0
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 1
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 1
            B2SLed adigit,11, 0
            B2SLed adigit,12, 1
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 76:
            B2SLed adigit,0,0
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 1
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 77:
            B2SLed adigit,0,0
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 0
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 1
            B2SLed adigit,9, 0
            B2SLed adigit,10, 1
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 78:
            B2SLed adigit,0,0
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 0
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 1
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 1
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 79:
            B2SLed adigit,0,1
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 1
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9,0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 80:
            B2SLed adigit,0,1
            B2SLed adigit,1, 1
            B2SLed adigit,2, 0
            B2SLed adigit,3, 0
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 1
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 81:
            B2SLed adigit,0,1
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 1
            B2SLed adigit,4, 1
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 1
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 82:
            B2SLed adigit,0,1
            B2SLed adigit,1, 1
            B2SLed adigit,2, 0
            B2SLed adigit,3, 0
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 1
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 1
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 83:
            B2SLed adigit,0,1
            B2SLed adigit,1, 0
            B2SLed adigit,2, 1
            B2SLed adigit,3, 1
            B2SLed adigit,4, 0
            B2SLed adigit,5, 1
            B2SLed adigit,6, 1
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 84:
            B2SLed adigit,0,1
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 0
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 1
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 1
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 85:
            B2SLed adigit,0,0
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 1
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 86:
            B2SLed adigit,0,0
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 0
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 1
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 1
            B2SLed adigit,15, 0
        Case 87:
            B2SLed adigit,0,0
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 0
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 1
            B2SLed adigit,13, 0
            B2SLed adigit,14, 1
            B2SLed adigit,15, 0
        Case 88:
            B2SLed adigit,0,0
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 0
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 1
            B2SLed adigit,9, 0
            B2SLed adigit,10, 1
            B2SLed adigit,11, 0
            B2SLed adigit,12, 1
            B2SLed adigit,13, 0
            B2SLed adigit,14, 1
            B2SLed adigit,15, 0
        Case 89:
            B2SLed adigit,0,0
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 0
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 1
            B2SLed adigit,9, 0
            B2SLed adigit,10, 1
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 1
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 90:
            B2SLed adigit,0,1
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 1
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 1
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 1
            B2SLed adigit,15, 0
        Case 92:
            B2SLed adigit,0,0
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 0
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 1
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 1
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 94:
            B2SLed adigit,0,0
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 0
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 1
            B2SLed adigit,13, 0
            B2SLed adigit,14, 1
            B2SLed adigit,15, 0
        Case 95:
            B2SLed adigit,0,0
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 1
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 96:
            B2SLed adigit,0,0
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 0
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 1
            B2SLed adigit,11, 1
            B2SLed adigit,12, 1
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
        Case 192:
            B2SLed adigit,0,1
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 1
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 0
            B2SLed adigit,7, 1
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 1
        Case 193:
            B2SLed adigit,0,0
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 0
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 1
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 1
        Case 194:
            B2SLed adigit,0,1
            B2SLed adigit,1, 1
            B2SLed adigit,2, 0
            B2SLed adigit,3, 1
            B2SLed adigit,4, 1
            B2SLed adigit,5, 0
            B2SLed adigit,6, 1
            B2SLed adigit,7, 1
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 1
        Case 195:
            B2SLed adigit,0,1
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 1
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 1
            B2SLed adigit,7, 1
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 1
        Case 196:
            B2SLed adigit,0,0
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 0
            B2SLed adigit,4, 0
            B2SLed adigit,5, 1
            B2SLed adigit,6, 1
            B2SLed adigit,7, 1
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 1
        Case 197:
            B2SLed adigit,0,1
            B2SLed adigit,1, 0
            B2SLed adigit,2, 1
            B2SLed adigit,3, 1
            B2SLed adigit,4, 0
            B2SLed adigit,5, 1
            B2SLed adigit,6, 1
            B2SLed adigit,7, 1
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 1
        Case 198:
            B2SLed adigit,0,1
            B2SLed adigit,1, 0
            B2SLed adigit,2, 1
            B2SLed adigit,3, 1
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 1
            B2SLed adigit,7, 1
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 1
        Case 199:
            B2SLed adigit,0,1
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 0
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 1
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 1
        Case 200:
            B2SLed adigit,0,1
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 1
            B2SLed adigit,4, 1
            B2SLed adigit,5, 1
            B2SLed adigit,6, 1
            B2SLed adigit,7, 1
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 1
        Case 201:
            B2SLed adigit,0,1
            B2SLed adigit,1, 1
            B2SLed adigit,2, 1
            B2SLed adigit,3, 1
            B2SLed adigit,4, 0
            B2SLed adigit,5, 1
            B2SLed adigit,6, 1
            B2SLed adigit,7, 1
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 1
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 1
        Case Else:
            B2SLed adigit,0,0
            B2SLed adigit,1, 0
            B2SLed adigit,2, 0
            B2SLed adigit,3, 0
            B2SLed adigit,4, 0
            B2SLed adigit,5, 0
            B2SLed adigit,6, 0
            B2SLed adigit,7, 0
            B2SLed adigit,8, 0
            B2SLed adigit,9, 0
            B2SLed adigit,10, 0
            B2SLed adigit,11, 0
            B2SLed adigit,12, 0
            B2SLed adigit,13, 0
            B2SLed adigit,14, 0
            B2SLed adigit,15, 0
    End Select
End Sub


'**************************
'PinUPPlayer
'**************************

'Dim PuPlayer
'Dim hasPUP: hasPUP=true

'Sub PinUPInit
'Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")
'PuPlayer.B2SInit "",CGameName
'end Sub

Sub PuPEvent(EventNum)
if hasPUP=false then Exit Sub

'PuPlayer.B2SData "D"&EventNum,1  'send event to puppack driver
PuPlayer.B2SData "E"&EventNum,1  'send event to puppack driver
   
End Sub

'---------- UltraDMD Unique Table Color preference -------------
Dim DMDColor, DMDColorSelect, UseFullColor
Dim DMDPosition, DMDPosX, DMDPosY, DMDSize, DMDWidth, DMDHeight 


UseFullColor = "True" '                           "True" / "False"
DMDColorSelect = "white"            ' Rightclick on UDMD window to get full list of colours

DMDPosition = True                               ' Use Manual DMD Position, True / False
DMDPosX = 2561                                  ' Position in Decimal
DMDPosY = 0                                     ' Position in Decimal

DMDSize = True                                     ' Use Manual DMD Size, True / False
DMDWidth = 1280                                    ' Width in Decimal
DMDHeight = 390                                   ' Height in Decimal 

'Note open Ultradmd and right click on window to get the various sizes in decimal 

'GetDMDColor
Sub GetDMDColor
Dim WshShell,filecheck,directory
Set WshShell = CreateObject("WScript.Shell")
If DMDSize then
WshShell.RegWrite "HKCU\Software\UltraDMD\w",DMDWidth,"REG_DWORD"
WshShell.RegWrite "HKCU\Software\UltraDMD\h",DMDHeight,"REG_DWORD"
End if
If DMDPosition then
WshShell.RegWrite "HKCU\Software\UltraDMD\x",DMDPosX,"REG_DWORD"
WshShell.RegWrite "HKCU\Software\UltraDMD\y",DMDPosY,"REG_DWORD"
End if
WshShell.RegWrite "HKCU\Software\UltraDMD\fullcolor",UseFullColor,"REG_SZ"
WshShell.RegWrite "HKCU\Software\UltraDMD\color",DMDColorSelect,"REG_SZ"
End Sub
'---------------------------------------------------
' MerlinRTP PupDMD Framework 
'********************* START OF PUPDMD FRAMEWORK v1.0 *************************
'******************** DO NOT MODIFY STUFF BELOW   THIS LINE!!!! ***************
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



Const pTopper=0
Const pDMD=1
Const pBackglass=2
Const pPlayfield=3
Const pMusic=4
Const pMusic2=5
Const pCallouts=6
Const pBackglass2=7
Const pTopper2=8
Const pPopUP=9
Const pPopUP2=10


'pages
Const pDMDBlank=0
Const pScores=1
Const pBigLine=2
Const pThreeLines=3
Const pTwoLines=4
Const pTargerLetters=5


'*************  starts PUP system,  must be called AFTER b2s/controller running so put in last line of table1_init
'****** PuP Variables ******
Const HasPuP = True   'dont set to false as it will break pup

Dim usePUP: Dim cPuPPack:  Dim PUPStatus: PUPStatus=false ' dont edit this line!!!
Dim PuPlayer: Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay") 

'*************************** PuP Settings for this table ********************************

usePUP   = False               ' enable Pinup Player functions for this table
cPuPPack = "MOTLEYCRUEPUP"    ' name of the PuP-Pack / PuPVideos folder for this table

'//////////////////// PINUP PLAYER: STARTUP & CONTROL SECTION //////////////////////////

' This is used for the startup and control of Pinup 
'************ PuP-Pack Startup **************

Sub PuPStart(cPuPPack)
    If PUPStatus=true then Exit Sub
    If usePUP=true then
        'Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")
		
        If PuPlayer is Nothing Then
            usePUP=false
            PUPStatus=false
        Else
			Dbg "Starting Pup Pack"
            PuPlayer.B2SInit "",cPuPPack 'start the Pup-Pack
            PUPStatus=true
        End If
    End If
End Sub




Sub PuPInit

	'Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")   
	'PuPlayer.B2SInit "", pGameName

	PuPlayer.LabelInit pBackglass
	PuPlayer.LabelInit pQRlocation

	pSetPageLayouts

	pSetPage(1)   'set blank text overlay page.
	if ScorbitActive = 1 Then DelayPairing

	pDMDStartUP				 ' firsttime running for like an startup video..

	'PuPlayer.LabelSet pBackglass,"FinalScore","TESTING MESSAGING",1,""
End Sub 'end PUPINIT

sub pSetPage(pagenum)    
    PuPlayer.LabelShowPage pQRlocation,pagenum,0,""   'set page to blank 0 page if want off
'    PDMDCurPage=pagenum
end Sub

sub pDMDStartUP				 ' firsttime running for like an startup video..

	if Scorbitactive then 
'		if Scorbit.DoInit(4371, "PupOverlays", myVersion, "motley-vpin") then 	' Staging
		if Scorbit.DoInit(4350, "PupOverlays", myVersion, "motley-vpin") then 	' Prod
			tmrScorbit.Interval=2000
			tmrScorbit.UserValue = 0
			tmrScorbit.Enabled=True 
			Scorbit.UploadLog = ScorbitUploadLog
		End if 
	End if 
'PuPlayer.LabelSet pBackglass,"FinalScore","TESTING MESSAGING",1,""
End Sub '

sub delayPairing
	triggerscript 2500,"CheckPairing"
end sub




sub CheckPairing

	if (Scorbit.bNeedsPairing) then 
		PuPlayer.playlistplayex pQRlocation,"PuPOverlays","PairingOverlay.png",0,1
		   PuPlayer.LabelSet pQRlocation, "ScorbitQR1", "PuPOverlays\\QRcode.png",1,"{'mt':2,'width':15.2, 'height':26.0,'xalign':0,'yalign':0,'ypos':38.25,'xpos':42.7}"
		'pLabelSetSizeImage "ScorbitQR1",13.3,24
		'pLabelSetPos "ScorbitQR1",21.4,83.7
		Dbg "Should be displaying QR Pairing Code"
		'plabelshow "ScorbitQR1"
		'plabelshow "ScorbitQRIcon1"
		DelayQRClaim.Interval=6000
		DelayQRClaim.Enabled=True
	end if
End sub

Sub hideScorbit
	if usePUP Then
		Dbg "Should be hiding QR Codes"
		PuPlayer.playlistplayex pQRlocation,"PuPOverlays","Blank.png",0,1
		plabelhide "ScorbitQR1"
		plabelhide "ScorbitQRIcon1"
		plabelhide "ScorbitQR2"
		plabelhide "ScorbitQRIcon2"
	end if
End Sub

Sub pLabelHide(labName)
	PuPlayer.LabelSet pQRlocation,labName,"",0,""  
end sub

Sub pLabelShow(labName)
	PuPlayer.LabelSet pQRlocation,labName,"",1,""   
end sub

sub pLabelSetPos(labName, xpos, ypos)
	PuPlayer.LabelSet pQRlocation,labName,"",1,"{'mt':2,'xpos':"&xpos& ",'ypos':"&ypos&"}"    
end sub

sub pLabelSetSizeImage(labName, lWidth, lHeight)
	PuPlayer.LabelSet pQRlocation,labName,"",1,"{'mt':2,'width':"& lWidth & ",'height':"&lHeight&"}" 
end sub

Sub pupCreateLabelImage(lName, lFilename,xpos, ypos, Iwidth, Iheight, pagenum, lvis)
	PuPlayer.LabelNew pQRlocation,lName ,"",50,RGB(100,100,100),0,1,1,1,1,pagenum,lvis
	PuPlayer.LabelSet pQRlocation,lName,lFilename,lvis,"{'mt':2,'width':"&IWidth&",'height':"&Iheight&",'xpos':"&xpos&",'ypos':"&ypos&"}"
end Sub

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
objIEDebugWindow.Left = 500
objIEDebugWindow.Top = 100
Do While objIEDebugWindow.Busy
Loop
objIEDebugWindow.Document.Title = "My Debug Window"
objIEDebugWindow.Document.Body.InnerHTML = "<b>CRUE Debug Window -TimeStamp: " & GameTime& "</b></br>"
End If

objIEDebugWindow.Document.Body.InnerHTML = objIEDebugWindow.Document.Body.InnerHTML & myDebugText & " --TimeStamp:<b> " & GameTime & "</b><br>" & vbCrLf
End Sub


'*****************************************************************

Sub pSetPageLayouts

Dbg "Calling psetpagelayouts"

DIM dmddef
DIM dmdalt
DIM dmdscr
DIM dmdfixed

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

	pupCreateLabelImage "ScorbitQRicon1","PuPOverlays\\QRcodeS.png",50,30,34,60,1,0
	pupCreateLabelImage "ScorbitQR1","PuPOverlays\\QRcode.png",50,30,34,60,1,0

	pupCreateLabelImage "ScorbitQRicon2","PuPOverlays\\QRcodeB.png",50,30,34,60,1,0
	pupCreateLabelImage "ScorbitQR2","PuPOverlays\\QRclaim.png",50,30,34,60,1,0

	
'	PuPlayer.LabelNew pBackglass,"FinalScore",		dmddef,	15,255	,0,1,1,50,5,1,0
End Sub




'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  SCORBIT Interface
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
	PlaySound "scorbit_login"
	pLabelHide "ScorbitQR1"
	pLabelHide "ScorbitQRIcon1"
	HideScorbit
End Sub 

Sub Scorbit_PlayerClaimed(PlayerNum, PlayerName)	' Scorbit callback when QR Is Claimed 
	PlaySound "scorbit_login"
'	PlaySoundGame "scorbit_login", 0, 1, 0 ,0, 0, 1
	ScorbitClaimQR(False)
End Sub 


Sub ScorbitClaimQR(bShow)	
	Dbg "In ScorbitClaim -----"

	if Scorbit.bSessionActive=False then Exit Sub 

	Dbg "1"
	if ScorbitShowClaimQR=False then Exit Sub

	Dbg "2"
	if Scorbit.bNeedsPairing then exit sub 

	Dbg " Here " &bShow &":" &bGameInPlay &":" &Scorbit.GetName(CurrentPlayer+1) &":"
	if bShow and BallsInGame=1 and bGameInPlay and Scorbit.GetName(CurrentPlayer+1)="" then 
		Dbg "Should be displaying Claim QR Code RTP"
		PuPlayer.playlistplayex pQRlocation,"PuPOverlays","ClaimOverlay.png",0,1
		 PuPlayer.LabelSet pQRlocation, "ScorbitQR2", "PuPOverlays\\QRcode.png",1,"{'mt':2,'width':15.2, 'height':26.0,'xalign':0,'yalign':0,'ypos':38.25,'xpos':42.7}"
		'pLabelSetSizeImage "ScorbitQR2",13.3,24
		'pLabelSetPos "ScorbitQR2",21.4,83.7
		'plabelshow "ScorbitQRIcon2"
		'plabelshow "ScorbitQR2"
	Else 
		HideScorbit
		Dbg "Should be displaying main overlay"
		PuPlayer.playlistplayex pQRlocation,"PuPOverlays","Blank.png",0,1
		Pupevent 999
		'pLabelHide "ScorbitQRIcon2"
		pLabelHide "ScorbitQR2"
	End if 
End Sub 

Sub StopScorbit
	Scorbit.StopSession Score(0), Score(1), Score(2), Score(3), PlayersPlayingGame   ' Stop updateing scores
End Sub

Sub ScorbitBuildGameModes(sMode)		' Custom function to build the game modes for better stats 
	dim GameModeStr
	if Scorbit.bSessionActive=False then Exit Sub 
	GameModeStr="NA:"

	if BallsRemaining(CurrentPlayer) <= 0 Then	'no balls left
		GameModeStr="NA{red}:YOU FAILED!!!"
	Else										'game on
		Select Case sMode
			Case "Getaway":
				GameModeStr="NA{yellow}:GETAWAY COMPLETED"
			Case "SwissBank":
				GameModeStr="NA{green}:SWISS BANK COMPLETED"
			Case "Fight":
				GameModeStr="NA{red}:FIGHT COMPLETED"
			Case "Passports":
				GameModeStr="NA{blue}:PASSPORTS COMPLETED"
			Case "Safehouse":
				GameModeStr="NA{red}:SAFEHOUSE COMPLETED"

		End Select


	End If ' endif balls remaining
	Scorbit.SetGameMode(GameModeStr)

End Sub 






' END ----------

Sub Scorbit_LOGUpload(state)	' Callback during the log creation process.  0=Creating Log, 1=Uploading Log, 2=Done 
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
			if Balls < gsBallsPerGame Then
				Scorbit.SendUpdate Score(0), Score(1), Score(2), Score(3), Balls, CurrentPlayer+1, PlayersPlayingGame
			End If
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
		' Get System UUID
		set Nads = objService.ExecQuery("SELECT * FROM Win32_ComputerSystemProduct")
		for each Nad in Nads 
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
								End if 
							End if
						End if 
					else												    ' Check for unclaim 
						if instr(1, ResponseStr, """player"":null")<>0 Then	' Someone doesnt have a name
							Parts=Split(ResponseStr,"[")						' split it 
							Parts2=Split(Parts(1),"}")							' split it 
							for i = 0 to Ubound(Parts2)
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

		bActive="false" 
		SendUpdate P1Score, P2Score, P3Score, P4Score, -1, -1, NumberPlayers
		bSessionActive=False
'		SendHeartbeat

		if bUploadLog and LogIdx<>0 and bCancel=False then 
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

	Public Sub SendUpdate(P1Score, P2Score, P3Score, P4Score, CurrentBall, CurrentPlayer, NumberPlayers)
		SendUpdateAsynch P1Score, P2Score, P3Score, P4Score, CurrentBall, CurrentPlayer, NumberPlayers, bRunAsynch
	End Sub 

	Public Sub SendUpdateAsynch(P1Score, P2Score, P3Score, P4Score, CurrentBall, CurrentPlayer, NumberPlayers, bAsynch)
		dim i
		Dim PostData
		Dim resultStr
		dim LogScores(4)

		if bUploadLog then 
			if NumberPlayers>=1 then LogScores(0)=P1Score
			if NumberPlayers>=2 then LogScores(1)=P2Score
			if NumberPlayers>=3 then LogScores(2)=P3Score
			if NumberPlayers>=4 then LogScores(3)=P4Score
			LOGFILE(LogIdx)=DateDiff("S", "1/1/1970", Now()) & "," & LogScores(0) & "," & LogScores(1) & "," & LogScores(2) & "," & LogScores(3) & ",,," &  CurrentPlayer & "," & CurrentBall & ",""" & GameModeOrig & """"
			LogIdx=LogIdx+1
		End if

		if bSessionActive=False then Exit Sub 
		if bEnabled=False then Exit Sub 
		if bWaitResp then exit sub ' Drop message until we get our next response 

'		msgbox "currentplayer: " & CurrentPlayer
		SaveCurrentPlayer=CurrentPlayer
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

			PostData = PostData & "&current_ball=" & CurrentBall & "&current_player=" & CurrentPlayer
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
		If VenueMachineID="" then
			If resultStr<>"" And Not InStr(resultStr, """machine_id"":" & machineID)=0 Then 'We Paired
				bNeedsPairing=False
				Scorbit_Paired()
			ElseIf resultStr<>"" And Not InStr(resultStr, """unpaired"":true")=0 Then 'We Did not Pair
				bNeedsPairing=True
			Else
				' Error (or not a heartbeat); do nothing
			End If

			TmpStr=GetJSONValue(resultStr, "venuemachine_id")
			if TmpStr<>"" then 
				VenueMachineID=TmpStr		
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
			rc = wsh.Run(Command, windowStyle, waitOnReturn)
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
				getStoken=True
			Else 
				getStoken=False
			End if 
		else 
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
			End If
		End With

	End Function

	Private Function pvToByteArray(sText)
		pvToByteArray = StrConv(sText, 128)		' vbFromUnicode
	End Function

End Class 
'  END SCORBIT 
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

Sub DelayQRClaim_Timer()
	if bOnTheFirstBall AND bBallInPlungerLane then ScorbitClaimQR(True)
	DelayQRClaim.Enabled=False
End Sub


Sub swPlunger_Unhit()
	bOnTheFirstBallScorbit = False
	hideScorbit
End Sub

Sub swPlunger_Hit()
	'ScorbitClaimQR(False)
	'hideScorbit 'backup call to make sure all scorbit QR codes are gone

	if bOnTheFirstBallScorbit And ScorbitActive = 1 And (Scorbit.bNeedsPairing) = false then 
		ScorbitClaimQR(True)
		Dbg "Should be showing QR code"
	End If
End Sub


'============================================================================================================
'Nail Busters Trigger Script

Dim pReset(9) 
Dim pStatement(9)           'holds future scripts
Dim FX
                                                                       
    
for fx=0 to 9
    pReset(FX)=0
    pStatement(FX)=""
next

DIM pTriggerCounter:pTriggerCounter=pTriggerScript.interval

Sub pTriggerScript_Timer()
  for fx=0 to 9  
       if pReset(fx)>0 Then    
          pReset(fx)=pReset(fx)-pTriggerCounter 
            if pReset(fx)<=0 Then
            pReset(fx)=0
            execute(pStatement(fx))
          end if     
       End if
  next
End Sub


Sub TriggerScript(pTimeMS,pScript) ' This is used to Trigger script after the pTriggerScript Timer on the playfield expires
for fx=0 to 9  
  if pReset(fx)=0 Then
    pReset(fx)=pTimeMS
    pStatement(fx)=pScript
    Exit Sub
  End If 
next
end Sub

function Balls
	Balls = gsBallsPerGame - BallsRemaining(CurrentPlayer)+1
end function