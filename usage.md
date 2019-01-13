Downloads videos, cuts each on parts and merges them.  Tries to merge all videos in one if needed. Determines length of each merged video. 
Requires installed ffmpeg and youtube-dl.

Usage: ytBatchCutter INPUTFILE

INPUTFILE format:
```
	[Config section]

	URL1
	[Description]
	[TIME PARTS]

	[URL2]
	...
```

TIME PARTS format:
```
	[TimeFrom1 TimeTo1]
	[TimeFrom2 TimeTo2]
	[...]
```
OR:
```
	[TimeFrom1 TimeTo1 DELIMITER TimeFrom2 TimeTo2 ... etc]
```

DELIMITER is any character except numbers and colons.
Time ranges defines parts that should be cut from original video and then merged.  
Order of URL, Description and time ranges can be arbitrary, but different video sections must be separated by one or more empty lines.
Time values are passed as it is to ffmpeg as `-ss TimeFrom1` and `-t TimeFrom2` parameters and must be supported by ffmpeg.

Videos are downloaded to folder '_downloaded'. Merged videos are created in folder `_concatenated`. Those folders are created in same folder as an INPUTFILE located.
INPUTFILE support # as comment line only at line start.

Length of each video groups are determined before downloading and it is written to an INPUTFILE.

Example of file:
```
	concat-all=movie.mp4

	https://www.youtube.com/watch?v=dQw4w9WgXcQ
	0 0:10
	1:12 3:40
	3:45 3:50 , 4:00 4:03 , 4:10 4:55
	Comment about video

	0:16 1:07
	https://www.youtube.com/watch?v=FPRCppkU6XU
```
Config section format:
```
	concat-all=Final video name
	youtube-dl=youtube-dl.exe `download parameters`
	ffmpeg-cut=ffmpeg.exe `each timerange processing parameters template`
	ffmpeg-concat=ffmpeg.exe `each video concatenating parameters template`
```

ffmpeg and youtube-dl parameters should contain path to ffmpeg.exe and youtube-dl.exe, respectively, if they are not specified in %PATH% variable.
If some parameter is not specified default config options will be used.
If concat-all parameter is not specified all videos will not be merged.
Empty parameter value will lead to skipping corresponding action.
