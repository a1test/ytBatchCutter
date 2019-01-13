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

`DELIMITER` is any character except numbers, colons and dots.
Time ranges defines parts that should be cut from original video and then merged.  
Order of URL, Description and time ranges can be arbitrary, but different video sections must be separated by one or more empty lines.
Time values are passed as it and must be supported by ffmpeg.

Videos are downloaded to folder `_downloads` which created in `INPUTFILE` parent folder.
`INPUTFILE` supports `#` as comment line only at line start.

Length of each video groups are determined before downloading and it is written to an `INPUTFILE`.

Example of file:
```
	output=movie.mp4

	https://www.youtube.com/watch?v=dQw4w9WgXcQ
	0.5 0:10.5
	1:12 3:40
	3:45 3:50 , 4:00 4:03 , 4:10 4:55
	Comment about video

	0:16 1:07
	https://www.youtube.com/watch?v=FPRCppkU6XU
```

`ffmpeg` and `youtube-dl` parameters should contain path to ffmpeg.exe and youtube-dl.exe, respectively, if they are not specified in %PATH% variable.
If some parameter is not specified the default value will be used. For view parameters info run program without parameters.
If `output` parameter is not specified the `INPUTFILE` name will be used as output file name.
Empty action-parameter value will lead to skipping corresponding action.
