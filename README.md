# 🙇‍♂️ week-focus



This parses a timetable of the shape: 


|              | Lun. | Mar. | Mer. | Jeu. | Ven. | Sam. | Dim. |
|--------------|------|------|------|------|------|------|------|
| side-project | x    | x    | x    | x    | x    |      |      |
| reading      |      |      |      | x    | x    | x    |      |
| dothis       | x    | x    | x    |      |      |      |      |
| dothat       | x    | x    | x    | x    | x    | x    | x    |


and ouputs items by day:


```
Monday
------
dothat
dothis
side-project

Tuesday
-------
dothat
dothis
side-project
etc...
```

Or vim snippets like:
```
snippet focuslun
- [ ] dothat
- [ ] dothis
- [ ] side-project
endsnippet
snippet focusmar
- [ ] dothat
- [ ] dothis
- [ ] side-project
endsnippet

etc..
```


