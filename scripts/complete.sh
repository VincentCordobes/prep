#/usr/bin/env bash

_gen_comp() {
  COMPREPLY=($(compgen -W "$1" -- "$2"))
}


_complete_ids () {
  ids=""
  for id in $(prep complete-ids); do
    # idk why compgen removes my escape :(
    x="$(printf '%q' "$id")"
    x="$(printf '%q' "$x")"
    ids="$x $ids"
  done

  _gen_comp "$(echo $ids)" "$1"
}

_prep() {
  cmd=${COMP_WORDS[1]}
  subcmd=${COMP_WORDS[2]}
  cur=${COMP_WORDS[COMP_CWORD]}

  case "$COMP_CWORD" in
    1)
      _gen_comp "add add-file add-box edit list-boxes move-down rate review remove show" "$cur"
      ;;

    2)
      case "$cmd" in
        rate)
          _gen_comp "bad again good easy" "$cur";;

        show | edit | remove | move-down)
          _complete_ids "$cur"
      esac;;

    3)
      case "$subcmd" in
        bad | again | good | easy)
          _complete_ids "$cur"
      esac

  esac

}



complete -F _prep prep
