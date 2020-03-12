#/usr/bin/env bash

_gen_comp() {
  COMPREPLY=($(compgen -W "$1" -- "$2"))
}

_prep() {
  cmd=${COMP_WORDS[1]}
  subcmd=${COMP_WORDS[2]}
  cur=${COMP_WORDS[COMP_CWORD]}

  case "$COMP_CWORD" in
    1)
      _gen_comp "add add-box edit list-boxes move-down rate review remove show" "$cur"
      ;;

    2)
      case "$cmd" in
        rate)
          _gen_comp "bad again good easy" "$cur";;

        show | edit | remove | move-down)
          _gen_comp "$(prep complete-ids)" "$cur";;
      esac;;

    3)
      case "$subcmd" in
        bad | again | good | easy)
          _gen_comp "$(prep complete-ids)" "$cur";;
      esac

  esac

}

complete -F _prep prep
