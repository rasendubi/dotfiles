function showqr
  tr -d '\n' | qrencode -t png -o - | feh -
end
