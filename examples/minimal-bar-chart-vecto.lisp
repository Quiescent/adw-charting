(with-chart (:bar 300 200)
  (add-series "Rank" '((0 10) (1 18) (2 19) (3 17)))
  (set-axis :y "Bang")
  (set-axis :x "Buck")
  (save-file "minimal-bar-chart-vecto.png"))