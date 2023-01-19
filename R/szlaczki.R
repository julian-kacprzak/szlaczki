
marks <- function() {
  c("`", "~", "!", "@", "#", "$", "%", "^", "&", "*", "(", ")", "-", "_", "=",
    "+", "[", "{", "]", "}", "\\", "|", ";", ":", "\"", ",", "<", ".", ">", "/",
    "?", " ")
}

#' Robi szlaczki
#'
#' @export
#'
#' @param tekst String, Tekst do umieszczenia w szlaczku - znajdzie sie na środku po lewo.
#' @param wys Liczba, Wysokosc szlaczka - domyslnie 7.
#' @param szer Liczba, Szerokosc szlaczka - domyslnie 79.
#' @param symbole Wektor symboli do napisania szlaczka - domyslnie zbior wszystkich symboli ASCII. Jezeli zostanie podana liczba n, wybranych zostanie losowo n symboli sposrod wszystkich symboli ASCII.
#' @param linie Liczba, Co ile linii ma sie pojawiac linia wypelniona symbolami. Jezeli zostanie podana linie = 1 lub wartosc logiczna FALSE, wszystkie linie beda wypelnione.
#' @param druk Logiczna, Czy wydrukowac szlaczek do konsoli.
#'
#' @return Szlaczek skopiowany do schowka (Windows) badz wydrukowany do konsoli.

szlaczki <- function(tekst, wys = 7, szer = 79, symbole = szlaczki:::marks(), linie = 2,
                     druk = FALSE, ...) {

  if (checkmate::test_number(symbole)) {
    symbole <- sample(szlaczki:::marks(), symbole, replace = F)
  }

  # Macierz pelna symboli
  s <- sample(symbole, wys*(szer + 1), replace = T, ...)
  m <- matrix(s, wys, szer + 1)

  # Linie
  if (linie) {
    m[-seq(1, wys, linie), ] = " "
  }

  # Hashtagi komentarzy
  m[, 1] = "#"
  m[, 2] = " "

  # Nowe linie
  m[, szer + 1] = "\n"

  # Tekst
  if (!missing(tekst)) {
    t <- c(" ", " ", strsplit(tekst, "")[[1]], " ", " ")

    for (i in 1:length(t)) {
      r <- ceiling((nrow(m) + 1)/2)
      m[r - 1, i + 2] = " "
      m[r, i + 2] = t[i]
      m[r + 1, i + 2] = " "
    }
  }

  if (druk) cat(paste0(c(t(m)), collapse = ""))
  writeClipboard(paste0(c(t(m)), collapse = ""))
}
