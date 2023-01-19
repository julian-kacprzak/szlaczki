
#' Funkcja pomocnicza, przechowuje symbole ASCII

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
#' @param hlinie Liczba, Co ile linii ma sie pojawiac horyzontalna linia wypelniona symbolami. Jezeli zostanie podane hlinie = 0 lub hlinie = 1, wszystkie linie beda wypelnione.
#' @param vlinie Liczba, Co ile linii ma sie pojawiac wertykalna linia wypelniona symbolami. Jezeli zostanie podane vlinie = 0 lub vlinie = 1, wszystkie linie beda wypelnione.
#' @param tekst_r Liczba, w ktorym rzedzie ma sie pojawic tekst, domyslnie wybierany jest srodkowy rzad.
#' @param tekst_k Liczba, w ktorej kolumnie ma sie pojawic tekst, domyslnie wybierana jest pierwsza kolumna.
#' @param druk Logiczna, Czy wydrukowac szlaczek do konsoli.
#'
#' @return Szlaczek skopiowany do schowka (Windows) badz wydrukowany do konsoli.

szlaczki <- function(tekst, wys = 7, szer = 79, symbole = szlaczki:::marks(),
                     hlinie = 0, vlinie = 0, tekst_r = round((wys + 1)/2),
                     tekst_k = 3, druk = FALSE, ...) {

  # Sprawdzenie argumentow
  if (!missing(tekst)) checkmate::assert_string(tekst)
  checkmate::assert_int(wys, lower = 1)
  checkmate::assert_int(szer, lower = 1)
  checkmate::assert_vector(symbole)
  checkmate::assert_int(hlinie, lower = 0)
  checkmate::assert_int(vlinie, lower = 0)
  checkmate::assert_int(tekst_r, lower = 1)
  checkmate::assert_int(tekst_k, lower = 1)
  checkmate::assert_logical(druk)

  # Podzbior symboli
  if (checkmate::test_number(symbole)) {
    symbole <- sample(szlaczki:::marks(), symbole, replace = F)
  }

  # Macierz pelna symboli
  s <- sample(symbole, wys*(szer + 1), replace = T, ...)
  m <- matrix(s, wys, szer + 1)

  # Linie
  if (hlinie > 0 & vlinie > 0){
    m[-seq(1, wys, hlinie), -seq(3, szer, vlinie)] <- " "
  } else if (hlinie > 0) {
    m[-seq(1, wys, hlinie), ] <- " "
  } else if (vlinie > 0) {
    m[ , -seq(3, szer, vlinie)] <- " "
  }

  # Tekst
  if (!missing(tekst)) {
    t <- c(" ", " ", strsplit(tekst, "")[[1]], " ", " ")

    for (i in tekst_k:(tekst_k + length(t) - 1)) {
      m[tekst_r - 1, i] <- " "
      m[tekst_r, i] <- t[i - tekst_k + 1]
      if (tekst_r != wys) m[tekst_r + 1, i] <- " "
    }
  }

  # Hashtagi komentarzy
  m[, 1] <- "#"
  m[, 2] <- " "

  # Nowe linie
  m[, szer + 1] <- "\n"

  if (druk) cat(paste0(c(t(m)), collapse = ""))
  clipr::write_clip(paste0(c(t(m)), collapse = ""))
}
