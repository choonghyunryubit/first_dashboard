result <- getTable("TB_D_KPIS")

daily_traffic <- result |> 
  filter(PLATFORM %in% "IWL") |> 
  select(DATE, MONTH, N_USER_D1, N_NEWUSER_D1, N_SESSION_D1, 
         N_NEWSESSION_D1, N_SCREEN_D1, TOT_TIMEONSITE_D1)
names(daily_traffic) <- tolower(names(daily_traffic))  

write.csv(daily_traffic, "/Users/choonghyunryu/Documents/03_edcation/first_dashboard/data/daily_traffic.csv", row.names = FALSE)


result <- getTable("TB_D_HITS")

daily_hits <- result |> 
  filter(PLATFORM %in% "IWL") |> 
  select(DATE, SCREEN_NAME, N_SCREEN, N_SESSION, N_DURATION, 
         N_EXIT, TOT_DURATION, SCREEN_PER_SESSION)
names(daily_hits) <- tolower(names(daily_hits))  


daily_hits <- daily_hits |> 
  group_by(screen_name) |> 
  summarise(tot = sum(n_screen)) |>
  arrange(desc(tot)) |> 
  head(30) |> 
  inner_join(daily_hits |> 
               filter(date >= "2021-01-01") |> 
               filter(date <= "2021-01-05"), by = "screen_name") |> 
  select(-tot)

daily_hits <- daily_hits |> 
  mutate(screen_name = ifelse(screen_name %in% "메인메뉴(회원)", "메인메뉴", screen_name)) |> 
  mutate(screen_name = ifelse(screen_name %in% "나의 계약정보" , "내정보", screen_name)) |> 
  mutate(screen_name = ifelse(screen_name %in% "메뉴", "메뉴 04", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "계약상세(계약)", "메뉴 32", screen_name)) |>  
  mutate(screen_name = ifelse(screen_name %in% "메인>상품찾기",  "상품찾기", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "메인>나의계약(로그인)", "메뉴 02", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "메인>전체메뉴(로그인)", "메뉴 06", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "대출안내화면(대출내역조회)", "메뉴 01", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "신청 및 안내", "메뉴 03", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "보험료 조회/납입", "메뉴 11", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "본인확인", "메뉴 23", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "대출내역조회 리스트", "메뉴 13", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "로그인 공인인증서", "메뉴 00", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "인증센터 공인인증서 등록", "인증센터", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "본인인증(휴대폰)", "본인인증", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "인증센터 공인인증서 등록", "로그인", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "인증센터 공인인증서 등록", "메뉴 16", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "동의 및 고객정보입력", "메뉴 54", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "메인(비회원)", "메뉴 38", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "회원가입 본인 확인", "메뉴 22", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "청구내용입력", "메뉴 33", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "상환방식/금액입력", "메뉴 44", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "대출내역 리스트", "메뉴 55", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "인증방법 선택", "메뉴 66", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "대출받을 금액 입력", "메뉴 77", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "서류 촬영 안내", "메뉴 99", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "메인>나의계약(비회원)", "메뉴 88", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "청구내용입력", "메뉴 76", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "나의자동이체정보", "메뉴 56", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "대출내역조회 상세", "메뉴 46", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "대출내역조회 상세", "메뉴 36", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "메인>전체메뉴(비회원)", "메뉴 87", screen_name)) |>
  mutate(screen_name = ifelse(screen_name %in% "메인(회원)", "장바구니", screen_name)) 
  
write.csv(daily_hits, "/Users/choonghyunryu/Documents/03_edcation/first_dashboard/data/daily_hits.csv", row.names = FALSE)

