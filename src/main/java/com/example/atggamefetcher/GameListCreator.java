package com.example.atggamefetcher;

import com.example.atggamefetcher.pojo.GameForJson;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public class GameListCreator {

    List<Game> todayGames;
    List<Game> leftOverGames;
    public List<LocalDateTime> bigGameDates;
    public List<String> bigGameTypes;

    public GameListCreator() throws IOException {
        todayGames = new ArrayList<>();
        bigGameDates = readBigGameDates();
        bigGameTypes = readBigGameTypes();
    }

    public List<Game> getGamesIncludingBigGames(List<GameForJson> gameForJsons) {
        todayGames = new ArrayList<>();
        for(int i = 0; i < gameForJsons.size(); i++) {
            GameForJson game = gameForJsons.get(i);
            //Set gameDay to start of day
            game.setStart(game.getStart().toLocalDate().atStartOfDay());
            if(bigGameDates.contains(game.getStart()) ) {
                todayGames.add(new Game(game.getName(), game.getType(), game.getStart(), true));
            } else if (bigGameTypes.contains(game.getType())) {
                if(game.getType().equalsIgnoreCase("V86") && game.getStart().getDayOfWeek() == DayOfWeek.WEDNESDAY) {
                    todayGames.add(new Game(game.getName(), game.getType(), game.getStart(), true));
                } else if (game.getType().equalsIgnoreCase("V75") && game.getStart().getDayOfWeek() == DayOfWeek.SATURDAY) {
                    todayGames.add(new Game(game.getName(), game.getType(), game.getStart(), true));
                } else if (game.getType().equalsIgnoreCase("GS75") && game.getStart().getDayOfWeek() == DayOfWeek.SUNDAY) {
                    todayGames.add(new Game(game.getName(), game.getType(), game.getStart(), true));
                } else {
                    todayGames.add(new Game(game.getName(), game.getType(), game.getStart(), false));
                }

            } else {
                todayGames.add(new Game(game.getName(), game.getType(), game.getStart(), false));
            }
        }
        return todayGames;
    }




    public List<LocalDateTime> readBigGameDates() throws IOException {
        String[] bigGameDates = readCommaSeperatedFile("src/main/resources/bigGameDates.csv");
        List<LocalDateTime> bigGameDatesList = new ArrayList<>();
        for (String bigGameDate : bigGameDates) {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("uuuu-MM-dd");
            LocalDateTime date = LocalDate.parse(bigGameDate, formatter).atStartOfDay();
            bigGameDatesList.add(date);
        }
        return bigGameDatesList;
    }

    public List<String> readBigGameTypes() throws IOException {
        String[] bigGameTypes = readCommaSeperatedFile("src/main/resources/bigGameTypes.csv");
        List<String> bigGameTypesList = new ArrayList<>();
        Collections.addAll(bigGameTypesList, bigGameTypes);
        return bigGameTypesList;
    }

    public String[] readCommaSeperatedFile(String path) throws IOException {
        Path filePath = Path.of(path);
        String content = Files.readString(filePath);
        return content.split(",");
    }

    public List<Game> addTodaysGames(List<Game> games) {
        todayGames = games.stream()
            .filter(game -> game.getStart().getDayOfYear()== LocalDateTime.now().getDayOfYear())
            .sorted(Comparator.comparing(Game::getStart))
            .collect(Collectors.toList());
        return todayGames;
    }

    public List<Game> addBigGames(List<Game> games) {
        List<Game> bigGames = games.stream()
            .filter(game -> Boolean.TRUE.equals(game.getBigGame()))
            .sorted(Comparator.comparing(Game::getStart)).toList();
        todayGames.addAll(bigGames);
        return bigGames;
    }

    public List<Game> addLeftOverGames(List<Game> games) {
        leftOverGames = games.stream()
            .filter(game -> game.getStart().getDayOfYear()!= LocalDateTime.now().getDayOfYear())
            .filter(game -> Boolean.FALSE.equals(game.getBigGame()))
            .sorted(Comparator.comparing(Game::getStart)).toList();
        todayGames.addAll(leftOverGames);
        return leftOverGames;
    }
}