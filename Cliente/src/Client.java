import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import processing.core.PApplet;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.*;
import java.net.Socket;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;


public class Client extends PApplet {
    public static class Reader implements Runnable {
        Variables vars;

        public Reader(Variables vars) {
            this.vars = vars;
        }

        public Element receiveMessage() throws IOException, ParserConfigurationException, SAXException {
            String response = vars.in.readLine().trim();
            //System.out.println("Received: " + response);
            response = response.replace("\\\"", "\"");
            response = response.replaceFirst("\"", "");
            response = response.replaceFirst("\"$", "");
            System.out.println(response);
            InputStream inputStream = new ByteArrayInputStream(response.getBytes());

            // Parse the XML
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document document = builder.parse(inputStream);

            // Normalize the document
            document.getDocumentElement().normalize();
            return document.getDocumentElement();
        }

        public void run() {
            try {
                while (true) {
                    Element root = receiveMessage();
                    //System.out.println("Root element: " + root.getTagName());
                    switch (root.getTagName()) {
                        case "reply":
                            String replyText = root.getAttribute("text");
                            System.out.println("Reply: " + replyText);
                            switch (replyText) {
                                case "Login successful":
                                    System.out.println("Login successful");
                                    vars.out.println("/Lv");
                                    vars.out.flush();
                                    Element newRoot1 = receiveMessage();
                                    while(!Objects.equals(root.getTagName(), "checkLV")) {
                                        if (newRoot1.getTagName().equals("checkLV")) {
                                            String level = newRoot1.getAttribute("level");
                                            vars.Lvl = level;
                                            System.out.println("Level: " + level);
                                            break;
                                        }
                                        else {
                                            vars.out.println("/Lv");
                                            vars.out.flush();
                                            newRoot1 = receiveMessage();
                                        }
                                    }
                                    vars.out.println("/Str");
                                    vars.out.flush();
                                    Element newRoot2 = receiveMessage();
                                    while(!Objects.equals(root.getTagName(), "checkStreak")) {
                                        if (newRoot2.getTagName().equals("checkStreak")) {
                                            String streak = newRoot2.getAttribute("streak");
                                            vars.streak = streak;
                                            vars.currentScene = "MatchPage";
                                            System.out.println("Streak: " + streak);
                                            break;
                                        }
                                        else {
                                            vars.out.println("/Str");
                                            vars.out.flush();
                                            newRoot2 = receiveMessage();
                                        }
                                    }
                                    break;
                                case "Account created successfully", "Account closed successfully",
                                     "Logged out successfully":
                                    vars.currentScene = "Menu";
                                    vars.username = "";
                                    vars.password = "";
                                    break;
                                case "Username already exists":
                                    vars.username = "";
                                    vars.password = "";
                                    System.out.println("Username already exists");
                                    break;
                                case "Account closure failed":
                                    vars.username = "";
                                    vars.password = "";
                                    System.out.println("Account closure failed");
                                    break;
                                case "Searching for a match.":
                                    vars.searching = true;
                                    break;
                                case "Stopped searching for a match.":
                                    vars.searching = false;
                                    break;
                                case "Match found!":
                                    System.out.println("Match found!2");
                                    vars.searching = false;
                                    vars.currentScene = "GamePage";
                                    break;
                                case "You win!", "You lose!", "Draw!":
                                    vars.out.println("/Lv");
                                    vars.out.flush();
                                    Element newRoot3 = receiveMessage();
                                    while(!Objects.equals(root.getTagName(), "checkLV")) {
                                        if (newRoot3.getTagName().equals("checkLV")) {
                                            String level = newRoot3.getAttribute("level");
                                            vars.Lvl = level;
                                            vars.currentScene = "MatchPage";
                                            System.out.println("Level: " + level);
                                            break;
                                        }
                                        else {
                                            vars.out.println("/Lv");
                                            vars.out.flush();
                                            newRoot3 = receiveMessage();
                                        }
                                    }
                                    vars.out.println("/Str");
                                    vars.out.flush();
                                    Element newRoot4 = receiveMessage();
                                    while(!Objects.equals(root.getTagName(), "checkStreak")) {
                                        if (newRoot4.getTagName().equals("checkStreak")) {
                                            String streak = newRoot4.getAttribute("streak");
                                            vars.streak = streak;
                                            vars.currentScene = "MatchPage";
                                            System.out.println("Streak: " + streak);
                                            break;
                                        }
                                        else {
                                            vars.out.println("/Str");
                                            vars.out.flush();
                                            newRoot4 = receiveMessage();
                                        }
                                    }
                                    break;
                                case "Invalid command":
                                    System.out.println("Invalid command");
                                    break;
                                default:
                                    System.out.println(replyText);
                                    break;
                            }
                            break;
                        case "leaderboard":
                            NodeList ldplayers = root.getElementsByTagName("user");
                            vars.leaderboard.clear();
                            for (int i = 0; i < ldplayers.getLength(); i++) {
                                Element player = (Element) ldplayers.item(i);
                                String username = player.getAttribute("username");
                                String level = player.getAttribute("level");
                                String streak = player.getAttribute("streak");
                                vars.leaderboard.add(new String[]{username, level, streak});
                                System.out.println("Username: " + username + ", Level: " + level + ", Streak: " + streak);
                            }
                            break;
                        case "gamedata":
                            NodeList players = root.getElementsByTagName("player1");
                            if (players.getLength() > 0) {
                                Element player1 = (Element) players.item(0);
                                vars.px1 = Float.parseFloat(player1.getAttribute("x"));
                                vars.py1 = Float.parseFloat(player1.getAttribute("y"));
                                vars.pt1 = Integer.parseInt(player1.getAttribute("score"));
                                NodeList projectiles1 = player1.getElementsByTagName("projectile");
                                vars.projectiles1.clear();
                                for (int i = 0; i < projectiles1.getLength(); i++) {
                                    Element projectile = (Element) projectiles1.item(i);
                                    String xAttr = projectile.getAttribute("x");
                                    String yAttr = projectile.getAttribute("y");
                                    if (!xAttr.isEmpty() && !yAttr.isEmpty()) {
                                        float x = Float.parseFloat(xAttr);
                                        float y = Float.parseFloat(yAttr);
                                        vars.projectiles1.add(new float[]{x, y});
                                    }
                                }
                            }

                            players = root.getElementsByTagName("player2");
                            if (players.getLength() > 0) {
                                Element player2 = (Element) players.item(0);
                                vars.px2 = Float.parseFloat(player2.getAttribute("x"));
                                vars.py2 = Float.parseFloat(player2.getAttribute("y"));
                                vars.pt2 = Integer.parseInt(player2.getAttribute("score"));
                                NodeList projectiles2 = player2.getElementsByTagName("projectile");
                                vars.projectiles2.clear();
                                for (int i = 0; i < projectiles2.getLength(); i++) {
                                    Element projectile = (Element) projectiles2.item(i);
                                    String xAttr = projectile.getAttribute("x");
                                    String yAttr = projectile.getAttribute("y");
                                    if (!xAttr.isEmpty() && !yAttr.isEmpty()) {
                                        float x = Float.parseFloat(xAttr);
                                        float y = Float.parseFloat(yAttr);
                                        vars.projectiles2.add(new float[]{x, y});
                                    }
                                }
                            }
                            NodeList Clock = root.getElementsByTagName("clock");
                            Element Clock1 = (Element) Clock.item(0);
                            String time = Clock1.getAttribute("time");
                            //System.out.println("Clock: " + time);

                            System.out.println("Clock: " + time);
                            vars.time = Integer.parseInt(time);

                            NodeList modifiers = root.getElementsByTagName("modifier");
                            vars.CDMods.clear();
                            vars.SPMods.clear();
                            for (int i = 0; i < modifiers.getLength(); i++) {
                                Element modifier = (Element) modifiers.item(i);
                                String xAttr = modifier.getAttribute("x");
                                String yAttr = modifier.getAttribute("y");
                                String valueAttr = modifier.getAttribute("value");
                                String typeAttr = modifier.getAttribute("type");
                                if (!xAttr.isEmpty() && !yAttr.isEmpty()) {
                                    float x = Float.parseFloat(xAttr);
                                    float y = Float.parseFloat(yAttr);
                                    float value = Float.parseFloat(valueAttr);
                                    if (typeAttr.equals("CD")) {
                                        vars.CDMods.add(new float[]{x, y, value});
                                    } else if (typeAttr.equals("SP")) {
                                        vars.SPMods.add(new float[]{x, y, value});
                                    }
                                }
                            }

                            //System.out.println("Player1: (" + vars.px1 + ", " + vars.py1 + ")");
                            //System.out.println("Player2: (" + vars.px2 + ", " + vars.py2 + ")");
                            break;
                        default:
                            break;

                    }
                }
            } catch (IOException e) {
                logger.log(Level.SEVERE, "Erro ao ler do servidor", e);
            } catch (ParserConfigurationException | SAXException e) {
                throw new RuntimeException(e);
            }
        }
    }

    public static class Variables {
        String anterior_Scene;
        String currentScene;
        String username;
        String password;
        boolean typingUsername;
        boolean typingPassword;
        boolean ignoreFirstClick;
        boolean searching;
        Socket socket;
        ReadVar out;
        BufferedReader in;
        String Lvl;
        String streak;
        float px1, py1;
        float px2, py2;
        int pt1, pt2;
        CopyOnWriteArrayList<float[]> projectiles1; // List for player1's projectiles
        CopyOnWriteArrayList<float[]> projectiles2; // List for player2's projectiles
        CopyOnWriteArrayList<float[]> CDMods;
        CopyOnWriteArrayList<float[]> SPMods;
        CopyOnWriteArrayList<String[]> leaderboard;
        int time;

        public Variables(int Port) {
            this.currentScene = "Menu";
            this.username = "";
            this.password = "";
            this.typingUsername = true;
            this.typingPassword = false;
            this.ignoreFirstClick = false;
            this.searching = false;
            this.Lvl = "";
            this.streak = "";
            this.px1 = 0;
            this.py1 = 0;
            this.pt1 = 0;
            this.px2 = 0;
            this.py2 = 0;
            this.pt2 = 0;
            this.projectiles1 = new CopyOnWriteArrayList<>();
            this.projectiles2 = new CopyOnWriteArrayList<>();
            this.CDMods = new CopyOnWriteArrayList<>();
            this.SPMods = new CopyOnWriteArrayList<>();
            this.leaderboard = new CopyOnWriteArrayList<>();
            try {
                //String ip = "188.37.73.48";
                String ip = "localhost";
                this.socket = new Socket(ip, Port);
                this.in = new BufferedReader(new InputStreamReader(this.socket.getInputStream()));
                this.out = new ReadVar(socket);
            } catch (IOException e) {
                logger.log(Level.SEVERE, "Erro na comunicação com o servidor", e);
            }
        }
    }

    public static class ReadVar {
        PrintWriter out;
        Lock lock = new ReentrantLock();

        public ReadVar(Socket socket) throws IOException {
            this.out = new PrintWriter(socket.getOutputStream(), true);
        }

        public void println(String message) {
            lock.lock();
            try {
                out.println(message);
            } finally {
                lock.unlock();
            }
        }

        public void flush() {
            lock.lock();
            try {
                out.flush();
            } finally {
                lock.unlock();
            }
        }
    }

    static Variables vars;
    int baseWidth = 800;
    int baseHeight = 800;
    float scaleFactorX;
    float scaleFactorY;

    public void settings() {
        size(baseWidth, baseHeight);
    }

    public void setup() {
        size(baseWidth, baseHeight);
        scaleFactorX = (float) width / baseWidth;
        scaleFactorY = (float) height / baseHeight;
        smooth();
        surface.setResizable(true);
        textSize(16);
        textAlign(CENTER, CENTER);
        Thread readerThread = new Thread(new Reader(vars));
        readerThread.start();
    }

    public void draw() {
        background(0);

        switch (vars.currentScene) {
            case "Login":
                drawLogin();
                break;
            case "CreateAccount":
                drawCreateAccount();
                break;
            case "MatchPage":
                drawMatchPage();
                break;
            case "GamePage":
                drawGamePage();
                break;
            case "Leaderboard":
                drawLeaderboard();
                break;
            case "DeleteAccount":
                drawDeleteAccount();
                break;
            default:
                drawMenu();
                break;
        }
    }

    private void drawMenu() {
        fill(255);
        textSize(32);
        textAlign(CENTER, CENTER);
        text("Menu", width * 0.5f, height * 0.4f);
        textSize(16);

        fill(100, 200, 100);
        rect(width * 0.4f, height * 0.45f, width * 0.2f, height * 0.05f, 10);
        fill(0);
        text("Login", width * 0.5f, height * 0.475f);

        fill(100, 200, 100);
        rect(width * 0.4f, height * 0.55f, width * 0.2f, height * 0.05f, 10);
        fill(0);
        text("Criar Conta", width * 0.5f, height * 0.575f);

        fill(100, 200, 100);
        rect(width * 0.4f, height * 0.65f, width * 0.2f, height * 0.05f, 10);
        fill(0);
        text("Leaderboard", width * 0.5f, height * 0.675f);

        fill(200, 100, 100);
        rect(width * 0.4f, height * 0.75f, width * 0.2f, height * 0.05f, 10);
        fill(0);
        text("Eliminar Conta", width * 0.5f, height * 0.775f);
    }

    private void drawLogin() {
        fill(255);
        textSize(32);
        text("Login", width * 0.5f, height * 0.3f);
        textSize(16);

        // Username box
        fill(vars.typingUsername ? 200 : 150);
        rect(width * 0.35f, height * 0.4f, width * 0.3f, height * 0.05f, 10);
        fill(0);
        if (vars.username.isEmpty()) {
            fill(100); // Darker faded-out color
            textAlign(CENTER, CENTER);
            text("Enter username", width * 0.5f, height * 0.425f);
        } else {
            fill(0);
            text(vars.username, width * 0.5f, height * 0.425f);
        }

        // Password box
        fill(vars.typingPassword ? 200 : 150);
        rect(width * 0.35f, height * 0.5f, width * 0.3f, height * 0.05f, 10);
        fill(0);
        if (vars.password.isEmpty()) {
            fill(100); // Darker faded-out color
            textAlign(CENTER, CENTER);
            text("Enter password", width * 0.5f, height * 0.525f);
        } else {
            fill(0);
            text("*".repeat(vars.password.length()), width * 0.5f, height * 0.525f);
        }

        // Login button
        fill(100, 200, 100);
        rect(width * 0.4f, height * 0.6f, width * 0.2f, height * 0.05f, 10);
        fill(0);
        text("Login", width * 0.5f, height * 0.625f);

        // Back button
        fill(200, 100, 100);
        rect(width * 0.4f, height * 0.7f, width * 0.2f, height * 0.05f, 10);
        fill(0);
        text("Voltar", width * 0.5f, height * 0.725f);
    }

    private void drawDeleteAccount() {
        fill(255);
        textSize(32);
        text("Eliminar conta", width * 0.5f, height * 0.3f);
        textSize(16);

        // Username box
        fill(vars.typingUsername ? 200 : 150);
        rect(width * 0.35f, height * 0.4f, width * 0.3f, height * 0.05f, 10);
        fill(0);
        if (vars.username.isEmpty()) {
            fill(100); // Darker faded-out color
            textAlign(CENTER, CENTER);
            text("Enter username", width * 0.5f, height * 0.425f);
        } else {
            fill(0);
            text(vars.username, width * 0.5f, height * 0.425f);
        }

        // Password box
        fill(vars.typingPassword ? 200 : 150);
        rect(width * 0.35f, height * 0.5f, width * 0.3f, height * 0.05f, 10);
        fill(0);
        if (vars.password.isEmpty()) {
            fill(100); // Darker faded-out color
            textAlign(CENTER, CENTER);
            text("Enter password", width * 0.5f, height * 0.525f);
        } else {
            fill(0);
            text("*".repeat(vars.password.length()), width * 0.5f, height * 0.525f);
        }

        // Login button
        fill(100, 200, 100);
        rect(width * 0.4f, height * 0.6f, width * 0.2f, height * 0.05f, 10);
        fill(0);
        text("Eliminar conta", width * 0.5f, height * 0.625f);

        // Back button
        fill(200, 100, 100);
        rect(width * 0.4f, height * 0.7f, width * 0.2f, height * 0.05f, 10);
        fill(0);
        text("Voltar", width * 0.5f, height * 0.725f);
    }

    private void drawCreateAccount() {
        fill(255);
        textSize(32);
        text("Criar Conta", width * 0.5f, height * 0.3f);
        textSize(16);

        // Username box
        fill(vars.typingUsername ? 200 : 150);
        rect(width * 0.35f, height * 0.4f, width * 0.3f, height * 0.05f, 10);
        fill(0);
        if (vars.username.isEmpty()) {
            fill(100); // Darker faded-out color
            textAlign(CENTER, CENTER);
            text("Enter username", width * 0.5f, height * 0.425f);
        } else {
            fill(0);
            text(vars.username, width * 0.5f, height * 0.425f);
        }

        // Password box
        fill(vars.typingPassword ? 200 : 150);
        rect(width * 0.35f, height * 0.5f, width * 0.3f, height * 0.05f, 10);
        fill(0);
        if (vars.password.isEmpty()) {
            fill(100); // Darker faded-out color
            textAlign(CENTER, CENTER);
            text("Enter password", width * 0.5f, height * 0.525f);
        } else {
            fill(0);
            text("*".repeat(vars.password.length()), width * 0.5f, height * 0.525f);
        }

        // Create Account button
        fill(100, 200, 100);
        rect(width * 0.4f, height * 0.6f, width * 0.2f, height * 0.05f, 10);
        fill(0);
        text("Criar Conta", width * 0.5f, height * 0.625f);

        // Back button
        fill(200, 100, 100);
        rect(width * 0.4f, height * 0.7f, width * 0.2f, height * 0.05f, 10);
        fill(0);
        text("Voltar", width * 0.5f, height * 0.725f);
    }

    private void drawMatchPage() {
        fill(255);
        textSize(32);
        textAlign(CENTER, CENTER); // Ensure text alignment is centered
        text("User: " + vars.username, width * 0.5f, height * 0.3f);

        textSize(16);
        text("Level: " + vars.Lvl, width * 0.5f, height * 0.35f);

        textSize(16);
        text("Streak: " + vars.streak, width * 0.5f, height * 0.4f);

        fill(100, 200, 100);
        rect(width * 0.4f, height * 0.5f, width * 0.2f, height * 0.05f, 10);
        fill(0);
        text(vars.searching ? "Searching" : "Match", width * 0.5f, height * 0.525f);

        fill(200, 100, 100);
        rect(width * 0.4f, height * 0.7f, width * 0.2f, height * 0.05f, 10);
        fill(0);
        text("Logout", width * 0.5f, height * 0.725f);

        // Botão Leaderboard
        fill(100, 200, 100);
        rect(width * 0.4f, height * 0.6f, width * 0.2f, height * 0.05f, 10);
        fill(0);
        text("Leaderboard", width * 0.5f, height * 0.625f);

        //Botao delete conta
    }

    private void drawGamePage() {
        float scaleFactorX = (float) width / baseWidth;
        float scaleFactorY = (float) height / baseHeight;

        // Desenhar as bordas a branco
        noFill();
        stroke(255);
        rect(50 * scaleFactorX, 100 * scaleFactorY, width - 100 * scaleFactorX, height - 200 * scaleFactorY);
        noStroke();

        pushMatrix();
        translate((width - baseWidth * scaleFactorX) / 2, (height - baseHeight * scaleFactorY) / 2);
        scale(scaleFactorX, scaleFactorY);  // Scale the coordinate system

        // Draw Player 1
        fill(0, 0, 255);
        ellipse(vars.px1, vars.py1, 50, 50);

        // Draw Player 2
        fill(255, 0, 0);
        ellipse(vars.px2, vars.py2, 50, 50);

        for (float[] projectile : vars.projectiles1) {
            fill(0, 255, 0);
            ellipse(projectile[0], projectile[1], 10, 10);
        }

        for (float[] projectile : vars.projectiles2) {
            fill(255, 255, 0);
            ellipse(projectile[0], projectile[1], 10, 10);
        }

        for (float[] mod : vars.CDMods) {
            if(mod[2] > 0) {
                fill(255, 165, 0);
                ellipse(mod[0], mod[1], 20, 20);
            }else
            {
                fill(0, 255, 0);
                ellipse(mod[0], mod[1], 20, 20);
            }
        }

        for (float[] mod : vars.SPMods) {
            if(mod[2] > 0) {
                //draw blue circle
                fill(0, 0, 255);
                ellipse(mod[0], mod[1], 20, 20);
            }else
            {
                //draw red circle
                fill(255, 0, 0);
                ellipse(mod[0], mod[1], 20, 20);
            }
        }
        popMatrix();

        fill(255);
        textSize(20);
        textAlign(LEFT, TOP);
        text("Player 1 Points: " + vars.pt1, 10, 10);
        text("Player 2 Points: " + vars.pt2, 10, 40);

        // Exibir o tempo do jogo
        int totalSeconds = vars.time / 1000;
        int minutos = totalSeconds / 60;
        int segundos = totalSeconds % 60;
        textAlign(CENTER,TOP);
        text(String.format("Tempo: %02d:%02d", minutos, segundos), (float) width / 2, 70);

    }

    private void drawLeaderboard() {
        fill(255);
        textSize(32);
        textAlign(CENTER, TOP);
        text("Leaderboard", width * 0.5f, 50);

        // Definições da tabela
        textSize(20);
        float y = 120;
        float rowHeight = 30;
        float headerHeight = 35;
        float col1 = width * 0.1f;
        float col2 = width * 0.45f;
        float col3 = width * 0.7f;
        float col4 = width * 0.9f;

        // Cabeçalhos
        fill(200);
        textAlign(CENTER, TOP);
        text("User", (col1 + col2) / 2, y);
        text("Level", (col2 + col3) / 2, y);
        text("Streak", (col3 + col4) / 2, y);

        // Desenhar linhas horizontais e verticais da tabela
        stroke(180);
        strokeWeight(2);
        // Linha superior do cabeçalho
        line(col1, y - 5, col4, y - 5);
        // Linha inferior do cabeçalho
        line(col1, y + headerHeight, col4, y + headerHeight);
        // Linhas verticais
        line(col1, y - 5, col1, y + headerHeight + vars.leaderboard.size() * rowHeight);
        line(col2, y - 5, col2, y + headerHeight + vars.leaderboard.size() * rowHeight);
        line(col3, y - 5, col3, y + headerHeight + vars.leaderboard.size() * rowHeight);
        line(col4, y - 5, col4, y + headerHeight + vars.leaderboard.size() * rowHeight);

        // Linhas e dados dos jogadores
        float rowY = y + headerHeight;
        fill(255);
        textAlign(CENTER, TOP);
        for (String[] player : vars.leaderboard) {
            // Linhas horizontais entre as linhas
            stroke(180);
            line(col1, rowY, col4, rowY);

            noStroke();
            text(player[0], (col1 + col2) / 2, rowY + 5);
            text(player[1], (col2 + col3) / 2, rowY + 5);
            text(player[2], (col3 + col4) / 2, rowY + 5);

            rowY += rowHeight;
        }
        // Linha inferior da tabela
        stroke(180);
        line(col1, rowY, col4, rowY);
        noStroke();

        // Botão de voltar
        fill(200, 100, 100);
        rect(width * 0.4f, height * 0.85f, width * 0.2f, height * 0.05f, 10);
        fill(0);
        text("Voltar", width * 0.5f, height * 0.865f);
    }

    public void mousePressed() {
        if (vars.currentScene.equals("Menu")) {
            // Check "Login" button
            if (mouseX > width * 0.4f && mouseX < width * 0.6f &&
                    mouseY > height * 0.45f && mouseY < height * 0.5f) {
                vars.currentScene = "Login";
                vars.typingUsername = false;
                vars.typingPassword = false;
                vars.ignoreFirstClick = true;
            }

            // Check "Create Account" button
            else if (mouseX > width * 0.4f && mouseX < width * 0.6f &&
                    mouseY > height * 0.55f && mouseY < height * 0.6f) {
                vars.currentScene = "CreateAccount";
                vars.typingUsername = false;
                vars.typingPassword = false;
                vars.ignoreFirstClick = true;
            }
            // Check "Leaderboard" button
            else if (mouseX > width * 0.4f && mouseX < width * 0.6f &&
                    mouseY > height * 0.65f && mouseY < height * 0.7f) {
                vars.currentScene = "Leaderboard";
                vars.anterior_Scene = "Menu";
                vars.out.println("/ld");
                vars.out.flush();
            }
            //Check "Delete Account" button
            else if (mouseX > width * 0.4f && mouseX < width * 0.6f &&
                    mouseY > height * 0.75f && mouseY < height * 0.8f) {
                vars.currentScene = "DeleteAccount";
                vars.typingUsername = false;
                vars.typingPassword = false;
                vars.ignoreFirstClick = true;
            }
        }

        if (vars.currentScene.equals("Login")) {
            if (vars.ignoreFirstClick) {
                vars.ignoreFirstClick = false;
                return;
            }
            vars.typingUsername = false;
            vars.typingPassword = false;

            // "Back" button
            if (mouseX > width * 0.4f && mouseX < width * 0.6f &&
                    mouseY > height * 0.7f && mouseY < height * 0.75f) {
                vars.currentScene = "Menu";
                vars.username = "";
                vars.password = "";
            }
            // "Login" button
            else if (mouseX > width * 0.4f && mouseX < width * 0.6f &&
                    mouseY > height * 0.6f && mouseY < height * 0.65f) {
                if (!vars.username.isEmpty() && !vars.password.isEmpty()) {
                    vars.out.println("/l " + vars.username + " " + vars.password);
                    vars.out.flush();
                }
            }
            // Username box
            else if (mouseX > width * 0.35f && mouseX < width * 0.65f &&
                    mouseY > height * 0.4f && mouseY < height * 0.45f) {
                vars.typingUsername = true;
            }
            // Password box
            else if (mouseX > width * 0.35f && mouseX < width * 0.65f &&
                    mouseY > height * 0.5f && mouseY < height * 0.55f) {
                vars.typingPassword = true;
            }
        }

        if (vars.currentScene.equals("CreateAccount")) {
            if (vars.ignoreFirstClick) {
                vars.ignoreFirstClick = false;
                return;
            }
            vars.typingUsername = false;
            vars.typingPassword = false;

            // "Back" button
            if (mouseX > width * 0.4f && mouseX < width * 0.6f &&
                    mouseY > height * 0.7f && mouseY < height * 0.75f) {
                vars.currentScene = "Menu";
                vars.username = "";
                vars.password = "";
            }
            // "Create Account" button
            else if (mouseX > width * 0.4f && mouseX < width * 0.6f &&
                    mouseY > height * 0.6f && mouseY < height * 0.65f) {
                if (!vars.username.isEmpty() && !vars.password.isEmpty()) {
                    vars.out.println("/cr " + vars.username + " " + vars.password);
                    vars.out.flush();
                }
            }
            // Username box
            else if (mouseX > width * 0.35f && mouseX < width * 0.65f &&
                    mouseY > height * 0.4f && mouseY < height * 0.45f) {
                vars.typingUsername = true;
            }
            // Password box
            else if (mouseX > width * 0.35f && mouseX < width * 0.65f &&
                    mouseY > height * 0.5f && mouseY < height * 0.55f) {
                vars.typingPassword = true;
            }
        }

        if (vars.currentScene.equals("MatchPage")) {
            if (vars.ignoreFirstClick) {
                vars.ignoreFirstClick = false;
                return;
            }
            // "Logout" button
            if (mouseX > width * 0.4f && mouseX < width * 0.6f &&
                    mouseY > height * 0.7f && mouseY < height * 0.75f) {
                vars.out.println("/q");
                vars.out.flush();
            }
            // "Match" button
            else if (mouseX > width * 0.4f && mouseX < width * 0.6f &&
                    mouseY > height * 0.5f && mouseY < height * 0.55f && !vars.searching) {
                vars.out.println("/f");
                vars.out.flush();
            }
            else if (mouseX > width * 0.4f && mouseX < width * 0.6f &&
                    mouseY > height * 0.6f && mouseY < height * 0.65f) {
                vars.out.println("/ld");
                vars.out.flush();
                vars.anterior_Scene = "MatchPage";
                vars.currentScene = "Leaderboard";
            }
            else if (mouseX > width * 0.4f && mouseX < width * 0.6f &&
                    mouseY > height * 0.5f && mouseY < height * 0.55f && vars.searching) {
                vars.out.println("/stop");
                vars.out.flush();
            }
        }

        if (vars.currentScene.equals("Leaderboard")) {
            // Botão Voltar
            if (mouseX > width * 0.4f && mouseX < width * 0.6f &&
                    mouseY > height * 0.85f && mouseY < height * 0.9f) {
                    if (Objects.equals(vars.anterior_Scene, "MatchPage")) {
                    vars.currentScene = "MatchPage";
                } else {
                        vars.currentScene = "Menu";
                }
            }
        }
        if (vars.currentScene.equals("DeleteAccount")){
            if (vars.ignoreFirstClick) {
                vars.ignoreFirstClick = false;
                return;
            }
            vars.typingUsername = false;
            vars.typingPassword = false;
            // "Back" button
            if (mouseX > width * 0.4f && mouseX < width * 0.6f &&
                    mouseY > height * 0.7f && mouseY < height * 0.75f) {
                vars.currentScene = "Menu";
                vars.username = "";
                vars.password = "";
            }
            // "Delete Account" button
            else if (mouseX > width * 0.4f && mouseX < width * 0.6f &&
                    mouseY > height * 0.6f && mouseY < height * 0.65f) {
                if (!vars.username.isEmpty() && !vars.password.isEmpty()) {
                    vars.out.println("/cl " + vars.username + " " + vars.password);
                    vars.out.flush();
                }
            }
            // Username box
            else if (mouseX > width * 0.35f && mouseX < width * 0.65f &&
                    mouseY > height * 0.4f && mouseY < height * 0.45f) {
                vars.typingUsername = true;
            }
            // Password box
            else if (mouseX > width * 0.35f && mouseX < width * 0.65f &&
                    mouseY > height * 0.5f && mouseY < height * 0.55f) {
                vars.typingPassword = true;
            }
        }
    }

    public void keyPressed() {
        if (vars.currentScene.equals("Login") || vars.currentScene.equals("CreateAccount") || vars.currentScene.equals("DeleteAccount")) {
            if (vars.typingUsername) {
                if (key == BACKSPACE && !vars.username.isEmpty()) {
                    vars.username = vars.username.substring(0, vars.username.length() - 1);
                } else if (key != BACKSPACE && key != ENTER && key != TAB && key != CODED) {
                    vars.username += key;
                } else if (key == TAB) {
                    vars.typingUsername = false;
                    vars.typingPassword = true;
                }
                println("Username: " + vars.username);
            } else if (vars.typingPassword) {
                if (key == BACKSPACE && !vars.password.isEmpty()) {
                    vars.password = vars.password.substring(0, vars.password.length() - 1);
                } else if (key != BACKSPACE && key != ENTER && key != TAB && key != CODED) {
                    vars.password += key;
                } else if (key == TAB) {
                    vars.typingPassword = false;
                    vars.typingUsername = true;
                }
                println("Password: " + vars.password);
            }
        }
        if (vars.currentScene.equals("GamePage")) {
            if (key == ' ') {
                vars.out.println("/m space");
                vars.out.flush();
            } else if (key == 'q') {
                vars.out.println("/s " + mouseX * (width/baseWidth) + " " + mouseY * (height/baseHeight));
                vars.out.flush();
            } else {
                vars.out.println("/m " + Character.toLowerCase(key));
                vars.out.flush();
            }
        }
    }

    private static final Logger logger = Logger.getLogger(Client.class.getName());

    public static void main(String[] args) {
        vars = new Variables(Integer.parseInt(args[0]));
        PApplet.main("Client");
    }
}