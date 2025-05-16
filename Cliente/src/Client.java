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
                                case "Searching for a match...":
                                    vars.searching = true;
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
                                    break;
                                case "Invalid command":
                                    System.out.println("Invalid command");
                                    break;
                                default:
                                    System.out.println(replyText);
                                    break;
                            }
                            break;
                        case "checkLV":
                            String level = root.getAttribute("level");
                            vars.Lvl = level;
                            vars.currentScene = "MatchPage";
                            System.out.println("Level: " + level);
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
        int time;

        public Variables() {
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
            try {
                this.socket = new Socket("192.168.1.218", Integer.parseInt("8000"));
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

    Variables vars = new Variables();
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
            default:
                drawMenu();
                break;
        }
    }

    private void drawMenu() {
        fill(255);
        textSize(32);
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

        fill(100, 200, 100);
        rect(width * 0.4f, height * 0.5f, width * 0.2f, height * 0.05f, 10);
        fill(0);
        text(vars.searching ? "Searching" : "Match", width * 0.5f, height * 0.525f);

        fill(200, 100, 100);
        rect(width * 0.4f, height * 0.6f, width * 0.2f, height * 0.05f, 10);
        fill(0);
        text("Logout", width * 0.5f, height * 0.625f);
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
                    mouseY > height * 0.6f && mouseY < height * 0.65f) {
                vars.out.println("/q");
                vars.out.flush();
            }
            // "Match" button
            else if (mouseX > width * 0.4f && mouseX < width * 0.6f &&
                    mouseY > height * 0.5f && mouseY < height * 0.55f) {
                vars.out.println("/f");
                vars.out.flush();
            }
        }
    }

    public void keyPressed() {
        if (vars.currentScene.equals("Login") || vars.currentScene.equals("CreateAccount")) {
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
        PApplet.main("Client");
    }
}