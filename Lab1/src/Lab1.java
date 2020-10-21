import TSim.*;
import java.util.concurrent.Semaphore;
import java.util.HashMap;

/**
* @Author Olle Westerlund, Tobias Engblom
*/
public class Lab1 {
  private HashMap<String, Semaphore> semaphoreMap = new HashMap<>();
  
  /**
  * @param speed1 The speed of the first train.
  * @param speed2 The speed of the second train.
  */
  public Lab1(int speed1, int speed2) {
    initSemaphores();
    Train train1 = new Train(1, speed1, Direction.DOWN, semaphoreMap);
    Train train2 = new Train(2, speed2, Direction.UP, semaphoreMap);
    
    train1.start();
    train2.start();
  }
  
  /**
  * The method creates all the semaphores needed and adds them to the hashMap.
  */
  private void initSemaphores() {
    semaphoreMap.put("fourWay", new Semaphore(1));
    semaphoreMap.put("one", new Semaphore(1));
    semaphoreMap.put("two", new Semaphore(1));
    semaphoreMap.put("three", new Semaphore(1));
    semaphoreMap.put("four", new Semaphore(1));
    semaphoreMap.put("five", new Semaphore(1));
  }
}


class Train extends Thread {
  private int id;
  private int speed;
  private Direction direction;
  private TSimInterface tsi = TSimInterface.getInstance();
  private HashMap<String, Semaphore> semaphoreMap;
  private String prevSensor = "";
  
  /**
  * @param id The id number of the train.
  * @param speed The speed for the train.
  * @param direction The starting direction of the train.
  * @param semaphoreMap The hashMap with all the semaphores.
  */
  public Train (int id, int speed, Direction direction, HashMap<String, Semaphore> semaphoreMap) {
    this.id = id;
    this.speed = speed;
    this.direction = direction;
    this.semaphoreMap = semaphoreMap;
  }
  
  
  @Override
  public void run() {
    try {
      while (true) {
        tsi.setSpeed(id, speed);
        SensorEvent event = tsi.getSensor(id);
        if (event.getStatus() == SensorEvent.ACTIVE) {
          onSensorEvent(event);
        }
      }
    } catch (Exception e) {
      e.printStackTrace();
      System.exit(1);
    }
  }
  
  /**
  * The method will always try to use the shortest path if possible, otherwise it will change the switch and 
  * take the other track. The shortest path is where the semaphores are placed. 
  * If a path (critical section) is blocked by a train the other one will stop and wait until the path is clear and 
  * then continue.
  * @param event The sensor event that the train activated.
  */
  private void onSensorEvent(SensorEvent event) throws Exception {
    int sensorXPos = event.getXpos();
    int sensorYPos = event.getYpos();
    String sensorName = getSensorName(sensorXPos, sensorYPos);
    
    switch (sensorName) {
      case "TTS1":
      if (tryAcquire("one")) {
        break;
      } 
      stopAtStation();
      break;
      case "TTS9":
      if (tryAcquire("five")) {
        break;
      }
      stopAtStation();
      break;
      case "BTS1":
      case "BTS7":
      stopAtStation();
      break;
      case "TTS2":
      if (direction.equals(Direction.UP)) {
        tryRelease("fourWay");
      } else {
        acquireSemaphore("fourWay");
      }
      this.prevSensor = sensorName;
      break;
      case "TTS3":
      if (direction.equals(Direction.UP)) {
        tryRelease("two");
        acquireSemaphore("fourWay");
      } else {
        tryRelease("fourWay");
        acquireSemaphore("two");
        setSwitch(SwitchPos.A, "LEFT");
      }
      this.prevSensor = sensorName;
      break;
      case "TTS4":
      if (prevSensor.equals("TTS5")) {
        tryRelease("three");
      }
      if (direction.equals(Direction.UP)) {
        if (tryAcquire("one")) {
          setSwitch(SwitchPos.A, "LEFT");
        } else {
          setSwitch(SwitchPos.A, "RIGHT");
        }
      } else {
        if (tryAcquire("three")) {
          setSwitch(SwitchPos.B, "LEFT");
        } else {
          setSwitch(SwitchPos.B, "RIGHT");
        }
        if (prevSensor.equals("TTS3")) {
          tryRelease("one");
        }
      }
      this.prevSensor = sensorName;
      break;
      case "TTS5":
      if (direction.equals(Direction.UP)) {
        acquireSemaphore("two");
        setSwitch(SwitchPos.B, "LEFT");
      } else {
        tryRelease("two");
      }
      this.prevSensor = sensorName;
      break;
      case "TTS6":
      if (direction.equals(Direction.UP)) {
        tryRelease("four");
      } else {
        acquireSemaphore("four");
        setSwitch(SwitchPos.C, "RIGHT");
      }
      this.prevSensor = sensorName;
      break;
      case "TTS7":
      if (direction.equals(Direction.UP)) {
        if (prevSensor.equals("TTS8")) {
          tryRelease("five");
        }
        if (tryAcquire("three")) {
          setSwitch(SwitchPos.C, "RIGHT");
        } else {
          setSwitch(SwitchPos.C, "LEFT");
        }
      } else {
        if (prevSensor.equals("TTS6")) {
          tryRelease("three");
        }
        if (tryAcquire("five")) {
          setSwitch(SwitchPos.D, "RIGHT");
        } else {
          setSwitch(SwitchPos.D, "LEFT");
        }
      }
      this.prevSensor = sensorName;
      break;
      case "TTS8":
      if (direction.equals(Direction.UP)) {
        acquireSemaphore("four");
        setSwitch(SwitchPos.D, "RIGHT");
      } else {
        tryRelease("four");
      }
      this.prevSensor = sensorName;
      break;
      case "BTS2":
      if (direction.equals(Direction.UP)) {
        tryRelease("fourWay");
      } else {
        acquireSemaphore("fourWay");
      }
      this.prevSensor = sensorName;
      break;
      case "BTS3":
      if (direction.equals(Direction.UP)) {
        tryRelease("two");
        acquireSemaphore("fourWay");
      } else {
        tryRelease("fourWay");
        acquireSemaphore("two");
        setSwitch(SwitchPos.A, "RIGHT");
      }
      this.prevSensor = sensorName;
      break;
      case "BTS4":
      if (direction.equals(Direction.UP)) {
        acquireSemaphore("two");
        setSwitch(SwitchPos.B, "RIGHT");
      } else {
        tryRelease("two");
      }
      this.prevSensor = sensorName;
      break;
      case "BTS5":
      if (direction.equals(Direction.UP)) {
        tryRelease("four");
      } else {
        acquireSemaphore("four");
        setSwitch(SwitchPos.C, "LEFT");
      }
      this.prevSensor = sensorName;
      break;
      case "BTS6":
      if (direction.equals(Direction.UP)) {
        acquireSemaphore("four");
        setSwitch(SwitchPos.D, "LEFT");
      } else {
        tryRelease("four");
      }
      this.prevSensor = sensorName;
      break;
      default:
      break;
    }
  }
  
  /**
  * The method takes the x- and y-coordinates of a sensor and returns a name of that sensor.
  * @param xPos The x-coordinate of the sensor.
  * @param yPos The y-coordinate of the sensor.
  * @return Returns the name of the sensor on the position.
  */
  private String getSensorName(int xPos, int yPos) {
    int value = (xPos * 10) + yPos;
    switch (value) {
      case 143:
      return "TTS1";
      case 66:
      return "TTS2";
      case 127:
      return "TTS3";
      case 198:
      return "TTS4";
      case 139:
      return "TTS5";
      case 69:
      return "TTS6";
      case 20:
      return "TTS7";
      case 61:
      return "TTS8";
      case 151:
      return "TTS9";
      case 145:
      return "BTS1";
      case 95:
      return "BTS2";
      case 128:
      return "BTS3";
      case 140:
      return "BTS4";
      case 70:
      return "BTS5";
      case 53:
      return "BTS6";
      case 153:
      return "BTS7";
      default:
      return "Error on sensor position.";
    }
  }
  
  /**
  * The method will try to acqurie a specific semaphore, if the path is blocked the 
  * train will stop and wait until the semaphore is free.
  * @param key The key to the hashMap for a specific semaphore.
  */
  private void acquireSemaphore(String key) throws InterruptedException {
    int currentSpeed = this.speed;
    Semaphore currentSemaphore = semaphoreMap.get(key);
    
    stopTrain();
    
    currentSemaphore.acquire();
    if (this.speed == 0) {
      this.speed = currentSpeed;
    }
  }
  
  private boolean tryAcquire(String key) {
    return semaphoreMap.get(key).tryAcquire();
  }
  
  /**
  * The method will try to release a semaphore.
  * @param key The key to the hashMap for a specific semaphore.
  */
  private void tryRelease(String key) {
    Semaphore semaphore = semaphoreMap.get(key);
    if (semaphore.availablePermits() < 1) {
      semaphore.release();
    }
  }
  
  /**
  * The method will stop the train and then wait a short time before changing 
  * direction of the train.
  */
  private void stopAtStation() throws Exception {
    int currentSpeed = this.speed; 
    stopTrain();
    int waitTime = 1000 + (20 * Math.abs(speed));
    Thread.sleep(waitTime);
    
    if (this.direction == Direction.UP) {
      this.direction = Direction.DOWN;
    } else {
      this.direction = Direction.UP;
    }
    this.speed = -currentSpeed;
  }
  
  /**
  * The method will stop the train.
  */
  private void stopTrain() {
    try {
      tsi.setSpeed(this.id, 0);
    } catch (CommandException e) {
      e.printStackTrace();
    }
  }
  
  /**
  * The method changes the position of the switch 
  * @param position The position of the switch.
  * @param changeFrom From witch position of the switch the train want to change from.
  */
  private void setSwitch(SwitchPos position, String changeFrom) throws CommandException {
    int xPos = 0;
    int yPos = 0;
    int dir = 0;
    
    if (changeFrom.equals("LEFT")) {
      dir = TSimInterface.SWITCH_RIGHT;
    } else if (changeFrom.equals("RIGHT")) {
      dir = TSimInterface.SWITCH_LEFT;
    } else {
      System.out.println("Bug in setSwitch.");
    }
    
    switch (position) {
      case A:
      xPos = 17;
      yPos = 7;
      break;
      case B:
      xPos = 15;
      yPos = 9;
      break;
      case C:
      xPos = 4;
      yPos = 9;
      break;
      case D:
      xPos = 3;
      yPos = 11;
      break;
      default:
      break;
    }
    
    if (dir > 0 && xPos > 0 && yPos > 0) {
      tsi.setSwitch(xPos, yPos, dir);
    }
  }
}

/**
* The direction of the train.
*/
enum Direction {
  UP,
  DOWN;
}

/**
* The postion of the switch.
*/
enum SwitchPos {
  A,
  B,
  C,
  D;
}