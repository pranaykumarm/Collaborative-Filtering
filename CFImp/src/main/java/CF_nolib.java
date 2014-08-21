import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by bartimaeus on 7/29/14.
 */
public class CF_nolib {
    ArrayList<ArrayList<Integer>> access;                       // UxM matrix
    ArrayList<ArrayList<Float>> profile_u;                      // UxM matrix
    ArrayList<ArrayList<Float>> profile_m;                      // MxU matrix
    ArrayList<ArrayList<Float>> RP;                             // UxU matrix
    HashMap<String,Integer> lookup_u;                           // Ux2 table
    HashMap<String,Integer> lookup_m;                           // Mx2 table

    int U = 0;
    int M = 0;
    int U_mod = 0;
    int M_mod = 0;

    CF_nolib(){
        // Initialize storage structures access & lookup tables
        access = new ArrayList<ArrayList<Integer>>();
        ArrayList<Integer> temp = new ArrayList<Integer>();
        temp.add(0);
        access.add(temp);

        lookup_u = new HashMap<String, Integer>();
        lookup_m = new HashMap<String, Integer>();
    }

    void increment(int u, int m){
        int val;
        val = access.get(u).get(m);
        access.get(u).set(m,val+1);
    }

    int user_lookup(String user) {
        int u;
        if (!lookup_u.containsKey(user)){
            U++;
            lookup_u.put(user,U);
            u = U;
            ArrayList<Integer> temp = new ArrayList<Integer>();
            for (int i=0; i<=M; i++){
                temp.add(0);
            }
            access.add(temp);
        }
        else{
            u = lookup_u.get(user);
        }
        return u;
    }

    int mach_lookup(String mach) {
        int m;
        if (!lookup_m.containsKey(mach)){
            M++;
            lookup_m.put(mach,M);
            m = M;
            for (int i=0; i<=U; i++){
                access.get(i).add(0);
            }
        }
        else{
            m = lookup_m.get(mach);
        }
        return m;
    }

    void upd_access(String user, String mach){
        int u = user_lookup(user);
        int m = mach_lookup(mach);
        increment(u,m);
        increment(0,m);
        increment(u,0);
        increment(0,0);
    }

    void build_model(){

        // build profile_u
        profile_u = new ArrayList<ArrayList<Float>>();
        for (int u=0; u<=U; u++){
            int tot = access.get(u).get(0);
            ArrayList<Float> temp = new ArrayList<Float>();
            if (tot==0){
                for (int m=0; m<=M; m++){
                    temp.add((float) 0.0);
                }
            }
            else{
                for (int m=0; m<=M; m++){
                    temp.add((float) (access.get(u).get(m) * 1.0 / tot));
                }
            }
            profile_u.add(temp);
            U_mod = U;
        }
        System.out.println("done with profile_u");

        // build profile_m
        profile_m = new ArrayList<ArrayList<Float>>();
        for (int m=0; m<=M; m++){
            int tot = access.get(0).get(m);
            ArrayList<Float> temp = new ArrayList<Float>();
            if (tot==0){
                for (int u=0; u<=U; u++){
                    temp.add((float) 0.0);
                }
            }
            else{
                for (int u=0; u<=U; u++){
                    temp.add((float) (access.get(u).get(m)*1.0/tot));
                }
            }
            profile_m.add(temp);
            M_mod = M;
        }
        System.out.println("done with profile_m");

        // build RP
        RP = new ArrayList<ArrayList<Float>>();
        for (int u=0; u<=U; u++){
            ArrayList<Float> temp = new ArrayList<Float>();
            for (int v=0; v<=U; v++){
                float val = 0;
                for (int m=1; m<=M; m++){
                    val += profile_u.get(u).get(m)*profile_m.get(m).get(v);
                }
                temp.add(val);
            }
            RP.add(temp);
        }
        System.out.println("done with RP");
    }

    float score(String user, String mach){
        if (!lookup_u.containsKey(user) || !lookup_m.containsKey(mach)){
            return -1;
        }

        int u = user_lookup(user);
        int m = user_lookup(mach);

        float val = 0;
        for (int v=1; v<=U; v++){
            val += RP.get(u).get(v)*profile_u.get(v).get(m);
        }
        return val;
    }

    public static void main(String[] args){

        CF_nolib test = new CF_nolib();

        String user = "Pranay";
        String mach = "Caspida1";
        test.upd_access(user,mach);

        user = "Prabhav";
        test.upd_access(user,mach);

        System.out.println(test.access);
        System.out.println(test.lookup_u);
        System.out.println(test.lookup_m);

        test.build_model();

        System.out.println(test.profile_u);
        System.out.println(test.profile_m);
        System.out.println(test.RP);

        mach = "Caspida2";
        float f = test.score(user,mach);
        System.out.println(f);
        /*try {


            BufferedReader reader = new BufferedReader(new FileReader("synthetic.overlap.output-1.txt"));
            String line = null;
            int i = 0;
            while ((line = reader.readLine()) != null && i<=100000) {
                i++;
                if (i % 10000 == 0) {
                    System.out.println(i);
                }
                String[] parts = line.split(" ");
                String user = parts[0];
                String mach = parts[1];
                test.upd_access(user, mach);
            }

            System.out.println("building the model...");

            test.build_model();

            //System.out.println(test.U);
            //System.out.println(test.M);
            //System.out.println(test.lookup_u);
            //System.out.println(test.RP);

            ArrayList<Float> scores = new ArrayList<Float>();
            while ((line = reader.readLine()) != null){
                i++;
                String[] parts = line.split(" ");
                String user = parts[0];
                String mach = parts[1];
                scores.add(test.score(user, mach));
            }

            int lim = scores.size();
            float max = 0;
            int max_ind = 0;

            for (int j=0; j<998; j++){
                if (max<scores.get(j)){
                    max = scores.get(j);
                    max_ind = j;
                }
            }
            System.out.println(max);
            System.out.println(max_ind);

            float min = 1;
            float max_all = 0;
            int min_ind = 0;

            for (int j=1001; j<lim; j++){
                if (min>scores.get(j)){
                    min = scores.get(j);
                    min_ind = j;
                }
                if (max_all<scores.get(j)){
                    max_all = scores.get(j);
                }
            }
            System.out.println(min);
            System.out.println(min_ind);
            System.out.println(max_all);

        }
        catch (IOException ioe){
            ioe.printStackTrace();
        }*/
    }
}
