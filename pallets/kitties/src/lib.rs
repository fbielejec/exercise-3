#![cfg_attr(not(feature = "std"), no_std)]

use frame_support::{pallet_prelude::*, traits::Randomness};
use frame_system::pallet_prelude::*;
pub use pallet::*;
use sp_io::hashing::blake2_128;
use sp_runtime::ArithmeticError;

#[derive(Encode, Decode, Clone, RuntimeDebug, PartialEq, Eq)]
pub enum Gender {
    Male,
    Female,
}

#[derive(Encode, Decode, Clone, RuntimeDebug, PartialEq, Eq)]
pub struct Kitty {
    pub id: u32,
    pub dna: [u8; 16],
    pub gender: Gender,
}

impl Kitty {
    fn gender(dna: &[u8; 16]) -> Gender {
        if (dna[0] % 2).eq(&0) {
            Gender::Male
        } else {
            Gender::Female
        }
    }
}

#[frame_support::pallet]
pub mod pallet {
    use super::*;

    #[pallet::config]
    pub trait Config: frame_system::Config + pallet_randomness_collective_flip::Config {
        type Event: From<Event<Self>> + IsType<<Self as frame_system::Config>::Event>;
    }

    /// Stores all the kitties. Key is (user, kitty_id).
    #[pallet::storage]
    #[pallet::getter(fn kitties)]
    pub type Kitties<T: Config> = StorageDoubleMap<
        _,
        Blake2_128Concat,
        T::AccountId,
        Blake2_128Concat,
        u32,
        Kitty,
        OptionQuery,
    >;

    /// Stores the next kitty Id.
    #[pallet::storage]
    #[pallet::getter(fn next_kitty_id)]
    pub type NextKittyId<T: Config> = StorageValue<_, u32, ValueQuery>;

    #[pallet::event]
    #[pallet::generate_deposit(pub(super) fn deposit_event)]
    #[pallet::metadata(T::AccountId = "AccountId")]
    pub enum Event<T: Config> {
        /// A kitty is created. \[owner, kitty_id, kitty\]
        KittyCreated(T::AccountId, u32, Kitty),
        /// A kitty is bred. \[owner, parent1 id, parent2 id, kitty_id, kitty\]
        KittyBreed(T::AccountId, u32, u32, u32, Kitty),
    }

    #[pallet::pallet]
    #[pallet::generate_store(pub(super) trait Store)]
    pub struct Pallet<T>(_);

    fn safely_increment_kitty_id<T: Config>() -> DispatchResult {
        NextKittyId::<T>::try_mutate(|current_id| {
            let next_id = current_id.checked_add(1).ok_or(ArithmeticError::Overflow)?;
            *current_id = next_id;
            Ok(())
        })
    }

    // Generate a random number from a given seed.
    // Note that there is potential bias introduced by using modulus operator.
    // NOTE: see https://github.com/paritytech/substrate/blob/master/frame/lottery/src/lib.rs#L471
    fn fair_die<T: Config>(total: u32) -> u32 {
        let (random_seed, _) = <pallet_randomness_collective_flip::Pallet<T> as Randomness<
            T::Hash,
            T::BlockNumber,
        >>::random_seed();

        let random_number = <u32>::decode(&mut random_seed.as_ref())
            .expect("secure hashes should always be bigger than u32; qed");

        random_number % total
    }

    fn generate_dna<T: Config>(sender: T::AccountId) -> [u8; 16] {
        let payload =
            (
                <pallet_randomness_collective_flip::Pallet<T> as Randomness<
                    T::Hash,
                    T::BlockNumber,
                >>::random_seed()
                .0,
                &sender,
                <frame_system::Pallet<T>>::extrinsic_index(),
            );

        payload.using_encoded(blake2_128)
    }

    #[pallet::error]
    pub enum Error<T> {
        NotKittyOwner,
        SameGender,
        DNACancer,
    }

    #[pallet::call]
    impl<T: Config> Pallet<T> {
        /// Create a new kitty
        #[pallet::weight(1000)]
        pub fn create(origin: OriginFor<T>) -> DispatchResult {
            let sender = ensure_signed(origin)?;
            let dna = generate_dna::<T>(sender.clone());
            let id = Self::next_kitty_id();
            let gender = Kitty::gender(&dna);
            let kitty = Kitty { id, dna, gender };

            Kitties::<T>::insert(&sender, id, kitty.clone());
            safely_increment_kitty_id::<T>()?;
            Self::deposit_event(Event::KittyCreated(sender, id, kitty));

            Ok(())
        }

        /// Create a new kitty
        #[pallet::weight(1000)]
        pub fn breed(origin: OriginFor<T>, parent1: u32, parent2: u32) -> DispatchResult {
            let sender = ensure_signed(origin)?;

            // ensure ownership
            let kitty1 = Self::kitties(sender.clone(), parent1).ok_or(Error::<T>::NotKittyOwner)?;
            let kitty2 = Self::kitties(sender.clone(), parent2).ok_or(Error::<T>::NotKittyOwner)?;
            // check genders
            ensure!(kitty1.gender.ne(&kitty2.gender), Error::<T>::SameGender);

            // let mut rng = rand::thread_rng();
            // let die = Uniform::from(1..3);

            let random_dna = generate_dna::<T>(sender.clone());

            let mut dna: [u8; 16] = [0; 16];

            dna.iter_mut().enumerate().for_each(|(i, nuc)| {
                let throw = fair_die::<T>(3);
                match throw {
                    1 => *nuc = kitty1.dna[i],
                    2 => *nuc = kitty2.dna[i],
                    // random mutation
                    3 => *nuc = random_dna[i],
                    // should never get here
                    _ => panic!("Kitten got cancer"),
                };
            });

            let id = Self::next_kitty_id();
            let gender = Kitty::gender(&dna);
            let kitty = Kitty { id, dna, gender };

            Kitties::<T>::insert(&sender, id, kitty.clone());
            safely_increment_kitty_id::<T>()?;

            Self::deposit_event(Event::KittyBreed(sender, kitty1.id, kitty2.id, id, kitty));

            Ok(())
        }
    }
}
